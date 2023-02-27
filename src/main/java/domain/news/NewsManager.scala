package domain.news

import akka.actor.typed.Behavior
import akka.actor.typed.scaladsl.Behaviors
import java.util.Calendar

object NewsManager extends NewsHelper {

  case class Post(id: String, date: Long, user: String, publishDay: Int, publishMonth: Int, publishYear: Int, publishPeriod: String, header: String, url: String, imageUrl: String, status: String = "new")
  case class VideoNews(id: String, date: Long, user: String, publishDay: Int, publishMonth: Int, publishYear: Int, publishPeriod: String, kind: String, url: String, status: String = "new")

  case class PublishNews(period: String)

  case class NewsPack(by: String, kz: String, cn: String, ru: String, ins1: String, ins2: String, ins3: String, ins4: String)

  def apply(): Behavior[PublishNews] = Behaviors.receive {
    (context, message) =>
      message match {
        case PublishNews(period: String) =>
          if (period != "unknown"){
            getPosts.filter(p => p.publishPeriod == period && isToday(p.publishDay, p.publishMonth, p.publishYear)).foreach(p => {
              p.status match {
                case "allow" =>
                  publishPost(p)
                  setPostStatus(p.id, "published")
                case "deny" =>
                  setPostStatus(p.id, "archive-denied")
                case _ =>
                  setPostStatus(p.id, "archive-skipped")
              }
            })
            val videoNews = getVideoNews
            videoNews.filter(p => p.publishPeriod == period && isToday(p.publishDay, p.publishMonth, p.publishYear)).foreach(p => {
              p.status match {
                case "allow" =>
                  if (p.kind != "ins"){
                    videoNews.find(x => x.kind == p.kind && x.status == "published") match {
                      case Some(publishNews) => setNewsStatus(publishNews.id, "archive-published")
                      case _ => None
                    }
                  }
                  setNewsStatus(p.id, "published")
                case "deny" =>
                  setNewsStatus(p.id, "archive-denied")
                case _ =>
                  setNewsStatus(p.id, "archive-skipped")
              }
            })
          }
          Behaviors.same
      }
  }

  def isToday(pDay: Int, pMonth: Int, pYear: Int): Boolean = {
    val c = Calendar.getInstance()
    val day = c.get(Calendar.DATE)
    val month = c.get(Calendar.MONTH)
    val year = c.get(Calendar.YEAR)
    day == pDay && month == pMonth && year == pYear
  }
}
