package domain.news

import akka.actor.typed.Behavior
import akka.actor.typed.scaladsl.Behaviors

import java.time.{LocalDate, ZoneId}
import java.util.{Calendar, Date}

object NewsManager extends NewsHelper {

  case class Post(id: String, date: Long, user: String, publishDate: Long, publishPeriod: String, header: String, url: String, imageUrl: String, status: String = "new")
  case class VideoNews(id: String, date: Long, user: String, publishDate: Long, publishPeriod: String, kind: String, url: String, status: String = "new")

  case class PublishNews(period: String)

  case class NewsPack(by: String, kz: String, cn: String, ru: String, ins1: String, ins2: String, ins3: String, ins4: String)

  def apply(): Behavior[PublishNews] = Behaviors.receive {
    (context, message) =>
      message match {
        case PublishNews(period: String) =>
          if (period != "unknown"){
            getPosts.filter(p => p.publishPeriod == period && new Date(p.publishDate).toInstant.atZone(ZoneId.systemDefault()).toLocalDate.toString == LocalDate.now().toString).foreach(p => {
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
            videoNews.filter(p => p.publishPeriod == period && new Date(p.publishDate).toInstant.atZone(ZoneId.systemDefault()).toLocalDate.toString == LocalDate.now().toString).foreach(p => {
              p.status match {
                case "allow" =>
                  videoNews.find(x => x.kind == p.kind && x.status == "published") match {
                    case Some(publishNews) => setNewsStatus(publishNews.id, "archive-published")
                    case _ => None
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
}
