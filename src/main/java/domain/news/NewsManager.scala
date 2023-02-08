package domain.news

import akka.actor.typed.Behavior
import akka.actor.typed.scaladsl.Behaviors

import java.time.{LocalDate, ZoneId}
import java.util.{Calendar, Date}

object NewsManager extends NewsHelper {

  case class Post(id: String, date: Long, user: String, publishDate: Long, publishPeriod: String, header: String, url: String, imageUrl: String, status: String = "new")

  case class PublishNews(period: String)

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
                  setPostStatus(p.id, "archive")
              }
            })
          }
          Behaviors.same
      }
  }
}
