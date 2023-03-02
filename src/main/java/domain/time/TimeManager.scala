package domain.time

import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.{ActorRef, Behavior}
import domain.news.NewsManager.{CheckVideoSubs, PublishNews}
import domain.news.NewsManager

import java.util.{Calendar, Date}
import scala.concurrent.duration.{Duration, MINUTES, SECONDS}

object TimeManager {

  trait TickMessage
  case class Tick() extends TickMessage
  case class TickSubsCheck() extends TickMessage

  def apply(newsPublisher: ActorRef[NewsManager.NewsManagerMessages]): Behavior[TickMessage] = {
    Behaviors.withTimers(timers => {
      timers.startTimerWithFixedDelay(Tick(), Duration.Zero, Duration(1, SECONDS))
      timers.startTimerWithFixedDelay(TickSubsCheck(), Duration.Zero, Duration(15, MINUTES))
      Behaviors.receiveMessage({
        case Tick() =>
          val c = Calendar.getInstance()
          val period = {
            if (c.getTime.getHours == 9 && c.getTime.getMinutes == 0 && c.getTime.getSeconds == 0){
              "morning"
            }
            else if (c.getTime.getHours == 15 && c.getTime.getMinutes == 0 && c.getTime.getSeconds == 0){
              "day"
            }
            else if (c.getTime.getHours == 21 && c.getTime.getMinutes == 0 && c.getTime.getSeconds == 0){
              "evening"
            }
            else{
              "unknown"
            }
          }
          if (period != "unknown"){
            newsPublisher ! PublishNews(period)
          }
          Behaviors.same
        case TickSubsCheck() =>
          newsPublisher ! CheckVideoSubs()
          Behaviors.same
      })
    })
  }
}
