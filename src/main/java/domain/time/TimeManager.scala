package domain.time

import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.{ActorRef, Behavior}
import domain.news.NewsManager.PublishNews
import domain.news.NewsManager

import java.util.{Calendar, Date}
import scala.concurrent.duration.{Duration, SECONDS}

object TimeManager {

  case class Tick()

  def apply(newsPublisher: ActorRef[NewsManager.PublishNews]): Behavior[Tick] = {
    Behaviors.withTimers(timers => {
      timers.startTimerWithFixedDelay(Tick(), Duration.Zero, Duration(1, SECONDS))
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
          //println(c.getTime.getSeconds)
          Behaviors.same
      })
    })
  }
}
