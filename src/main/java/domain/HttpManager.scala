package domain

import akka.Done
import akka.actor.typed.ActorSystem
import akka.actor.typed.scaladsl.Behaviors
import akka.http.scaladsl.Http
import akka.http.scaladsl.model.Multipart.BodyPart
import akka.http.scaladsl.model.{ContentTypes, HttpEntity, Multipart}
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import akka.stream.scaladsl.{FileIO, Sink}
import ch.megard.akka.http.cors.scaladsl.CorsDirectives.cors
import com.mongodb.client.model.Updates.set
import domain.auth.AuthManager.checkAuth
import domain.database.DBManager
import domain.news.NewsManager
import domain.news.NewsManager.Post
import domain.time.TimeManager
import io.circe.generic.auto._
import io.circe.syntax.EncoderOps
import org.mongodb.scala.model.Filters.{and, equal}
import sttp.client3.{Request, SimpleHttpClient, UriContext, asStringAlways, basicRequest, multipartFile}

import java.io.File
import java.util.UUID
import scala.concurrent.duration.{Duration, SECONDS}
import scala.concurrent.{Await, ExecutionContextExecutor, Future}
import scala.io.StdIn

object HttpManager extends AppProps{

  def apply(): Unit ={
    implicit val system: ActorSystem[Any] = ActorSystem[Any](Behaviors.setup(context => {
      val newsManager = context.spawn(NewsManager(), "newsPublisher")
      val timeManager = context.spawn(TimeManager(newsManager), "timeManager")
      Behaviors.empty
    }), "http")

    implicit val executionContext: ExecutionContextExecutor = system.executionContext

    val route: Route = cors(){
      concat(
        (get & path("login") & parameter("login") & parameter("pass") & parameter("token")) { (login, pass, token) =>
          complete(HttpEntity(ContentTypes.`application/json`, checkAuth(login, pass, token).asJson.noSpaces))
        },
        (post & path("file") & entity(as[Multipart.FormData]) ){ (formData) =>
          var fileName = ""
          var fileUrl = ""
          val done: Future[Done] = formData.parts.mapAsync(1) {
            case b: BodyPart if b.name == "file" =>
              fileName = b.filename.get
              var pathId = UUID.randomUUID().toString.substring(0, 8)
              var file = new File(cloudDirectory + "/" + pathId)
              while (file.exists()){
                pathId = UUID.randomUUID().toString.substring(0, 8)
                file = new File(cloudDirectory + "/" + pathId)
              }
              file.mkdir()
              file = new File(cloudDirectory + "/" + pathId + "/" + fileName)
              fileUrl = restUrl + "/files/" + pathId + "/" + fileName
              b.entity.dataBytes.runWith(FileIO.toPath(file.toPath))
              Future.successful(Done)
            case _ => Future.successful(Done)
          }.runWith(Sink.ignore)
          onSuccess(done) { _ =>
            complete(HttpEntity(fileUrl))
          }
        },
        (get & path("files" / Segment / Segment)){ (path, name) =>
          getFromFile(cloudDirectory + "/" + path + "/" + name)
        },
        (post & path("addPost")  & entity(as[String])) { (postValue) =>
          NewsManager.addPost(postValue)
          complete(HttpEntity("success"))
        },
        (get & path("posts")) {
          complete(HttpEntity(NewsManager.getPosts.asJson.noSpaces))
        },
        (get & path("publishPost") & parameter("id")) { (id) =>
          NewsManager.publishPost(id)
          complete(HttpEntity("success"))
        },
        (get & path("setPostStatus") & parameter("id", "status")) { (id, status) =>
          NewsManager.setPostStatus(id, status)
          complete(HttpEntity("success"))
        },
      )
    }

    val bindingFuture: Future[Http.ServerBinding] = Http().newServerAt(ip, port.toInt).bind(route)

    StdIn.readLine() // let it run until user presses return
    bindingFuture
      .flatMap(_.unbind()) // trigger unbinding from the port
      .onComplete(_ => system.terminate()) // and shutdown when done
  }
}
