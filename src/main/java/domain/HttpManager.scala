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
import domain.auth.AuthManager.checkAuth
import domain.news.NewsManager
import domain.time.TimeManager
import domain.time.TimeManager.Time
import io.circe.generic.auto._
import io.circe.syntax.EncoderOps
import sttp.model.HeaderNames.ContentType

import java.io.File
import java.util.{Date, UUID}
import scala.concurrent.{ExecutionContextExecutor, Future}
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
        (withoutSizeLimit & post & path("file") & entity(as[Multipart.FormData])){ (formData) =>
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
        (get & path("files-download" / Segment / Segment)){ (path, name) =>
          getFromFile(new File(cloudDirectory + "/" + path + "/" + name), ContentTypes.NoContentType)
        },
        (get & path("files-download" / Segment / Segment) & parameter("fileName")){ (path, name, fileName) =>
          getFromFile(new File(cloudDirectory + "/" + path + "/" + fileName), ContentTypes.NoContentType)
        },
        (post & path("addPost")  & entity(as[String])) { (postValue) =>
          NewsManager.addPost(postValue)
          complete(HttpEntity("success"))
        },
        (post & path("addVideoNews") & entity(as[String])) { (postValue) =>
          NewsManager.addVideoNews(postValue)
          //NewsManager.publishVideoNews(postValue)
          complete(HttpEntity("success"))
        },
        (get & path("publishVideoNewsInTelegram")) {
          NewsManager.publishVideoNewsInTelegram()
          complete(HttpEntity("success"))
        },
        (get & path("sendToSubscribers")) {
          NewsManager.publishVideoNewsToSubscribers()
          complete(HttpEntity("success"))
        },
        (get & path("posts")) {
          complete(HttpEntity(NewsManager.getPosts.asJson.noSpaces))
        },
        (get & path("videoNews")) {
          complete(HttpEntity(NewsManager.getVideoNews.asJson.noSpaces))
        },
        (get & path("publishPost") & parameter("id")) { (id) =>
          NewsManager.publishPost(id)
          complete(HttpEntity("success"))
        },
        (get & path("setPostStatus") & parameter("id", "status")) { (id, status) =>
          NewsManager.setPostStatus(id, status)
          complete(HttpEntity("success"))
        },
        (get & path("setVideoNewsStatus") & parameter("id", "status")) { (id, status) =>
          NewsManager.setNewsStatus(id, status)
          //NewsManager.publishVideoNews(id, status)
          complete(HttpEntity("success"))
        },
        (get & path("time")) {
          complete(HttpEntity(new Date().getTime.toString.asJson.noSpaces))
        },
        (get & path("time-struct")) {
          complete(HttpEntity(Time.fromDate(new Date()).asJson.noSpaces))
        },
        (get & path("newsPack")) {
          complete(HttpEntity(NewsManager.getVideoNewsPack.asJson.noSpaces))
        },
        (get & path("newsCaptions") & parameter("file")) { (file) =>
          complete(HttpEntity(NewsManager.getCaptionsByFile(file).asJson.noSpaces))
        },
        (get & path("updateClientStatus") & parameter("client", "status")) { (client, status) =>
          complete(HttpEntity(NewsManager.updateClientStatus(client, status).asJson.noSpaces))
        },
        (get & path("clientLog") & parameter("client")) { (client) =>
          complete(HttpEntity(NewsManager.getClientLog(client).asJson.noSpaces))
        },
        (get & path("updateCGTNUrls") & parameter("urls")) { (urls) =>
          complete(HttpEntity(NewsManager.addCGTNUrls(urls).asJson.noSpaces))
        },
        (get & path("CGTNUrls")) {
          complete(HttpEntity(NewsManager.getCGTNUrls.asJson.noSpaces))
        },
        (get & path("addSubscriber") & parameter("name", "email")) { (name, email) =>
          complete(HttpEntity(NewsManager.addSubscriber(name, email).asJson.noSpaces))
        },
        (get & path("subscribers")) {
          complete(HttpEntity(NewsManager.getSubscribers.asJson.noSpaces))
        },
        (post & path("addPushSubscriber") & entity(as[String])) { (payload) =>
          complete(HttpEntity(NewsManager.addPushSubscriber(payload)))
        },
        (post & path("deletePushSubscriber") & entity(as[String])) { (endpoint) =>
          complete(HttpEntity(NewsManager.deletePushSubscriber(endpoint)))
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
