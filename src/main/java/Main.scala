import akka.Done
import akka.actor.typed.ActorSystem
import akka.actor.typed.scaladsl.Behaviors
import akka.http.scaladsl.Http
import akka.http.scaladsl.model.Multipart.BodyPart
import akka.http.scaladsl.model._
import akka.http.scaladsl.server.Directives.{pass, _}
import akka.stream.scaladsl.{FileIO, Sink}
import ch.megard.akka.http.cors.scaladsl.CorsDirectives.cors
import com.mongodb.client.model.Updates.set
import org.mongodb.scala.MongoCollection
import io.circe.parser._
import io.circe.syntax.EncoderOps
import io.circe._
import io.circe.generic.JsonCodec
import io.circe.generic.auto._
import io.circe.syntax._
import io.circe.generic.semiauto._
import org.mongodb.scala.model.Filters.{and, equal}
import sttp.client3.{Request, Response, SimpleHttpClient, UriContext, asStringAlways, basicRequest, multipart, multipartFile}

import java.io.{File, FileInputStream}
import java.util.{Date, UUID}
import scala.concurrent.duration.{Duration, SECONDS}
import scala.concurrent.{Await, ExecutionContextExecutor, Future}
import scala.io.{Source, StdIn}

object Main {

  val ip = "10.70.0.100"
  val url = "http://188.242.27.197"
  val port = "5055"
  val restUrl: String = url + ":" + port
  //val cloudDirectory = "C:/files"
  val cloudDirectory = "/files"
  val botApi = "5912354671:AAG5R1ZuEijhqKLhP6X5QoYQAb_S4_BBmz0"
  //val chatId = "-1001265788563" //EU24
  val chatId = "-1001823572552" //TEST

  val users: List[UserAuth] = List(
    UserAuth("editor", "TheEurasian124578editor", "create"),
    UserAuth("head-editor", "TheEurasian124578editor", "publish"),
  )

  case class UserAuth(login: String, password: String, permissions: String, token: String = "")
  case class Post(id: String, date: Long, header: String, url: String, imageUrl: String, publish: String = "new")

  def main(args: Array[String]): Unit = {
    implicit val system: ActorSystem[Any] = ActorSystem(Behaviors.empty, "http")
    // needed for the future flatMap/onComplete in the end
    implicit val executionContext: ExecutionContextExecutor = system.executionContext

    val route = cors(){
      concat(
        (get & path("login") & parameter("login") & parameter("pass") & parameter("token")) { (login, pass, token) =>
          val errorUser = UserAuth("error", "error", "error")
          val res = DBManager.GetMongoConnection() match {
            case Some(mongo) =>
              val userTokensCollection: MongoCollection[UserAuth] = mongo.getCollection("userTokens")
              val userTokens = Await.result(userTokensCollection.find().toFuture(), Duration(30, SECONDS)) match {
                case values: Seq[UserAuth] => values.toList
                case _ => List.empty[UserAuth]
              }
              userTokens.find(_.token == token) match {
                case Some(tokenFound) => tokenFound
                case _ =>
                  users.find(x => x.login == login && x.password == pass) match {
                    case Some(userFound) =>
                      val user = userFound.copy(token = UUID.randomUUID().toString)
                      Await.result(userTokensCollection.insertOne(user).toFuture(), Duration(30, SECONDS))
                      user
                    case _ => errorUser
                  }
              }
            case _ => errorUser
          }
          complete(HttpEntity(ContentTypes.`application/json`, res.asJson.noSpaces))
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
          decode[Post](postValue) match {
            case Right(post) =>
              DBManager.GetMongoConnection() match {
                case Some(mongo) =>
                  val posts: MongoCollection[Post] = mongo.getCollection("posts")
                  Await.result(posts.insertOne(post.copy(id = UUID.randomUUID().toString)).toFuture(), Duration(30, SECONDS))
                case _ =>
              }
            case Left(value) =>
          }
          complete(HttpEntity("success"))
        },
        (get & path("posts")) {
          val posts: List[Post] = DBManager.GetMongoConnection() match {
            case Some(mongo) =>
              Await.result(mongo.getCollection("posts").find[Post].toFuture(), Duration(30, SECONDS)) match {
                case files => files.toList
                case _ => List.empty[Post]
              }
            case _ => List.empty[Post]
          }
          complete(HttpEntity(posts.asJson.noSpaces))
        },
        (get & path("publishPost") & parameter("id") & parameter("admit")) { (id, admit) =>
          if (admit == "allow"){
            DBManager.GetMongoConnection() match {
              case Some(mongo) =>
                val posts = Await.result(mongo.getCollection("posts").find[Post](equal("id", id)).toFuture(), Duration(30, SECONDS)) match {
                  case files => files.toList
                  case _ => List.empty[Post]
                }
                val client = SimpleHttpClient()

                if (posts.nonEmpty){

                  val post = posts.head
                  val postUrl = s"<a href=\"${post.url}\">${post.url}</a>"
                  val text = post.header + " " + postUrl

                  val filePath = post.imageUrl.replace(url + ":" + port + "/files", cloudDirectory)
                  val request: Request[String, Any] = basicRequest
                    .response(asStringAlways)
                    .multipartBody(multipartFile("photo", new File(filePath)))
                    .post(uri"https://api.telegram.org/bot$botApi/sendPhoto?chat_id=$chatId&parse_mode=HTML&caption=$text")
                  val response = client.send(request)
                  val res = response


                }
              case _ => List.empty[Post]
            }
          }
          DBManager.GetMongoConnection() match {
            case Some(mongo) =>
              val postsCollection = mongo.getCollection("posts")
              Await.result(postsCollection.updateOne(and(equal("id", id)), set("publish", admit)).toFuture(), Duration(30, SECONDS))
            case _ => List.empty[Post]
          }
          complete(HttpEntity("success"))
        },
      )
    }

    val bindingFuture = Http().newServerAt(ip, port.toInt).bind(route)

    StdIn.readLine() // let it run until user presses return
    bindingFuture
      .flatMap(_.unbind()) // trigger unbinding from the port
      .onComplete(_ => system.terminate()) // and shutdown when done
  }
}
