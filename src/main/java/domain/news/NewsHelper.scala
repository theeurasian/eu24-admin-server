package domain.news

import domain.AppProps
import domain.database.DBManager
import domain.news.NewsManager.Post
import io.circe.jawn.decode
import org.mongodb.scala.MongoCollection
import io.circe.generic.auto._
import org.mongodb.scala.model.Filters.equal
import org.mongodb.scala.model.Updates._
import sttp.client3.{Request, SimpleHttpClient, UriContext, asStringAlways, basicRequest, multipartFile}

import java.io.File
import java.util.UUID
import scala.concurrent.Await
import scala.concurrent.duration.{Duration, SECONDS}

trait NewsHelper extends AppProps{


  def getPosts: List[Post] = {
    DBManager.GetMongoConnection() match {
      case Some(mongo) =>
        Await.result(mongo.getCollection("posts").find[Post].toFuture(), Duration(50, SECONDS)) match {
          case posts => posts.toList
          case _ => List.empty[Post]
        }
      case _ => List.empty[Post]
    }
  }
  def getPost(id: String): List[Post] = {
    DBManager.GetMongoConnection() match {
      case Some(mongo) =>
        Await.result(mongo.getCollection("posts").find[Post](equal("id", id)).toFuture(), Duration(50, SECONDS)) match {
          case posts: List[Post] => posts
          case _ => List.empty[Post]
        }
      case _ => List.empty[Post]
    }
  }
  def addPost(postValue: String): Unit = {
    decode[Post](postValue) match {
      case Right(post) =>
        DBManager.GetMongoConnection() match {
          case Some(mongo) =>
            val posts: MongoCollection[Post] = mongo.getCollection("posts")
            Await.result(posts.insertOne(post.copy(id = UUID.randomUUID().toString)).toFuture(), Duration(50, SECONDS))
          case _ =>
        }
      case Left(value) =>
    }
  }
  def setPostStatus(id: String, status: String): Unit = {
    DBManager.GetMongoConnection() match {
      case Some(mongo) =>
        val posts: MongoCollection[Post] = mongo.getCollection("posts")
        Await.result(posts.updateOne(equal("id", id), set("status", status)).toFuture(), Duration(50, SECONDS))
      case _ =>
    }
  }
  def publishPost(post: Post): Unit ={
    val client = SimpleHttpClient()
    val postUrl = post.url //s"<a href=\"${post.url}\">${post.url}</a>"
    val text = post.header + " " + postUrl
    val filePath = post.imageUrl.replace(restUrl + "/files", cloudDirectory)
    val request: Request[String, Any] = basicRequest
      .response(asStringAlways)
      .multipartBody(multipartFile("photo", new File(filePath)))
      .post(uri"https://api.telegram.org/bot$botApi/sendPhoto?chat_id=$chatId&parse_mode=HTML&caption=$text")
    val response = client.send(request)
    val res = response
  }
  def publishPost(id: String): Unit ={
    getPost(id).take(1).foreach(p => publishPost(p))
  }
}