package domain.news

import domain.AppProps
import domain.database.DBManager
import domain.news.NewsManager.{NewsPack, Post, VideoNews, cloudDirectory, restUrl}
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
  def getVideoNews: List[VideoNews] = {
    DBManager.GetMongoConnection() match {
      case Some(mongo) =>
        Await.result(mongo.getCollection("video-news").find[VideoNews].toFuture(), Duration(50, SECONDS)) match {
          case posts => posts.toList
          case _ => List.empty[VideoNews]
        }
      case _ => List.empty[VideoNews]
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
  def addVideoNews(postValue: String): Unit = {
    decode[VideoNews](postValue) match {
      case Right(post) =>
        DBManager.GetMongoConnection() match {
          case Some(mongo) =>
            val posts: MongoCollection[VideoNews] = mongo.getCollection("video-news")
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
  def setNewsStatus(id: String, status: String): Unit = {
    DBManager.GetMongoConnection() match {
      case Some(mongo) =>
        val posts: MongoCollection[VideoNews] = mongo.getCollection("video-news")
        Await.result(posts.updateOne(equal("id", id), set("status", status)).toFuture(), Duration(50, SECONDS))
      case _ =>
    }
  }
  def setVideoNewsYouTubeUrl(id: String, youTubeUrl: String): Unit = {
    DBManager.GetMongoConnection() match {
      case Some(mongo) =>
        val posts: MongoCollection[VideoNews] = mongo.getCollection("video-news")
        Await.result(posts.updateOne(equal("id", id), set("youTubeUrl", youTubeUrl)).toFuture(), Duration(50, SECONDS))
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
  def getVideoNewsPack: NewsPack = {
    val videos = getVideoNews.filter(_.status == "published")
    val inserts = videos.filter(_.kind == "ins").sortBy(_.date).reverse.map(_.url)
    val ads = videos.filter(_.kind.contains("ad")).sortBy(_.date)
    NewsPack(
      videos.find(_.kind == "by").map(_.url).getOrElse(""),
      videos.find(_.kind == "kz").map(_.url).getOrElse(""),
      videos.find(_.kind == "cn").map(_.url).getOrElse(""),
      videos.find(_.kind == "ru").map(_.url).getOrElse(""),
      ads.filter(_.kind.contains("by")).map(_.url),
      ads.filter(_.kind.contains("kz")).map(_.url),
      ads.filter(_.kind.contains("cn")).map(_.url),
      ads.filter(_.kind.contains("ru")).map(_.url),
      getCaptions("by"),
      getCaptions("kz"),
      getCaptions("cn"),
      getCaptions("ru"),
      inserts
    )
  }
  def getCaptions(url: String): List[String] ={
    val filePath = url.replace(restUrl + "/files", cloudDirectory)
    val directory = if (new File(filePath).exists()) new File(filePath).getParent + File.separator else ""
    val files = if (new File(directory).exists()) new File(directory).listFiles().toList else List.empty[File]
    files.filter(_.getName.contains(".vtt")).map(x => directory.replace(cloudDirectory, restUrl + "/files") + x.getName)
  }
}
