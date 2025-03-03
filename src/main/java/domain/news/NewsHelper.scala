package domain.news

import com.google.api.client.auth.oauth2.{Credential, StoredCredential}
import com.google.api.client.extensions.java6.auth.oauth2.AuthorizationCodeInstalledApp
import com.google.api.client.extensions.jetty.auth.oauth2.LocalServerReceiver
import com.google.api.client.googleapis.auth.oauth2.{GoogleAuthorizationCodeFlow, GoogleClientSecrets}
import com.google.api.client.http.javanet.NetHttpTransport
import com.google.api.client.json.JsonFactory
import com.google.api.client.json.jackson2.JacksonFactory
import com.google.api.client.util.store.{DataStore, FileDataStoreFactory}
import domain.AppProps
import domain.database.DBManager
import io.circe.generic.auto._
import io.circe.jawn.decode
import org.mongodb.scala.MongoCollection
import org.mongodb.scala.model.Filters.{equal, regex}
import org.mongodb.scala.model.Updates._
import org.simplejavamail.api.mailer.config.TransportStrategy
import org.simplejavamail.email.EmailBuilder
import org.simplejavamail.mailer.MailerBuilder
import sttp.client3.{Request, SimpleHttpClient, UriContext, asStringAlways, basicRequest, multipartFile}

import java.io.File
import java.util.{Calendar, Date, UUID}
import scala.collection.JavaConverters._
import scala.concurrent.Await
import scala.concurrent.duration.{Duration, SECONDS}
import scala.io.{Codec, Source}

trait NewsHelper extends AppProps{

  case class Post(id: String, date: Long, user: String, publishDay: Int, publishMonth: Int, publishYear: Int, publishPeriod: String, header: String, url: String, imageUrl: String, status: String = "new")
  case class VideoNews(id: String, date: Long, user: String, publishDay: Int, publishMonth: Int, publishYear: Int, publishPeriod: String, kind: String, url: String, youTubeUrl: String, status: String = "new")

  trait NewsManagerMessages
  case class PublishNews(period: String) extends NewsManagerMessages
  case class CheckVideoSubs() extends NewsManagerMessages
  case class AutoRemove() extends NewsManagerMessages
  case class UpdateClientStatus(client: String, status: String)
  case class ClientStatus(client: String, status: String, date: Long)
  case class CGTNUrl(url: String, date: Long)
  case class Subscriber(name: String, email: String)
  case class NewsPack(by: String, kz: String, cn: String, ru: String, byAds: List[String], kzAds: List[String], cnAds: List[String], ruAds: List[String], byCaptions: List[String], kzCaptions: List[String], cnCaptions: List[String], ruCaptions: List[String], ins: List[String], byMobile: String, kzMobile: String, cnMobile: String, ruMobile: String, mergeMobile: String, mergeMobileCaptions: List[String])
  case class Payload(value: String)

  val langs: List[String] = List("by", "kz", "cn", "ru")
  val subtitleLangs: List[String] = List("ja", "pt", "es", "ru", "fr", "en", "zh-Hans", "ar", "hi", "kk")
  val CREDENTIALS_DIRECTORY = ".oauth-credentials"
  val HTTP_TRANSPORT = new NetHttpTransport()
  val VIDEO_FILE_FORMAT = "video/*"
  val JSON_FACTORY: JsonFactory = new JacksonFactory()
  val scopes: List[String] = List("https://www.googleapis.com/auth/youtube.upload")

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
            Await.result(posts.insertOne(post.copy(id = UUID.randomUUID().toString, date = new Date().getTime)).toFuture(), Duration(50, SECONDS))
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
            val id = UUID.randomUUID().toString
            val newPost = post.copy(id = id, date = new Date().getTime)
            val videos: MongoCollection[VideoNews] = mongo.getCollection("video-news")
            Await.result(videos.insertOne(newPost).toFuture(), Duration(50, SECONDS))
            if (post.status == "published" && post.kind.contains("merge")){
              sendToTelegram(newPost)
              sendToSubscribers(newPost)
            }
          case _ =>
        }
      case Left(value) =>
    }
  }
  def deleteVideoNews(id: String): Unit = {
    DBManager.GetMongoConnection() match {
      case Some(mongo) =>
        val posts: MongoCollection[VideoNews] = mongo.getCollection("video-news")
        Await.result(posts.deleteOne(equal("id", id)).toFuture(), Duration(50, SECONDS))
      case _ =>
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
      .post(uri"https://api.telegram.org/bot$botApi/sendPhoto?chat_id=$chatIdEU24&parse_mode=HTML&caption=$text")
    val response = client.send(request)
    val res = response
  }
  def sendToTelegram(video: VideoNews): Unit ={
    val client = SimpleHttpClient()
    val videoUrl = video.url.replace(restUrl + cloudDirectory + "/", "https://eurasian24.ru/watch?url=")
    val text = "Выпуск новостей от " + addLeftZeroes(video.publishDay) + "." + addLeftZeroes(video.publishMonth + 1) + "." + addLeftZeroes(video.publishYear) + " " + videoUrl
    val filePath = "/files/logo24.jpg"
    val request: Request[String, Any] = basicRequest
      .response(asStringAlways)
      .multipartBody(multipartFile("photo", new File(filePath)))
      .post(uri"https://api.telegram.org/bot$botApi/sendPhoto?chat_id=$chatIdEU24TV&parse_mode=HTML&caption=$text")
    val response = client.send(request)
    val res = response
  }
  def sendToSubscribers(video: VideoNews): Unit ={
    try{
      val url = video.url
      val watchUrl = video.url.replace(restUrl + cloudDirectory + "/", "https://eurasian24.ru/watch?url=")
      val header = "Выпуск новостей от " + addLeftZeroes(video.publishDay) + "." + addLeftZeroes(video.publishMonth + 1) + "." + addLeftZeroes(video.publishYear)
      val downloadUrl = video.url.replace("files", "files-download").replace("merge-mobile", header) + "?fileName=merge-mobile.mp4"

      getSubscribers.groupBy(_.email).foreach(group => {
        try{
          val html = Source.fromResource("email.html")(Codec.UTF8).mkString.replace("&publish", header).replace("&url", watchUrl).replace("&videoUrl", url).replace("&downloadUrl", downloadUrl)
          val mailer = MailerBuilder
            .withSMTPServer("smtp.mail.ru", 465, mailRuLogin, mailRuPass)
            .withTransportStrategy(TransportStrategy.SMTPS)
            .withDebugLogging(true)
            .buildMailer()

          val mail = EmailBuilder.startingBlank()
            .from("Eurasian 24", mailRuLogin)
            .to(group._2.head.name, group._1)
            .withSubject("Eurasian 24 ежедневный выпуск новостей")
            .withHTMLText(html)
            .buildEmail()

          mailer.sendMail(mail)
        }
        catch {
          case e: Exception => println(e.toString)
        }
      })
    }
    catch {
      case e: Exception => None
    }
  }
  def sendGreetingToSubscribeer(name: String, email: String): Unit ={
    try{
      try{
        val text = "Благодарим Вас за оформление подписки на новостную рассылку Eurasian 24. Раз в день Вам будет приходить письмо с видеовыпуском актуальных новостей."
        val html = Source.fromResource("greetings.html")(Codec.UTF8).mkString.replace("&text", text)
        val mailer = MailerBuilder
          .withSMTPServer("smtp.mail.ru", 465, mailRuLogin, mailRuPass)
          .withTransportStrategy(TransportStrategy.SMTPS)
          .withDebugLogging(true)
          .buildMailer()

        val mail = EmailBuilder.startingBlank()
          .from("Eurasian 24", mailRuLogin)
          .to(name, email)
          .withSubject("Eurasian 24 подписка на новостную рассылку оформлена")
          .withHTMLText(html)
          .buildEmail()

        mailer.sendMail(mail)
      }
      catch {
        case e: Exception => println(e.toString)
      }
    }
    catch {
      case e: Exception => None
    }
  }
  def addLeftZeroes(in: Int, length: Int = 2): String ={
    var res = in.toString
    while (res.length < length){
      res = "0" + res
    }
    res
  }
  def publishVideoNewsInTelegram(): Unit ={
    getVideoNews.find(x => x.status == "published" && x.kind.contains("merge")) match {
      case Some(value) => sendToTelegram(value)
      case _ => None
    }
  }
  def publishVideoNewsToSubscribers(): Unit ={
    getVideoNews.find(x => x.status == "published" && x.kind.contains("merge")) match {
      case Some(value) => sendToSubscribers(value)
      case _ => None
    }
  }
  def publishVideoNews(video: VideoNews): Unit ={
    sendToTelegram(video)
    sendToSubscribers(video)
    sendToPushSubscribers()
  }
  def publishPost(id: String): Unit ={
    getPost(id).take(1).foreach(p => publishPost(p))
  }
  def getVideoNewsPack: NewsPack = {
    val videos = getVideoNews.filter(_.status == "published")
    val inserts = videos.filter(_.kind == "ins").sortBy(_.date).reverse.map(_.url)
    val ads = videos.filter(_.kind.contains("ad")).sortBy(_.date)
    val by = videos.find(_.kind == "by").map(_.url).getOrElse("")
    val kz = videos.find(_.kind == "kz").map(_.url).getOrElse("")
    val cn = videos.find(_.kind == "cn").map(_.url).getOrElse("")
    val ru = videos.find(_.kind == "ru").map(_.url).getOrElse("")
    val byMobile = videos.find(_.kind == "by-mobile").map(_.url).getOrElse("")
    val kzMobile = videos.find(_.kind == "kz-mobile").map(_.url).getOrElse("")
    val cnMobile = videos.find(_.kind == "cn-mobile").map(_.url).getOrElse("")
    val ruMobile = videos.find(_.kind == "ru-mobile").map(_.url).getOrElse("")
    val mergeMobile = videos.find(_.kind == "merge-mobile").map(_.url).getOrElse("")
    NewsPack(
      by,
      kz,
      cn,
      ru,
      ads.filter(_.kind.contains("by")).map(_.url),
      ads.filter(_.kind.contains("kz")).map(_.url),
      ads.filter(_.kind.contains("cn")).map(_.url),
      ads.filter(_.kind.contains("ru")).map(_.url),
      getCaptions(by),
      getCaptions(kz),
      getCaptions(cn),
      getCaptions(ru),
      inserts,
      byMobile,
      kzMobile,
      cnMobile,
      ruMobile,
      mergeMobile,
      getCaptions(mergeMobile)
    )
  }
  def getCaptions(url: String): List[String] ={
    val filePath = url.replace(restUrl + "/files", cloudDirectory)
    val directory = if (new File(filePath).exists()) new File(filePath).getParent + File.separator else ""
    val files = if (new File(directory).exists()) new File(directory).listFiles().toList else List.empty[File]
    files.filter(_.getName.contains(".vtt")).map(x => directory.replace(cloudDirectory, restUrl + "/files") + x.getName)
  }
  def getCaptionsByFile(file: String): List[String] ={
    val filePath = cloudDirectory + "/" + file
    val directory = if (new File(filePath).exists()) new File(filePath).getParent + File.separator else ""
    val files = if (new File(directory).exists()) new File(directory).listFiles().toList else List.empty[File]
    files.filter(_.getName.contains(".vtt")).map(x => directory.replace(cloudDirectory, restUrl + "/files") + x.getName)
  }
  def updateClientStatus(client: String, status: String): String ={
    DBManager.GetMongoConnection() match {
      case Some(mongo) =>
        val clientStatus: MongoCollection[ClientStatus] = mongo.getCollection("client-status")
        Await.result(clientStatus.insertOne(ClientStatus(client, status, new Date().getTime)).toFuture(), Duration(50, SECONDS))
      case _ =>
    }
    "success"
  }
  def getClientLog(client: String): List[ClientStatus] ={
    DBManager.GetMongoConnection() match {
      case Some(mongo) =>
        if (client == "all"){
          Await.result(mongo.getCollection("client-status").find[ClientStatus]().toFuture(), Duration(50, SECONDS)) match {
            case posts => posts.toList.sortBy(_.date).reverse
            case _ => List.empty[ClientStatus]
          }
        }
        else{
          Await.result(mongo.getCollection("client-status").find[ClientStatus](equal("client", client)).toFuture(), Duration(50, SECONDS)) match {
            case posts => posts.toList.sortBy(_.date).reverse
            case _ => List.empty[ClientStatus]
          }
        }
      case _ => List.empty[ClientStatus]
    }
  }
  def addCGTNUrls(urls: String): String ={
    DBManager.GetMongoConnection() match {
      case Some(mongo) =>
        val clientStatus: MongoCollection[CGTNUrl] = mongo.getCollection("cgtn-urls")
        Await.result(clientStatus.insertOne(CGTNUrl(urls, new Date().getTime)).toFuture(), Duration(50, SECONDS))
      case _ =>
    }
    "success"
  }
  def getCGTNUrls: List[CGTNUrl] ={
    DBManager.GetMongoConnection() match {
      case Some(mongo) =>
        Await.result(mongo.getCollection("cgtn-urls").find[CGTNUrl]().toFuture(), Duration(50, SECONDS)) match {
          case urls => urls.toList.sortBy(_.date).reverse
          case _ => List.empty[CGTNUrl]
        }
      case _ => List.empty[CGTNUrl]
    }
  }
  def addSubscriber(name: String, email: String): String ={
    DBManager.GetMongoConnection() match {
      case Some(mongo) =>
        val clientStatus: MongoCollection[Subscriber] = mongo.getCollection("subscribers")
        Await.result(clientStatus.insertOne(Subscriber(name, email)).toFuture(), Duration(50, SECONDS))
        sendGreetingToSubscribeer(name, email)
      case _ =>
    }
    "success"
  }
  def getSubscribers: List[Subscriber] ={
    DBManager.GetMongoConnection() match {
      case Some(mongo) =>
        Await.result(mongo.getCollection("subscribers").find[Subscriber]().toFuture(), Duration(50, SECONDS)) match {
          case subscribers => subscribers.toList
          case _ => List.empty[Subscriber]
        }
      case _ => List.empty[Subscriber]
    }
  }

  def addPushSubscriber(payload: String): String ={
    DBManager.GetMongoConnection() match {
      case Some(mongo) =>
        val clientStatus: MongoCollection[Payload] = mongo.getCollection("push-subscribers")
        Await.result(clientStatus.insertOne(Payload(payload)).toFuture(), Duration(50, SECONDS))
      case _ =>
    }
    "success"
  }
  def deletePushSubscriber(endpoint: String): String ={
    DBManager.GetMongoConnection() match {
      case Some(mongo) =>
        val clientStatus: MongoCollection[Payload] = mongo.getCollection("push-subscribers")
        Await.result(clientStatus.deleteOne(regex("value", endpoint)).toFuture(), Duration(50, SECONDS))
      case _ =>
    }
    "success"
  }
  def sendPushSubscriber(payload: Payload): Unit ={
    val client = SimpleHttpClient()
    val request: Request[String, Any] = basicRequest
      .response(asStringAlways)
      .body(payload.value)
      .post(uri"https://eurasian24.tv/push/roll")
    val response = client.send(request)
    val res = response
  }
  def getPushSubscribers: List[Payload] ={
    DBManager.GetMongoConnection() match {
      case Some(mongo) =>
        Await.result(mongo.getCollection("push-subscribers").find[Payload]().toFuture(), Duration(50, SECONDS)) match {
          case subscribers => subscribers.toList
          case _ => List.empty[Payload]
        }
      case _ => List.empty[Payload]
    }
  }
  def sendToPushSubscribers(): Unit = {
    getPushSubscribers.foreach(p => {
      sendPushSubscriber(p)
    })
  }

  def isToday(pDay: Int, pMonth: Int, pYear: Int): Boolean = {
    val c = Calendar.getInstance()
    val day = c.get(Calendar.DATE)
    val month = c.get(Calendar.MONTH)
    val year = c.get(Calendar.YEAR)
    day == pDay && month == pMonth && year == pYear
  }



  def authorize(scopes: List[String], credentialDatastore: String): Credential = {
    val clientSecretReader = Source.fromResource("client_secrets.json").bufferedReader()
    val clientSecrets = GoogleClientSecrets.load(JSON_FACTORY, clientSecretReader)
    val fileDataStoreFactory = new FileDataStoreFactory(new File(System.getProperty("user.home") + "/" + CREDENTIALS_DIRECTORY))
    val datastore: DataStore[StoredCredential] = fileDataStoreFactory.getDataStore(credentialDatastore)
    val flow = new GoogleAuthorizationCodeFlow.Builder(HTTP_TRANSPORT, JSON_FACTORY, clientSecrets, scopes.asJava).setCredentialDataStore(datastore).setAccessType("offline").build
    val localReceiver = new LocalServerReceiver.Builder().setPort(8080).build
    new AuthorizationCodeInstalledApp(flow, localReceiver).authorize("user")
  }

  def publishNewsAux(period: String): Unit = {
    val posts = getPosts
    posts.filter(p => p.publishPeriod == period && isToday(p.publishDay, p.publishMonth, p.publishYear)).foreach(p => {
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
          if (p.kind != "ins" && !p.kind.contains("ad")){
            videoNews.find(x => x.kind == p.kind && x.status == "published") match {
              case Some(publishNews) => setNewsStatus(publishNews.id, "archive-published")
              case _ => None
            }
          }
          setNewsStatus(p.id, "published")
          if (p.kind.contains("merge")){
            publishVideoNews(p)
          }
        case "deny" =>
          setNewsStatus(p.id, "archive-denied")
        case _ =>
          setNewsStatus(p.id, "archive-skipped")
      }
    })
  }
  def publishNews(): Unit = {
    val videoNews = getVideoNews
    videoNews.filter(_.status == "allow").foreach(p => {
      if (p.kind != "ins" && !p.kind.contains("ad")){
        videoNews.find(x => x.kind == p.kind && x.status == "published") match {
          case Some(publishNews) => setNewsStatus(publishNews.id, "archive-published")
          case _ => None
        }
      }
      setNewsStatus(p.id, "published")
      if (p.kind.contains("merge")){
        publishVideoNews(p)
      }
    })
  }

}
