package domain.news

import akka.actor.typed.Behavior
import akka.actor.typed.scaladsl.Behaviors
import com.google.api.client.http.javanet.NetHttpTransport
import com.google.api.services.youtube.YouTube
import com.google.api.client.json.jackson2.JacksonFactory
import com.google.api.client.auth.oauth2.Credential
import com.google.api.client.auth.oauth2.StoredCredential
import com.google.api.client.extensions.java6.auth.oauth2.AuthorizationCodeInstalledApp
import com.google.api.client.extensions.jetty.auth.oauth2.LocalServerReceiver
import com.google.api.client.googleapis.auth.oauth2.GoogleAuthorizationCodeFlow
import com.google.api.client.googleapis.auth.oauth2.GoogleClientSecrets
import com.google.api.client.util.store.DataStore
import com.google.api.client.util.store.FileDataStoreFactory
import jdk.xml.internal.SecuritySupport
import com.google.api.client.googleapis.media.MediaHttpUploader
import com.google.api.client.googleapis.media.MediaHttpUploaderProgressListener
import com.google.api.client.http.InputStreamContent
import com.google.api.services.youtube.YouTube
import com.google.api.services.youtube.model.Video
import com.google.api.services.youtube.model.VideoSnippet
import com.google.api.services.youtube.model.VideoStatus

import java.io.{File, FileInputStream, IOException, InputStreamReader}
import java.text.SimpleDateFormat
import java.util
import java.util.Calendar
import scala.collection.JavaConverters._
import java.util.Calendar
import scala.io.Source

object NewsManager extends NewsHelper {

  case class Post(id: String, date: Long, user: String, publishDay: Int, publishMonth: Int, publishYear: Int, publishPeriod: String, header: String, url: String, imageUrl: String, status: String = "new")
  case class VideoNews(id: String, date: Long, user: String, publishDay: Int, publishMonth: Int, publishYear: Int, publishPeriod: String, kind: String, url: String, youTubeUrl: String, status: String = "new")

  trait NewsManagerMessages
  case class PublishNews(period: String) extends NewsManagerMessages
  case class CheckVideoSubs() extends NewsManagerMessages

  case class NewsPack(by: String, kz: String, cn: String, ru: String, ins1: String, ins2: String, ins3: String, ins4: String, byAds: List[String], kzAds: List[String], cnAds: List[String], ruAds: List[String])

  val langs = List("by", "kz", "cn", "ru")
  val CREDENTIALS_DIRECTORY = ".oauth-credentials"
  val HTTP_TRANSPORT = new NetHttpTransport()
  val JSON_FACTORY = new JacksonFactory()
  val VIDEO_FILE_FORMAT = "video/*"
  val scopes: List[String] = List("https://www.googleapis.com/auth/youtube.upload")

  def apply(): Behavior[NewsManagerMessages] = Behaviors.receive {
    (context, message) =>
      message match {
        case PublishNews(period: String) =>
          if (period != "unknown"){
            getPosts.filter(p => p.publishPeriod == period && isToday(p.publishDay, p.publishMonth, p.publishYear)).foreach(p => {
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
                case "deny" =>
                  setNewsStatus(p.id, "archive-denied")
                case _ =>
                  setNewsStatus(p.id, "archive-skipped")
              }
            })
          }
          Behaviors.same
        case CheckVideoSubs() =>
          val videoNews = getVideoNews.filter(p => langs.contains(p.kind)).filter(_.status == "published")
          videoNews.foreach(p => {
            val filePath = p.url.replace(restUrl + "/files", cloudDirectory)
            val directory = new File(filePath).getParent
            val files = new File(directory).listFiles().toList
            val findVtt = files.find(_.toString.contains(".vtt"))
            if (p.youTubeUrl == ""){
              val credential = authorize(scopes, "uploadvideo")
              val youtube = new YouTube.Builder(new NetHttpTransport(), new JacksonFactory(), credential).setApplicationName("eurasian24").build()
              val videoObjectDefiningMetadata = new Video()
              val status = new VideoStatus()
              status.setMadeForKids(false)
              status.setSelfDeclaredMadeForKids(false)
              status.setPrivacyStatus("private")
              videoObjectDefiningMetadata.setStatus(status)
              val snippet = new VideoSnippet()
              val c = Calendar.getInstance()
              c.set(Calendar.YEAR, p.publishYear)
              c.set(Calendar.MONTH, p.publishMonth)
              c.set(Calendar.DATE, p.publishDay)
              snippet.setTitle("Выпуск новостей от " + new SimpleDateFormat("dd.MM.yyyy").format(c.getTime) + ". " + (p.kind match {
                case "by" => "Беларусь"
                case "kz" => "Казахстан"
                case "cn" => "Китай"
                case "ru" => "Россия"
              }) + ".")
              snippet.setDefaultLanguage("ru")
              snippet.setDefaultAudioLanguage("ru")
              videoObjectDefiningMetadata.setSnippet(snippet)

              val mediaContent = new InputStreamContent(VIDEO_FILE_FORMAT, new FileInputStream(filePath))
              val videoInsert = youtube.videos.insert("snippet,statistics,status", videoObjectDefiningMetadata, mediaContent)

              val uploader = videoInsert.getMediaHttpUploader
              uploader.setDirectUploadEnabled(false)

              val returnedVideo = videoInsert.execute

              System.out.println("\n================== Returned Video ==================\n")
              System.out.println("  - Id: " + returnedVideo.getId)
              System.out.println("  - Title: " + returnedVideo.getSnippet.getTitle)
              System.out.println("  - Tags: " + returnedVideo.getSnippet.getTags)
              System.out.println("  - Privacy Status: " + returnedVideo.getStatus.getPrivacyStatus)
              System.out.println("  - Video Count: " + returnedVideo.getStatistics.getViewCount)
              setVideoNewsYouTubeUrl(p.id, "https://youtu.be/" + returnedVideo.getId)
            }
            else if (findVtt.isEmpty){

            }
          })
          Behaviors.same
      }
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
    val flow = new GoogleAuthorizationCodeFlow.Builder(HTTP_TRANSPORT, JSON_FACTORY, clientSecrets, scopes.asJava).setCredentialDataStore(datastore).build
    val localReceiver = new LocalServerReceiver.Builder().setPort(8080).build
    new AuthorizationCodeInstalledApp(flow, localReceiver).authorize("user")
  }
}
