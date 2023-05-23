package domain.news

import akka.actor.typed.Behavior
import akka.actor.typed.scaladsl.Behaviors
import com.google.api.client.http.javanet.NetHttpTransport
import com.google.api.services.youtube.YouTube
import com.google.api.client.auth.oauth2.{Credential, StoredCredential}
import com.google.api.client.extensions.java6.auth.oauth2.AuthorizationCodeInstalledApp
import com.google.api.client.extensions.jetty.auth.oauth2.LocalServerReceiver
import com.google.api.client.googleapis.auth.oauth2.GoogleAuthorizationCodeFlow
import com.google.api.client.googleapis.auth.oauth2.GoogleClientSecrets
import com.google.api.client.util.store.DataStore
import com.google.api.client.util.store.FileDataStoreFactory
import com.google.api.client.http.InputStreamContent
import com.google.api.client.json.JsonFactory
import com.google.api.client.json.jackson2.JacksonFactory
import com.google.api.services.youtube.model.Video
import com.google.api.services.youtube.model.VideoSnippet
import com.google.api.services.youtube.model.VideoStatus

import java.io.{BufferedReader, File, FileInputStream, InputStreamReader}
import java.text.SimpleDateFormat
import java.util.{Calendar, Date}
import scala.collection.JavaConverters._
import scala.io.Source
import scala.reflect.io.Directory

object NewsManager extends NewsHelper {

  case class Post(id: String, date: Long, user: String, publishDay: Int, publishMonth: Int, publishYear: Int, publishPeriod: String, header: String, url: String, imageUrl: String, status: String = "new")
  case class VideoNews(id: String, date: Long, user: String, publishDay: Int, publishMonth: Int, publishYear: Int, publishPeriod: String, kind: String, url: String, youTubeUrl: String, status: String = "new")

  trait NewsManagerMessages
  case class PublishNews(period: String) extends NewsManagerMessages
  case class CheckVideoSubs() extends NewsManagerMessages

  case class NewsPack(by: String, kz: String, cn: String, ru: String, byAds: List[String], kzAds: List[String], cnAds: List[String], ruAds: List[String], byCaptions: List[String], kzCaptions: List[String], cnCaptions: List[String], ruCaptions: List[String], ins: List[String], byMobile: String, kzMobile: String, cnMobile: String, ruMobile: String, mergeMobile: String, mergeMobileCaptions: List[String])

  val langs: List[String] = List("by", "kz", "cn", "ru")
  val subtitleLangs: List[String] = List("ja", "pt", "es", "ru", "fr", "en", "zh-Hans")
  val CREDENTIALS_DIRECTORY = ".oauth-credentials"
  val HTTP_TRANSPORT = new NetHttpTransport()
  val VIDEO_FILE_FORMAT = "video/*"
  val JSON_FACTORY: JsonFactory = new JacksonFactory()
  val scopes: List[String] = List("https://www.googleapis.com/auth/youtube.upload")

  def apply(): Behavior[NewsManagerMessages] = Behaviors.receive {
    (context, message) =>
      message match {
        case PublishNews(period: String) =>
          if (period != "unknown"){
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
            videoNews.filter(p => p.status == "archive-published" || p.status == "archive-skipped").foreach(v => {
              val today = new Date().getTime
              if (today - v.date > 1000 * 60 * 60 * 24 * 14){
                val filePath = v.url.replace(restUrl + "/files", cloudDirectory)
                val directory = new File(filePath).getParent + File.separator
                new Directory(new File(directory)).deleteRecursively()
                println("Deleted directory: " + directory)
              }
            })
          }
          Behaviors.same
        case CheckVideoSubs() =>
          val videoNews = getVideoNews.filter(p => langs.contains(p.kind) || p.kind == "merge-mobile").filter(_.status == "published")
          videoNews.foreach(p => {
            val filePath = p.url.replace(restUrl + "/files", cloudDirectory)
            if (new File(filePath).exists()){
              val directory = new File(filePath).getParent + File.separator
              val files = if (new File(directory).exists()) new File(directory).listFiles().toList else List.empty[File]
              if (p.youTubeUrl == ""){
                try{
                  val credential = authorize(scopes, "uploadvideo")
                  val youtube = new YouTube.Builder(new NetHttpTransport(), JSON_FACTORY, credential).setApplicationName("eurasian24").build()
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
                    case "merge-mobile" => "Мобильная версия"
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
                catch {
                  case e: Exception => println(e.toString)
                }
              }
              else if(files.count(_.getName.contains(".vtt")) != subtitleLangs.length) {
                try{
                  val subLangs = subtitleLangs.mkString(",")
                  Runtime.getRuntime.exec(s"yt-dlp --cookies /files/cookies.txt --write-auto-subs --sub-langs $subLangs --skip-download ${p.youTubeUrl} -P $directory -o captions --sleep-subtitles 60")
                }
                catch {
                  case e: Exception => println(e.toString)
                }
              }
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
    val flow = new GoogleAuthorizationCodeFlow.Builder(HTTP_TRANSPORT, JSON_FACTORY, clientSecrets, scopes.asJava).setCredentialDataStore(datastore).setAccessType("offline").build
    val localReceiver = new LocalServerReceiver.Builder().setPort(8080).build
    new AuthorizationCodeInstalledApp(flow, localReceiver).authorize("user")
  }
}
