package domain.news

import akka.actor.typed.Behavior
import akka.actor.typed.scaladsl.Behaviors
import com.google.api.client.http.InputStreamContent
import com.google.api.client.http.javanet.NetHttpTransport
import com.google.api.services.youtube.YouTube
import com.google.api.services.youtube.model.{Video, VideoSnippet, VideoStatus}
import java.io.{File, FileInputStream}
import java.text.SimpleDateFormat
import java.util.{Calendar, Date}

object NewsManager extends NewsHelper {


  def apply(): Behavior[NewsManagerMessages] = Behaviors.receive {
    (context, message) =>
      message match {
        case PublishNews(period: String) =>
          publishNews()
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
                  Runtime.getRuntime.exec(s"yt-dlp --cookies /files/cookies.txt --write-auto-subs --sub-langs $subLangs --skip-download ${p.youTubeUrl} -P $directory -o captions --sleep-subtitles 60 --no-overwrite")
                }
                catch {
                  case e: Exception => println(e.toString)
                }
              }
            }
          })
          Behaviors.same
        case AutoRemove() =>
          println("AutoRemove")
          val videoNews = getVideoNews.filter(x => x.kind == "ins" && x.status == "published")
          val today = new Date().getTime
          val directories = new File("/files/").listFiles()
          directories.foreach(d => {
            if (d.isDirectory){
              val date = d.lastModified()
              if (today - date > 1000 * 60 * 60 * 24 * 7){
                val dirFiles = d.listFiles()
                dirFiles.foreach(file => {
                  val fileName = file.getName
                  if (!videoNews.exists(x => x.url == restUrl + "/files/" + fileName)){
                    file.delete()
                    println("Deleted file: " + fileName)
                  }
                })
                if (d.listFiles().length == 0){
                  d.delete()
                  println("Deleted directory: " + d.getName)
                }
              }
            }
          })

          Behaviors.same
      }
  }


}
