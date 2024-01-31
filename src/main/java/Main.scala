import domain.HttpManager
import domain.news.NewsHelper


object Main extends NewsHelper {
  def main(args: Array[String]): Unit = {
    HttpManager()
  }
}
