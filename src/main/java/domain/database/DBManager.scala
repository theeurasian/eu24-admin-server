package domain.database

import domain.auth.AuthManager.UserAuth
import domain.news.NewsManager.{CGTNUrl, ClientStatus, NewsPack, Post, Subscriber, VideoNews}
import domain.time.TimeManager.Time
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import io.circe.{Decoder, Encoder}
import org.bson.codecs.configuration.CodecRegistries.{fromProviders, fromRegistries}
import org.bson.codecs.configuration.CodecRegistry
import org.mongodb.scala.MongoClient.DEFAULT_CODEC_REGISTRY
import org.mongodb.scala.{MongoClient, MongoDatabase}
import org.mongodb.scala.bson.codecs.Macros._

object DBManager {
  private val mongoClient: MongoClient = MongoClient("mongodb://localhost")

  implicit val UserAuthDecoder: Decoder[UserAuth] = deriveDecoder[UserAuth]
  implicit val UserAuthEncoder: Encoder[UserAuth] = deriveEncoder[UserAuth]

  implicit val PostDecoder: Decoder[Post] = deriveDecoder[Post]
  implicit val PostEncoder: Encoder[Post] = deriveEncoder[Post]

  implicit val NewsPackDecoder: Decoder[NewsPack] = deriveDecoder[NewsPack]
  implicit val NewsPackEncoder: Encoder[NewsPack] = deriveEncoder[NewsPack]

  implicit val VideoNewsDecoder: Decoder[VideoNews] = deriveDecoder[VideoNews]
  implicit val VideoNewsEncoder: Encoder[VideoNews] = deriveEncoder[VideoNews]

  implicit val ClientStatusDecoder: Decoder[ClientStatus] = deriveDecoder[ClientStatus]
  implicit val ClientStatusEncoder: Encoder[ClientStatus] = deriveEncoder[ClientStatus]

  implicit val CGTNUrlDecoder: Decoder[CGTNUrl] = deriveDecoder[CGTNUrl]
  implicit val CGTNUrlEncoder: Encoder[CGTNUrl] = deriveEncoder[CGTNUrl]

  implicit val SubscriberDecoder: Decoder[Subscriber] = deriveDecoder[Subscriber]
  implicit val SubscriberEncoder: Encoder[Subscriber] = deriveEncoder[Subscriber]

  implicit val TimeDecoder: Decoder[Time] = deriveDecoder[Time]
  implicit val TimeEncoder: Encoder[Time] = deriveEncoder[Time]

  val codecRegistry: CodecRegistry = fromRegistries(fromProviders(
    classOf[UserAuth],
    classOf[Post],
    classOf[NewsPack],
    classOf[VideoNews],
    classOf[ClientStatus],
    classOf[CGTNUrl],
    classOf[Subscriber],
  ), DEFAULT_CODEC_REGISTRY)


  def GetMongoConnection(): Option[MongoDatabase] = {
    Option(mongoClient.getDatabase("eurasian").withCodecRegistry(codecRegistry))
  }
}
