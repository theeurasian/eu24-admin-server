import Main.{Post, UserAuth}
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import io.circe.{Decoder, Encoder}
import org.bson.codecs.configuration.CodecRegistries.{fromProviders, fromRegistries}
import org.bson.codecs.configuration.CodecRegistry
import org.mongodb.scala.MongoClient.DEFAULT_CODEC_REGISTRY
import org.mongodb.scala.bson.codecs.Macros._
import org.mongodb.scala.{MongoClient, MongoDatabase}

object DBManager {
  private val mongoClient: MongoClient = MongoClient("mongodb://192.168.1.26")

  implicit val UserAuthDecoder: Decoder[UserAuth] = deriveDecoder[UserAuth]
  implicit val UserAuthEncoder: Encoder[UserAuth] = deriveEncoder[UserAuth]

  implicit val PostDecoder: Decoder[Post] = deriveDecoder[Post]
  implicit val PostEncoder: Encoder[Post] = deriveEncoder[Post]

  val codecRegistry: CodecRegistry = fromRegistries(fromProviders(
    classOf[UserAuth],
    classOf[Post],
  ), DEFAULT_CODEC_REGISTRY)


  def GetMongoConnection(): Option[MongoDatabase] = {
    Option(mongoClient.getDatabase("3degdatabase").withCodecRegistry(codecRegistry))
  }
}
