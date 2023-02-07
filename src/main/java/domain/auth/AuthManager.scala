package domain.auth

import domain.database.DBManager
import org.mongodb.scala.MongoCollection

import java.util.UUID
import scala.concurrent.Await
import scala.concurrent.duration.{Duration, SECONDS}

object AuthManager {
  case class UserAuth(login: String, password: String, permissions: String, token: String = "")

  val users: List[UserAuth] = List(
    UserAuth("editor", "TheEurasian124578", "create"),
    UserAuth("head-editor", "TheEurasian124578editor", "publish"),
  )

  def checkAuth(login: String, pass: String, token: String): UserAuth = {
    val errorUser = UserAuth("error", "error", "error")
    DBManager.GetMongoConnection() match {
      case Some(mongo) =>
        val userTokensCollection: MongoCollection[UserAuth] = mongo.getCollection("userTokens")
        val userTokens = Await.result(userTokensCollection.find().toFuture(), Duration(50, SECONDS)) match {
          case values: Seq[UserAuth] => values.toList
          case _ => List.empty[UserAuth]
        }
        userTokens.find(_.token == token) match {
          case Some(tokenFound) => tokenFound
          case _ =>
            users.find(x => x.login == login && x.password == pass) match {
              case Some(userFound) =>
                val user = userFound.copy(token = UUID.randomUUID().toString)
                Await.result(userTokensCollection.insertOne(user).toFuture(), Duration(50, SECONDS))
                user
              case _ => errorUser
            }
        }
      case _ => errorUser
    }
  }

}
