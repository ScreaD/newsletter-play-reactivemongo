package models

import play.api.libs.json._
import reactivemongo.bson.BSONObjectID

case class Issue(_id: BSONObjectID, name: String, links: Seq[Link])

case class Link(_id: BSONObjectID, title: String, url: String, readTime: Option[String])

/**
  * Helper for pagination.
  */
case class Page[A](items: Seq[A], page: Int, offset: Long, total: Long) {
  lazy val prev = Option(page - 1).filter(_ >= 0)
  lazy val next = Option(page + 1).filter(_ => (offset + items.size) < total)
}

object Formatters {

  import play.api.libs.json.Json

  // Generates Writes and Reads for Feed and User thanks to Json Macros
  implicit val issueFormat: Format[Issue] = Json.format[Issue]
  implicit val linksFormat: Format[Link] = Json.format[Link]
}
