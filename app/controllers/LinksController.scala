package controllers

import java.util.concurrent.TimeoutException

import javax.inject.Inject

import scala.concurrent.Future
import scala.concurrent.duration.DurationInt
import scala.math.signum

import play.api.Logger
import play.api.i18n.MessagesApi
import play.api.mvc.{ Action, Controller }
import play.api.data.Form
import play.api.data.Forms.{ optional, text, ignored, mapping, nonEmptyText }
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import play.api.libs.json.Json, Json.toJsFieldJsValueWrapper
import play.api.Play.current

import play.modules.reactivemongo.{
MongoController, ReactiveMongoApi, ReactiveMongoComponents
}
import reactivemongo.play.json._, collection.JSONCollection
import reactivemongo.api.QueryOpts
import reactivemongo.bson.BSONObjectID

import models.{Link, Formatters, Page }
import views.html

class LinksController@Inject() (val reactiveMongoApi: ReactiveMongoApi, val messagesApi: MessagesApi)
  extends Controller with MongoController with ReactiveMongoComponents {

  implicit val timeout = 10.seconds

  lazy val config = current.configuration

  /**
    * Describe the link form (used in both edit and create screens).
    */
  val linkForm = Form(
    mapping(
      "id" -> ignored(BSONObjectID.generate: BSONObjectID),
      "title" -> nonEmptyText,
      "url" -> nonEmptyText,
      "readTime" -> optional(text))(Link.apply)(Link.unapply))

  /*
   * Get a JSONCollection (a Collection implementation that is designed to work
   * with JsObject, Reads and Writes.)
   * Note that the `collection` is not a `val`, but a `def`. We do _not_ store
   * the collection reference to avoid potential problems in development with
   * Play hot-reloading.
   */
  def collection: Future[JSONCollection] =
    database.map(_.collection[JSONCollection]("links"))

  // ------------------------------------------ //
  // Using case classes + Json Writes and Reads //
  // ------------------------------------------ //
  import play.api.data.Form
  import models._
  import models.Formatters._

  /**
    * Handle default path requests, redirect to link list
    */
  def index = Action { Home }

  /**
    * This result directly redirect to the linksController home.
    */
  val Home = Redirect(routes.LinksController.list())

  /**
    * Display the paginated list of links.
    *
    * @param page Current page number (starts from 0)
    * @param orderBy Column to be sorted
    * @param filter Filter applied on link names
    */
  def list(page: Int, orderBy: Int, filter: String) = Action.async { implicit request =>
    val mongoFilter = {
      if (filter.length > 0) Json.obj("name" -> filter)
      else Json.obj()
    }
    val sortFilter = orderBy match {
      case 2|(-2) => Json.obj("title" -> signum(orderBy))
    }

    val pageSize = config.getInt("page.size").filter(_ > 0).getOrElse(20)
    val offset = page * pageSize
    val futureTotal = collection.flatMap(_.count(Some(mongoFilter)))
    val filtered = collection.flatMap(
      _.find(mongoFilter).options(QueryOpts(skipN = page * pageSize)).sort(sortFilter).cursor[Link]().collect[List](pageSize))

    futureTotal.zip(filtered).map { case (total, links) => {
      implicit val msg = messagesApi.preferred(request)

      Ok(html.links.listLinks(Page(links, page, offset, total), orderBy, filter))
    }}.recover {
      case t: TimeoutException =>
        Logger.error("Problem found in link list process")
        InternalServerError(t.getMessage)
    }
  }

  /**
    * Display the 'edit form' of a existing link.
    *
    * @param id Id of the link to edit
    */
  def edit(id: String) = Action.async { request =>
    val futureEmp = collection.flatMap(_.find(Json.obj("_id" -> Json.obj("$oid" -> id))).cursor[Link]().collect[List]())

    futureEmp.map { emps: List[Link] =>
      implicit val msg = messagesApi.preferred(request)

      Ok(html.links.editFormLink(id, linkForm.fill(emps.head)))
    }.recover {
      case t: TimeoutException =>
        Logger.error("Problem found in link edit process")
        InternalServerError(t.getMessage)
    }
  }

  /**
    * Handle the 'edit form' submission
    *
    * @param id Id of the link to edit
    */
  def update(id: String) = Action.async { implicit request =>
    linkForm.bindFromRequest.fold(
      { formWithErrors =>
        implicit val msg = messagesApi.preferred(request)
        Future.successful(BadRequest(html.links.editFormLink(id, formWithErrors)))
      },
      link => {
        val futureUpdateEmp = for {
          oid <- Future.fromTry(BSONObjectID parse id)
          res <- collection.flatMap(_.update(Json.obj("_id" -> Json.obj("$oid" -> id)), link.copy(_id = oid)))
        } yield res

        futureUpdateEmp.map { result =>
          Home.flashing("success" -> s"link ${link.title} has been updated")
        }.recover {
          case t: TimeoutException =>
            Logger.error("Problem found in link update process")
            InternalServerError(t.getMessage)
        }
      })
  }

  /**
    * Display the 'new link form'.
    */
  def create = Action { request =>
    implicit val msg = messagesApi.preferred(request)
    Ok(html.links.createFormLink(linkForm))
  }

  /**
    * Handle the 'new link form' submission.
    */
  def save = Action.async { implicit request =>
    linkForm.bindFromRequest.fold(
      { formWithErrors =>
        implicit val msg = messagesApi.preferred(request)
        Future.successful(BadRequest(html.links.createFormLink(formWithErrors)))
      },
      link => {
        val futureUpdateEmp = collection.flatMap(_.insert(link.copy(_id = BSONObjectID.generate)))

        futureUpdateEmp.map { result =>
          Home.flashing("success" -> s"link ${link.title} has been created")
        }.recover {
          case t: TimeoutException =>
            Logger.error("Problem found in link update process")
            InternalServerError(t.getMessage)
        }
      })
  }

  /**
    * Handle link deletion.
    */
  def delete(id: String) = Action.async {
    val futureInt = collection.flatMap(_.remove(Json.obj("_id" -> Json.obj("$oid" -> id)), firstMatchOnly = true))

    futureInt.map(i => Home.flashing("success" -> "link has been deleted")).recover {
      case t: TimeoutException =>
        Logger.error("Problem deleting link")
        InternalServerError(t.getMessage)
    }
  }

}
