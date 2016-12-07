package controllers

import java.util.concurrent.TimeoutException
import javax.inject.Inject

import models.{Issue, Link}
import play.api.Logger
import play.api.Play.current
import play.api.data.Form
import play.api.data.Forms._
import play.api.i18n.MessagesApi
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import play.api.libs.json.Json
import play.api.libs.json.Json.toJsFieldJsValueWrapper
import play.api.mvc.{Action, Controller}
import play.modules.reactivemongo.{MongoController, ReactiveMongoApi, ReactiveMongoComponents}
import reactivemongo.api.QueryOpts
import reactivemongo.bson.BSONObjectID
import reactivemongo.play.json._
import reactivemongo.play.json.collection.JSONCollection
import views.html

import scala.concurrent.Future
import scala.concurrent.duration.DurationInt
import scala.math.signum

class Application @Inject()(val reactiveMongoApi: ReactiveMongoApi, val messagesApi: MessagesApi)
  extends Controller with MongoController with ReactiveMongoComponents {

  implicit val timeout = 10.seconds

  lazy val config = current.configuration

  /**
    * Describe the issue form (used in both edit and create screens).
    */
  val issueForm: Form[Issue] = Form(
    mapping(
      "id" -> ignored(BSONObjectID.generate: BSONObjectID),
      "name" -> nonEmptyText,
      "links" -> seq(
        mapping(
          "id" -> ignored(BSONObjectID.generate: BSONObjectID),
          "title" -> nonEmptyText,
          "url" -> nonEmptyText,
          "readTime" -> optional(text)
        )(Link.apply)(Link.unapply)
      )
    )(Issue.apply)(Issue.unapply)
  )

  /*
   * Get a JSONCollection (a Collection implementation that is designed to work
   * with JsObject, Reads and Writes.)
   * Note that the `collection` is not a `val`, but a `def`. We do _not_ store
   * the collection reference to avoid potential problems in development with
   * Play hot-reloading.
   */
  def collection: Future[JSONCollection] =
    database.map(_.collection[JSONCollection]("issues"))

  // ------------------------------------------ //
  // Using case classes + Json Writes and Reads //
  // ------------------------------------------ //
  import models.Formatters._
  import models._

  /**
    * Handle default path requests, redirect to issue list
    */
  def index = Action {
    Home
  }

  /**
    * This result directly redirect to the application home.
    */
  val Home = Redirect(routes.Application.list())

  /**
    * Display the paginated list of issues.
    *
    * @param page    Current page number (starts from 0)
    * @param orderBy Column to be sorted
    * @param filter  Filter applied on issue names
    */
  def list(page: Int, orderBy: Int, filter: String) = Action.async { implicit request =>
    val mongoFilter = {
      if (filter.length > 0) Json.obj("name" -> filter)
      else Json.obj()
    }
    val sortFilter = orderBy match {
      case 2 | (-2) => Json.obj("name" -> signum(orderBy))
    }

    val pageSize = config.getInt("page.size").filter(_ > 0).getOrElse(20)
    val offset = page * pageSize
    val futureTotal = collection.flatMap(_.count(Some(mongoFilter)))
    val filtered = collection.flatMap(
      _.find(mongoFilter).options(QueryOpts(skipN = page * pageSize)).sort(sortFilter).cursor[Issue]().collect[List](pageSize))

    futureTotal.zip(filtered).map { case (total, issues) => {
      implicit val msg = messagesApi.preferred(request)

      Ok(html.issues.list(Page(issues, page, offset, total), orderBy, filter))
    }
    }.recover {
      case t: TimeoutException =>
        Logger.error("Problem found in issue list process")
        InternalServerError(t.getMessage)
    }
  }

  /**
    * Display the 'edit form' of a existing issue.
    *
    * @param id Id of the issue to edit
    */
  def edit(id: String) = Action.async { request =>
    val futureEmp = collection.flatMap(_.find(Json.obj("_id" -> Json.obj("$oid" -> id))).cursor[Issue]().collect[List]())

    futureEmp.map { emps: List[Issue] =>
      implicit val msg = messagesApi.preferred(request)

      Ok(html.issues.editForm(id, issueForm.fill(emps.head)))
    }.recover {
      case t: TimeoutException =>
        Logger.error("Problem found in issue edit process")
        InternalServerError(t.getMessage)
    }
  }

  /**
    * Handle the 'edit form' submission
    *
    * @param id Id of the issue to edit
    */
  def update(id: String) = Action.async { implicit request =>
    issueForm.bindFromRequest.fold(
      { formWithErrors =>
        implicit val msg = messagesApi.preferred(request)
        Future.successful(BadRequest(html.issues.editForm(id, formWithErrors)))
      },
      issue => {
        val futureUpdateEmp = for {
          oid <- Future.fromTry(BSONObjectID parse id)
          res <- collection.flatMap(_.update(Json.obj("_id" -> Json.obj("$oid" -> id)), issue.copy(_id = oid)))
        } yield res

        futureUpdateEmp.map { result =>
          Home.flashing("success" -> s"issue ${issue.name} has been updated")
        }.recover {
          case t: TimeoutException =>
            Logger.error("Problem found in issue update process")
            InternalServerError(t.getMessage)
        }
      })
  }

  /**
    * Display the 'new issue form'.
    */
  def create = Action { request =>
    implicit val msg = messagesApi.preferred(request)
    Ok(html.issues.createForm(issueForm))
  }

  /**
    * Handle the 'new issue form' submission.
    */
  def save = Action.async { implicit request =>
    issueForm.bindFromRequest.fold(
      { formWithErrors =>
        implicit val msg = messagesApi.preferred(request)
        Future.successful(BadRequest(html.issues.createForm(formWithErrors)))
      },
      issue => {
        val futureUpdateEmp = collection.flatMap(_.insert(issue.copy(_id = BSONObjectID.generate)))

        futureUpdateEmp.map { result =>
          Home.flashing("success" -> s"issue ${issue.name} has been created")
        }.recover {
          case t: TimeoutException =>
            Logger.error("Problem found in issue update process")
            InternalServerError(t.getMessage)
        }
      })
  }

  /**
    * Handle issue deletion.
    */
  def delete(id: String) = Action.async {
    val futureInt = collection.flatMap(_.remove(Json.obj("_id" -> Json.obj("$oid" -> id)), firstMatchOnly = true))

    futureInt.map(i => Home.flashing("success" -> "issue has been deleted")).recover {
      case t: TimeoutException =>
        Logger.error("Problem deleting issue")
        InternalServerError(t.getMessage)
    }
  }

}
