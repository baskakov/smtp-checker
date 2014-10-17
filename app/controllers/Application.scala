package controllers

import play.api._
import play.api.mvc._
import play.api.data._
import play.api.data.Forms._
import com.arcww.smtp.{MailHostsLookup, SMTPMXLookup, SMTPSession}
import ws.bask.smtp.{SmtpCheckResult, EmailChecker}

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

case class CheckData(email: String)

object Application extends Controller {

  def index = Action {
    Ok(views.html.index(checkForm))
  }


  private val checkForm = Form(
    mapping(
      "email" -> text
    )(CheckData.apply)(CheckData.unapply)
  )

  def submit = Action.async { implicit request =>
    val data = checkForm.bindFromRequest.value.map({
      case CheckData(email) =>
        EmailChecker.check(email)
    })

    val ft = data.getOrElse(Future.successful(Nil))

    ft.map(ls => Ok(views.html.index(checkForm.bindFromRequest, Some(ls))))
  }

}