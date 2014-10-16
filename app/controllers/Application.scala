package controllers

import play.api._
import play.api.mvc._
import play.api.data._
import play.api.data.Forms._
import com.arcww.smtp.SMTPSession

case class CheckData(host: String, port: Int, email: String)

object Application extends Controller {

  def index = Action {
    Ok(views.html.index(checkForm))
  }


  private val checkForm = Form(
    mapping(
      "host" -> text,
      "port" -> number,
      "email" -> text
    )(CheckData.apply)(CheckData.unapply)
  )

  def submit = Action { implicit request =>
    val data = checkForm.bindFromRequest.value.map({
      case CheckData(host,port,email) =>
        val session = new SMTPSession(host, port, email, "test@mail.ru", "Subject", "Body")
        session.sendMessage()
    })

    Ok(views.html.index(checkForm.bindFromRequest, data))
  }

}