package controllers

import play.api._
import play.api.mvc._
import play.api.data._
import play.api.data.Forms._
import com.arcww.smtp.{SMTPMXLookup, SMTPSession}

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
        val res = try {
          session.sendMessage() :: "----------------------------" :: Nil
        }
        catch {
          case e => session.log :: e.getMessage :: "----------------------------" :: Nil
        }

        import scala.collection.JavaConversions._
        res ++ SMTPMXLookup.isAddressValid(email).toIndexedSeq.toList
    })

    Ok(views.html.index(checkForm.bindFromRequest, data))
  }

}