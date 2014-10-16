package ws.bask.smtp

import java.io._
import java.net._
import java.util._
import ws.bask.util.EmailReg._
import javax.naming.NamingException
import scala.util.Try

trait Existence

object Exists extends Existence
object NotExists extends Existence
object Unknown extends Existence

case class ExistenceCheck(email: String, exists: Boolean, proof: String)

class SmtpEmailChecker {

  private val SocketTimeOut = 15*1000

  def exists(email: String) = {
    def result(exists: Boolean, proof: String) = ExistenceCheck(email, exists(), proof)
    def askMailServers(mailServers: Seq[MxRecord]) = {
      mailServers match {
        case server :: tail :: Nil => {
          askEmailServer(email, server)
        }
        case Nil => result(false, "Rejected by all email servers")
      }
    }
    email match {
      case EmailDomain(domain) => {
        val mailServers = MxRecordsFinder.safeLookup(domain)
        if(mailServers.isEmpty) result(false, "No DNS record or mail servers found")
        else askMailServers(mailServers)
      }
      case _ => result(false, "Incorrect domain")
    }
  }

  def askEmailServer(email: String, host: String) = {
    var smtpSocket: Socket = null
    var in: BufferedReader = null
    var out: OutputStreamWriter = null
    var proof = ""

    Try {
      def getResponse: String = {
        var response: String = ""
        var line: String = null
        do {
          line = in.readLine
          if ((line == null) || (line.length < 3)) {
            throw new IOException("Bad response from server.")
          }
          response += line + "\n"
        } while ((line.length > 3) && (line.charAt(3) == '-'))
        response
      }

      smtpSocket = new Socket(host, 25)
      smtpSocket.setSoTimeout(SocketTimeOut)
      in = new BufferedReader(new InputStreamReader(smtpSocket.getInputStream))
      out = new OutputStreamWriter(smtpSocket.getOutputStream)
    }
  }
/

}
