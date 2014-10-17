package ws.bask.smtp

import java.io._
import java.net.Socket

import ws.bask.util.EmailReg

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

object EmailChecker {
  def check(email: String) = email match {
    case EmailReg.Domain(domain) => Domain(domain).check(email)
    case _ => Future.failed(new IllegalArgumentException("Incorrect domain"))
  }
}

case class Domain(name: String) {
  lazy val mailServers = Future{MxRecordsFinder.lookup(name)} map (mxs => DomainMailServers(name, mxs))

  def check(email: String) = mailServers.flatMap(_.check(email))
}

case class SmtpCheckResult(mxRecord: MxRecord, code: String, proof: List[String]) {
  def isBad = code.startsWith("2")
}

case class DomainMailServers(domainName: String, mxRecords: List[MxRecord]) {
  def check(email: String): Future[List[SmtpCheckResult]] = askNext(email, mxRecords, Future.successful(List.empty[SmtpCheckResult]))

  private def askNext(email: String, mxRecords: List[MxRecord], acc: Future[List[SmtpCheckResult]]): Future[List[SmtpCheckResult]] = mxRecords match {
    case head :: tail => askServer(email, head).flatMap(res => askNext(email, tail, acc.map(x => x :+ res)))
    case Nil => acc
  }

  private def askServer(email: String, mxRecord: MxRecord) = {
    val sock = SmtpSocket(mxRecord)
    sock.check(email).recover({
      case e => SmtpCheckResult(mxRecord, "exc", sock.journal :+ e.getMessage) //SmtpCheckResult("exc". e.)
    })
  }
}


case class SmtpSocket(server: MxRecord) {
  private val socketTimeout = 10000
  protected var smtpSocket: Socket = null
  protected var in: BufferedReader = null
  protected var out: OutputStreamWriter = null
  var journal = List.empty[String]
  def sender="saprahan@mail.ru"

  private def connect {
    smtpSocket = new Socket(server.host, 25)
    smtpSocket.setSoTimeout(socketTimeout)
    in = new BufferedReader(new InputStreamReader(smtpSocket.getInputStream))
    out = new OutputStreamWriter(smtpSocket.getOutputStream)
  }

  /**
   * Sends given command and waits for a response from server.
   * @return response received from the server.
   */
  private def sendCommand(commandString: String): String = {
    journal :+= commandString
    out.write(commandString + "\n")
    out.flush
    val response: String = getResponse
    journal :+= response
    return response
  }

  private def doCommand(commandString: String, expectedResponseStart: Char) {
    val response: String = sendCommand(commandString)
    checkServerResponse(response, expectedResponseStart)
  }

  private def checkServerResponse(response: String, expectedResponseStart: Char) {
    if (response.charAt(0) != expectedResponseStart) throw new IOException(response)
  }

  private def getResponse: String = {
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

  def check(email: String): Future[SmtpCheckResult] = Future {
    connect
    val response = getResponse
    journal :+= response
    checkServerResponse(response, '2')
    doCommand("HELO " + smtpSocket.getLocalAddress.toString, '2')
    doCommand("MAIL FROM: <" + sender + ">", '2')
    doCommand("RCPT TO: <" + email + ">", '2')
    sendCommand("RSET")
    sendCommand("QUIT")
    SmtpCheckResult(server, "OK", journal)
  }
}
