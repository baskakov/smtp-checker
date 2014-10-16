package ws.bask.smtp

import javax.naming.directory.InitialDirContext
import ws.bask.util.Int
import scala.Int
import scala.util.Try

case class MxRecord(priority: Int, host: String)

object MxRecordsFinder {
  private val MxExtractor = """^(\d*)\s(.*[^.])\.*$""".r

  def lookup(domain: String) = {
    val dirContext = new InitialDirContext()
    val attributes = dirContext.getAttributes("dns:/" + domain, Array[String]("MX"))
    Option(attributes.get("MX")) match {
      case None => List(MxRecord(0,domain))
      case Some(attr) =>
        (0 until attr.size).flatMap(i => "" + attr.get(i) match {
          case MxExtractor(Int(p), h) => Some(MxRecord(p,h))
          case _ => None
        }).sortBy(_.priority)
    }
  }

  def safeLookup(domain: String) = Try(lookup(domain)).getOrElse(Nil)
}

