package ws.bask.util

/**
 * Created by DmiBaska on 16.10.2014.
 */
object Int {
   def unapply(s : String) : Option[Int] = try {
     Some(s.toInt)
   } catch {
     case _ : java.lang.NumberFormatException => None
   }
 }
