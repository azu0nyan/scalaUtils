package utils

import java.util.logging.Logger

object Logging {
  implicit lazy val logger:Logger = {
//    System.setProperty("java.util.logging.SimpleFormatter.format", "[%1$tF %1$tT] [%4$-7s] %5$s %n")
    Logger.getLogger("UTILS")
  }

}
