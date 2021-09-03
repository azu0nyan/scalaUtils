package utils.log

import java.io.{PrintWriter, StringWriter}

import scala.jdk.CollectionConverters._
import java.util.Date
import java.util.logging.{Formatter, LogRecord}

class ConsoleFormatter extends Formatter {
  import java.text.SimpleDateFormat

  private val df = new SimpleDateFormat("hh:mm:ss`SSS")


  def sourceFormat(record: LogRecord): String =
    f"${if (record.getSourceClassName != null) record.getSourceClassName else record.getLoggerName}%35.35s:${record.getSourceMethodName}%-20.20s "

  def threadFormat(record: LogRecord):String =  f"${Thread.getAllStackTraces.keySet().asScala.find(_.getId == record.getLongThreadID).map(_.getName).getOrElse("Unknown")}%-15.15s"



  override def format(record: LogRecord): String =
    f"[${df.format(new Date(record.getMillis))}][${sourceFormat(record)}][${threadFormat(record)}][${record.getLevel}%-6.6s] ${record.getMessage}\n" + {
      if (record.getThrown != null) {
        val sw = new StringWriter()
        record.getThrown.printStackTrace(new PrintWriter(sw))
        sw.toString
      } else {
        ""
      }
    }

}

class FileFormatter extends Formatter {

  import java.text.SimpleDateFormat

  private val df = new SimpleDateFormat("yyyy-MM-dd hh:mm:ss`SSS")

  def sourceFormat(record: LogRecord): String =
    f"${if (record.getSourceClassName != null) record.getSourceClassName else record.getLoggerName}%60.60s:${record.getSourceMethodName}%-30.30s"

  def threadFormat(record: LogRecord):String =  f"${Thread.getAllStackTraces.keySet().asScala.find(_.getId == record.getLongThreadID).map(_.getName).getOrElse("Unknown")}%-15.15s"

  override def format(record: LogRecord): String =
    f"[${df.format(new Date(record.getMillis))}][${sourceFormat(record)}][${threadFormat(record)}][${record.getLevel}%-6.6s] ${record.getMessage}\n" + {
      if (record.getThrown != null) {
        val sw = new StringWriter()
        record.getThrown.printStackTrace(new PrintWriter(sw))
        sw.toString
      } else {
        ""
      }
    }

}
