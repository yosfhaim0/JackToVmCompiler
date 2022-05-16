import java.io.{File, PrintWriter}

class xmlWriter(outFile: File, outTokenFile: File) extends Writer {
  val printWriter: PrintWriter = new PrintWriter(outFile)
  val tokensPrintWriter: PrintWriter = new PrintWriter(outTokenFile)

  override def openTag(string: String): Unit = {
    printWriter.println("<" + string + ">")
  }

  override def closeTag(string: String): Unit = {
    printWriter.println("</" + string + ">")
  }

  def writeTokenTag(ty: TYPE, bodyTag: String): Unit = {
    val nameTag: String = ty.toString.toLowerCase()
    val CurrentTag: String = s"<$nameTag> $bodyTag </$nameTag>"
    printWriter.print(s"$CurrentTag\n")
    tokensPrintWriter.print(s"$CurrentTag\n")
  }

  override def openClass(): Unit = {
    printWriter.println("<class>")
    tokensPrintWriter.println("<tokens>")
  }

  override def closeClass(): Unit = {
    tokensPrintWriter.println("</tokens>")
    printWriter.println("</class>")
  }
}
