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
    var nameTag: String = ty.toString.toLowerCase()
    var body = bodyTag
    if (ty == TYPE.SYMBOL) {
      val sym = bodyTag.toCharArray.head.toInt
      sym match {
        case 60 => body = "&lt;" //ascii value of <
        case 62 => body = "&gt;" //ascii value of >
        case 34 => body = "&quot;" //ascii value of "
        case 38 => body = "&amp;" //ascii value of &l
        case _ =>
      }
    }
    if (ty == TYPE.STRING_CONST) {
      nameTag = "stringConstant"
    }
    if (ty == TYPE.INT_CONST) {
      nameTag = "integerConstant"
    }
    val CurrentTag: String = s"<$nameTag> $body </$nameTag>"
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
    tokensPrintWriter.close()
    printWriter.close()
  }
}
