import java.io.{File, PrintWriter}
import javax.swing.text.Segment
import scala.collection.Map
import scala.util.matching.UnanchoredRegex


object SEGMENT {
  val CONST: String = "constant"
  val ARG: String = "argument"
  val LOCAL: String = "local"
  val STATIC: String = "static"
  val THIS: String = "this"
  val THAT: String = "that"
  val POINTER: String = "pointer"
  val TEMP: String = "temp"
}

object COMMAND {
  val ADD: String = "add"
  val SUB: String = "sub"
  val NEG: String = "neg"
  val EQ: String = "eq"
  val GT: String = "gt"
  val LT: String = "lt"
  val AND: String = "and"
  val OR: String = "or"
  val NOT: String = "not"

}

val commands: Map[String, String] = Map("+" -> "add", "-" -> "sub",
  "neg" -> "neg",
  "*" -> "call Math.multiply 2", "/" -> "call Math.divide 2",
  "&" -> "and", "|" -> "or", "<" -> "lt",
  ">" -> "gt", "=" -> "eq", "~" -> "not")

class VMWriter(outFile: File) extends Writer {
  val printWriter: PrintWriter = new PrintWriter(outFile)

  override def closeClass(): Unit = {
    printWriter.close()
  }

  override def openTag(string: String): Unit = {
  }

  override def closeTag(string: String): Unit = {
  }

  override def writeTokenTag(ty: TYPE, bodyTag: String): Unit = {
  }

  override def openClass(): Unit = {
  }

  def writePush(segment: String, index: Int): Unit = {
    writeCommand("push", segment, index.toString)
  }

  def writePop(segment: String, index: Int): Unit = {
    writeCommand("pop", segment, index.toString)
  }

  def writeAritmetic(command: String): Unit = {
    writeCommand(commands(command))
  }

  def writeLabel(label: String): Unit = {
    printWriter.println(s"label $label")
  }

  def writeGoto(label: String): Unit = {
    printWriter.println(s"goto $label")
  }

  def writeIf(label: String): Unit = {
    printWriter.println(s"if-goto $label")
  }

  def writeCall(name: String, nArgs: Int): Unit = {
    writeCommand("call", name, nArgs.toString)
  }

  def writeFunction(name: String, nLocals: Int): Unit = {
    writeCommand("function", name, nLocals.toString)
  }

  def writeReturn(): Unit = {
    printWriter.println("return")
  }

  def writeCommand(cmd: String, arg1: String = "", arg2: String = ""): Unit = {
    printWriter.println(s"$cmd $arg1 $arg2")
  }


  def error(token: String): Unit = {
    printWriter.close()
    throw new IllegalStateException(s"missing: $token.")
  }
}

