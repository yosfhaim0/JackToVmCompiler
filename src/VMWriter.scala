import java.io.{File, PrintWriter}
import javax.swing.text.Segment
import scala.collection.Map
import scala.util.matching.UnanchoredRegex


object segment {
  val CONST: String = "constant"
  val ARG: String = "argument"
  val LOCAL: String = "local"
  val STATIC: String = "static"
  val THIS: String = "this"
  val THAT: String = "that"
  val POINTER: String = "pointer"
  val TEMP: String = "temp"
}

object command {
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

class VMWriter(outFile: File) extends Writer {
  val printWriter: PrintWriter = new PrintWriter(outFile)

  override def closeClass(): Unit = {
    printWriter.close()
  }

  override def openTag(string: String): Unit = {
    string match {
      case _ =>
    }
  }

  override def closeTag(string: String): Unit = {

  }

  override def writeTokenTag(ty: TYPE, bodyTag: String): Unit = {
    //    ty match {
    //      case TYPE.SYMBOL => bodyTag match {
    //        case "+" => printWriter.print("add")
    //        case "-" => printWriter.print("sub")
    //        case "*" => printWriter.print("call Math.multiply 2")
    //        case "/" => printWriter.print("call Math.divide 2")
    //        case "&" => printWriter.print("and")
    //        case "|" => printWriter.print("or")
    //        case "<" => printWriter.print("lt")
    //        case ">" => printWriter.print("gt")
    //        case "=" => printWriter.print("eq")
    //        case _ => error("symbol")
    //      }
    //      case TYPE.INT_CONST =>
    //      case TYPE.KEYWORD =>
    //      case TYPE.NONE =>
    //    }
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
    command match {
              case "+" => printWriter.print("add")
              case "-" => printWriter.print("sub")
              case "*" => printWriter.print("call Math.multiply 2")
              case "/" => printWriter.print("call Math.divide 2")
              case "&" => printWriter.print("and")
              case "|" => printWriter.print("or")
              case "<" => printWriter.print("lt")
              case ">" => printWriter.print("gt")
              case "=" => printWriter.print("eq")
              case _ => error("symbol")
            }
  }

  def writeLabel(label: String): Unit = {

  }

  def writeGoto(label: String): Unit = {

  }

  def writeIf(label: String): Unit = {

  }

  def writeCall(name: String, nArgs: Int): Unit = {
    writeCommand("call", name, nArgs.toString)
  }

  def writeFunction(name: String, nLocals: Int): Unit = {
    writeCommand("function", name, nLocals.toString)
  }

  def writeReturn(): Unit = {

  }

  def writeCommand(cmd: String, arg1: String = "", arg2: String = ""): Unit = {
    printWriter.println(s"$cmd $arg1 $arg2")
  }

  

  def error(token: String): Unit = {
    printWriter.close()
    throw new IllegalStateException(s"missing: $token.")
  }
}

