import java.io.{File, FileWriter, PrintWriter}
import java.util
import scala.util.control.Breaks.break

class CompilationEngine(JackFile: File, wri: Writer) {
  val tokenizer: JackTokenizer = new JackTokenizer(JackFile)
  val writer: Writer = wri

  def compileClass(): Unit = {
    tokenizer.advance()
    var ty: TYPE = tokenizer.currentTokenType
    if (ty != TYPE.KEYWORD || tokenizer.keyword() != KEYWORD.CLASS) {
      error("class")
    }
    writer.openClass()
    writer.writeTokenTag(ty, tokenizer.getToken())
    tokenizer.advance()
    ty = tokenizer.getTokenType()
    if (ty != TYPE.IDENTIFIER) {
      error("class name")
    }
    writer.writeTokenTag(ty, tokenizer.getToken())
    requireSymbol("{")
    compileClassVarDec()
    compileSubroutineDec()
    requireSymbol("}")
    if (tokenizer.hasMoreTokens()) {
      throw new IllegalStateException("Unexpected Exception")
    }
    writer.closeClass()
  }

  def compileClassVarDec(): Unit = {
    tokenizer.advance()
    //if { found this is end of class return
    try {
      tokenizer.symbol()
      tokenizer.pointerBack()
      return
    } catch {
      case i: _ =>
    }

    if (tokenizer.getTokenType() != TYPE.KEYWORD) {
      error("keyword")
    }

    if (List(KEYWORD.CONSTRUCTOR, KEYWORD.FUNCTION, KEYWORD.METHOD).contains(tokenizer.keyword())) {
      tokenizer.pointerBack()
      return
    }
    writer.openTag("classVarDec")
    if (!List(KEYWORD.STATIC, KEYWORD.FIELD).contains(tokenizer.keyword())) {
      error("static or field")
    }
    writer.writeTokenTag(tokenizer.getTokenType(), tokenizer.getToken())
    compileType()
    while (true) {
      //varName
      tokenizer.advance()
      if (tokenizer.getTokenType() != TYPE.IDENTIFIER) {
        error("identifier")
      }
      writer.writeTokenTag(tokenizer.getTokenType(), tokenizer.getToken())
      //',' or ';'
      tokenizer.advance()
      val symbol: Char = tokenizer.symbol()
      if (tokenizer.getTokenType() != TYPE.SYMBOL || !List(',', ';').contains(symbol)) {
        error("',' or ';'")
      }
      writer.writeTokenTag(tokenizer.getTokenType(), symbol.asInstanceOf[String])
      if (tokenizer.symbol() == ';') {
        break
      }
    }
    writer.closeTag("classVarDec")
    compileClassVarDec()
  }

  def compileType(): Unit = {
    tokenizer.advance()
    if (List(KEYWORD.INT, KEYWORD.CHAR, KEYWORD.BOOLEAN).contains(tokenizer.keyword())) {

    }
  }

  def compileSubroutineDec(): Unit = {

  }

  //help function

  def requireSymbol(symbol: String): Unit = {
    tokenizer.advance()
    val ty: TYPE = tokenizer.getTokenType()
    if (ty == TYPE.SYMBOL && tokenizer.currentToken == symbol) {
      writer.writeTokenTag(ty, symbol)
    } else error(s"$symbol")
  }

  def error(token: String): Unit = {
    throw new IllegalStateException(s"Expected token missing: $token. Current token: ${tokenizer.getToken()}")
  }


}
