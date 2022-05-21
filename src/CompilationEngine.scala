import java.io.{File, FileWriter, PrintWriter}
import java.util
import scala.util.control.Breaks.break

class CompilationEngine(JackFile: File, wri: Writer) {
  val tokenizer: JackTokenizer = new JackTokenizer(JackFile)
  val writer: Writer = wri
  var symbolTable = new SymbolTable

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
    writeToken()
    requireSymbol("{")
    tokenizer.advance()
    while (!List(KEYWORD.CONSTRUCTOR, KEYWORD.FUNCTION, KEYWORD.METHOD).contains(tokenizer.keyword())) {
      tokenizer.pointerBack()
      compileClassVarDec()
      tokenizer.advance()
    }
    while (tokenizer.getToken() != "}") {
      tokenizer.pointerBack()
      compileSubroutineDec()
      tokenizer.advance()
    }
    tokenizer.pointerBack()
    requireSymbol("}")
    if (tokenizer.hasMoreTokens()) {
      throw new IllegalStateException("Unexpected Exception")
    }
    writer.closeClass()
  }

  def compileClassVarDec(): Unit = {
    tokenizer.advance()
    //if { found this is end of class return
    if (tokenizer.getTokenType() == TYPE.SYMBOL) {
      tokenizer.pointerBack()
      return
    }
    if (tokenizer.getTokenType() != TYPE.KEYWORD) {
      error("keyword")
    }
    writer.openTag("classVarDec")
    if (!List(KEYWORD.STATIC, KEYWORD.FIELD).contains(tokenizer.keyword())) {
      error("static or field")
    }
    writeToken() //static or field
    tokenizer.advance()
    writeToken() //type
    tokenizer.advance()
    while (tokenizer.getTokenType() == TYPE.IDENTIFIER) {
      writeToken() //varName
      tokenizer.advance() //',' or ';'
      if (tokenizer.getToken() == ",") {
        writeToken()
        tokenizer.advance()
      } else {
        writeToken()
        writer.closeTag("classVarDec")
        return
      }
    }
    writer.closeTag("classVarDec")
  }


  def compileSubroutineDec(): Unit = {
    tokenizer.advance()
    if (tokenizer.getToken() == "}") {
      tokenizer.pointerBack()
      return
    }

    if (!List(KEYWORD.CONSTRUCTOR, KEYWORD.METHOD, KEYWORD.FUNCTION).contains(tokenizer.keyword())) {
      error("SubroutineDec")
    }
    writer.openTag("subroutineDec")
    writeToken()
    tokenizer.advance()
    if (tokenizer.getToken() != "void") {
      tokenizer.pointerBack()
      tokenizer.advance()
      writeToken() //TODO
    } else {
      writeToken()
    }
    tokenizer.advance()
    writeToken() //Subroutine NAME
    requireSymbol("(")
    compileParameterList()
    requireSymbol(")")
    compileSubroutineBody()
    writer.closeTag("subroutineDec")

  }

  def compileParameterList(): Unit = {
    writer.openTag("parameterList")
    tokenizer.advance()
    if (tokenizer.getToken() == ")") { //if no param
      tokenizer.pointerBack()
      writer.closeTag("parameterList")
      return
    }
    writeToken() //type of parameter
    tokenizer.advance()
    writeToken() //name of param
    tokenizer.advance()
    while (tokenizer.getToken() == ",") {
      tokenizer.pointerBack()
      requireSymbol(",")
      tokenizer.advance()
      writeToken() //type of param
      tokenizer.advance()
      writeToken() //name of param
      tokenizer.advance()
    }
    tokenizer.pointerBack()
    writer.closeTag("parameterList")
  }

  def compileSubroutineBody(): Unit = {
    writer.openTag("subroutineBody")
    requireSymbol("{") //includ advance
    tokenizer.advance()
    while (!List("let", "while", "if", "do", "return").contains(tokenizer.getToken())) {
      //follow of varDec ==first of Statements
      tokenizer.pointerBack()
      compileVarDec()
      tokenizer.advance()
    }
    tokenizer.pointerBack()
    compileStatements()
    requireSymbol("}")
    writer.closeTag("subroutineBody")
  }

  def compileVarDec(): Unit = {
    writer.openTag("varDec")
    tokenizer.advance()
    writeToken() //"var"
    tokenizer.advance()
    writeToken() //type
    tokenizer.advance()
    writeToken() //varName
    tokenizer.advance()
    while (tokenizer.getToken() != ";") {
      writeToken() //","
      tokenizer.advance()
      writeToken() //varName
      tokenizer.advance()
    }
    writeToken() //";"
    writer.closeTag("varDec")

  }

  def compileStatements(): Unit = {
    writer.openTag("statements")
    tokenizer.advance()
    while (tokenizer.getToken() != "}") {
      tokenizer.getToken() match {
        //this 5 cases not need tokenizer.advance() in start of func
        case "let" => compileLet()
        case "if" => compileIf()
        case "while" => compileWhile()
        case "do" => compileDo()
        case "return" => compileReturn()
        case _ => tokenizer.pointerBack()
          writer.closeTag("statements")
          return
      }
      tokenizer.advance()
    }
    tokenizer.pointerBack()
    writer.closeTag("statements")
  }

  def compileLet(): Unit = {
    writer.openTag("letStatement")
    writeToken()
    tokenizer.advance()
    writeToken() //varName
    tokenizer.advance()
    if (tokenizer.getToken() == "[") {
      writeToken()
      compileExpression()
    } else {
      tokenizer.pointerBack()
      requireSymbol("=")
      compileExpression()
      requireSymbol(";")
      writer.closeTag("letStatement")
      return
    }
    requireSymbol("]")
    requireSymbol("=")
    compileExpression()
    requireSymbol(";")
    writer.closeTag("letStatement")
  }

  def compileIf(): Unit = {
    writer.openTag("ifStatement")
    writeToken()
    requireSymbol("(")
    compileExpression()
    requireSymbol(")")
    requireSymbol("{")
    compileStatements()
    requireSymbol("}")
    tokenizer.advance()
    if (tokenizer.getToken() != "else") {
      tokenizer.pointerBack()
      writer.closeTag("ifStatement")
      return
    }
    writeToken()
    requireSymbol("{")
    compileStatements()
    requireSymbol("}")
    writer.closeTag("ifStatement")
  }

  def compileWhile(): Unit = {
    writer.openTag("whileStatement")
    writeToken()
    requireSymbol("(")
    compileExpression()
    requireSymbol(")")
    requireSymbol("{")
    compileStatements()
    requireSymbol("}")
    writer.closeTag("whileStatement")
  }

  def compileDo(): Unit = {
    writer.openTag("doStatement")
    writeToken() //"do
    tokenizer.advance()
    writeToken() //"subrotineName or className or varName
    tokenizer.advance()
    if (tokenizer.getToken() == "(") {
      writeToken()
      compileExpressionList()
      requireSymbol(")")
    } else { //"."
      writeToken()
      tokenizer.advance()
      writeToken() //subrotion Name
      requireSymbol("(")
      compileExpressionList()
      requireSymbol(")")
    }
    requireSymbol(";")
    writer.closeTag("doStatement")
  }

  def compileReturn(): Unit = {
    writer.openTag("returnStatement")
    writeToken()
    tokenizer.advance()
    if (tokenizer.getToken() == ";") {
      writeToken()
    } else {
      tokenizer.pointerBack()
      compileExpression()
      tokenizer.advance()
      writeToken()
    }
    writer.closeTag("returnStatement")

  }

  def compileExpression(): Unit = {
    writer.openTag("expression")
    compileTerm()
    tokenizer.advance()
    while (List("+", "-", "*", "/", "&", "|", "<", ">", "=").contains(tokenizer.getToken())) {
      writeToken()
      compileTerm()
      tokenizer.advance()
    }
    tokenizer.pointerBack()
    writer.closeTag("expression")
  }

  def compileTerm(): Unit = {
    writer.openTag("term")
    tokenizer.advance()
    if (tokenizer.getTokenType() == TYPE.INT_CONST) {
      writeToken()
    }
    else if (tokenizer.getTokenType() == TYPE.STRING_CONST) {
      writeConstStringToken()
    } else if (List("true", "false", "null", "this").contains(tokenizer.getToken())) {
      writeToken()
    } else if (tokenizer.getToken() == "(") {
      writeToken()
      compileExpression()
      requireSymbol(")")
    } else if (tokenizer.getToken() == "-" || tokenizer.getToken() == "~") {
      writeToken()
      compileTerm()
    } else if (tokenizer.getTokenType() == TYPE.IDENTIFIER) {
      writeToken()
      tokenizer.advance()
      tokenizer.getToken() match {
        case "[" =>
          writeToken()
          compileExpression()
          requireSymbol("]")
        case "(" =>
          writeToken()
          compileExpressionList()
          requireSymbol(")")
        case "." =>
          writeToken()
          tokenizer.advance()
          writeToken() //subrotion Name
          requireSymbol("(")
          compileExpressionList()
          requireSymbol(")")
        case _ =>
          tokenizer.pointerBack()
      }
    }
    writer.closeTag("term")
  }


  def compileExpressionList(): Unit = {
    writer.openTag("expressionList")
    tokenizer.advance()
    if (tokenizer.getToken() == ")") {
      tokenizer.pointerBack()
      writer.closeTag("expressionList")
      return
    }
    if (List(TYPE.INT_CONST, TYPE.STRING_CONST, TYPE.IDENTIFIER).contains(tokenizer.getTokenType()) ||
      List("true", "false", "null", "this", "(", "-", "~").contains(tokenizer.getToken())) {
      tokenizer.pointerBack()
      compileExpression()
    }
    tokenizer.advance()
    while (tokenizer.getToken() == ",") {
      writeToken()
      compileExpression()
      tokenizer.advance()
    }
    tokenizer.pointerBack()
    writer.closeTag("expressionList")
  }

  //help function

  def writeToken(): Unit = {
    writer.writeTokenTag(tokenizer.getTokenType(), tokenizer.getToken())
  }

  //בשביל ליישר קו עם ההצעה למימוש של הספר
  def writeConstStringToken(): Unit = {
    writer.writeTokenTag(tokenizer.getTokenType(), tokenizer.stringVal())
  }

  def requireSymbol(symbol: String): Unit = {
    tokenizer.advance()
    val ty: TYPE = tokenizer.getTokenType()
    if (ty == TYPE.SYMBOL && tokenizer.currentToken == symbol) {
      writer.writeTokenTag(ty, symbol)
    } else error(s"$symbol")
  }

  def error(token: String): Unit = {
    writer.closeClass()
    throw new IllegalStateException(s"Expected token missing: $token. Current token: ${tokenizer.getToken()}")
  }


}
