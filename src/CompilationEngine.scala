import java.io.{File, FileWriter, PrintWriter}
import java.util
import scala.util.control.Breaks.break

/**
 * Each function that starts with a compileXXX build
 * according to the grammar that exists in project 10 in the book nand2tetris
 *
 * @param JackFile jack file for compiling
 * @param wri      For example could be a writer XML or a writer VM
 */
class CompilationEngine(JackFile: File, wri: Writer) {
  val tokenizer: JackTokenizer = new JackTokenizer(JackFile)
  /**
   * For example could be a writer XML or a writer VM
   */
  val writer: Writer = wri
  /**
   * manage vars in jack file
   */
  var symbolTable = new SymbolTable
  /**
   * Each file is basically a class so I save each file its own class
   */
  var currentClass = ""
  /**
   * In the flow of compile Jack file we are within a
   * body of one function at any given moment
   */
  var currentSubrotione = ""
  /**
   * also keep har type
   */
  var currentSubrotioneType = ""
  /**
   * label index for unique label
   */
  var labelIndex = 0

  def compileClass(): Unit = {
    writer.openClass()
    writeToken()//"class"
    currentClass = writeToken()
    requireSymbol("{")
    while (!List(KEYWORD.CONSTRUCTOR, KEYWORD.FUNCTION, KEYWORD.METHOD).contains(tokenizer.keyword())) {
      compileClassVarDec()
    }
    while (tokenizer.getToken() != "}") {
      symbolTable.startSubroutine()
      compileSubroutineDec()
    }
    if(tokenizer.getToken()!="}"){
      throw new IllegalStateException("Unexpected Exception")
    }
    writer.writeTokenTag(tokenizer.getTokenType(), tokenizer.getToken())//"}"
    writer.closeClass()
  }

  def compileClassVarDec(): Unit = {
    //if { found this is end of class return
    if (tokenizer.getTokenType() == TYPE.SYMBOL) {
      return
    }
    writer.openTag("classVarDec")
    val staticOrField = writeToken() //static or field
    val typee = writeToken() //type
    var name = writeToken()
    symbolTable.define(name, typee, staticOrField)
    while (tokenizer.getToken() != ";") {
      writeToken()//","
      name = writeToken() //varName
      symbolTable.define(name, typee, staticOrField)
    }
    writeToken() //";"
    writer.closeTag("classVarDec")
  }


  def compileSubroutineDec(): Unit = {
    if (tokenizer.getToken() == "}") {
      return
    }
    writer.openTag("subroutineDec")
    val keyword = tokenizer.keyword()
    if (keyword == KEYWORD.METHOD)
      symbolTable.define("this", currentClass, KIND.ARG)
    //constractor or mathod or function
    currentSubrotioneType = writeToken()

    if (tokenizer.getToken() != "void") {
      writeToken() //TODO
    } else {
      writeToken()
    }
    currentSubrotione = writeToken() //Subroutine NAME
    requireSymbol("(")
    compileParameterList()
    requireSymbol(")")
    compileSubroutineBody()
    writer.closeTag("subroutineDec")

  }

  def compileParameterList(): Unit = {
    writer.openTag("parameterList")
    if (tokenizer.getToken() == ")") { //if no param
      writer.closeTag("parameterList")
      return
    }
    var typ = writeToken() //type of parameter
    var name = writeToken() //name of param
    symbolTable.define(name, typ, KIND.ARG)
    while (tokenizer.getToken() == ",") {
      writeToken()
      typ = writeToken() //type of param
      name = writeToken() //name of param
      symbolTable.define(name, typ, KIND.ARG)
    }
    writer.closeTag("parameterList")
  }

  def compileSubroutineBody(): Unit = {
    writer.openTag("subroutineBody")
    requireSymbol("{") //includ advance
    while (!List("let", "while", "if", "do", "return").contains(tokenizer.getToken())) {
      //follow of varDec ==first of Statements
      compileVarDec()
    }
    //writeFunctionDec
    writer.writeFunction(currentSubrotioneAndClass(), symbolTable.VarCount(KIND.VAR))
    currentSubrotioneType match {
      case "constructor" =>
        writer.writePush(SEGMENT.CONST, symbolTable.VarCount(KIND.FIELD))
        writer.writeCall("Memory.alloc", 1)
        writer.writePop(SEGMENT.POINTER, 0)
      case "method" =>
        writer.writePush(SEGMENT.ARG, 0)
        writer.writePop(SEGMENT.POINTER, 0)
      case _ =>

    }
    //
    compileStatements()
    requireSymbol("}")
    writer.closeTag("subroutineBody")
  }


  def compileVarDec(): Unit = {
    writer.openTag("varDec")
    writeToken() //"var"
    var typ = writeToken() //type
    var name = writeToken() //varName
    symbolTable.define(name, typ, KIND.VAR)
    while (tokenizer.getToken() != ";") {
      typ = writeToken() //","
      name = writeToken() //varName
      symbolTable.define(name, typ, KIND.VAR)
    }
    writeToken() //";"
    writer.closeTag("varDec")

  }

  def compileStatements(): Unit = {
    writer.openTag("statements")
    while (tokenizer.getToken() != "}") {
      tokenizer.getToken() match {
        //this 5 cases not need tokenizer.advance() in start of func
        case "let" => compileLet()
        case "if" => compileIf()
        case "while" => compileWhile()
        case "do" => compileDo()
        case "return" => compileReturn()
        case _ =>
          writer.closeTag("statements")
          return
      }
    }
    writer.closeTag("statements")
  }

  def compileLet(): Unit = {
    writer.openTag("letStatement")
    writeToken()
    val varName = writeToken() //varName
    var expExist = false
    if (tokenizer.getToken() == "[") {
      expExist = true
      writeToken()
      writer.writePush(getSegment(symbolTable.KindOf(varName)), symbolTable.IndexOf(varName))
      compileExpression()
    } else {
      requireSymbol("=")
      compileExpression()
      requireSymbol(";")
      //pop expression value directly
      writer.writePop(getSegment(symbolTable.KindOf(varName)), symbolTable.IndexOf(varName))
      writer.closeTag("letStatement")
      return
    }
    requireSymbol("]")
    writer.writeAritmetic("+")
    requireSymbol("=")
    compileExpression()
    requireSymbol(";")
    if (expExist) {
      //TODO UNDERSTAND THE CODE!!!
      //*(base+offset) = expression
      //pop expression value to temp
      writer.writePop(SEGMENT.TEMP, 0)
      //pop base+index into 'that'
      writer.writePop(SEGMENT.POINTER, 1)
      //pop expression value into *(base+index)
      writer.writePush(SEGMENT.TEMP, 0)
      writer.writePop(SEGMENT.THAT, 0)
    }
    writer.closeTag("letStatement")
  }

  def compileIf(): Unit = {
    writer.openTag("ifStatement")
    writeToken()
    requireSymbol("(")
    compileExpression()
    requireSymbol(")")

    writer.writeAritmetic("~")
    val elseStatment = labelIndexIncrement()
    val afterElse = labelIndexIncrement()
    writer.writeIf(label(elseStatment))

    requireSymbol("{")
    compileStatements()
    requireSymbol("}")

    writer.writeGoto(label(afterElse))

    writer.writeLabel(label(elseStatment))

    if (tokenizer.getToken() != "else") {
      writer.closeTag("ifStatement")
      writer.writeLabel(label(afterElse))
      return
    }

    writeToken()
    requireSymbol("{")
    compileStatements()
    requireSymbol("}")
    writer.writeLabel(label(afterElse))
    writer.closeTag("ifStatement")
  }

  def compileWhile(): Unit = {
    writer.openTag("whileStatement")
    writeToken()
    //save the index for the end of while
    val startOfWhile = labelIndexIncrement()
    val endOfWhile = labelIndexIncrement()
    writer.writeLabel(label(startOfWhile))

    requireSymbol("(")
    compileExpression()
    requireSymbol(")")

    writer.writeAritmetic("~")
    writer.writeIf(label(endOfWhile))

    requireSymbol("{")
    compileStatements()
    requireSymbol("}")

    writer.writeGoto(label(startOfWhile))
    writer.writeLabel(label(endOfWhile))

    writer.closeTag("whileStatement")
  }

  /**
   * for compileDo and compileTerm handel write Function Declaration
   */
  def compileSubroutineCall(subrotineOrClass: String): Unit = {
    val name = subrotineOrClass
    var nArgs = 0
    if (tokenizer.getToken() == "(") {
      //subrotine
      writeToken()
      writer.writePush(SEGMENT.POINTER, 0)
      val symbolType = symbolTable.TypeOff(name)
      nArgs = 1
      nArgs += compileExpressionList()
      requireSymbol(")")
      writer.writeCall(currentClass + "." + name, nArgs)

    } else { //"."
      writeToken()
      val nameOfSubrotine = writeToken() //subrotion Name
      requireSymbol("(")
      if (symbolTable.TypeOff(name) == KIND.NONE) { //static func
        nArgs += compileExpressionList()
        requireSymbol(")")
        writer.writeCall(name + "." + nameOfSubrotine, nArgs)
      } else { //method of object
        writer.writePush(getSegment(symbolTable.KindOf(name)), symbolTable.IndexOf(name))
        nArgs += compileExpressionList() + 1
        requireSymbol(")")
        writer.writeCall(symbolTable.TypeOff(name) + "." + nameOfSubrotine, nArgs)
      }
    }

  }

  def compileDo(): Unit = {
    writer.openTag("doStatement")
    writeToken() //"do
    compileSubroutineCall(writeToken()) //"subrotineName or className or varName
    writer.writePop(SEGMENT.TEMP, 0)
    requireSymbol(";")
    writer.closeTag("doStatement")
  }

  def compileReturn(): Unit = {
    writer.openTag("returnStatement")
    writeToken()
    if (tokenizer.getToken() == ";") {
      writeToken()
      writer.writePush(SEGMENT.CONST, 0)
    } else {
      compileExpression()
      writeToken()
    }
    writer.writeReturn()
    writer.closeTag("returnStatement")
  }

  def compileExpression(): Unit = {
    writer.openTag("expression")
    compileTerm()
    while (List("+", "-", "*", "/", "&", "|", "<", ">", "=").contains(tokenizer.getToken())) {
      val op = writeToken()
      compileTerm()
      writer.writeAritmetic(op)
    }
    writer.closeTag("expression")
  }

  def compileTerm(): Unit = {
    writer.openTag("term")
    if (tokenizer.getTokenType() == TYPE.INT_CONST) {
      //integerConstant just push its value onto stack
      writer.writePush(SEGMENT.CONST, tokenizer.intVal())
      writeToken()
    }
    else if (tokenizer.getTokenType() == TYPE.STRING_CONST) {
      //stringConstant new a string and append every char to the new stack
      val str = tokenizer.stringVal()
      writer.writePush(SEGMENT.CONST, str.length())
      writer.writeCall("String.new", 1)
      for (ch <- str) {
        writer.writePush(SEGMENT.CONST, ch.toInt)
        writer.writeCall("String.appendChar", 2)
      }
      writeConstStringToken()
    } else if (List("true", "false", "null", "this").contains(tokenizer.getToken())) {
      writeToken() match {
        case "true" => writer.writePush(SEGMENT.CONST, 0)
          writer.writeAritmetic("~")
        case "this" => writer.writePush(SEGMENT.POINTER, 0)
        case "null" => writer.writePush(SEGMENT.CONST, 0)
        case "false" => writer.writePush(SEGMENT.CONST, 0)
      } //todo this handel!!!!!

    } else if (tokenizer.getToken() == "(") {
      writeToken()
      compileExpression()
      requireSymbol(")")
    } else if (tokenizer.getToken() == "-" || tokenizer.getToken() == "~") {
      val token = writeToken()
      compileTerm()
      token match {
        case "-" => writer.writeAritmetic("neg")
        case "~" => writer.writeAritmetic("~")
      }
    } else if (tokenizer.getTokenType() == TYPE.IDENTIFIER) {
      val classOrSubrotineOrArrayOrVar = writeToken()
      tokenizer.getToken() match {
        case "[" => //if array
          writeToken()
          writer.writePush(getSegment(symbolTable.KindOf(classOrSubrotineOrArrayOrVar)), symbolTable.IndexOf(classOrSubrotineOrArrayOrVar))
          compileExpression()
          requireSymbol("]")
          writer.writeAritmetic("+")
          writer.writePop(SEGMENT.POINTER, 1)
          writer.writePush(SEGMENT.THAT, 0)
        case "(" =>
          compileSubroutineCall(classOrSubrotineOrArrayOrVar)
        case "." =>
          compileSubroutineCall(classOrSubrotineOrArrayOrVar)
        case _ =>
          writer.writePush(getSegment(symbolTable.KindOf(classOrSubrotineOrArrayOrVar)), symbolTable.IndexOf(classOrSubrotineOrArrayOrVar))
      }
    }
    writer.closeTag("term")
  }


  def compileExpressionList(): Int = {
    writer.openTag("expressionList")
    if (tokenizer.getToken() == ")") {
      writer.closeTag("expressionList")
      return 0
    }
    var nArgs = 0
    if (List(TYPE.INT_CONST, TYPE.STRING_CONST, TYPE.IDENTIFIER).contains(tokenizer.getTokenType()) ||
      List("true", "false", "null", "this", "(", "-", "~").contains(tokenizer.getToken())) {
      nArgs = 1
      compileExpression()
    }
    while (tokenizer.getToken() == ",") {
      writeToken()
      compileExpression()
      nArgs += 1
    }
    writer.closeTag("expressionList")
    nArgs
  }

  //help function

  /**
   * if i have a var and i want to mapping him to segment that exist in VM language
   *
   * @param KI can be VAR ARG FIELD STATIC segment in Jack program
   * @return equivalent segment in VM program
   */
  def getSegment(KI: String): String = {
    KI match {
      case KIND.FIELD => SEGMENT.THIS
      case KIND.STATIC => SEGMENT.STATIC
      case KIND.VAR => SEGMENT.LOCAL
      case KIND.ARG => SEGMENT.ARG
      case _ => ""
    }
  }

  /**
   * I need a unique label so I build this function and send
   * it a unique index, I wanted all
   * the management of label printing to be managed by a function so I built it
   *
   * @param index unique index
   * @return unique LABEL
   */
  def label(index: Int): String = {
    s"LABEL_$index"
  }

  /**
   * I need a different label for the entire length of the file, so I need a
   * function that returns an index to the label each time so that it is unique.
   *
   * @return unique index
   */
  def labelIndexIncrement(): Int = {
    val index = labelIndex
    labelIndex += 1
    index
  }

  def currentSubrotioneAndClass(): String = {
    currentClass + "." + currentSubrotione
  }

  /**
   * For code shortening,
   * I usually use both writeTokenTag and advance,
   * the function returns the previous current token
   * because I usually also use this token
   *
   * @return Returns the previous current token
   */
  def writeToken(): String = {
    val r = tokenizer.getToken()
    writer.writeTokenTag(tokenizer.getTokenType(), r)
    tokenizer.advance()
    r
  }

  def writeConstStringToken(): Unit = {
    writer.writeTokenTag(tokenizer.getTokenType(), tokenizer.stringVal())
    tokenizer.advance()
  }

  /**
   * shorten of writing a symbol and also a test
   * (although not consumed) whether the symbol is really who we expect
   *
   * @param symbol A symbol that we expect to have the next token
   */
  def requireSymbol(symbol: String): Unit = {
    val ty: TYPE = tokenizer.getTokenType()
    if (ty == TYPE.SYMBOL && tokenizer.currentToken == symbol) {
      writer.writeTokenTag(ty, symbol)
      tokenizer.advance()
    } else error(s"$symbol")
  }

  /**
   * The function throws an error with an output that explains what the problem is.
   * And the function also closes the writer so we can see what the file looks like until an error occurs
   *
   * @param token Explains where the problem happened
   * @throws IllegalStateException when token missing
   */
  def error(token: String): Unit = {
    writer.closeClass()
    throw new IllegalStateException(s"Expected token missing: $token. Current token: ${tokenizer.getToken()}")
  }


}
