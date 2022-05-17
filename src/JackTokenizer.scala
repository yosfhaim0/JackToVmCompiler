import java.io.{File, FileWriter}
import java.util
import scala.collection.*
import scala.util.matching.*

enum TYPE:
  case KEYWORD, SYMBOL, IDENTIFIER, INT_CONST, STRING_CONST, NONE

enum KEYWORD:
  case CLASS, METHOD, FUNCTION, CONSTRUCTOR, INT, BOOLEAN, CHAR, VOID, VAR, STATIC,
  FIELD, LET, DO, IF, ELSE, WHILE, RETURN, TRUE, FALSE, NULL, THIS, NONE

class JackTokenizer(inFile: File) {
  var currentToken: String = null
  var currentTokenType: TYPE = TYPE.NONE
  var pointer = 0
  var tokens = new util.ArrayList[String]()
  val keywordReg: Regex = "class|constructor|function|method|field|static|var|int|char|boolean|void|true|false|null|this|let|do|if|else|return|while".r
  val symbolReg: Regex = "[\\&\\*\\+\\(\\)\\.\\/\\,\\-\\]\\;\\~\\}\\|\\{\\>\\=\\[\\<]".r
  val intReg: Regex = "[0-9]+".r
  val strReg: Regex = """"[^"\n]*"""".r //the three """ for padding, the fourt is for matching
  val idReg: Regex = "[a-zA-Z_][\\w]*".r
  val tokenPatterns: Regex = s"$symbolReg|$intReg|$strReg|$idReg".r
  val keywordMap: Map[String, KEYWORD] = Map("class" -> KEYWORD.CLASS, "constructor" -> KEYWORD.CONSTRUCTOR, "function" -> KEYWORD.FUNCTION,
    "method" -> KEYWORD.METHOD, "field" -> KEYWORD.FIELD, "static" -> KEYWORD.STATIC,
    "var" -> KEYWORD.VAR, "int" -> KEYWORD.INT, "char" -> KEYWORD.CHAR, "boolean" -> KEYWORD.BOOLEAN,
    "void" -> KEYWORD.VOID, "true" -> KEYWORD.TRUE, "false" -> KEYWORD.FALSE, "null" -> KEYWORD.NULL,
    "this" -> KEYWORD.THIS, "let" -> KEYWORD.LET, "do" -> KEYWORD.DO, "if" -> KEYWORD.IF,
    "else" -> KEYWORD.ELSE, "while" -> KEYWORD.WHILE, "return" -> KEYWORD.RETURN,
  )


  val JackFileLines: Iterator[String] = io.Source.fromFile(inFile).getLines()
  var JackString = ""
  for (i <- JackFileLines) {
    JackString += i + "\n"
  }
  JackString = noComments(JackString)
  val MatchsFromJackString = tokenPatterns.findAllMatchIn(JackString)
  for (i <- MatchsFromJackString) {
    tokens.add(i.toString())
  }
  //advance()

  def getToken(): String = {
    currentToken
  }

  def hasMoreTokens(): Boolean = {
    pointer < tokens.size()
  }

  def advance(): Unit = {
    if (hasMoreTokens()) {
      currentToken = tokens.get(pointer)
      pointer += 1
    } else {
      throw new IllegalStateException("No more tokens")
    }
    if (symbolReg.matches(currentToken)) {
      currentTokenType = TYPE.SYMBOL
    } else if (keywordReg.matches(currentToken)) {
      currentTokenType = TYPE.KEYWORD
    } else if (intReg.matches(currentToken)) {
      currentTokenType = TYPE.INT_CONST
    } else if (strReg.matches(currentToken)) {
      currentTokenType = TYPE.STRING_CONST
    } else if (idReg.matches(currentToken)) {
      currentTokenType = TYPE.IDENTIFIER
    } else {
      throw new IllegalArgumentException("Unknown token: $currentToken")
    }
  }

  def pointerBack(): Unit = {
    pointer = if (pointer > 0) pointer - 1 else pointer
  }

  def getTokenType(): TYPE = {
    currentTokenType
  }

  def keyword(): KEYWORD = {
    keywordMap(currentToken)
  }

  def symbol(): Char = {
    currentToken.toCharArray.head
  }

  def identifier(): String = {
    currentToken
  }

  def intVal(): Int = {
    currentToken.asInstanceOf[Int]
  }

  def stringVal(): String = {
    currentToken.substring(1, currentToken.length - 1)
  }


  def noComments(strIn: String): String = {
    strIn.replaceAll("""\/\*.+?\*\/|\/\/.*(?=[\n\r])""", "").trim()
  }
}
