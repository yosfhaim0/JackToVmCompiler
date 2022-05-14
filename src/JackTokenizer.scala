import java.io.{File, FileWriter}
import java.util
import scala.collection.*
import scala.util.matching.*

enum TYPE:
  case KEYWORD, SYMBOL, IDENTIFIER, INT_CONST, STRING_CONST, NONE

enum KEYWORD:
  case CLASS, METHOD, FUNCTION, CONSTRUCTOR, INT, BOOLEAN, CHAR, VOID, VAR, STATIC,
  FIELD, LET, DO, IF, ELSE, WHILE, RETURN, TRUE, FALSE, NULL, THIS, NONE

//class Token(tok: String, Ty: TYPE) {
//  val token = tok
//  val Type = Ty
//}
//
class JackTokenizer(inFile: File) {
  var currentToken: String = null
  var currentTokenType: TYPE = TYPE.NONE
  var pointer = 0
  var tokens = new util.ArrayList[String]()
  val keywordReg ="class|constructor|function|method|field|static|var|int|char|boolean|void|true|false|null|this|let|do|if|else|return|while".r
  //    """(class) | (constructor) | (function) | (method) | (field) | (static) |
////(var) | (int) | (char) | (boolean) |
////  (void )| (true )| (false) | (null) | (this)|
////  (let) |(do) |(if) | (else) |(while) | (return)""".r
  val symbolReg = "[\\&\\*\\+\\(\\)\\.\\/\\,\\-\\]\\;\\~\\}\\|\\{\\>\\=\\[\\<]".r
  val intReg = "[0-9]+".r
  val strReg = """"[^"\n]*"""".r //the three """ for padding, the fourt is for matching
  val idReg = "[a-zA-Z_][\\w]*".r
  val tokenPatterns = s"$symbolReg|$intReg|$strReg|$idReg".r
  val keywordMap = Map("class" -> KEYWORD.CLASS, "constructor" -> KEYWORD.CONSTRUCTOR, "function" -> KEYWORD.FUNCTION,
    "method" -> KEYWORD.METHOD, "field" -> KEYWORD.FIELD, "static" -> KEYWORD.STATIC,
    "var" -> KEYWORD.VAR, "int" -> KEYWORD.INT, "char" -> KEYWORD.CHAR, "boolean" -> KEYWORD.BOOLEAN,
    "void" -> KEYWORD.VOID, "true" -> KEYWORD.TRUE, "false" -> KEYWORD.FALSE, "null" -> KEYWORD.NULL,
    "this" -> KEYWORD.THIS, "let" -> KEYWORD.LET, "do" -> KEYWORD.DO, "if" -> KEYWORD.IF,
    "else" -> KEYWORD.ELSE, "while" -> KEYWORD.WHILE, "return" -> KEYWORD.RETURN,
  )


  var c = io.Source.fromFile(inFile).getLines()
  var y = ""
  for (i <- c) {
    y += i + "\n"
  }
  y = noComments(y)
  var u = tokenPatterns.findAllMatchIn(y)
  for (p <- u) {
    tokens.add(p.toString())
  }
  currentToken = tokens.get(0)
  pointer += 1

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

  def tokenType(): TYPE = {
    currentTokenType
  }

  def noComments(strIn: String): String = {
    strIn.replaceAll("""\/\*.+?\*\/|\/\/.*(?=[\n\r])""", "").trim()
  }
}
