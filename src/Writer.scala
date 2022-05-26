/**
 * interface for writer language from jack to 
 * (like XML or VM...)
 */
trait Writer {
  /**
   * open scope like while,for,if...
   *
   * @param string
   */
  def openTag(string: String): Unit

  /**
   * close scope
   *
   * @param string
   */
  def closeTag(string: String): Unit

  /**
   * write token and is type like (ty=SYMBOL,bodyTag="{")...
   *
   * @param ty      type of token
   * @param bodyTag the token itself
   */
  def writeTokenTag(ty: TYPE, bodyTag: String): Unit

  /**
   * each vm file have a class so close the class is to close the file
   */
  def closeClass(): Unit

  /**
   * in start each class there is few specific action to do
   * so we write specific func for that
   */
  def openClass(): Unit

  def writePush(segment: String, index: Int): Unit

  def writePop(segment: String, index: Int): Unit

  def writeAritmetic(command: String): Unit

  def writeLabel(label: String): Unit

  def writeGoto(label: String): Unit

  def writeIf(label: String): Unit

  def writeCall(name: String, nArgs: Int): Unit

  def writeFunction(name: String, nLocals: Int): Unit

  def writeReturn(): Unit
}


