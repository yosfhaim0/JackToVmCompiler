trait Writer {
  def openTag(string: String): Unit

  def closeTag(string: String): Unit

  def writeTokenTag(ty: TYPE, bodyTag: String): Unit

  def closeClass(): Unit

  def openClass(): Unit
}


