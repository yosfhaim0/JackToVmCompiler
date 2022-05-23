trait Writer {
  def openTag(string: String): Unit

  def closeTag(string: String): Unit

  def writeTokenTag(ty: TYPE, bodyTag: String): Unit

  def closeClass(): Unit

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


