import java.io.{File, FileWriter}
import java.util
import javax.sql.rowset.spi.XmlWriter

object JackAnalyzer {

  def main(args: Array[String]): Unit = {
    //System.getProperty("user.dir")
    val obj = new File(args(0))
    var jackFiles: List[File] = null
    if (obj.isDirectory) {
      jackFiles = handleDirectory(obj)
    }
    else {
      jackFiles = handelFile(obj)
    }
    for (currentJackFile <- jackFiles) {
      val nameOfJaFile = currentJackFile.toString.substring(currentJackFile.toString.lastIndexOf("\\") + 1, currentJackFile.toString.lastIndexOf("."))
      val treeOutputFile = "My_" + nameOfJaFile + ".xml"
      val tokenOutputFile = "My_" + nameOfJaFile + "T.xml"
      val parent = currentJackFile.getParent

      val outFile: File = new File(parent, treeOutputFile)
      val outTokenFile: File = new File(parent, tokenOutputFile)

      val writerInc: Writer = new xmlWriter(outFile, outTokenFile)

      val compilationEngine = new CompilationEngine(currentJackFile, writerInc)
      try {
        compilationEngine.compileClass()
      }
      catch {
        case _ => writerInc.closeClass()
      }

      println("File created :" + treeOutputFile)
      println("File created : " + tokenOutputFile)

    }

    def handelFile(dir: File): List[File] = {
      if (!dir.getName.endsWith(".jack")) {
        throw new Exception("missing files!!!")
      }
      List(dir)
    }

    def handleDirectory(dir: File): List[File] = {
      val ret = dir.listFiles.filter(_.isFile).toList.filter(p => p.toString.endsWith(".jack"))
      if (ret.length == 0) {
        throw new Exception("mising files!!!")
      }
      ret
    }
  }
}
