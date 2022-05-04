import java.io.{File, FileWriter}
import java.util

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
    for (jaFi <- jackFiles) {
      val nameOfJaFile = jaFi.getName.substring(jaFi.getName.lastIndexOf("\\"), jaFi.getName.lastIndexOf("."))
      val treeOutputFile = "My_" + nameOfJaFile + ".xml"
      val tokenOutputFile="My_" + nameOfJaFile + "T.xml"
      val parent=jaFi.getParent
    }

    def handelFile(dir: File): List[File] = {
      if (!dir.getName.endsWith(".jack")) {
        throw new Exception("mising files!!!")
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
