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
      val vmOutFile = nameOfJaFile + ".vm"
      val parent = currentJackFile.getParent

      val outFile: File = new File(parent, treeOutputFile)
      val outTokenFile: File = new File(parent, tokenOutputFile)
      val outVmFile: File = new File(parent, vmOutFile)

      val writerInc: Writer = new XmlFileWriter(outFile, outTokenFile)
      val VMwriterInc: Writer = new VMWriter(outVmFile)

      val compilationEngine = new CompilationEngine(currentJackFile, writerInc)
      val VMcompilationEngine = new CompilationEngine(currentJackFile, VMwriterInc)

      compilationEngine.compileClass()

      VMcompilationEngine.compileClass()

            println("File created :" + treeOutputFile)
            println("File created : " + tokenOutputFile)

      println("File created : " + vmOutFile)


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
