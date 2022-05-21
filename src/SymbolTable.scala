import scala.collection.Map


object KIND {
  val STATIC: String = "static"
  val FIELD: String = "field"
  val ARG: String = "argument"
  val VAR: String = "var"
  val NONE: String = "NONE"
}

//List(typee, kind,index)
class SymbolTable {
  val classLevelTableSymbols = scala.collection.mutable.Map[String, List[String]]()
  val subroutineLevelTableSymbols = scala.collection.mutable.Map[String, List[String]]()

  def startSubroutine(): Unit = {
    subroutineLevelTableSymbols.clear()
  }

  def define(name: String, typee: String, kind: String): Unit = {
    if (kind == KIND.FIELD || kind == KIND.STATIC) {
      classLevelTableSymbols += ((name, List(typee, kind)))
    } else {
      subroutineLevelTableSymbols += (name -> List(typee, kind))
    }
  }

  def VarCount(kind: String): Int = {
    var sum = 0
    for ((k, v) <- classLevelTableSymbols) {
      if (v(3) == kind)
        sum += 1
    }
    for ((k, v) <- subroutineLevelTableSymbols) {
      if (v(3) == kind)
        sum += 1
    }
    sum
  }

  def KindOf(name: String): String = {
    if (subroutineLevelTableSymbols.contains(name)) {
      return subroutineLevelTableSymbols(name)(3)
    }
    KIND.NONE
  }

  def TypeOf(name: String): String = {
    if (subroutineLevelTableSymbols.contains(name)) {
      return subroutineLevelTableSymbols(name)(2)
    }
    KIND.NONE
  }

  def IndexOf(name: String): Unit = {
    def TypeOf(name: String): String = {
      if (subroutineLevelTableSymbols.contains(name)) {
        return subroutineLevelTableSymbols(name)(3)
      }
      KIND.NONE
    }
  }

}
//class rowInTable {
//  var typ: String
//  var kind: KIND
//  var index: Int
//}
//
//// a two-arg constructor
//def apply(typ: String, kind: KIND): rowInTable = {
//  val row = new rowInTable
//  row.typ = typ
//  row.kind = kind
//  row
//}
//
//// a two-arg constructor
//def apply(typ: String, kind: KIND, index: Int): rowInTable = {
//  val row = new rowInTable
//  row.typ = typ
//  row.kind = kind
//  row.index = index
//  row
//}