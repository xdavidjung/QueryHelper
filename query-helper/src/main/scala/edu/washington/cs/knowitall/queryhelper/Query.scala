package edu.washington.cs.knowitall.queryhelper

class Query(a1: String, r: String, a2: String) {
  
  val arg1 = if (a1 == null) "" else a1
  val rel = if (r == null) "" else r
  val arg2 = if (a2 == null) "" else a2
  
  def isFilled = arg1 != "" && rel != "" && arg2 != ""
  def hasArg1 = arg1 != ""
  def hasRel = rel != ""
  def hasArg2 = arg2 !=""
  
  override def toString = "(" + arg1 + ", " + rel + ", " + arg2 + ")"
  
  override def equals(other: Any): Boolean = {
    other match {
      case that: Query =>
        (that canEqual this) &&
        arg1 == that.arg1 &&
        rel == that.rel &&
        arg2 == that.arg2
      
      case _ => false
    }
  }
  
  def canEqual(other: Any): Boolean = other.isInstanceOf[Query]
}