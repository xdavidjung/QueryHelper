package edu.washington.cs.knowitall.queryhelper

/** Represents a module that will help users of the openie demo to formulate
 *  better queries. 
 *  
 *  What this module wants to do:
 *    Help introduce users to lesser-known features of the three-keyword search
 *      such as "type:animal", "entity:Golden retriever"
 *    If a user knows how to make proper searches, help them make queries more
 *      efficiently (autocomplete with known entities/types).
 *    If a user makes a query with an easy fix, automatically fix it. 
 *  
 *  what this module does NOT do:
 *    Spellcheck
 * 
 *  @author David H Jung
 */
object OpenIEQueryHelper extends QueryHelper {
  
  // this is done before betterQuery
  def queryHelp(query: Query): String = {
    val sb: StringBuilder = new StringBuilder()
    val arg1 = query.arg1
    val rel = query.rel
    val arg2 = query.arg2

    // if all three slots are filled in and both args are not type searches, try replacing an argument with a type
    if (query.isFilled && (!arg1.contains("type:") || !arg2.contains("type:"))) {
      sb.append("\nConsider replacing an argument with a type.\n\tExample: \"animals\" -> \"type:animal\".")
    }
    // if an arg contains what, which, try replacing with type:
    else if (arg1.contains("who") || arg1.contains("what") || arg1.contains("which") || arg2.contains("what") || arg2.contains("which") || arg2.contains("who"))
      sb.append("\n\nConsider searching for types.\n\tExample: \"type:Swimmer\" instead of \"which swimmer\"")
    
    // if either arg is a type search, ask for more general types
    if (query.isFilled && (arg1.contains("type:") || arg2.contains("type:")))
      sb.append("\nIt is possible that a type you are searching for is not defined in our database. Try broadening the type (e.g., \"type:athlete\" rather than \"type:swimmer\") or removing it altogether.")
    
    // general advice:
    sb.append("\nMake sure all words are spelled correctly.")
    sb.append("\nRelation queries should be a single verb containing no nouns.")
    return sb.toString
  }
  
  // at the moment all this does is look at the arguments for "what" or "which" and 
  // replaces those with type queries. 
  def betterQuery(query: Query): Query = {
    val arg1 = query.arg1.toLowerCase.split(" ")
    val rel = query.rel.toLowerCase
    val arg2 = query.arg2.toLowerCase.split(" ")
    
    // these are what are actually returned
    var a1 = arg1.mkString(" ")
    var r = rel
    var a2 = arg2.mkString(" ")
    
    // replace "which" and "what" with "type:"
    if (arg1.length == 2 && (arg1(0) == "which" || arg1(0) == "what")) { a1 = "type:" + arg1(1) }
    if (arg2.length == 2 && (arg2(0) == "which" || arg2(0) == "what")) { a2 = "type:" + arg2(1) }
    
    return new Query(a1, r, a2)
  }
}