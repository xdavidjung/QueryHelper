package edu.washington.cs.knowitall.queryhelper

/** Represents a module that will help users of the openie demo to formulate
 *  better queries. 
 *  
 *  What this module wants to do:
 *    Help introduce users to lesser-known features of the three-keyword search
 *      such as "type:animal", "entity:Golden retriever"
 *    If a user knows how to make proper searches, help them make queries more
 *      efficiently (autocomplete with known entities/types).
 *    If a user makes a query with an easy fix, show them reformed, fixed query.
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
    val arg1 = query.arg1.toLowerCase
    val rel = query.rel.toLowerCase
    val arg2 = query.arg2.toLowerCase

    val singleBoxFilled = (query.hasArg1 && !query.hasRel && !query.hasArg2 && arg1.split(" ").length >= 3) || 
                          (!query.hasArg1 && query.hasRel && !query.hasArg2 && rel.split(" ").length >= 3) || 
                          (!query.hasArg1 && !query.hasRel && query.hasArg2 && arg2.split(" ").length >= 3) 
    val filledAndNoTypes = query.isFilled && !arg1.contains("type:") && !arg2.contains("type:") 
    val argsContainW = arg1.contains("who") || arg1.contains("what") || arg1.contains("which") || arg2.contains("what") || arg2.contains("which") || arg2.contains("who")
    val filledAndType = query.isFilled && (arg1.contains("type:") || arg2.contains("type:"))
    
    val haveSuggestions = filledAndNoTypes || argsContainW || filledAndType || singleBoxFilled
    
    if (haveSuggestions) sb.append("Suggestions:")
    
    // user tries to fill only a single box with a query
    if (singleBoxFilled)
      sb.append("\n\tIf you are putting an entire query in a single box, see the " +
          "sample queries on the home page for examples of well-formed queries.")
    
    // all three slots are filled in and both args are not type searches
    //  leave an argument blank
    //  or replace an argument with a type.
    else if (filledAndNoTypes)
      sb.append("\n\tFilling out all three boxes is often unnecessary." +
          "\n\t\tFor example, searching for (chemicals, kill, bacteria) " +
          "will return less results than leaving the first argument blank " +
          "and searching for (_, kill, bacteria) will return results for anything that kills bacteria." +
          "\n\t\tYou can also replace an argument with a relevant type: " +
          "(type:chemical, kill, bacteria) will return all chemicals that kill bacteria.")

    // if an arg contains what, which, try replacing with type
    else if (argsContainW)
      sb.append("\n\tConsider searching for types.\n\t\tExample: \"type:Swimmer\" instead of \"which swimmer\"")
    
    // if either arg is a type search, ask for more general types
    if (filledAndType)
      sb.append("\n\tIt is possible that a type you are searching for is not defined in our database. " + 
          "Try making the type more general or removing it altogether.")
    
    
    if (haveSuggestions) sb.append("\n\n")
    // general advice:
    sb.append("General search tips:")
    sb.append("\n\tMake sure all words are spelled correctly.")
    sb.append("\n\tClick on the example queries on the home page for proper usage.")
    sb.append("\n\tTry making searches less specific.")
    sb.append("\n\tRelation box should contain only a single verb, no nouns.")
    return sb.toString
  }
  
  // cases covered:
  //   arg1/arg2 have "which x" or "what x" -> replaced by "type:x"
  //   rel has "verb a|an|the|some x" and arg2 is a2 and 
  //     a2 is not of the form "which x" or "what x" -> rel: verb, arg2: x + a
  //   if the last token in a box ends with a punctuation mark, remove it.
  //   if the query is of the form (where, is, x), immediately return (x, is located in, _)
  def betterQuery(query: Query): Query = {
    if (query.arg1.toLowerCase == "where" && query.rel == "is" && query.hasArg2) {
      return betterQuery(new Query(query.arg2, "is located in", ""))
    }
    
    val arg1 = query.arg1.toLowerCase.split(" ")
    val rel = query.rel.toLowerCase.split(" ")
    val arg2 = query.arg2.toLowerCase.split(" ")
    
    val replaceArg1 = arg1.length == 2 && (arg1(0) == "which" || arg1(0) == "what")
    val replaceArg2 = arg2.length == 2 && (arg2(0) == "which" || arg2(0) == "what")
    val splitRel = !query.hasArg2 && rel.length > 2 && (rel(1) == "a" || rel(1) == "an" || rel(1) == "the" || rel(1) == "some")
    
    var a1 = ""
    var r = ""
    var a2 = ""
      
    a1 = if (replaceArg1) "type:" + arg1(1) else query.arg1
    
    // split only if arg2 does not need to be replaced
    if (splitRel && !replaceArg2) {
      r = rel(0)
      a2 = (rel.drop(2).mkString(" ") + " " + query.arg2) trim
    } else {
      r = query.rel
      a2 = query.arg2
    }
    
    if (replaceArg2) a2 = "type:" + arg2(1)
    
    // check for punctuation
    if (a1.endsWith(".") || a1.endsWith("!") || a1.endsWith("?")) a1 = a1.dropRight(1)
    if (r.endsWith(".") || r.endsWith("!") || r.endsWith("?")) r = r.dropRight(1)
    if (a2.endsWith(".") || a2.endsWith("!") || a2.endsWith("?")) a2 = a2.dropRight(1)
    
    new Query(a1, r, a2)
  }
}