package edu.washington.cs.knowitall.queryhelper

abstract class QueryHelper {
  
  /** Given a completed query, finds potential issues with the arguments
   *  and returns an informative message about how to fix them. 
   *  
   *  @param query a completed Query
   *  @return a message detailing potential problems with the query; if none,
   *          returns an empty string.
   */
  def queryHelp(query: Query): String
  
  /** Given a completed query, attempts to extrapolate the user's intention
   *  and returns a better query.
   * 
   * @param query a completed Query
   * @return a better Query with the meaning of the argument query intact. 
   */
  def betterQuery(query: Query): Query
  
}