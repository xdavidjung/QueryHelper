import org.scalatest.Suite
import edu.washington.cs.knowitall.queryhelper.Query
import edu.washington.cs.knowitall.queryhelper.OpenIEQueryHelper

class OpenIEQueryHelperTest extends Suite {
  
  // for queries that have an arg1 starting with "which", "what"
  val wnArg1s = Array[Query](
                  new Query("which swimmer", "smoke", "pot")
                )
  
  // for queries that have an arg2 starting with "which", "what"
  val wnArg2s = Array[Query](
                  new Query("golden retriever", "eat", "what food")
                )
  
  // for queries that have both args starting with "which", "what"
  val wnBoths = Array[Query](
                 new Query("what food", "grown in", "which country")
               )
  
  // for queries that have neither arg starting with "which", "what"
  val unfixable = Array[Query](
                 new Query("fda", "approved", "chemicals")
               )
  
  def betterQuery(query: Query): Query = OpenIEQueryHelper.betterQuery(query)
  def queryHelp(query: Query): String = OpenIEQueryHelper.queryHelp(query)
  
  def testWnArg1s = {
    expect(new Query("type:swimmer", "smoke", "pot")) {
      betterQuery(wnArg1s(0))
    }
  }
  
  def testWnArg2s = {
    expect(new Query("golden retriever", "eat", "type:food")) {
      betterQuery(wnArg2s(0))
    }
  }
  
  def testWnBoths = {
    expect(new Query("type:food", "grown in", "type:country")) {
      betterQuery(wnBoths(0))
    }
  }
  
  def testWnNones = {
    expect(unfixable(0)) {
      betterQuery(unfixable(0))
    }
  }
}