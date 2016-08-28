// The scala tutorial http://www.kazachonak.com/2012/06/reactive-programming-tutorial-in-scala_11.html
package calculator

object TweetLength {
  final val MaxTweetLength = 140

  def tweetRemainingCharsCount(tweetText: Signal[String]): Signal[Int] = {
    // This is not correct ,since the signal is changing, dont stablize a value to it
    val text = tweetText()
    // Alternative: Signal(int_number)
    Var(MaxTweetLength - tweetLength(tweetText()))

  }

  def colorForRemainingCharsCount(remainingCharsCount: Signal[Int]): Signal[String] =
  //Signal constructor has to be out of the match pattern
    Signal {
  // Change the signal back to int this line
    remainingCharsCount() match {
      case x if (x >= 15) => "green"
      // You don't need to say x>=0 and x <=14, since it is read line by line
      case x if (x >= 0) => "orange"
      // Don't need to specify the last condition, _ can cover that, being more concise
      case _ => "red"
    }

  }

  /** Computes the length of a tweet, given its text string.
   *  This is not equivalent to text.length, as tweet lengths count the number
   *  of Unicode *code points* in the string.
   *  Note that this is still a simplified view of the reality. Full details
   *  can be found at
   *  https://dev.twitter.com/overview/api/counting-characters
   */
  private def tweetLength(text: String): Int = {
    /* This should be simply text.codePointCount(0, text.length), but it
     * is not implemented in Scala.js 0.6.2.
     */
    if (text.isEmpty) 0
    else {
      text.length - text.init.zip(text.tail).count(
          (Character.isSurrogatePair _).tupled)
    }
  }
}
