package reductions

import scala.annotation._
import org.scalameter._
import common._

object ParallelParenthesesBalancingRunner {

  @volatile var seqResult = false

  @volatile var parResult = false

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 40,
    Key.exec.maxWarmupRuns -> 80,
    Key.exec.benchRuns -> 120,
    Key.verbose -> true
  ) withWarmer (new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val length = 100000000
    val chars = new Array[Char](length)
    val threshold = 10000
    val seqtime = standardConfig measure {
      seqResult = ParallelParenthesesBalancing.balance(chars)
    }
    println(s"sequential result = $seqResult")
    println(s"sequential balancing time: $seqtime ms")

    val fjtime = standardConfig measure {
      parResult = ParallelParenthesesBalancing.parBalance(chars, threshold)
    }
    println(s"parallel result = $parResult")
    println(s"parallel balancing time: $fjtime ms")
    println(s"speedup: ${seqtime / fjtime}")
  }
}

object ParallelParenthesesBalancing {

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
    */
  def balance(chars: Array[Char]): Boolean = {
    var count = 0
    for (c <- chars) {
      count = if (c == '(') count + 1 else if (c == ')') count - 1 else count
      if (count < 0) return false
    }
    count == 0
  }

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
    */
  def parBalance(chars: Array[Char], threshold: Int): Boolean = {

    def traverse(from: Int, until: Int, openers: Int, closers: Int): Int = {
      if (from == until) openers - closers
      else if (chars(from) == '(') traverse(from + 1, until, openers + 1, closers)
      else if (chars(from) == ')') traverse(from + 1, until, openers, closers + 1)
      else traverse(from + 1, until, openers, closers)
    }

    def reduce(from: Int, until: Int): Int = {
      if (until - from <= threshold)
        traverse(from, until, 0, 0)
      else {
        val m = from + (until - from) / 2
        val (ta, tb) = parallel(reduce(from, m), reduce(m, until))
        ta + tb
      }
    }

    reduce(0, chars.length) == 0
  }

  // For those who want more:
  // Prove that your reduction operator is associative!

}
