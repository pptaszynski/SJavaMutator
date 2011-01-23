/**
 * 
 */
package com.pp.sjm
package mutation

import ast._

import scala.util.parsing.input.{OffsetPosition,Position}
/**
 * @author Pawel
 *
 */
trait ThisMutations extends Mutations {
  class ThisRemovalMutation(cand: ThisMutationCandidate) extends Mutation {
    override def apply(source: String) = {
      var line = cand.pos.lineContents
      // Why it's -2and +3? Shall be -1 and +4 IMO.
      var newLine = line.substring(0,cand.pos.column - 2) + line.substring(cand.pos.column -2).replaceFirst("this.", "")
      val dest = source.linesWithSeparators.toArray
      dest(cand.pos.line - 1) = newLine
      //val ret = new StringBuilder(source.length)
      println("Applied this removal operator mutation at line: " + cand.pos.line +", col: " + cand.pos.column)
      print("O: "+line)
      print("M: "+newLine)
      for (i <- 0 until cand.pos.column+2) {
          print(" ")
      }
      println("^\n") 
      dest.mkString
    }
  }
  
  class ThisMutationCandidate(val node: ThisExpr) extends MutationCandidate[ThisExpr] {
    val pos: OffsetPosition = node.pos.asInstanceOf[OffsetPosition]
  }
}