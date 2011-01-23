/**
 * 
 */
package com.pp.sjm
package mutation

import ast._

import scala.util.{Random}
import java.lang.Math
import scala.util.parsing.input.{OffsetPosition}
import java.util.{Date}
/**
 * @author Pawel
 *
 */
trait RelationalOpMutations extends Mutations {
  class RelationalOpChangeMutation(cand: RelationalOpMutationCandidate) extends Mutation {
    override def apply(source: String) : String = {
      def newOp(op: String) : String = {
        if (isEquality(op)) op match {
          case "!=" => "=="
          case "==" => "!="
        } else if (isRelational(op)) {
          var diffOps = relationalOps.filter(_ != op)
          var gen = new Random((new Date()).getTime)
          var upperLim = diffOps.length
          var randomIdx = ((gen.nextInt(diffOps.length - 1) + gen.nextInt(diffOps.length - 1) + Math.log(gen.nextInt(100)))%3).asInstanceOf[Int]
          var newOp = diffOps.toIndexedSeq.apply(randomIdx)
          newOp
        } else op
      }
      
      var line = cand.pos.asInstanceOf[OffsetPosition].lineContents
      var newLine = line.replaceFirst(cand.op, newOp(cand.op))
      val dest = source.linesWithSeparators.toArray
      dest(cand.pos.line - 1) = newLine
      //val ret = new StringBuilder(source.length)
      println("Applied relational operator mutation at line: " + cand.pos.line +", col: " + cand.pos.column)
      print("O: "+line)
      print("M: "+newLine)
      for (i <- 0 until cand.pos.column+3) {
          print(" ")
      }
      println("^\n") 
      dest.mkString
    }
    
    private val equalityOps: List[String] = List[String]("!=", "==")
    private val relationalOps: List[String] = List[String]("<",">","<=",">=")
    private def isRelational(op: String) : Boolean = relationalOps contains op
    private def isEquality(op: String) : Boolean = equalityOps contains op
  }

  class RelationalOpMutationCandidate(val node: Relational) extends MutationCandidate[Relational] {
    lazy val pos: OffsetPosition = node.op.pos.asInstanceOf[OffsetPosition]
    lazy val op = node.op.chars
  }
  
}