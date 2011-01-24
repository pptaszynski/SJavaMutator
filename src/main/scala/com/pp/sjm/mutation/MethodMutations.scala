/**
 * 
 */
package com.pp.sjm
package mutation

import ast._
import util._

import scala.util.parsing.input.{OffsetPosition,Position}
import scala.util.{Random}

import java.util.{Date}
/**
 * @author Pawel
 *  
 */
trait MethodsMutations extends Mutations {
  class ChangeGetterMutation(cand: ReturnValueMutationCandidate, val alternatives: Iterable[String]) extends Mutation {
    val kind = MutationKind.CHANGE_GETTER 
    override def apply(source: String) : String = {
      def randName(cup: Iterable[String]) : String = {
        var gen = new Random((new Date()).getTime)
        var upperLim = cup.size
        var randomIdx = 0
        if (cup.size > 1) randomIdx = ((gen.nextInt(cup.size - 1) + gen.nextInt(cup.size - 1) + Math.log(gen.nextInt(100)))%3).asInstanceOf[Int]
        var newName = cup.toIndexedSeq(randomIdx)
        newName
      }
      var line = cand.pos.lineContents
      var newName = randName(alternatives)
      var newLine = line.substring(0,cand.pos.column - 1) + line.substring(cand.pos.column - 1).replaceFirst(cand.node.eToString, newName)
      val dest = source.linesWithSeparators.toArray
      dest(cand.pos.line - 1) = newLine
      //val ret = new StringBuilder(source.length)
        println("Applied Getter Change operator mutation at line: " + cand.pos.line +", col: " + cand.pos.column)
        print("O: "+line)
        print("M: "+newLine)
        for (i <- 0 until cand.pos.column+9) {
            print(" ")
        }
        println("^\n")
      dest.mkString
    }
  }
  
  class RemoveThrowMutation(cand: RemoveThrowMutationsCandidate) extends Mutation {
    val kind = MutationKind.REMOVE_THROW 
    override def apply(source: String) : String = {
      var line = cand.pos.lineContents
      var throwEndsHere = cand.pos.source.subSequence(cand.pos.offset,cand.pos.source.length-1).toString.indexOf(';')
      var newLinesInSlice = cand.pos.source.subSequence(cand.pos.offset, throwEndsHere + cand.pos.offset).toString.count(_ == '\n')
      var secondPart = {
        if (newLinesInSlice == 0) {
          line.substring(cand.pos.column + throwEndsHere-1)
        } else {
          ";\n"
        }
      }
      var newLine = line.substring(0,cand.pos.column - 7) + secondPart
      val dest = source.linesWithSeparators.toArray
      dest(cand.pos.line - 1) = newLine
      for (i <- 0 until newLinesInSlice) {
        dest(cand.pos.line+i) = " \n"
      }
      //val ret = new StringBuilder(source.length)
        println("Applied Remove Throw operator mutation at line: " + cand.pos.line +", col: " + cand.pos.column)
        print("O: "+line)
        print("M: "+newLine)
        for (i <- 0 until cand.pos.column+9) {
            print(" ")
        }
        println("^\n")
      dest.mkString
    }
  }
  /**
   * Candidate to mutate value returned by body.
   * 
   * @param node A AST node for return Statement
   * @param alternatives Possible alternative for the mutation.
   * @author Pawel
   *
   */
  class ReturnValueMutationCandidate(val node: ReturnStatement) extends MutationCandidate[ReturnStatement] {
    lazy val pos: OffsetPosition = node.pos.asInstanceOf[OffsetPosition]
  }
  
  /**
   * Candidate to remove throw statement 
   */
  class RemoveThrowMutationsCandidate(val node: ThrowStatement) extends MutationCandidate[ThrowStatement] {
    lazy val pos: OffsetPosition = node.pos.asInstanceOf[OffsetPosition]
  }
}