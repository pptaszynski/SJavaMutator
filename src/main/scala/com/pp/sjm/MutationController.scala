/**
 * 
 */
package com.pp.sjm

import symbols.{Scoping}
import parser.{JavaParser}
import ast._
import mutation._

import scala.collection.immutable.{StringOps}
import scala.collection.mutable.{LinkedList,HashSet}
import scala.util.Random

import java.util.Date

object MutationKind extends Enumeration {
  type MutationKind = Value
  val RELATIONAL_OP_CHANGE, REMOVE_THIS, CHANGE_GETTER, REMOVE_THROW = Value
}

/**
 * @author Pawel
 *
 */
object MutationController extends Scoping with Mutations with RelationalOpMutations with ThisMutations with MethodsMutations {
  import MutationKind._
  //def ast = parseResults.filter(_.isInstanceOf[ClassNode])
  var source: StringOps = new StringOps("")
  var candidates: HashSet[Mutation] = new HashSet[Mutation]
  val gen = new Random((new Date()).getTime())
  var enabledMutations = List[MutationKind]()
  val parser = new JavaParser()
  
  def applyMutations(n: Int) = {
    val mutatedIndexes = HashSet[Int]()
    val queue = candidates.filter(enabledMutations contains _.kind).zipWithIndex
    for(i <- 0 until n.min(queue.size)) {
      var idx = gen.nextInt(queue.size)
      while (mutatedIndexes.contains(idx)) idx = gen.nextInt(queue.size) 
      val mut = queue.find(_._2 == idx)
      mut match {
        case Some(op) => { 
          source = op._1(source); mutatedIndexes.add(idx);
        }
        case None => println("For some unknown reason couldn't find mutation of selected index: " + idx)
      }
    }
  }
  
  def addMutationCandidate(candidate: Node, kind: MutationKind, alternativesSet: Iterable[String] = Nil) {
    var mutation: Mutation = kind match {
      case RELATIONAL_OP_CHANGE => candidate match {
        case c: Relational => new RelationalOpChangeMutation(new RelationalOpMutationCandidate(c))
        case _ => throw new MatchError("Tried to perform " + kind + " mutation on not Relational node. Actual node type is " + candidate.getClass.getSimpleName +".")
      }
      case REMOVE_THIS => candidate match {
        case c: ThisExpr => new ThisRemovalMutation(new ThisMutationCandidate(c))
        case _ => throw new MatchError("Tried to perform " + kind + " mutation on not this. node. Actual node type is " + candidate.getClass.getSimpleName +".")
      }
      case CHANGE_GETTER => candidate match {
        case c: ReturnStatement => new ChangeGetterMutation(new ReturnValueMutationCandidate(c), alternativesSet)
        case _ => throw new MatchError("Tried to perform " + kind + " mutation on not return statement node. Actual node type is " + candidate.getClass.getSimpleName +".") 
      }
      case REMOVE_THROW => candidate match {
        case c: ThrowStatement => new RemoveThrowMutation(new RemoveThrowMutationsCandidate(c))
        case _ => throw new MatchError("Tried to perform " + kind + " mutation on not throw statement node. Actual node type is " + candidate.getClass.getSimpleName +".")
      }
      case _ => throw new NotDefinedError("The mutation " + kind + " is not defined.")
    }
    candidates.add(mutation)
  }
  
  def mutate(source: String, n: Int, enabled: List[MutationKind]): String = {
    reset()
    this.enabledMutations = enabled
    this.source = new StringOps(source)
    parser(this.source) match {
      case Some(_) => {
        println("Source parsing succeded...")
        println("Applying mutations... ")
        applyMutations(n)
        return this.source
      }
      case None => {
        return "<mutation error: Source invalid>"
      }
      
    } 
  }
  
  def reset() {
    this.candidates = new HashSet[Mutation]()
    this.source = new StringOps("")
  }
}