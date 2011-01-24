/**
 * 
 */
package com.pp.sjm
package mutation

import ast._
import scala.util.parsing.input.{Position, OffsetPosition}

/**
 * @author Pawel
 *
 */
trait Mutations {
  type Candidate <: MutationCandidate[Node]
  
  trait Mutation {
    val kind: MutationKind.Value
    def apply(source: String) : String 
  }
  
  trait MutationCandidate[T <: Node] {
    def node: T
    def pos: Position
  }
  
}

object Mutations extends Mutations