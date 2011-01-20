package com.pp.sjm
package util

object utils {
  /** Returns a given result, but executes the supplied closure before returning.
 * (The effect of this closure does not influence the returned value.)
 *
 * @param result the result to be returned
 * @param block  code to be executed, purely for its side-effects
 */
  trait ReturnAndDo[T]{
    def andDo(block: => Unit): T
  } // gotta love Smalltalk syntax :-)
  
  def return_[T](result: T): ReturnAndDo[T] =
    new ReturnAndDo[T] {
      val r = result
      def andDo(block: => Unit): T = {block; r}
  }
}