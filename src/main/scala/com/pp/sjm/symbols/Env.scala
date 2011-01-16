package com.pp.sjm.symbols

import com.pp.sjm.ast._

import scala.collection.mutable.{HashSet,LinkedList}
/**
 * Created by IntelliJ IDEA.
 * User: Pawel
 * Date: 1/6/11
 * Time: 12:15 PM
 * To change this template use File | Settings | File Templates.
 */
object Env {
  /** List of available environments */
  private var environments: List[Env] = List[Env]()

  /** Last environment */
  def current: Env = environments.head

  /**
   * Creates new Environment linked to the previous one
   *
   * @return created environment
   */
  def getNew: Env = {
    val env: Env = new Env(current)

    environments = env +: environments
    return environments.head
  }

  /**
   * Removes the current Environment.
   * Makes the environment previous to current the new current.
   *
   * @return New current environment.
   */
  def reduce: Env = {
    environments = environments.tail
    return environments.head
  }


}

protected class Env(val prev: Env) {
   protected val set: HashSet[Node] = HashSet[Node]()
}