package com.pp.sjm
package symbols

import util.{utils => util}
import scala._
import com.pp.sjm.ast._

import scala.collection.mutable.{HashSet,LinkedList,LinkedHashMap}
/**
 * Created by IntelliJ IDEA.
 * User: Pawel
 * Date: 1/6/11
 * Time: 12:15 PM
 * To change this template use File | Settings | File Templates.
 */

trait Scoping {
  import util._
  
  object Env {
    /** List of available environments */
    private var environments: LinkedHashMap[Int, Env] = LinkedHashMap[Int, Env]()
    
    private var _currentEnvId: Int = -1
     
    /** Last created environment */
    def last: Env = environments.last._2
    
    /** Get's Option[Env] of environment.
     * 
     * @return Some[Env] if current environment exists. None otherwise
     */
    def current: Option[Env] = if (this._currentEnvId >= 0) environments.get(_currentEnvId) else None
    
    /**
     * Creates new Environment linked to the previous one
     *
     * @return Created environment. Current environment is set to new one.
     */
    def getNew: Env = {
      var env: Env = new Env(_currentEnvId, new Env(_currentEnvId, null))
      current match {
        case Some(curr) => {
          env = new Env(_Env.genId, current.get)
        }
        case _ => env = new Env(_Env.genId, env)
      }     
      environments += Tuple2(env.id, env)
      _currentEnvId  = env.id 
      return environments(env id)
    }
  
    /** De-activates current environment and sets the previous one as the current
     * 
     * @return Previous current environment. The old one stored in the list of environemnts.
     */
    def reduce: Env = return_(environments(environments(_currentEnvId).prev.id)) andDo {_currentEnvId = environments(_currentEnvId).prev.id}
    
    /** Remove environment from list */
    def remove(e: Env) = if (environments.filter(_._2.prev.id == e.id).isEmpty) environments remove e.id
    
    private object _Env {
      private var currentId = 0
      private[Scoping] def genId = return_(currentId) andDo {currentId = currentId + 1}
    }
  }
  
  class Env(val id: Int, val prev: Env) {
   protected val set: LinkedHashMap[String, ScopedNode] = LinkedHashMap[String,ScopedNode]()
   
   def apply(name: String): Option[TypedNode] = {
     set.get(name) match {
       case Some(node) => Some(node)
       case None => prev(name)
     }
   }
   
   def mount(node: ScopedNode) {
     set += Tuple2(node.name, node)
   }
   
   def searchOnlyCurrent(name: String): Option[ScopedNode] = set.get(name)
   
   def apply(node: TypedNode): Iterable[ScopedNode] = {
     set.values.filter(_.compatibile(node)) ++ prev(node)
   }
  }
}
