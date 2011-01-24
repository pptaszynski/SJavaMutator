/**
 * 
 */
package com.pp.sjm
package util

/**
 * @author Pawel Ptaszynski
 *
 */
object ProgramOptions {
  var verbose = false
  var help = false
  var files = List[String]()
  var outputSuffix = ""
  var mutationsPerFile = 1
  var enabledMutations = List[MutationKind.Value]()
  
	def readOptions(args: Array[String]) = {
		var argsMap = args.toIterator
		argsMap.next()
    for (a <- argsMap) a match {
      case "-v" => verbose = true
      case "-h" => help = true
      case "-os" => if (argsMap.hasNext) outputSuffix = argsMap.next
      case "-N" | "-n" => if(argsMap.hasNext) mutationsPerFile = argsMap.next.toInt
      case "REMOVE_THIS" => enabledMutations = MutationKind.REMOVE_THIS :: enabledMutations
      case "RELATIONAL_OP_CHANGE" => enabledMutations  = MutationKind.RELATIONAL_OP_CHANGE :: enabledMutations
      case "CHANGE_GETTER" => enabledMutations = MutationKind.CHANGE_GETTER :: enabledMutations
      case "REMOVE_THROW" => enabledMutations = MutationKind.REMOVE_THROW :: enabledMutations
      case s: String => if (s.matches(".*?\\.java")) addFile(s)
    }
	}
  def addFile(name: String) = {
    this.files = this.files ::: List(name)
  }
  def filesIterator = files.iterator
} 