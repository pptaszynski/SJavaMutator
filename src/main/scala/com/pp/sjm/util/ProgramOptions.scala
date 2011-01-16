/**
 * 
 */
package com.pp.sjm

/**
 * @author Pawel Ptaszynski
 *
 */
object ProgramOptions {
	def readOptions(args: Array[String]): ProgramOptions = {
		val options = new ProgramOptions(false, false)
		var fileName: String = ""
		for (a <- args) a match {
	      case "-h" | "-help"    =>
	        println("Option not implemented yet")
	      case "-v" | "-verbose" =>
	        options.Verbose(true)
	      case "-f"| "-file" =>
	        fileName = args(args.findIndexOf((x: String) => x == a) + 1)
	      case x =>
	        println("Unknown option: '" + x + "'")
	    }
		
		return options
	}
	 
} 
 
class ProgramOptions(var verbose: Boolean = false, var help: Boolean = false) {
	var files: List[String] = List() 
	
	def Verbose(value: Boolean): Unit = {
		verbose = value
	}
	def Verbose = {
		verbose == true
	}
	def Help(value: Boolean): Unit = {
		help = value
	}
	def Help = {
		help == true 
	}
	
	def addFile(name: String): Unit = {
		if (!files.exists(a => a==name)) {
			files = name :: files;
		}
	}
	
	def getFilesIterator: Iterator[String] = {
		files.iterator
	}
}