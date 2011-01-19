package com.pp.sjm

import parser._
import symbols._
import ast._

import scala.util.parsing.input._
import scala.io.{Source,Codec,UTF8Codec}
import java.io.{FileReader}



object Main {
  /**
   * @param args the command line arguments
   */
  val parser = new JavaParser

  def main(args: Array[String]) {
	import parser._

    var verbose = false
    var fileName = "HessianServlet.java"

    for (a <- args) a match {
      case "-h" | "-help"    =>
        println("Option not implemented yet")
      case "-v" | "-verbose" =>
        verbose = true
      case "-f"| "-file" =>
        fileName = args(args.findIndexOf((x: String) => x == a) + 1);
      case x =>
        println("Unknown option: '" + x + "'")
    }

    if (verbose)
      println("How are you today?")
      
    val file = Source.fromFile(fileName)("UTF8")
    var fileContents = new CharSequenceReader(file.toArray)
    val scanner = new parser.lexical.Scanner(fileContents)
    val packratReader = new parser.PackratReader(scanner)

    parser.phrase(compilationUnit)(packratReader) match {
      case parser.Success(item,next) => {
    	var parser = item // for debugging
    	
        println("Parsing succeeded \n"+parser+"\n\n"+next )
      }
      case parser.Failure(msg, next) => {
        println("Parsing failed with msg: " + msg +"\nAt position: " + next.pos + " in \n" +
          next.source.toString.lines.drop(next.pos.line - 1).next()
        )
        
        for (i <- 0 until next.pos.column) {
          print(" ")
        }
        println("^")
      }
      case x: Any => println("Unknown result!" + x.toString())
    }

    return 0;
  }
}
