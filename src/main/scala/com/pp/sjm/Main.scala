package com.pp.sjm

import parser._
import symbols._
import ast._

import scala.util.parsing.input._
import scala.io.{Source,Codec,UTF8Codec}
import java.io._
import java.nio.charset._



object Main {
  /**
   * @param args the command line arguments
   */

  def main(args: Array[String]) {
    var verbose = false
    var fileName = "Client.java"

    for (a <- args) a match {
      case "-f"| "-file" =>
        fileName = args(args.findIndexOf((x: String) => x == a) + 1);
      case x =>
        println("Unknown option: '" + x + "'")
    }

    if (verbose)
      println("How are you today?")
    
    val file = new InputStreamReader(new FileInputStream(fileName), Charset.forName("UTF-8"))
    val fileReader = new BufferedReader(file)
    //val file = Source.fromFile(fileName)("UTF8")
    var fileContents = ""
    var ch = fileReader.read()
    while (ch != -1) {
      fileContents += ch.toChar
      ch = fileReader.read()
    }
    file.close
    val fileBackup = new BufferedWriter(new FileWriter(fileName + ".backup"))
    fileBackup.write(fileContents)
    fileBackup.close()
    //val scanner = new parser.lexical.Scanner(fileContents)
    //val packratReader = new parser.PackratReader(scanner)
    
    val mutated = MutationController.mutate(fileContents, 3)
    
    //println(mutated)
    
    try {
      val mutatedFile = new BufferedWriter(new FileWriter(fileName))
      mutatedFile.write(mutated)
      mutatedFile.close()
    } catch {
      case e: IOException => {println(e.getMessage); println(e.getStackTrace)}
    } 
    
//    parser.phrase(compilationUnit)(scanner) match {
//      case parser.Success(item,next) => {
//    	var par = item // for debugging
//    	  //
//    	  //  TRZEBA BEDZIE PRZECHODZIC AST PO PARSOWANIU
//    	  //
//    	
//        println("Parsing succeeded \n"+par+"\n\n"+next )
//      }
//      case parser.Failure(msg, next) => {
//        println("Parsing failed with msg: " + msg +"\nAt position: " + next.pos + " in \n" +
//          next.source.toString.lines.drop(next.pos.line - 1).next()
//        )
//        
//        for (i <- 0 until next.pos.column) {
//          print(" ")
//        }
//        println("^")
//      }
//      case x: Any => println("Unknown result!" + x.toString())
//    }

    return 0;
  }
}
