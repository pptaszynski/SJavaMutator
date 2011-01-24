package com.pp.sjm

import parser._
import symbols._
import ast._
import util._

import scala.util.parsing.input._
import scala.io.{Source,Codec,UTF8Codec}
import java.io._
import java.nio.charset._



object Main {
  /**
   * @param args the command line arguments
   */

  def main(args: Array[String]) {
    println("Simple Java Mutator - v1.0\tPawel Ptaszynski")
    ProgramOptions.readOptions(args)
    if (args.length < 2 || ProgramOptions.files.size < 1)
      ProgramOptions.help = true
    if (ProgramOptions.help) {
      println("Program usage: ")
      println("sjm [-h] [-v] [-os <output_files_suffix>] [-[n|N] <number>] <mutation_list> <input_file1> [<input_file2> ...]")
      println("\t-h \tPrints this help")
      println("\t-v \tVerbose mode - prints a lot of messy information.")
      println("\t-os \t<output_files_suffix>\tSpecifies the suffix for output files. If not specified then input files are overwriten, but backup will be done.")
      println("\t-[N|n]\t<number>\tSpecifies the number of mutations to be applied per file.")
      println("\t\t\t\tIf not specified the default value of 1 will be used.")
      println("\t<mutation_list>\t sequence of one or more of:")
      println("\t\t\t\t"+"CHANGE_GETTER,RELATIONAL_OP_CHANGE,REMOVE_THIS,REMOVE_THROW")
      println("\t<input_fileX>\t Names/Paths of input files")
      return 0
    }
    
    for (fileName <- ProgramOptions.filesIterator) {
      println("======= " + fileName + " ==== Processing mutation")
      var fileContents = ""
      try {
        val file = new InputStreamReader(new FileInputStream(fileName), Charset.forName("UTF-8"))
        val fileReader = new BufferedReader(file)
        //val file = Source.fromFile(fileName)("UTF8")
        var ch = fileReader.read()
        while (ch != -1) {
          fileContents += ch.toChar
          ch = fileReader.read()
        }
        file.close
      } catch {
        case e: IOException => {
          println("Error reading input file.")
          if (ProgramOptions.verbose) {
            println(e.getMessage)
            println(e.getStackTrace)
          }
          return -1
        }
      }
      if (ProgramOptions.outputSuffix == "") {
        try {
          val fileBackup = new BufferedWriter(new FileWriter(fileName + ".backup"))
          fileBackup.write(fileContents)
          fileBackup.close()
        } catch {
          case e: IOException => {
            println("Backup of input file failed! Stoping!")
            if (ProgramOptions.verbose) {
              println(e.getMessage)
              println(e.getStackTrace)
            }
            return -1
          }
        }
      }
      //val scanner = new parser.lexical.Scanner(fileContents)
      //val packratReader = new parser.PackratReader(scanner)
      
      val mutated = MutationController.mutate(fileContents, ProgramOptions.mutationsPerFile, ProgramOptions.enabledMutations)
      
      //println(mutated)
      
      try {
        val mutatedFile = new BufferedWriter(new FileWriter(fileName+ProgramOptions.outputSuffix))
        mutatedFile.write(mutated)
        mutatedFile.close()
      } catch {
        case e: IOException => {
          println("Error writing outpu file.")
          if (ProgramOptions.verbose) {
            println(e.getMessage); println(e.getStackTrace)
          }
        }
      } 
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
