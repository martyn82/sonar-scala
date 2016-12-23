package com.sagacify.sonar.scala

import scala.io.Source
import scala.collection.JavaConversions._
import org.sonar.api.batch.fs.{FileSystem, InputFile}
import org.sonar.api.batch.Sensor
import org.sonar.api.batch.SensorContext
import org.sonar.api.measures.{CoreMetrics => CM}
import org.sonar.api.resources.Project

class ScalaSensor(scala: Scala, fs: FileSystem) extends Sensor {

  def shouldExecuteOnProject(project: Project): Boolean = {
    fs.hasFiles(fs.predicates().hasLanguage(scala.getKey))
  }

  def analyse(project: Project, context: SensorContext): Unit = {
    val charset = fs.encoding().toString
    val version = "2.11.8"

    val inputFiles = fs.inputFiles(fs.predicates().hasLanguage(scala.getKey))
    val blocksPerFile = collection.mutable.Map.empty[String, List[(List[String], Int)]]
    val files = collection.mutable.Map.empty[String, InputFile]

    inputFiles.foreach { inputFile =>
      files.put(inputFile.absolutePath(), inputFile)
      context.saveMeasure(inputFile, CM.FILES, 1.0)

      val sourceCode = Source.fromFile(inputFile.file, charset).mkString
      val tokens = Scala.tokenize(sourceCode, version)

      context.saveMeasure(inputFile, CM.COMMENT_LINES, Measures.countCommentLines(tokens))
      context.saveMeasure(inputFile, CM.NCLOC, Measures.countNCLoC(tokens))
      context.saveMeasure(inputFile, CM.CLASSES, Measures.countClasses(tokens))

      Scala.parse(sourceCode, version).map { ast =>
        val functions = Measures.extractFunctions(ast)
        val functionComplexities = functions.map(Measures.calculateComplexity).filter(_ > 0)

        val classes = Measures.extractClasses(ast)
        val classComplexities = classes.map(Measures.calculateComplexity).filter(_ > 0)

        context.saveMeasure(inputFile, CM.FUNCTIONS, functionComplexities.length + classComplexities.length)
        context.saveMeasure(inputFile, CM.COMPLEXITY, functionComplexities.sum + classComplexities.sum)
        context.saveMeasure(inputFile, CM.COMPLEXITY_IN_FUNCTIONS, functionComplexities.sum + classComplexities.sum)
        context.saveMeasure(inputFile, CM.COMPLEXITY_IN_CLASSES, classComplexities.sum)
      } getOrElse {
        throw new RuntimeException("Source code does not contain AST")
      }

      // context.saveMeasure(input, CM.PUBLIC_API, publicApiChecker.getPublicApi())
      // context.saveMeasure(input, CM.PUBLIC_DOCUMENTED_API_DENSITY, publicApiChecker.getDocumentedPublicApiDensity())
      // context.saveMeasure(input, CM.PUBLIC_UNDOCUMENTED_API, publicApiChecker.getUndocumentedPublicApi())
    }
  }
}
