package com.sagacify.sonar.scala

import java.io.File
import java.util

import org.sonar.api.batch.AbstractCpdMapping
import org.sonar.api.resources.{AbstractLanguage, Resource}

class ScalaCpdMapping(private val scala: Scala) extends AbstractCpdMapping {
  override def getTokenizer = new net.sourceforge.pmd.cpd.AnyTokenizer()

  override def getLanguage: AbstractLanguage = scala
}
