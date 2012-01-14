package net.categoricaldata

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import scala.math._
import net.categoricaldata.api.Ontology
import net.categoricaldata.api.FiniteMorphisms
import net.categoricaldata.api.Ontologies
import net.categoricaldata.examples.Examples
import net.categoricaldata.util.CustomMatchers
import net.categoricaldata.api.Box
/*
 * This should always compile when checked in.
 */

@RunWith(classOf[JUnitRunner])
class DevTest extends FlatSpec with ShouldMatchers with CustomMatchers {
  // NOTE to use the DSL, you need this line:
  import net.categoricaldata.dsl.Sentences._



}