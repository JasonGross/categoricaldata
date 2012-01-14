package net.categoricaldata.json

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import net.categoricaldata.examples.Examples

@RunWith(classOf[JUnitRunner])
class PackTest extends FlatSpec with ShouldMatchers {
  "Ontology" should "pass through packing and unpacking successfully" in {
    for (ontology <- List(Examples.Isomorphism)) {
      val packed = Pack.packOntology(ontology)
      val unpacked = packed.unpack

      ontology should equal(unpacked)
    }
  }
  "Translation" should "pass through packing and unpacking successfully" in {
    for (translation <- List(Examples.PointedSetsToIsomorphism, Examples.ReverseGraph)) {
      val packed = Pack.packTranslation(translation)
      val unpacked = packed.unpack

      translation should equal(unpacked)
    }
  }
  "Dataset" should "pass through packing and unpacking successfully" in {
    for (dataset <- List(Examples.TerminalBigraph)) {
      val packed = Pack.packDataset(dataset)
      val unpacked = packed.unpack

      dataset should equal(unpacked)
    }
  }

}

