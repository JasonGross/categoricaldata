package net.categoricaldata.server.json

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import net.categoricaldata.examples.Examples

@RunWith(classOf[JUnitRunner])
class SerializationTest extends FlatSpec with ShouldMatchers {
  import net.liftweb.json.Serialization.write
  import net.liftweb.json.JsonParser.parse

  implicit val formats = net.liftweb.json.DefaultFormats

  "Ontology" should "pass through serialization successfully" in {
    for (ontology <- List(Examples.Isomorphism)) {
      val json = ontology.toJSON
      val string = write(json)
      val result = parse(string).extract[Ontology]

      json should equal(result)
    }

  }
  "Dataset" should "pass through serialization successfully" in {
    for (dataset <- List(Examples.TerminalBigraph, Examples.ReverseGraph.__*(Examples.TerminalBigraph))) {
      val json = dataset.toJSON
      val string = write(json)
      val result = parse(string).extract[Dataset]

      json should equal(result)
    }

  }
  "Translation" should "pass through serialization successfully" in {
    for (translation <- List(Examples.ReverseGraph)) {
      val json = translation.toJSON
      val string = write(json)
      val result = parse(string).extract[Translation]

      json should equal(result)
    }

  }

}

