package net.categoricaldata.server.transformers

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import net.categoricaldata.examples.Examples

@RunWith(classOf[JUnitRunner])
class TransformersTest extends FlatSpec with ShouldMatchers {
   import net.liftweb.json.Serialization.write
  import net.liftweb.json.JsonParser.parse

  implicit val formats = net.liftweb.json.DefaultFormats
 
  "Ontology" should "pass through the Bowler transformer successfully" in {
    for (ontology <- List(Examples.Isomorphism)) {
      new OntologyTransformer().toValue(write(ontology.toJSON)) should equal(Some(ontology))
    }
  }
  "Translation" should "pass through the Bowler transformer successfully" in {
    for (translation <- List(Examples.PointedSetsToIsomorphism, Examples.ReverseGraph)) {
      new TranslationTransformer().toValue(write(translation.toJSON)) should equal(Some(translation))
    }
    
   val literal = """{
  "source": {
    "objects": [
      "an edge",
      "a vertex"
    ],
    "arrows": [
      {
        "source": "an edge",
        "target": "a vertex",
        "label": "has as source"
      },
      {
        "source": "an edge",
        "target": "a vertex",
        "label": "has as target"
      }
    ],
    "relations": []
  },
  "target": {
    "objects": [
      "an edge",
      "a vertex"
    ],
    "arrows": [
      {
        "source": "an edge",
        "target": "a vertex",
        "label": "has as source"
      },
      {
        "source": "an edge",
        "target": "a vertex",
        "label": "has as target"
      }
    ],
    "relations": []
  },
  "onObjects": {
    "an edge": "an edge",
    "a vertex": "a vertex"
  },
  "onGenerators": [
    {
      "arrow": {
        "source": "an edge",
        "target": "a vertex",
        "label": "has as source"
      },
      "path": [
        {
          "source": "an edge",
          "target": "a vertex",
          "label": "has as target"
        }
      ]
    },
    {
      "arrow": {
        "source": "an edge",
        "target": "a vertex",
        "label": "has as target"
      },
      "path": [
        {
          "source": "an edge",
          "target": "a vertex",
          "label": "has as source"
        }
      ]
    }
  ]
}"""
     
     new TranslationTransformer().toValue(literal) should not be('isEmpty)
  }
  "Dataset" should "pass through the Bowler transformer successfully" in {
    for (dataset <- List(Examples.TerminalBigraph)) {
      new DatasetTransformer().toValue(write(dataset.toJSON)) should equal(Some(dataset))
    }
  }

}

