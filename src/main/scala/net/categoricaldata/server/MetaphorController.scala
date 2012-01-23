package net.categoricaldata.server

import org.bowlerframework.controller.Controller
import org.bowlerframework.controller.FunctionNameConventionRoutes
import org.bowlerframework.Request
import net.categoricaldata.examples.Examples
import net.categoricaldata.ontology.Ontology
import net.categoricaldata.ontology.Dataset
import net.categoricaldata.ontology.Translation
import net.categoricaldata.ontology.Box
import com.thoughtworks.paranamer.CachingParanamer
import com.thoughtworks.paranamer.BytecodeReadingParanamer
import org.bowlerframework.view.Renderable
import org.bowlerframework.RequestScope
import net.categoricaldata.load.CSVLoader

case class apiParameter(name: String, `type`: String)
case class apiHint(path: String, hasParameters: Boolean, parameters: List[apiParameter])
case class apiHints(hints: List[apiHint])

class MetaphorController extends Controller with FunctionNameConventionRoutes { metaphorController =>

  // TODO use reflection to look up methods on Examples (also, to generate lists)

  // TODO toJSON should embed the request URL
  // TODO when extracting from JSON, should store the original representation (in progress)

  def `GET /metaphor/examples/ontologies/Chain`(n: Int): Ontology = Examples.Chain(n)
  def `GET /metaphor/examples/ontologies/Isomorphism`: Ontology = Examples.Isomorphism
  def `GET /metaphor/examples/ontologies/Graph`: Ontology = Examples.Graph
  def `GET /metaphor/examples/ontologies/FiniteCyclicMonoid`(n: Int, k: Int): Ontology = Examples.FiniteCyclicMonoid(n, k)
  def `GET /metaphor/examples/datasets/TerminalBigraph`: Dataset = Examples.TerminalBigraph
  def `GET /metaphor/examples/translations/ReverseGraph`: Translation = Examples.ReverseGraph

  def `GET /metaphor/compute/leftPushforward`(translation: Translation, dataset: Dataset): Dataset = {
    translation.__!(dataset)
  }
  def `GET /metaphor/compute/rightPushforward`(translation: Translation, dataset: Dataset): Dataset = {
    translation.__*(dataset)
  }
  def `GET /metaphor/compute/pullback`(translation: Translation, dataset: Dataset): Dataset = {
    translation.^*(dataset)
  }

  def `GET /metaphor/compute/grothendieck`(dataset: Dataset): Ontology = {
    dataset.grothendieck
  }
  def `GET /metaphor/compute/yoneda`(ontology: Ontology, box: String): Dataset = {
    ontology.yoneda(Box(box))
  }
  def `GET /metaphor/compute/translationToDataset`(translation: Translation): Dataset = {
    translation.asPartialDataset.toDataset
  }
  
  def `GET /metaphor/dataset/source`(dataset: Dataset): Ontology = dataset.source
  def `GET /metaphor/translation/source`(translation: Translation): Ontology = translation.source
  def `GET /metaphor/translation/target`(translation: Translation): Ontology = translation.target
  
  def `GET /metaphor/load/dataset/CSV`(url: String): Dataset = CSVLoader(url)
  
  def `GET /metaphor/help` = hints("")
  def `GET /metaphor/examples` = hints("/metaphor/examples")
  def `GET /metaphor/examples/ontologies` = hints("/metaphor/examples/ontologies")
  def `GET /metaphor/examples/translations` = hints("/metaphor/examples/translations")
  def `GET /metaphor/examples/datasets` = hints("/metaphor/examples/datasets")

  def `GET /metaphor/passthru/ontology`(ontology: Ontology): Ontology = ontology
  def `GET /metaphor/passthru/translation`(translation: Translation): Translation = translation
  def `GET /metaphor/passthru/dataset`(dataset: Dataset): Dataset = dataset

  def `POST /metaphor/store/ontology`(ontology: Ontology) = RequestScope.response.sendRedirect("/metaphor/store/ontology?hash=" + MetaphorStore.add(ontology))
  def `GET /metaphor/store/ontology`(hash: String): Ontology = MetaphorStore.lookupOntology(hash)
  def `POST /metaphor/store/translation`(translation: Translation) = RequestScope.response.sendRedirect("/metaphor/store/translation?hash=" + MetaphorStore.add(translation))
  def `GET /metaphor/store/translation`(hash: String): Translation = MetaphorStore.lookupTranslation(hash)
  def `POST /metaphor/store/dataset`(dataset: Dataset) = RequestScope.response.sendRedirect("/metaphor/store/dataset?hash=" + MetaphorStore.add(dataset))
  def `GET /metaphor/store/dataset`(hash: String): Dataset = MetaphorStore.lookupDataset(hash)
  
  lazy val allHints = {
    val paranamer = new BytecodeReadingParanamer();
    apiHints(metaphorController.getClass().getDeclaredMethods().toList.filter({
      method => method.getName().startsWith("GET")
    }).map({
      method =>
        {
          val names = paranamer.lookupParameterNames(method).toList
          val types = method.getParameterTypes().toList.map(_.getName())
          apiHint(method.getName().stripPrefix("GET$u0020").replaceAllLiterally("$div", "/"), names.nonEmpty, (names zip types).map(p => apiParameter(p._1, p._2)))
        }
    }))
  }
  def hints(filter: String) = apiHints(allHints.hints.filter(_.path.startsWith(filter)))
}