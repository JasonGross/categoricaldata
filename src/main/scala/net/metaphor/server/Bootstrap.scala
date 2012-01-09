package net.metaphor.server
import org.bowlerframework.view.scalate._
import org.bowlerframework.Request
import org.bowlerframework.BowlerConfigurator
import org.bowlerframework.view.RenderStrategy
import org.bowlerframework.view.JsonViewRenderer
import com.recursivity.commons.bean.TransformerRegistry

/**
 * This class acts as the starting point and bootstrap point for our application
 */
class Bootstrap {
  // *always* render JSON, regardless of what the client asked for.
  BowlerConfigurator.setRenderStrategy(new RenderStrategy {
    def resolveViewRenderer(request: Request) = new ModelViewRenderer
  })
  
  
  TransformerRegistry.registerTransformer(classOf[net.metaphor.api.Ontology], classOf[net.metaphor.server.transformers.OntologyTransformer])
  TransformerRegistry.registerTransformer(classOf[net.metaphor.api.Dataset], classOf[net.metaphor.server.transformers.DatasetTransformer])
  TransformerRegistry.registerTransformer(classOf[net.metaphor.api.Translation], classOf[net.metaphor.server.transformers.TranslationTransformer])
  
  // I think we're ready to start and instantiate our Controller.
  val controller = new MetaphorController
}