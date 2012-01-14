package net.categoricaldata.server
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
  
  
  TransformerRegistry.registerTransformer(classOf[net.categoricaldata.api.Ontology], classOf[net.categoricaldata.server.transformers.OntologyTransformer])
  TransformerRegistry.registerTransformer(classOf[net.categoricaldata.api.Dataset], classOf[net.categoricaldata.server.transformers.DatasetTransformer])
  TransformerRegistry.registerTransformer(classOf[net.categoricaldata.api.Translation], classOf[net.categoricaldata.server.transformers.TranslationTransformer])
  
  // I think we're ready to start and instantiate our Controller.
  val controller = new MetaphorController
}