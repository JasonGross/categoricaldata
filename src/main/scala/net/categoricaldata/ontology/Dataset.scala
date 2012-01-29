package net.categoricaldata.ontology
import net.categoricaldata.sets.FFunction
import net.categoricaldata.dsl.Sentences
import net.categoricaldata.category.FunctorsToSet

trait Dataset extends net.categoricaldata.category.FunctorToSet with net.categoricaldata.category.Functor.withFinitelyPresentedSource {
  override val source: Ontology
  def grothendieck: Ontology
}

trait Datamap extends net.categoricaldata.category.NaturalTransformationToSet

object Datasets extends FunctorsToSet {
  type O = Dataset
  type M = Datamap
  
  def internalize(f: net.categoricaldata.category.FunctorToSet) = {
    f.source match {
      case s: Ontology => s.internalize(f)
    }
  }
  def internalize(t: net.categoricaldata.category.NaturalTransformationToSet) = {
    ???
  }
}

object Dataset {
  def apply(source: Ontology, onObjects: String => Traversable[String], onMorphisms: Sentences.StringArrow => (String => String), _json: Option[String] = None): source.Dataset = {

    val objectMap = (for (s <- source.objects) yield {
      s -> onObjects(s.name).toList
    }).toMap
    val morphismMap = (for (
      a <- source.allGenerators
    ) yield {
      a -> onMorphisms(Sentences.StringArrow(a.source.name, a.target.name, a.name)).asInstanceOf[Any => Any]
    }).toMap

    (new source.Dataset {

      verifyRelations

      override def onObjects(o: source.O) = objectMap(o)
      // WEIRD: changing source.G to Arrow (which should be fine) results in AbstractMethodError at runtime. Compiler bug?
      override def onGenerators(a: source.G): FFunction = new DatasetFunction(a) {
        override def toFunction = morphismMap(a)
      }

      override def toJSON = super.toJSON.copy(json = _json)
    }).memo
  }
}
