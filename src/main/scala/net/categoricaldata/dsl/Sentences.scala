package net.categoricaldata.dsl
import net.categoricaldata.api.Arrow
import net.categoricaldata.api.Box
import net.categoricaldata.api.FiniteMorphisms
import net.categoricaldata.api.Ontology
import net.categoricaldata.api.Ontologies
import net.categoricaldata.api.Translation
import net.categoricaldata.api.FFunction
import net.categoricaldata.api.Path

object Sentences {
  implicit def stringAsPath(s: String) = StringSource(s)

  trait StringPath {
    def source: String
    def arrows: List[StringArrow]
    def target: String

    def ===(other: StringPath) = StringRelation(this, other)
  }
  case class ConcreteStringPath(source: String, arrows: List[StringArrow]) extends StringPath {
    def ---(label: String) = IncompleteStringPath(this, label)
    def target = arrows.lastOption.map(_.target).getOrElse(source)
  }
  case class IncompleteStringPath(path: StringPath, label: String) {
    def -->(target: String) = ConcreteStringPath(path.source, path.arrows ::: List(StringArrow(path.target, target, label)))
  }
  case class StringArrow(source: String, target: String, label: String) extends StringPath {
    def arrows = List(this)
    def ---(label: String) = IncompleteStringPath(ConcreteStringPath(source, List(this)), label)
  }
  case class StringSource(source: String) extends StringPath {
    def arrows = Nil
    def target = source
    def ---(label: String) = IncompleteStringArrow(label)

    def identity = ConcreteStringPath(source, Nil)

    case class IncompleteStringArrow(label: String) {
      def -->(target: String) = StringArrow(source, target, label)
    }
  }

  case class StringRelation(left: StringPath, right: StringPath)

  class ConcreteOntology(_objects: Traversable[String], _arrows: Traversable[StringArrow], _relations: Traversable[StringRelation]) extends Ontology {
    private val boxes = _objects.toList map { Box(_) }

    private implicit def stringToBox(s: String) = boxes.find(_.name == s).get
    private implicit def stringArrow2Arrow(sa: StringArrow) = Arrow(sa.source, sa.target, sa.label)
    
    private val allArrows: List[Arrow] = for (sa <- _arrows.toList) yield stringArrow2Arrow(sa)
    private val arrowMap = allArrows.groupBy(a => (a.source, a.target)).withDefaultValue(Nil)

    private val _allRelations: List[(Path, Path)] = (for(StringRelation(lhs, rhs) <- _relations.toList) yield {
      val source: Box = lhs.source
      val target: Box = lhs.target
      val leftMorphisms = lhs.arrows.map(stringArrow2Arrow(_))
      val rightMorphisms = rhs.arrows.map(stringArrow2Arrow(_))
      (Path(source, target, leftMorphisms), Path(source, target, rightMorphisms))
    })
    private val relationsMap = _allRelations.groupBy(a => (a._1.source,a._1.target)).withDefaultValue(Nil)
    
    val minimumLevel = 0
    val maximumLevel = 0
    def objectsAtLevel(k: Int) = if (k == 0) boxes else Nil
    override def generators(source: Box, target: Box) = arrowMap(source, target)
    override def relations(source: Box, target: Box) = relationsMap(source, target)
    
    override def pathEquality(p1: Path, p2: Path) = ???
  }

  def Ontology(objects: Traversable[String], arrows: Traversable[StringArrow], relations: Traversable[StringRelation] = Nil): Ontology = {
    // Construct a new ontology object
    new ConcreteOntology(objects, arrows, relations)
  }

  class ConcreteTranslation(override val source: Ontology, override val target: Ontology, onObjects: String => String, onMorphisms: StringArrow => StringPath) extends Translation {
    private val objectMap: Map[Box, Box] = (for (s <- source.objects) yield {
      val t = target.objects.find(_.name == onObjects(s.name)).get
      s -> t
    }).toMap
    private val morphismMap: Map[Arrow, target.M] = (for (
      a <- source.allGenerators
    ) yield {
      val morphisms = for (StringArrow(ts, to, tp) <- onMorphisms(StringArrow(a.source.name, a.target.name, a.name)).arrows) yield {
        target.generatorAsMorphism(target.allGenerators.find(a => a.source.name == ts && a.name == tp && a.target.name == to).get)
      }
      a -> target.compose(objectMap(source.generatorSource(a)), morphisms)
    }).toMap

    verifyRelations
    
    override def onObjects(o: Box) = objectMap(o)
    // And again, replacing source.G with the apparently equivalent Arrow causes an AbstractMethodError
    override def onGenerators(a: source.G) = morphismMap(a)
  }

  def Translation(source: Ontology, target: Ontology, onObjects: String => String, onMorphisms: StringArrow => StringPath): Translation = {
    // construct a new translation object
    new ConcreteTranslation(source, target, onObjects, onMorphisms)
  }

  def Dataset(source: Ontology, onObjects: String => Traversable[String], onMorphisms: StringArrow => (String => String)): source.Dataset = {

    val objectMap = (for (s <- source.objects) yield {
      s -> onObjects(s.name).toList
    }).toMap
    val morphismMap = (for (
      a <- source.allGenerators
    ) yield {
      a -> onMorphisms(StringArrow(a.source.name, a.target.name, a.name)).asInstanceOf[Any => Any]
    }).toMap

    (new source.Dataset {
      
      verifyRelations
      
      override def onObjects(o: source.O) = objectMap(o)
      // WEIRD: changing source.G to Arrow (which should be fine) results in AbstractMethodError at runtime. Compiler bug?
      override def onGenerators(a: source.G): FFunction = new DatasetFunction(a) {
        override def toFunction = morphismMap(a)
      }
    }).memo
  }
}

