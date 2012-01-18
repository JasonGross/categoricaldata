package net.categoricaldata.dsl
import net.categoricaldata.ontology.Arrow
import net.categoricaldata.ontology.Box
import net.categoricaldata.ontology.Ontology
import net.categoricaldata.ontology.Ontologies
import net.categoricaldata.ontology.Translation
import net.categoricaldata.sets.FFunction
import net.categoricaldata.category.Path

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
}

