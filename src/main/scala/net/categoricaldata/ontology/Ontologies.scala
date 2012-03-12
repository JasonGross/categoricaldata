package net.categoricaldata.ontology
import net.categoricaldata.universalalgebra._
import net.categoricaldata.category.Category

object Ontologies extends Category with InitialObject with TerminalObject {
  override type O = Ontology
  override type M = Translation

  override val terminalObject = net.categoricaldata.examples.Examples.Chain(0)
  override def morphismToTerminalObject(ontology: Ontology): Translation = new Translation {
    override val source: ontology.type = ontology
    override val target: terminalObject.type = terminalObject
    override def onObjects(o: Box) = terminalObject.objects.head
    override def onGenerators(g: Arrow) = terminalObject.identity(terminalObject.objects.head)
  }
  override val initialObject = net.categoricaldata.examples.Examples.Chain(-1)
  override def morphismFromInitialObject(ontology: Ontology): Translation = new Translation {
    override val source: initialObject.type = initialObject
    override val target: ontology.type = ontology
    override def onObjects(o: Box) = throw new IllegalArgumentException
    override def onGenerators(g: Arrow) = throw new IllegalArgumentException
  }

  override def identity(o: Ontology) = o.identityTranslation
  override def source(m: Translation) = m.source
  override def target(m: Translation) = m.target
  override def compose(m1: Translation, m2: Translation): Translation = new Translation {
    override val source: m1.source.type = m1.source
    override val target: m2.target.type = m2.target
    override def onObjects(o: source.O) = m2(m1(o).asInstanceOf[m2.source.O])
    override def onGenerators(g: source.G) = m2(m1.onGenerators(g).asInstanceOf[m2.source.M])
  }
}

