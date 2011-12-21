package net.metaphor.api

case class Box(name: String) {
  def identity = Path(this, Nil)
}

case class Arrow(source: Box, target: Box, name: String) {
  def asPath = Path(source, List(this))
}

case class Path(source: Box, arrows: List[Arrow]) {
  def target = arrows.last.target
}

trait Dataset extends FunctorToSet[Box, Path, Ontology]
trait Datamap extends NaturalTransformationToSet[Box, Path, Ontology]

trait Ontology extends FinitelyPresentedCategory[Box, Path, Ontology] { ontology =>
  override def compose(m1: Path, m2: Path) = {
    require(m2.target == m1.source)
    Path(m1.source, m1.arrows ::: m2.arrows)
  }
  override def source(m: Path) = m.source
  override def target(m: Path) = m.target
  override def identity(o: Box) = o.identity
  
  override lazy val adjoinTerminalObject: WithTerminalObject = new Ontology with WithTerminalObject { ato =>
    val terminalObject = Box("*")
    def morphismFrom(o: Box) = ???
    
    def objects = terminalObject :: ontology.objects
    def generators(source: Box, target: Box) = {
      if(target == terminalObject) {
        List(Arrow(source, target, "*").asPath)
      } else {
        ontology.generators(source, target)
      }
    }
    def relations(source: Box, target: Box) = {
      if(target == terminalObject) {
        ???
      } else {
        ontology.relations(source, target)
      }}
    
    /* TODO (David) toString needs to *return* a string, not print one.
     * Also, please don't commit code that doesn't compile: at very least, comment out the lines that aren't compiling,
     * and leave a comment explaining what's wrong. ---S
     */
    
    /*
     *  Here you're actually "overriding" an existing method (every Scala object comes with a predefined toString method,
     *  but by default it isn't very useful). Whenever you override, it's good practice to write "override def" instead of just "def".
     *  This tells the compiler to generate an error if, in fact, you're *not* overriding something.
     *  
     *  Moreover, once a method has the "override" keyword, anyone who later overrides it has to explicitly mark their method with "override".
     *  The default "toString" already has the "override" keyword, so it's actually necessary here.
     *  
     *  I've added it for you, but try removing it to see the compile error.
     */
    
    override def toString() = {
      println("Ontology")
      println("Objects: ")
      for (i <- objects) println(i)
      println();println("Arrows: ")
      /*
       * TODO (David) I commented the next line  out, as it wasn't compiling. You're looking for "allGenerators", rather than arrows. Remind me
       * to give you a quick lesson on working out what methods are defined on which classes.  ---S
       */
//      for (i <- arrows) println(i)
      
      /*
       *  Finally, I'm adding the next line (which just generates an exception, if the program ever gets to here, because
       *  the toString method must return a String, and the compiler knows this and complains if it can't see a return value being generated.
       */
      ???
      }
    
  }
  
  trait Dataset extends FunctorToSet with net.metaphor.api.Dataset
  trait Datamap extends NaturalTransformationToSet with net.metaphor.api.Datamap
  
  override val functorsToSet = Datasets
  object Datasets extends FunctorsToSet
}

// TODO implement InitialOntology similarly
object TerminalOntology extends TerminalFinitelyPresentedCategory[Box, Path, Ontology] with Ontology {
  val terminalObject = Box("*")
  def morphismFrom(o: Box) = terminalObject.identity
}

object Ontologies extends FinitelyPresentedCategories[Box, Path, Ontology]

trait Translation extends Functor[Box, Path, Ontology] {
  val source: Ontology
  val target: Ontology
  // FIXME, not compiling yet
//  def pullback: HeteroFunctor[source.FunctorToSet, source.NaturalTransformationToSet, source.FunctorsToSet, target.FunctorToSet, target.NaturalTransformationToSet, target.FunctorsToSet] = ???
  
  def pullback: Any => Any = { x: Any => ??? }
  def pushforward: Any => Any = { x: Any => ??? }
  def shriek: Any => Any = { x: Any => ??? }
  
  def ^* = pullback
  def __! = shriek
  def __* = pushforward
}