package net.metaphor.dsl

object Sentences {
	implicit def stringAsSubject(s: String) = new Subject(s)
	
	class Subject(s: String) {
	  def >>(p: String) = new SubjectPredicate(s, p)
	}
	
	class SubjectPredicate(s: String, p: String) {
	  def >>(o: String) = new SubjectPredicateObject(s, p, o)
	}
	
	class SubjectPredicateObject(s: String, p: String, o: String) {
	  println(s + " " + p + " " + o)
	}
	
	def model(contents: SubjectPredicateObject*) { }
}

