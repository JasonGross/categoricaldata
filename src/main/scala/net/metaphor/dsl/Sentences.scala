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
}

object SentencesExamples extends App {
	import Sentences._
	
	"a department" >> "has as secretary" >> "an employee"

}