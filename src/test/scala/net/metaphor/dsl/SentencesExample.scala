package net.metaphor.dsl

object SentencesExample extends App {
  import Sentences._

  model(
    "a fusion object" >> "has as principal graph" >> "a graph",
    "a fusion object" >> "sits inside" >> "a fusion category",
    "a fusion category" >> "has as fusion ring" >> "a positive ring"
  )
}