package net.metaphor

package object api {
	def ??? = throw new NoSuchMethodException //???
	
	implicit def asSet(i: Iterable[Any]): Set = new Set {
	  override def toIterable = i
	  override def sizeIfFinite = Some(i.size)
	}
//	implicit def asFunction(f: Any => Any): Function = new Function {
//	  override def toFunction = f
//	}
}