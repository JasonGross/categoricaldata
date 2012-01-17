package net.categoricaldata

package object api {
	def ??? = throw new NoSuchMethodException
	
	implicit def asSet(i: Iterable[Any]): FSet = new FSet {
	  override def toIterable = i
	  override def sizeIfFinite = Some(i.size)
	}
}