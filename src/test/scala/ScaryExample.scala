object ScaryExample extends App {

  trait X {
    type A
    trait Z {
      def f(a: A): String
    }
  }

  trait Y extends X {
    type B = (A, A)
    trait Z extends super.Z {
      def g(b: B): String
      def h(a: A): B = (a, a)
      override def f(a: A) = g(h(a))
    }
  }

  trait Z extends Y {
    def z = new Z {
      override def g(b: B) = b.toString
    }
  }
  
  val z = new Z {
    type A = String
  }

  
  println(z.z.f("a"))
}