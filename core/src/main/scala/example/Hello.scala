package example

object Hello extends Greeting with App {
  println(greeting)
  println("some other thing to print")
}

trait Greeting {
  lazy val greeting: String = "hello"
}
