package example

import blocks._

object Test {

  def method1(f: OcapBlock[Int,Int]): Unit = {
    println(f(3))
  }

  def main(args: Array[String]): Unit = {
    val y: Int = 3
    val f: Int => Int = { (x) => x + y + 1 }

    method1(CopyableBlock(f))
    method1(CopyableBlock{(x) => x + y + 2})
  }

  /* We can probably demand that all code that is of type Block[A,B]{ type
   * Prop[S] = ... } has the form SomeBlockMaker{ .... } and that we check all
   * code that is within the block.  E.g. we should check that all captured
   * variables within the block has the specified property of the block type
   * member, i.e. all captured variables conforms to some type class and use
   * the corresponding implicit object somehow
   *
   * To develop further we could try to check the property transitively for
   * functions 
   *
   * To start with:
   * Code that has type Block... must be checked in the following ways
   *
   * 1. We take all captured variables and check that there is a corresponding
   *    type class implicit in scope
   *    Alternatively we demand that all captured variables are captured through
   *    a copy or similar, so that all variables captured are captured explicitly.
   * 2. We check that all initialized classes follows the ocap model
   */
  

}



