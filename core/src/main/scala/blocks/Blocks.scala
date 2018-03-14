package blocks

import scala.language.higherKinds


sealed trait Block[-A, +B] {
  type Prop[S]

  def apply(a: A) : B
}

class BlockMaker[P[_]] {

  def apply[A,B](f: A => B) : Block[A,B]{ type Prop[S] = P[S] } = 
    new Block[A,B] { 
      type Prop[S] = P[S]
      def apply(a: A) : B = { f(a) }
    }
}

