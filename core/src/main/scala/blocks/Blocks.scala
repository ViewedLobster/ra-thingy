package blocks

import scala.language.higherKinds

trait OcapBlock[-A, +B] {
  type Prop[S]

  def apply(a: A) : B
}

class BlockMaker[P[_]] {

  def apply[A,B](f: A => B) : OcapBlock[A,B]{ type Prop[S] = P[S] } = 
    new OcapBlock[A,B] { 
      type Prop[S] = P[S]
      def apply(a: A) : B = { f(a) }
    }
}

/* A BlockMaker takes a Block with type A => B where the first statements
 * specifies the captured variables. All captured variables should have a
 * corresponding implicit object (be of the type class) Prop. For each captured
 * variable the P.init method will be applied later att class creation. The
 * returned function will be the last expression in the block.
 *
 * E.g. a valid one would be
 *  BlockMaker[Copyable] { 
 *    val x = 4 + someCapturedVar
 *    val y = 3
 *    (z: Int) => z + x + y
 *  }
 * An invalid:
 *  BlockMaker[Copyable] { 
 *    val x = 4 + someCapturedVar
 *    val y = 3
 *    (z: Int) => z + x + y + someOtherCapturedVar
 *  }
 */
