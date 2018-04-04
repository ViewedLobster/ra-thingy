package blocks
import scala.language.higherKinds

/* Type class for copyable object */
trait Copyable[S] {
  def copy(s: S) : S
}

class Immutable[S]

sealed trait Dummy[S]

object Copyable {
  def copy[S : Copyable](s: S) : S = {
    implicitly[Copyable[S]].copy(s)
  }


  implicit val doubleCopier : Copyable[Double] = new Copyable[Double] {
    def copy(d: Double) : Double = d
  }
}

object Immutable {
  implicit val immutInt: Immutable[Int] = new Immutable[Int]
}

object CopyableBlock extends BlockMaker[Copyable] 

