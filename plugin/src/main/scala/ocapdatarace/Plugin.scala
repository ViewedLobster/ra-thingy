package ocapdatarace

import scala.tools.nsc
import nsc.Global
import nsc.Phase
import nsc.plugins.Plugin
import nsc.plugins.PluginComponent


class BlockChecker( val global: Global ) extends Plugin {
  import global._

  val name = "blockchecker"
  val description = "checks ocap blocks"
  val components = List[PluginComponent](TestComponent)


  private object TestComponent extends PluginComponent {
    val global: BlockChecker.this.global.type = BlockChecker.this.global
    val runsAfter = List[String]("refchecks")

    val phaseName = BlockChecker.this.name
    override val description = "the ocap block checker phase"
    def newPhase(_prev: Phase) = new TestPhase(_prev)


    class TestPhase(prev: Phase) extends StdPhase(prev) {
      override def apply(unit: CompilationUnit): Unit = {
        val t = new TestTraverser(unit)
        t.traverse(unit.body)
      }
    }

    class TestTraverser(unit: CompilationUnit) extends Traverser {
      override def traverse(tree: Tree): Unit = tree match {
        case Apply(fun, args) => 
          println(s"Wow traversing ${fun.symbol.name}")
          println(s"Owner: ${fun.symbol.owner.name}")
          args.foreach(traverse(_))
        case _ => super.traverse(tree)
      }
    }
  }
}
