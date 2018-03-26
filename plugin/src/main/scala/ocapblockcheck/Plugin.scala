package ocapblockcheck

import scala.tools.nsc
import nsc.Global
import nsc.Phase
import nsc.plugins.Plugin
import nsc.plugins.PluginComponent
import blocks._
import scala.language.higherKinds


class BlockChecker( val global: Global ) extends Plugin {
  import global._

  val name = "blockchecker"
  val description = "plugin for checkng ocap blocks"
  val components = List[PluginComponent](TestComponent)

  def transitiveOwner( symb: Symbol, owned: Symbol ) : Boolean = 
    owned match {
      case NoSymbol => false
      case _ => owned.owner == symb || transitiveOwner(symb, owned.owner)
    }

  def optionSymbol( tree: Tree ) : Option[Symbol] = 
    if ( tree.symbol != null ) Some(tree.symbol)
    else None

  private object TestComponent extends PluginComponent {
    val global: BlockChecker.this.global.type = BlockChecker.this.global
    val runsAfter = List[String]("refchecks")
    val blockType = typeOf[OcapBlock[_,_]]
    val blockMakerConstr = typeOf[BlockMaker[Dummy]].typeConstructor

    val phaseName = BlockChecker.this.name
    override val description = "ocap block check"
    def newPhase(_prev: Phase) = new TestPhase(_prev)

    

    class TestPhase(prev: Phase) extends StdPhase(prev) {
      override def apply(unit: CompilationUnit): Unit = {
        println("starting testphase")
        global.settings.debug.value = true
        val t = new TestTraverser(unit)
        t.traverse(unit.body)
        global.settings.debug.value = false
      }
    }

    // TODO clean this up and change some names
    class TestTraverser(unit: CompilationUnit) extends Traverser {
      override def traverse(tree: Tree): Unit = tree match {
        case t: TermTree => 
          if (t.isTyped) {
            val tpe = t.tpe
            if ( tpe <:< blockType ) {
              println(s"Found OcapBlock type: ${t}")
              t match {
                case Apply(TypeApply(Select(blockmaker, tn), List(tt1, tt2)), List(argfun)) => 
                  val tpe = blockmaker.tpe
                  val tpeConstr = blockmaker.tpe.typeConstructor
                  println(s"${tpe}, ${tpeConstr}, ${tpeConstr <:< blockMakerConstr}")

                  if ( tpe != tpeConstr && tpeConstr <:< blockMakerConstr ) {
                    argfun match {
                      case f @ Function(argList, body)=>
                        println(s"Traversing a tree with symbol ${argfun.symbol} ###############")
                        
                        val bgt = new BlockGenTraverser( unit, argfun.symbol )
                        bgt.traverse(body)
                      case _ => reporter.error(argfun.pos, "BlockMaker needs an anonymous function as argument")
                    }
                  } else {
                    reporter.error(t.pos, s"Term of type ${blockType} must be apply call of BlockMaker, found instead ${showRaw(t)}")
                  }
                case _ =>
                    reporter.error(t.pos, s"Term of type ${blockType} must be apply call of BlockMaker, found instead ${showRaw(t)}")
              }
            }
            traverseTrees(t.children)
          } else super.traverse(t)
        case _ => super.traverse(tree)
      }
    }

    class BlockGenTraverser( unit: CompilationUnit, owner: Symbol ) extends Traverser {
      val copyMethodSymbol = typeOf[Copyable.type].decl(TermName("copy")).asMethod
      val copyableObject = typeOf[Copyable.type].termSymbol.asModule
      
      override def traverse(tree: Tree): Unit = {
        /* Check that there are no captured variables */

        def isCapturedValue(symb: Symbol) : Boolean =
          symb.isGetter || symb.isSetter || symb.isVal || symb.isVar

        if (tree.isTerm) {
          global.
          optionSymbol(tree) match {
            case Some(symb) =>
              if ( isCapturedValue(symb) && !transitiveOwner(owner, symb) )
                reporter.error(tree.pos, 
                  s"This is an unhealthy capture: ${symb}, underlying name: ${symb.name}, \n${showRaw(tree)}: ${symb.owner}, isMethod: ${symb.isMethod}")
            case _ => 
          }
          super.traverse(tree)
        } else super.traverse(tree)
      }
    }
  }
}

