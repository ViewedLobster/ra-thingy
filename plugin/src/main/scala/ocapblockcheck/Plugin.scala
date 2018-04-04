package ocapblockcheck

import scala.tools.nsc
import nsc._
import nsc.transform._
import nsc.plugins.Plugin
import nsc.plugins.PluginComponent
import blocks._
import scala.language.higherKinds



class BlockChecker( val global: Global ) extends Plugin with TypingTransformers {
  import global._

  val name = "blockchecker"
  val description = "plugin for checkng ocap blocks"
  val components = List[PluginComponent](TestComponent)

  def transitiveOwner(owned: Symbol)(symb: Symbol) : Boolean = 
    owned match {
      case NoSymbol => false
      case _ => owned.owner == symb || transitiveOwner(owned.owner)(symb)
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
      
      /** Traverser that finds all OcapBlocks and typechecks ocap. An ocap
       *  block can only be initialized from a BlockMaker object 
       */
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
                  //println(s"${tpe}, ${tpeConstr}, ${tpeConstr <:< blockMakerConstr}")

                  if ( tpe != tpeConstr && tpeConstr <:< blockMakerConstr ) {
                    
                    def typeClass(tpe: Type): List[Type] = 
                      if (tpe.typeConstructor <:< blockMakerConstr) {
                        tpe.typeArgs match {
                          case Nil => tpe.parents flatMap {typeClass(_)}
                          case l => l
                        }
                      } else Nil

                    val blockTypeClass = typeClass(tpe).head
                    println(s"OcapBlock property type class: ${blockTypeClass}")
                    
                    argfun match {
                      case b @ Block(prelude, expr) =>
                        val pc = new PreludeChecker( unit, blockTypeClass )
                        pc.traverseTrees(prelude)
                        
                        expr match {
                          case f @ Function(args, body) =>
                            val bft = new BlockFunTraverser( unit, f.symbol, pc.preludeSymbols )
                            bft.traverse(body)
                          case _ => reporter.error(expr.pos, "Final expression of block should be an anon fun")
                        }
                        
                      case f @ Function(argList, body) =>
                        println(s"Traversing a tree with symbol ${f.symbol} ###############")
                        val bgt = new BlockFunTraverser( unit, f.symbol, Set() )
                        bgt.traverse(body)

                      case _ => reporter.error(argfun.pos, "BlockMaker needs an anonymous function or block as argument")
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

    class PreludeChecker( unit: CompilationUnit, typeClass: Type ) extends Traverser {
      var preludeSymbols: Set[Symbol] = Set()

      override def traverse(tree: Tree): Unit = tree match {
        case vd @ ValDef(mods, tname, tpt, rhs) =>
          println(s"prelude var/val found: ${vd.symbol}")

          // Add the symbol to the prelude 
          preludeSymbols = preludeSymbols + vd.symbol

          // Check that the type is of the correct type class 
//          val tmpTpe = typeClass.instantiateTypeParams(typeClass.typeParams, List(tpt.tpe))
//          println(tmpTpe)
//          val pt = new PreludeTransformer(unit, typeClass)
//          pt.transform(vd)

        case _ => reporter.error(tree.pos, "Non-value definition in block prelude")
      }

    }

    class PreludeTransformer( unit: CompilationUnit, typeClass: Type ) extends TypingTransformer(unit) { 
      override def transform( tree: Tree ): Tree = {
        
        tree match {
          case vd @ ValDef(mods, tname, tpt, rhs) =>
            val tmpTpe = typeClass.instantiateTypeParams(typeClass.typeParams, List(tpt.tpe))
            localTyper.atOwner(tree.symbol)
            println(analyzer.inferImplicitByType(tmpTpe, localTyper.context1))
        }
        tree

        
      }
    }

    class BlockFunTraverser( unit: CompilationUnit, owner: Symbol, prelude: Set[Symbol] ) extends Traverser {
      val copyMethodSymbol = typeOf[Copyable.type].decl(TermName("copy")).asMethod
      val copyableObject = typeOf[Copyable.type].termSymbol.asModule
      
      override def traverse(tree: Tree): Unit = {
        /* Check that there are no captured variables */

        def isValue(symb: Symbol) : Boolean =
          symb.isGetter || symb.isSetter || symb.isVal || symb.isVar
        
        def preludeOwned(symb: Symbol): Boolean = (prelude contains symb) || (prelude exists {transitiveOwner(symb)(_)})

        if (tree.isTerm) {
          optionSymbol(tree) match {
            case Some(symb) =>
              // println(s"${symb}: ${isValue(symb)}, ${transitiveOwner(symb)(owner)}, ${preludeOwned(symb)}")
              if ( isValue(symb) && !transitiveOwner(symb)(owner) && !preludeOwned(symb) )
                reporter.error(tree.pos, 
                  s"This is an unhealthy capture: ${symb}, underlying name: ${symb.name}, \n${showRaw(tree)}: ${symb.owner}, isMethod: ${symb.isMethod}")
              else super.traverse(tree)
            case _ => super.traverse(tree)
          }
        } else super.traverse(tree)
      }
    }
  }
}

