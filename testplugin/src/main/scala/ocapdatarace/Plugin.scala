package ocapdatarace

import scala.tools.nsc
import nsc.Global
import nsc.Phase
import nsc.plugins.Plugin
import nsc.plugins.PluginComponent
import blocks._


class TestChecker( val global: Global ) extends Plugin {
  import global._

  val name = "testchecker"
  val description = "just for trying out some things"
  val components = List[PluginComponent](TestComponent)

  def transitiveOwner( symb: Symbol, owned: Symbol ) : Boolean = 
    owned match {
      case NoSymbol => false
      case _ => owned.owner == symb || transitiveOwner(symb, owned.owner)
    }

  private object TestComponent extends PluginComponent {
    val global: TestChecker.this.global.type = TestChecker.this.global
    val runsAfter = List[String]("refchecks")

    val phaseName = TestChecker.this.name
    override val description = "phase for trying out some things"
    def newPhase(_prev: Phase) = new TestPhase(_prev)


    class TestPhase(prev: Phase) extends StdPhase(prev) {
      override def apply(unit: CompilationUnit): Unit = {
        val t = new TestTraverser(unit)
        t.traverse(unit.body)
      }
    }

    class TestTraverser(unit: CompilationUnit) extends Traverser {
      val tt = typeOf[OcapBlock[_,_]]
      //type OB = OcapBlock[_,_]
        // OcapBlock[A,B] forSome { type A; type B }
      //val OB = CopyableBlock{ (x: Int) => x + 2 }
      //
      var method: MethodSymbol = null

      override def traverse(tree: Tree): Unit = tree match {
        case temp @ Template(_,_,_) => super.traverse(tree)
        //case id @ Ident(tn) =>
        //  println(s"Found ${id}, ${id.pos.line}:${id.pos.column}")
        //  val idsymb = id.symbol
        //  println(idsymb.owner)
        //case vd @ ValDef(mods, name, tpt, rhs) =>
        //  println(mods)
        //  println(name)
        //  println(tpt)
        //  println(rhs)
        //case t @ Apply(fun, args) => 
        //  //println(s"Wow traversing ${fun.symbol.name}")
        //  //println(showRaw(t))
        //  //println(t.fun.tpe)
        //  //println(t.children)
        //  //println(t.freeTerms)
        //  //println(s"Owner: ${fun.symbol.owner.name}")
        //  args.foreach(traverse(_))
        case t: TermTree => 
          if (t.isTyped) {
            val tpe = t.tpe
            if ( tpe <:< tt ) {
              checkBlockGen(t)
            }
            
            println(s"${t.pos.line}:${t.pos.column} is ${t}: ${tpe}")
            println(showRaw(t))
            println( tpe <:< tt )
            t.children.foreach(traverse(_))
          } else super.traverse(t)
        //case md : DefDef =>
        //  method = md.symbol.asInstanceOf[MethodSymbol]
        //  super.traverse(tree)
        //case id @ Ident(tn) =>
        //  val symb = id.symbol
        //  println(s"The owner of ${id}(${symb}) is ${symb.owner}")
        //  println(s"${symb.isValue}, ${symb.isVariable}")
        //  if (method != null) {
        //    println(transitiveOwner(method, symb))
        //  }
        case _ => super.traverse(tree)
      }

      def checkBlockGen( tree: Tree ) : Unit = {
        
      }
    }

    class BlockGenTraverser( unit: CompilationUnit, owner: Symbol ) extends Traverser {
      val copyMethodSymbol = typeOf[Copyable.type].decl(TermName("copy")).asMethod
      val copyableObject = typeOf[Copyable.type].termSymbol.asModule
      
      override def traverse(tree: Tree): Unit = {
        /* Check that there are no captured variables outside a copy application
         */


        /* This is a bit chaotic right now, what it does is the following
         * 
         * If it encounters an identifier that is not owned (transitively) by 
         * owner (which should be the surrounding function within the BlockConstructor)
         * it will report a warning (for now, this should be an error later).
         * If the traverser encounters the copy method from Copyable companion
         * object, applied to a single identifier it will skip this part of the tree.
         * This means in effect we have that all captured variables must have the copy
         * method applied to them.
         */

        tree match {
          case id @ Ident(tn) =>
            tn match {
              case TermName(name) =>
                val sym = id.symbol
                if (sym.isValue && !transitiveOwner(owner, sym) ) {
                  global.reporter.warning(id.pos, s"Captured var ${id} not in copy")
                }
            }
          case Apply(Select(id,tn), List(Ident(_))) =>
            if ( id.symbol == copyableObject && tn == TermName("copy")) {
              // Do nothing
            } else super.traverse(tree)
          case _ => super.traverse(tree)
        }
      }
    }
  }
}
