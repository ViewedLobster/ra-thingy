
package blocks

import scala.language.higherKinds
import scala.language.experimental.macros
import scala.reflect.macros.blackbox.Context
import scala.annotation.tailrec

object Macros {

  // TODO make sure that we are allowed to capture ClassSymbols

  // In order to avoid the type parameters A and B, maybe we could change type signature
  // of `fun` to AnyRef and instead to checks in macro
  //def block[P[_], A, B] ( fun: A => B ): OcapBlock[A,B] = macro blockImpl[P, A, B]

  def block[P[_], A, B] ( code: A => B ): OcapBlock[A,B]{ type Prop[S] = P[S] } = macro blockImpl[P[Nothing], A, B]

  def blockImpl[P: c.WeakTypeTag, A: c.WeakTypeTag, B: c.WeakTypeTag](c: Context)( code: c.Tree ): c.Tree = {
    import c.universe._
    val blockName = TypeName("blk")
    // HELPER METHODS
    def checkValueCaptures( prelude: List[Tree] ): List[(Symbol, Tree)] = {
      prelude map {
        case vd @ ValDef(mods, tname, tpt, expr) => 
          (vd.symbol, expr)
        case t => c.abort(t.pos, "Prelude must consist of valdefs only") 
      }
    }

    def getPreludeTypes( prelude: List[Tree] ): List[Type] = {
      prelude map {
        case ValDef(mods, tname, tpt, expr) => tpt.tpe
        case t => c.abort(t.pos, "Prelude must consist of valdefs only")
      }
    }

    @tailrec
    def isOwnedTransitively(symb: Symbol)(owner: Symbol) : Boolean = {
      if ( symb == owner ) true
      else symb match {
        case NoSymbol => false
        case _ => symb.owner == owner || isOwnedTransitively(symb.owner)(owner)
      }
    }
    

    // NOTE
    // Can we use TermSymbol to figure out if the referenced value is stable?
    //
    // TODO
    // Check that all captured values are stable, e.g. not a var
    // Check that all expressions in prelude are identifiers or selects
    // Check that body only references the prelude and parameters
    // Check existance of implicits (is done when generating code)
    //
    // Check the ocap constraint (is done in plugin)

    // Checks that the expression is an identifier or select expr and that the 
    // reference is non-modifiable, i.e. it is stable?
    def checkCaptureExpr( expr: Tree ) : Unit = {
      expr match {
        case id @ Ident(tn) =>
          if (!id.symbol.asTerm.isStable)
            c.error(expr.pos, "Referenced symbol not stable")
        case sel @ Select(_, _) =>
          if (!sel.symbol.asTerm.isStable)
            c.error(expr.pos, "Referenced symbol not stable")
// TODO this keyword, what to do?
//        case th @ This(_) => 
//          println(th.symbol.isStatic)
//          println(th.symbol.isModule)
//          println(th.symbol.isModuleClass)
//          if (!th.symbol.isStatic)
//            c.error(expr.pos, "Referenced symbol not static")
        case e => 
          println(showRaw(e))
          c.abort(e.pos, "Captured expression must be identifier or select")
      }
    }

    def getCapturedFromBody( prelSymbols: Set[Symbol], fun: Function ): (Set[Symbol], Set[Type]) = {
      // Create traverser and get all captured
      val cg = new CaptureGatherer(prelSymbols, fun.symbol)
      cg.traverse(fun.body)
      (cg.captured, cg.capturedTypes)
    }

    class CaptureGatherer( preludeSymbols: Set[Symbol], fun: Symbol ) extends Traverser {
      var captured: Set[Symbol] = Set()
      var capturedTypes: Set[Type] = Set()

      def isCaptured(symb: Symbol): Boolean = 
        !((preludeSymbols contains symb) || isOwnedTransitively(symb)(fun))

      override def traverse(tree: Tree): Unit = tree match {
        case id @ Ident(tn) =>
          if (isCaptured(id.symbol)) {
            captured += id.symbol
            capturedTypes += id.tpe
          }
        case sel @ Select(_,_) =>
          if (isCaptured(sel.symbol)) {
            captured += sel.symbol
            capturedTypes += sel.tpe
          }
        case _ => super.traverse(tree)
      }
    }

    // Check that `funbody` only references parameters and the allowed symbols
    def checkForUndeclared( funbody: Tree, allowedSymbols: Set[Symbol], funsymb: Symbol ): Unit = {
      new UndeclaredTraverser(allowedSymbols, funsymb).traverse(funbody)
    }

    class UndeclaredTraverser( allowed: Set[Symbol], ownerfun: Symbol ) extends Traverser {
      def isStaticPath( s: Symbol ) : Boolean = {
        s != NoSymbol && {
          (s.isMethod && isStaticPath(s.owner)) || {
            (s.isModule || s.isModuleClass || s.isPackage || s.isPackageClass) &&
            (s.isStatic || isStaticPath(s.owner))
          }
        }
      }

      def isAccessible( tree: Tree ): Boolean = 
        (allowed contains tree.symbol) || isOwnedTransitively(tree.symbol)(ownerfun) ||
        isStaticPath(tree.symbol)

        // Shouldn' this look at selects instead since in no case should we allow
        // capturing `this`? Statically accessible methods should be allowed 
      override def traverse(tree: Tree) : Unit = tree match {
        // There are 4 ways to reference an object:
        case New(_) => () // New is always allowed since it is a class instantiation
                          // Instansiation checks will be done in compiler plugin
        case id @ Ident(_) =>  
          if (!isAccessible(id)) {
            c.error(tree.pos, s"Reference not allowed ${showRaw(tree)}: ${tree.symbol}")
          }
        case th @ This(_) => // Checks for isolation will be carried out later (ocap check)
          if (!isAccessible(th)) {
            c.error(tree.pos, s"Reference not allowed ${showRaw(tree)}: ${tree.symbol}")
          }
        case su @ Super(_) => // Checks for isolation will be carried out later (ocap check)
          if (!isAccessible(su)) {
            c.error(tree.pos, s"Reference not allowed ${showRaw(tree)}: ${tree.symbol}")
          }
        case _ => super.traverse(tree)
      }
    }

    class TermNameGenerator {
      var capCount: Int = 0
      var paramCount: Int = 0

      val baseStringCap = "blockcap$"
      val baseStringPar = "blockpar$"
      def getNextCap: TermName = {
        capCount += 1
        TermName(baseStringCap+capCount)
      }

      def getNextPar: TermName = {
        paramCount += 1
        TermName(baseStringPar+paramCount)
      }
    }

    def generateBlockTree(capturedSyms: List[Symbol],
                          paramSyms: List[Symbol],
                          funBody: Tree,
                          prop: Type,
                          resType: Type,
                          constrParams: List[Tree],
                          tng: TermNameGenerator = new TermNameGenerator): Tree = {


      // method to generate replacement trees for captures
      def capturedReplacementTrees(capturedSymbols: List[Symbol]) : ( List[Tree], List[Tree] ) = {
        val termNamesAndTypes = capturedSymbols map { cs => (tng.getNextCap, cs.typeSignature) }
        val capReplacementValDefs = termNamesAndTypes map { case (tn,  t: Type) => q"val $tn: $t" }
        val capReplacementRefs = termNamesAndTypes map { case (tn, _) => Select(Ident(TermName("self")), tn) } 

        ( capReplacementValDefs, capReplacementRefs )
      }

      // method to generate replacement trees for parameters
      
      def paramReplacementTrees( paramSymbols: List[Symbol] ) = {

        val termNamesAndTypes = paramSymbols map { cs => (tng.getNextPar, cs.typeSignature) }
        val parReplacementValDefs = termNamesAndTypes map 
                                    { case (tn: TermName,  t: Type) => ValDef(Modifiers(Flag.PARAM), tn, TypeTree(t), EmptyTree) }
        val parReplacementRefs = termNamesAndTypes map { case (tn, _) => Ident(tn) } 
        ( parReplacementValDefs, parReplacementRefs )
      }

      class SubstitutionTransformer( sub: Map[Symbol, Tree] ) extends Transformer {
        override def transform( tree: Tree ): Tree = tree match {
          case id @ Ident(_) => sub.getOrElse(id.symbol, tree)
          case _ => super.transform(tree)
        }
      }

      def inferImplicitProperties( tps: List[Type], prop: Type ): List[Tree] = {
        tps flatMap { t => {
          val appProp = appliedType(prop, List(t))
          c.inferImplicitValue(appProp) match {
            case EmptyTree => 
              c.error(code.pos, s"Cannot infer implicit property of type $appProp")
              Nil
            case tree => List(tree)
          }
        }}
      }

      def replaceReferences( body: Tree, substitution: Map[Symbol, Tree] ): Tree = {
        new SubstitutionTransformer( substitution ).transform(body)
      }

      // generate new trees for captured vals
      val ( newTreesCapturedValDefs, newTreesCapturedRefs ) = capturedReplacementTrees(capturedSyms)
      val ( newTreesParamValDefs, newTreesParamRefs ) = paramReplacementTrees( paramSyms )

      val argTypes = paramSyms map { ps => ps.typeSignature }

      val substitution = ((capturedSyms ++ paramSyms) zip (newTreesCapturedRefs ++ newTreesParamRefs)).toMap
      
      // replace references in function body tree and untypecheck so we can
      // typecheck properly later
      val newBody = c.untypecheck(replaceReferences( funBody, substitution ))
      val bclassname = c.freshName(blockName)

      val capturedTypes = (capturedSyms map { cs => cs.typeSignature })
      val implicitStmts = capturedTypes map { t => q"implicitly[blocks.Immutable[$t]]" }
      // TODO should we do something with this?
      val implicitTrees: List[Tree] = inferImplicitProperties(capturedTypes, prop)

      val propPathList = prop.toString.split('.')
      val propPathEncapsulated = propPathList.init.map{s => TermName(s)} :+ TypeName(propPathList.last)

      // TODO this is really pathological and hard to read
      val propTreeFun = propPathEncapsulated.tail.foldLeft{ (t: Tree) => t }
        {
          (inner: Tree => Tree, next: Name) =>
            {(t: Tree) => Select(t, next) } compose inner 
        }
      
      val propTree = AppliedTypeTree(propTreeFun(Ident(propPathEncapsulated.head)), List(Ident(TypeName("S"))))
      
      val newCode = q"""
      class $bclassname (..$newTreesCapturedValDefs) extends OcapBlock[..$argTypes, $resType] { 
        self =>
        type Prop[S] = $propTree
        
        
        def apply( ..$newTreesParamValDefs ): $resType = $newBody
      }
      new $bclassname(..$constrParams)
      """
      
      println(showRaw(newCode))
      println("Macro generated:")
      println(newCode)

      //c.typecheck( newCode )
      newCode
    
      // TODO
      // DONE  generate new trees for the captured vals, i.e. prelude
      //        This should be of the form
      //          Select(Ident(TermName("self")), TermName("varname"))
      //        since the captured variables will be fields in the block
      // DONE  generate new trees and definitions for the parameters
      //        THis should be of the form 
      //          Ident(TermName("paramname"))
      //          ( the parameter should be of the form
      //            ValDef(pMods, TermName("paramname"), paramType, EmptyTree)
      //          )
      //        since they will be parameters to the block apply function
      // DONE  transform the trees by replacing all trees with symbols that refers to any of captured or parameters with new trees
      // DONE  make sure to untypecheck and then typecheck
      //
      // DONE  Add implicitly things
      //
      // DONE  Different way to get the implicits check.
      //
      // DONE  Generate the property properly
      //
      //
    }

    // Actual code

    val prop = c.weakTypeOf[P].typeConstructor

    // Extract prelude and function implementation
    val (prelude, fun) = code match {
      case b @ Block(exprs, f @ Function(_,_)) => (exprs, f)
      case fd @ Function(_, _) => (Nil, fd)
      case _ => c.abort(code.pos, "block body not of the correct form")
    }

    val preludeDefs = checkValueCaptures(prelude)
    val (preludeSymbols, preludeExprs) = (preludeDefs.unzip)
    //println(preludeExprs)
    val preludeExprsOk = preludeExprs.foreach(checkCaptureExpr)
    val preludeTypes = getPreludeTypes(prelude)
    // TODO should we just take captured variables and check that they 
    // are stable and immutable, maybe using this ---v
    //
    //val (capturedSymbols, capturedTypes) = getCapturedFromBody( preludeSymbols.toSet, fun )

    val functionParamSyms = fun match {
      case Function(params, body) => params map {_.symbol}
    }
    val allowedSymbols = preludeSymbols.toSet ++ functionParamSyms.toSet
    // CHeck that no values exept parameters and 
    checkForUndeclared(fun.body, allowedSymbols, fun.symbol)

//    println(s"${showRaw(code)}")
//    println(fun.symbol)
//    println(s"code symbol: ${code.symbol}")
//    println(s"code is term: ${code.isTerm}")

    c.typecheck(generateBlockTree(preludeSymbols,
                      functionParamSyms,
                      fun.body,
                      prop,
                      c.weakTypeOf[B],
                      preludeExprs))
    
  }
}
  /** Nice rewrite:
   *  block[P] {
   *    import somepackage.implicits._
   *    val v1: C1 = someExpr1
   *    val v2: C2 = someExpr2
   *    val v3: C3 = someExpr3
   *
   *    ( a: A ) => { 
   *      functionBody 
   *    }
   *  }
   *
   *  given that there is implicit object of type Prop[CN]: call this implN in scope this
   *  would be rewritten as
   *  class Block$k extends OcapBlock[A,B]{ type Prop[S] = P[S] } {
   *    import somepackage.implicits._
   *    def this(v1: C1, v2: C2, v3: C3) = {
   *      val this.v1 = implicitly[P[C1]].init(v1)
   *      val this.v2 = implicitly[P[C2]].init(v2)
   *      val this.v3 = implicitly[P[C3]].init(v3)
   *    }
   *    override def apply( a : A ): B = {
   *      functionBody
   *    }
   *  }
   *  new Block$k(someExpr1, someExpr2, someExpr3)
   *
   *
   *  or maybe instead 
   *  class Block$k(val v1: C1, val v2: C2, val v3: C3)  extends OcapBlock[A,B]{ type Prop[S] = P[S] } {
   *    import somepackage.implicits._
   *    val properyImplicits = ( implicitly[P[C1]], implicitly[P[C2]], implicitly[P[C3]] )
   *  
   *    
   *    override def init:  somemethod to generate something from the block
   *    override def apply( a : A ): B = {
   *      functionBody
   *    }
   *  }
   *  new Block$k(someExpr1, someExpr2, someExpr3)
   *
   *  Can you use implicit method to somehow generate an init method for the whole
   *  block class?
   *
   *  Maybe change the Property typeclasses to include a second type param which
   *  holds the result type class maybe...k
   *
   *  would have to place imports of implicits somewhere aswell, probably before
   *
   */


