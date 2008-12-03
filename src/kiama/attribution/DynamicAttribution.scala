package kiama.attribution

object DynamicAttribution extends DynamicAttribution

trait DynamicAttribution extends AttributionBase {

    import scala.collection.mutable._
    import scala.collection.jcl.IdentityHashMap

    type ChangeBuffer = ArrayBuffer[(DynamicAttribute[_, _], PartialFunction[_, _])]
    
    private var currentRecordedChanges : ChangeBuffer = null
    private val allRecordedChanges = new IdentityHashMap[AnyRef, ChangeBuffer]    
    private var equationsVersion = 0
    
    /**
     * Lazily resets all memoisation tables.
     */
    def resetMemo = equationsVersion += 1

    /**
     * Define an attribute of T nodes of type U by the function f.
     */ 
    def attr[T <: Attributable,U] (f : PartialFunction[T,U]) : PartialFunction[T,U] =
        new DynamicAttribute (f)

    /**
     * Define an attribute of T nodes of type U by the function f,
     * which takes the current node and its parent as its arguments.
     */ 
    def childAttr[T <: Attributable,U] (f : T => PartialFunction[Attributable,U]) : PartialFunction[T,U] = {
        val childF = new PartialFunction[T,U] {
            def apply (t : T) = f (t) (t.parent)
            def isDefinedAt (t : T) = f (t) isDefinedAt t.parent
        }
        attr (childF)
    }

    /**
     * Implicitly converts partial functions to support the + operator.
     **/
    implicit def internalToDynamicAttribute[T <: Attributable,U] (f : Function[T,U]) : DynamicAttribute[T,U] =
        f match {
            case f : DynamicAttribution#DynamicAttribute[_, _] => f.asInstanceOf[DynamicAttribute[T,U]]
            case f => throw new UnsupportedOperationException("Cannot only add partial functions to existing attributes")
        }

    def using[T] (attributeInitializer : => AnyRef) (block : => T) = {
        try {
            use (attributeInitializer)
            block
        } finally {
            endUse (attributeInitializer)
        }
    }
    
    def use[T] (attributeInitializer : => AnyRef) {
        val prevRecordedChanges = currentRecordedChanges
        try {
            currentRecordedChanges = new ArrayBuffer
            val recordedChanges = currentRecordedChanges
            
            val initialized = attributeInitializer // import initializer
            currentRecordedChanges = null
    
            if (allRecordedChanges contains initialized) reuse (initialized)
        } finally {  
           currentRecordedChanges = prevRecordedChanges
        }  
    }
    
    private def reuse (attributeInitializer : AnyRef) {
        for ((attr, function) <- allRecordedChanges (attributeInitializer))
            attr += function
    }
    
    def endUse (attributeInitializer : AnyRef) {
        for ((attr, function) <- allRecordedChanges (attributeInitializer))
            attr -= function
    }
    
    class DynamicAttribute[T,U] (private var f : PartialFunction[T,U]) extends PartialFunction[T,U] {
        private val memo = new java.util.IdentityHashMap[T, Option[U]]
        private var memoVersion = equationsVersion
    
        def apply (t : T) = {
            if (memoVersion != equationsVersion) {
                memoVersion = equationsVersion
                memo.clear
            }
            
            memo.get (t) match {
                case Some (u) => u
                case None    => throw new IllegalStateException("Cycle detected in attribute evaluation")
                case null =>
                    memo.put (t, None)
                    val result = f (t)
                    memo.put (t, Some (result))
                    result
            }
        }
        
        def isDefinedAt (t : T) = f isDefinedAt t
        
        def composedF : ComposedPartialFunction[T,U] =
            f match {
                case _ : ComposedPartialFunction[_,_] => f.asInstanceOf[ComposedPartialFunction[T,U]]
                case _ : PartialFunction[_,_]         => val g = new ComposedPartialFunction(f); f = g; g
            }
        
        def += (g : PartialFunction[T,U]) {
            val uncached : PartialFunction[T,U] = g match {
                case g : DynamicAttribute[_, _] => g.f
                case _                          => g
            }
            
            if (currentRecordedChanges != null) currentRecordedChanges += (this, uncached)
            composedF += uncached
            resetMemo
        }
        
        def -= (g : PartialFunction[T,U]) {
            composedF -= g
            resetMemo
        }
    }
            
    /**
     * A partial function composed of an ordered, mutable buffer of
     * PartialFunction instances.
     */
    class ComposedPartialFunction[T,U] (f : PartialFunction[T,U]) extends PartialFunction[T,U] {
        val functions = new ArrayBuffer[PartialFunction[T,U]]
      
        def isDefinedAt (i : T) = functions.exists(_ isDefinedAt i)
        
        def apply (t : T) : U = {
            for (i <- (functions.size - 1) until (-1, -1)) {
                if (functions(i) isDefinedAt t) return functions(i)(t)
            }
            throw new MatchError("Function not defined for " + t)
        }
        
        def += (g : PartialFunction[T,U]) {
            functions += g
        }
        
        def -= (g : PartialFunction[T,U]) {
            val removed = functions.lastIndexOf(f)
            functions.remove(removed)
        }
        
        this += f
    }
}
