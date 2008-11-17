package kiama.attribution

object DynamicAttribution { // TODO: extend Attribution (work around serious scalac b0rkage)
    import scala.collection.mutable._
    import Attribution.Attributable
    
    type ChangeBuffer = Buffer[(Attr[_, _], PartialFunction[_, _])]
    private var currentRecordedChanges : ChangeBuffer = null
    private val allRecordedChanges = new HashMap[AnyRef, ChangeBuffer]    
    private var equationsVersion = 0

    def attr[TIn <: Attributable, TOut](f : PartialFunction[TIn, TOut]) : PartialFunction[TIn, TOut] =
        new Attr(new ComposedPartialFunction[TIn, TOut] { add(f) })
    
    def childAttr[T <: Attributable,U] (f : PartialFunction[(T, Attributable), U]) : PartialFunction[T, U] = {
        val childF = new PartialFunction[T, U] {
            def apply(t : T) = f((t, t.parent))
            def isDefinedAt(t : T) = f.isDefinedAt((t, t.parent))
        }
        attr(childF)
    }
    
    def circular[T,U] (init : U) (f : T => U) : T => U =
        new Attribution.CircularAttribute(init, f)

    /**
     * Implicitly converts (partial) functions to support the + operator.
     **/
    implicit def internalToAttr[TIn <: Attributable, TOut](f : Function[TIn, TOut]) : Attr[TIn, TOut] =
        f match {
            case f : PartialFunction[_, _] => attr(f)
            case f => throw new UnsupportedOperationException("Can only convert a partial function to an Attr")
            // TODO: Implement a + operator for a (non-partial function + partial function)?
        }

    def using[T](attributeInitializer : => AnyRef)(block : => T) = {

        try {
            use(attributeInitializer)
            block
        } finally {
            endUse(attributeInitializer)
        }
    }
    
    def use[T](attributeInitializer : => AnyRef) {
        val prevRecordedChanges = currentRecordedChanges
        try {
            currentRecordedChanges = new ArrayBuffer
            val recordedChanges = currentRecordedChanges
            
            val initialized = attributeInitializer // import initializer
            currentRecordedChanges = null
    
            if (allRecordedChanges contains initialized) reuse(initialized)
        } finally {  
           currentRecordedChanges = prevRecordedChanges
        }  
    }
    
    private def reuse(attributeInitializer : AnyRef) {
        for ((attr, function) <- allRecordedChanges(attributeInitializer))
            attr.f.add(function)
    }
    
    def endUse(attributeInitializer : AnyRef) {
        for ((attr, function) <- allRecordedChanges(attributeInitializer))
            attr.f.remove(function)
    }
    
    // TODO: Rename to DynamicAttribute, inherit and reuse from Attribute?
    class Attr[TIn <: Attributable, TOut] (private[DynamicAttribution] var f : ComposedPartialFunction[TIn, TOut])
            extends PartialFunction[TIn, TOut] {
    
        private val cache = new scala.collection.jcl.WeakHashMap[TIn, Option[TOut]]
        private var cacheVersion = equationsVersion
    
        def apply(node : TIn) = {
            if (cacheVersion != equationsVersion) {
                cacheVersion = equationsVersion
                cache.clear
            }
          
            if (cache contains node) {
                cache(node) match {
                    case Some(node) => node
                    case None       => throw new IllegalStateException("Cycle detected in attribute evaluation")
                }
            } else {
                cache(node) = None
                val result = f(node)
                cache(node) = Some(result)
                result
            }
        }
        
        def isDefinedAt(node : TIn) = f.isDefinedAt(node)
        
        def +=(that : PartialFunction[TIn, TOut]) {
            if (currentRecordedChanges != null) currentRecordedChanges += (this, that)
            
            f.add(that)
        }
    }
            
    trait ComposedPartialFunction[TIn, TOut] extends PartialFunction[TIn, TOut] {
        val functions = new scala.collection.mutable.ArrayBuffer[PartialFunction[TIn, TOut]]
      
        def isDefinedAt(i : TIn) = functions.exists(_ isDefinedAt i)
        
        def apply(node : TIn) : TOut = {
            for (i <- (functions.size - 1) until (-1, -1)) {
                if (functions(i) isDefinedAt node) return functions(i)(node)
            }
            throw new MatchError("Function not defined for " + node)
        }
        
        def remove(f : PartialFunction[TIn, TOut]) {
            val removed = functions.lastIndexOf(f)
            functions.remove(removed)
            equationsVersion += 1 // clear all caches
        }
        
        def add(f : PartialFunction[TIn, TOut]) {
            functions += f
            equationsVersion += 1 // clear all caches
        }
    }
}
