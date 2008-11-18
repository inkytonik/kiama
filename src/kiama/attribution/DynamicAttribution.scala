package kiama.attribution

object DynamicAttribution extends DynamicAttribution

trait DynamicAttribution {
    // TODO: extend Attribution (work around some serious scalac b0rkage)
    import Attribution._
    type Attributable = Attribution.Attributable

    import scala.collection.mutable._
    import scala.collection.jcl.IdentityHashMap

    type ChangeBuffer = Buffer[(Attr[_, _], PartialFunction[_, _])]
    private var currentRecordedChanges : ChangeBuffer = null
    private val allRecordedChanges = new IdentityHashMap[AnyRef, ChangeBuffer]    
    private var equationsVersion = 0

    def attr[TIn <: Attributable, TOut](f : PartialFunction[TIn, TOut]) : Attr[TIn, TOut] =
        new Attr(new ComposedPartialFunction[TIn, TOut] { add(f) })

    def circular[TIn,TOut] (init : TOut) (f : TIn => TOut) : TIn => TOut =
        Attribution.circular(init)(f)
    
    def childAttr[T <: Attributable,U] (f : PartialFunction[(T, Attributable), U]) : PartialFunction[T, U] = {
        val childF = new PartialFunction[T, U] {
            def apply(t : T) = f((t, t.parent))
            def isDefinedAt(t : T) = f.isDefinedAt((t, t.parent))
        }
        attr(childF)
    }

    /**
     * Implicitly converts (partial) functions to support the + operator.
     **/
    implicit def internalToAttr[TIn <: Attributable, TOut](f : Function[TIn, TOut]) : Attr[TIn, TOut] =
        f match {
            case f : DynamicAttribution#Attr[_, _] => f.asInstanceOf[Attr[TIn,TOut]]
            case f => throw new UnsupportedOperationException()
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
    
    class Attr[TIn <: Attributable, TOut] (var f : ComposedPartialFunction[TIn, TOut])
            extends PartialFunction[TIn, TOut] {
    
        private val cache = new IdentityHashMap[TIn, Option[TOut]]
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
        
        def +=(that : PartialFunction[TIn, TOut]) = {
            if (currentRecordedChanges != null) currentRecordedChanges += (this, that)
            
            f.add(that)
        }
    }
            
    trait ComposedPartialFunction[TIn, TOut] extends PartialFunction[TIn, TOut] {
        val functions = new ArrayBuffer[PartialFunction[TIn, TOut]]
      
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
