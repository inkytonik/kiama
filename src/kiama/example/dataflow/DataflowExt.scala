package kiama.example.dataflow

import kiama.attribution.DynamicAttribution
import Dataflow._ // import case classes

case class Foreach(cond : Var, body : Stm) extends Stm

object DataflowForeach extends DynamicAttribution {
    
    Dataflow.succ += attr { case t @ Foreach (_, body) => t->following + body }
    
    Dataflow.following += 
        childAttr {
            case (_, t @ Foreach (_, body)) => t->following + body
        }
}

case class For(init : Stm, c : Stm, inc : Stm, body : Stm) extends Stm
    
object DataflowFor extends DynamicAttribution {

    Dataflow.succ += { case For (init, c, inc, body) => Set(init) }
    
    Dataflow.following += 
        childAttr {
            case (s, t @ For (init, c, inc, body)) if s == init => Set (c)
            case (s, t @ For (init, c, inc, body)) if s == body => Set (inc)
            case (s, t @ For (init, c, inc, body)) if s == c    => (t->following) + body
            case (s, t @ For (init, c, inc, body)) if s == inc  => Set (body)
        }

}
