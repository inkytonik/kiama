package kiama.example.dataflow

import Dataflow._
import kiama.attribution.DynamicAttribution._

case class Foreach(cond : Var, body : Stm) extends Stm

object DataflowForeach {
    
    Dataflow.succ += { case Foreach (_, s) => Set(s) }
    
    Dataflow.following += 
        childAttr {
            case (_, t @ Foreach (_, _)) => Set (t)
        }

}

case class For(init : Stm, c : Stm, inc : Stm, body : Stm) extends Stm
    
object DataflowFor {

    Dataflow.succ += { case For (init, c, inc, body) => Set(init) }
    
    Dataflow.following += 
        childAttr {
            case (s, t @ For (init, c, inc, body)) if s == init => Set (c)
            case (s, t @ For (init, c, inc, body)) if s == body => Set (inc)
            case (s, t @ For (init, c, inc, body)) if s == c    => t->following + body
            case (s, t @ For (init, c, inc, body)) if s == inc  => Set (body)
        }

}
