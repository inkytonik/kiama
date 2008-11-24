package kiama.example.dataflow

import kiama.attribution.DynamicAttribution
import Dataflow._ // import case classes

case class Foreach(cond : Var, body : Stm) extends Stm

object DataflowForeach extends DynamicAttribution {
    
    Dataflow.succ += attr { case t @ Foreach (_, body) => t->following + body }
    
    Dataflow.following += 
        childAttr {
            _ => {
                case t @ Foreach (_, body) => following(t) + body
            }
        }
}

case class For(init : Stm, c : Stm, inc : Stm, body : Stm) extends Stm
    
object DataflowFor extends DynamicAttribution {

    Dataflow.succ += { case For (init, c, inc, body) => Set(init) }
    
    Dataflow.following += 
        childAttr {
            S => {
                case t @ For (S, c, _, _) => Set (c)
                case t @ For (_, S, _, b) => following(t) + b
                case t @ For (_, c, S, _) => Set (c)
                case t @ For (_, _, i, S) => Set (i)
            }
        }

}
