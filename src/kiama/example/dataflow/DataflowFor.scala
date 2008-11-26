package kiama.example.dataflow

import kiama.attribution.DynamicAttribution._
import Dataflow._

case class Foreach (cond : Var, body : Stm) extends Stm

object DataflowForeach {
    
    Dataflow.succ +=
        attr { case t @ Foreach (_, body) => t->following + body }
    
    // Using the childAttr notation
    Dataflow.following += 
        childAttr {
            _ => { case t @ Foreach(_, body) => following(t) + body }
        }
    
    // Alternatively, using the regular attr notation
    Dataflow.following += 
        attr {
            case t if t.parent.isInstanceOf[Foreach] =>
                 val parent = t.parent[Foreach]
                 following(parent) + parent.body
        }
}

case class For(init : Stm, c : Stm, inc : Stm, body : Stm) extends Stm
    
object DataflowFor {

    Dataflow.succ += 
        attr { case For (init, c, inc, body) => Set (init) }
    
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
