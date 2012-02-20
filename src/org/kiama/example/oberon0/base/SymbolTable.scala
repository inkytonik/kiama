package org.kiama
package example.oberon0
package base

import util.Environments

/**
 * Basic symbol table definitions.
 */
trait SymbolTable extends Environments {

    /**
     * An entity that represents an error situation.  These entities are
     * accepted in most situations to avoid cascade errors.
     */
    trait ErrorEntity extends Entity
    
    /**
     * A entity represented by names for whom we have seen more than one
     * declaration so we are unsure what is being represented.
     */
    case object MultipleEntity extends ErrorEntity {
        lazy val ident = "multiple"
    }

    /**
     * An unknown entity, represented by names whose declarations are missing.
     */
    case object UnknownEntity extends ErrorEntity {
        lazy val ident = "unknown"
    }

}
