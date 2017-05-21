/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2014-2017 Anthony M Sloane, Macquarie University.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.bitbucket.inkytonik.kiama
package util

/**
 * An entity that represents some program object.
 */
abstract class Entity

/**
 * An entity that represents an error situation. These entities are
 * usually accepted in most situations to avoid cascade errors.
 */
abstract class ErrorEntity extends Entity

/**
 * A entity represented by names for whom we have seen more than one
 * declaration so we are unsure what is being represented.
 */
case class MultipleEntity() extends ErrorEntity

/**
 * An unknown entity, for example one that is represened by names whose
 * declarations are missing.
 */
case class UnknownEntity() extends ErrorEntity
