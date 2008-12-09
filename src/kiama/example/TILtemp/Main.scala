/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2008 Anthony M Sloane, Macquarie University.
 *
 * Kiama is free software: you can redistribute it and/or modify it under
 * the terms of the GNU Lesser General Public License as published by the
 * Free Software Foundation, either version 3 of the License, or (at your
 * option) any later version.
 *
 * Kiama is distributed in the hope that it will be useful, but WITHOUT ANY
 * WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public License for
 * more details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with Kiama.  (See files COPYING and COPYING.LESSER.)  If not, see
 * <http://www.gnu.org/licenses/>.
 */                         

package kiama.example.til

 /**
  * Standard main program for TIL chairmarks.
  */
 trait Main {
 
     import java.io.{FileNotFoundException,FileReader,Reader}
 
     /**
      * Accept file name arguments and pass them one-by-one to the process
      * function.
      */
     def main (args : Array[java.lang.String]) {
         for (arg <- args) {
             try {
                 val reader = new FileReader (arg)
                 process (reader)
             } catch {
                 case e : FileNotFoundException =>
                     println ("can't open " + arg + " for reading")
             }
         }
     }

     /**
      * Process the file given by the argument reader.
      */
     def process (reader : Reader)

 }
 
 