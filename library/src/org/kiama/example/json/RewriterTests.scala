/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2011-2015 Anthony M Sloane, Macquarie University.
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

package org.kiama
package example.json

import org.kiama.util.Tests

/**
 * Tests that check that the rewriting and decoration works correctly.
 */
class RewriterTests extends Tests {

    import JSONTree._
    import Rewriter._
    import scala.collection.immutable.Seq

    /**
     * Some components of company test data to refer to by name.
     */
    val ralf =
        JObject (
          Seq (
            JName ("type") -> JString ("employee"),
            JName ("name") -> JString ("Ralf"),
            JName ("address") -> JString ("Koblenz"),
            JName ("salary") -> JNumber (1234.0)))

    val klaus =
        JObject (
          Seq (
            JName ("name") -> JString ("Klaus"),
            JName ("address") -> JString ("Boston"),
            JName ("salary") -> JNumber (23456.0)))

    /**
     * Test company (see also test/101comp.json)
     */
    val company : JValue =
        JObject (
          Seq (
            JName ("depts") -> JArray (
              Seq (
                JObject (
                  Seq (
                    JName ("name") -> JString ("Research"),
                    JName ("manager") -> JObject (
                      Seq (
                        JName ("name") -> JString ("Craig"),
                        JName ("address") -> JString ("Redmond"),
                        JName ("salary") -> JNumber (123456.0))),
                    JName ("subunits") -> JArray (
                      Seq (
                        JObject (
                          Seq (
                            JName ("type") -> JString ("employee"),
                            JName ("name") -> JString ("Erik"),
                            JName ("address") -> JString ("Utrecht"),
                            JName ("salary") -> JNumber (12345.0))),
                        ralf)))),
                JObject (
                  Seq (
                    JName ("name") -> JString ("Development"),
                    JName ("manager") -> JObject (
                      Seq (
                        JName ("name") -> JString ("Ray"),
                        JName ("address") -> JString ("Redmond"),
                        JName ("salary") -> JNumber (234567.0))),
                    JName ("subunits") -> JArray (
                      Seq (
                        JObject (
                          Seq (
                            JName ("type") -> JString ("department"),
                            JName ("name") -> JString ("Dev1"),
                            JName ("manager") -> klaus,
                            JName ("subunits") -> JArray (
                              Seq (
                                JObject (
                                  Seq (
                                    JName ("type") -> JString ("department"),
                                    JName ("name") -> JString ("Dev1.1"),
                                    JName ("manager") -> JObject (
                                      Seq (
                                        JName ("name") -> JString ("Karl"),
                                        JName ("address") -> JString ("Riga"),
                                        JName ("salary") -> JNumber (2345.0))),
                                    JName ("subunits") -> JArray (
                                      Seq (
                                        JObject (
                                          Seq (
                                            JName ("type") -> JString ("employee"),
                                            JName ("name") -> JString ("Joe"),
                                            JName ("address") -> JString (
                                              "Wifi City"),
                                            JName ("salary") -> JNumber (2344.0)))))))))))))))))))

    test ("total salary is correct") {
        assertResult (399747.0) (total (company))
    }

    test ("halvng salaries in company works") {
        val newcompany =
            JObject (
              Seq (
                JName ("depts") -> JArray (
                  Seq (
                    JObject (
                      Seq (
                        JName ("name") -> JString ("Research"),
                        JName ("manager") -> JObject (
                          Seq (
                            JName ("name") -> JString ("Craig"),
                            JName ("address") -> JString ("Redmond"),
                            JName ("salary") -> JNumber (61728.0))),
                        JName ("subunits") -> JArray (
                          Seq (
                            JObject (
                              Seq (
                                JName ("type") -> JString ("employee"),
                                JName ("name") -> JString ("Erik"),
                                JName ("address") -> JString ("Utrecht"),
                                JName ("salary") -> JNumber (6172.5))),
                            JObject (
                              Seq (
                                JName ("type") -> JString ("employee"),
                                JName ("name") -> JString ("Ralf"),
                                JName ("address") -> JString ("Koblenz"),
                                JName ("salary") -> JNumber (617.0))))))),
                    JObject (
                      Seq (
                        JName ("name") -> JString ("Development"),
                        JName ("manager") -> JObject (
                          Seq (
                            JName ("name") -> JString ("Ray"),
                            JName ("address") -> JString ("Redmond"),
                            JName ("salary") -> JNumber (117283.5))),
                        JName ("subunits") -> JArray (
                          Seq (
                            JObject (
                              Seq (
                                JName ("type") -> JString ("department"),
                                JName ("name") -> JString ("Dev1"),
                                JName ("manager") -> JObject (
                                  Seq (
                                    JName ("name") -> JString ("Klaus"),
                                    JName ("address") -> JString ("Boston"),
                                    JName ("salary") -> JNumber (11728.0))),
                                JName ("subunits") -> JArray (
                                  Seq (
                                    JObject (
                                      Seq (
                                        JName ("type") -> JString ("department"),
                                        JName ("name") -> JString ("Dev1.1"),
                                        JName ("manager") -> JObject (
                                          Seq (
                                            JName ("name") -> JString ("Karl"),
                                            JName ("address") -> JString ("Riga"),
                                            JName ("salary") -> JNumber (1172.5))),
                                        JName ("subunits") -> JArray (
                                          Seq (
                                            JObject (
                                              Seq (
                                                JName ("type") -> JString ("employee"),
                                                JName ("name") -> JString ("Joe"),
                                                JName ("address") -> JString (
                                                  "Wifi City"),
                                                JName ("salary") -> JNumber (1172.0)))))))))))))))))))
        assertResult (newcompany) (cut (company))
    }

}
