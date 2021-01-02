/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2011-2021 Anthony M Sloane, Macquarie University.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.bitbucket.inkytonik.kiama
package example.json

import org.bitbucket.inkytonik.kiama.util.KiamaTests

/**
 * Tests that check that the rewriting and decoration works correctly.
 */
class RewriterTests extends KiamaTests {

    import JSONTree._
    import Rewriter._

    /**
     * Some components of company test data to refer to by name.
     */
    val ralf =
        JObject(
            Vector(
                JName("type") -> JString("employee"),
                JName("name") -> JString("Ralf"),
                JName("address") -> JString("Koblenz"),
                JName("salary") -> JNumber(1234.0)
            )
        )

    val klaus =
        JObject(
            Vector(
                JName("name") -> JString("Klaus"),
                JName("address") -> JString("Boston"),
                JName("salary") -> JNumber(23456.0)
            )
        )

    /**
     * Test company (see also test/101comp.json)
     */
    val company : JValue =
        JObject(
            Vector(
                JName("depts") -> JArray(
                    Vector(
                        JObject(
                            Vector(
                                JName("name") -> JString("Research"),
                                JName("manager") -> JObject(
                                    Vector(
                                        JName("name") -> JString("Craig"),
                                        JName("address") -> JString("Redmond"),
                                        JName("salary") -> JNumber(123456.0)
                                    )
                                ),
                                JName("subunits") -> JArray(
                                    Vector(
                                        JObject(
                                            Vector(
                                                JName("type") -> JString("employee"),
                                                JName("name") -> JString("Erik"),
                                                JName("address") -> JString("Utrecht"),
                                                JName("salary") -> JNumber(12345.0)
                                            )
                                        ),
                                        ralf
                                    )
                                )
                            )
                        ),
                        JObject(
                            Vector(
                                JName("name") -> JString("Development"),
                                JName("manager") -> JObject(
                                    Vector(
                                        JName("name") -> JString("Ray"),
                                        JName("address") -> JString("Redmond"),
                                        JName("salary") -> JNumber(234567.0)
                                    )
                                ),
                                JName("subunits") -> JArray(
                                    Vector(
                                        JObject(
                                            Vector(
                                                JName("type") -> JString("department"),
                                                JName("name") -> JString("Dev1"),
                                                JName("manager") -> klaus,
                                                JName("subunits") -> JArray(
                                                    Vector(
                                                        JObject(
                                                            Vector(
                                                                JName("type") -> JString("department"),
                                                                JName("name") -> JString("Dev1.1"),
                                                                JName("manager") -> JObject(
                                                                    Vector(
                                                                        JName("name") -> JString("Karl"),
                                                                        JName("address") -> JString("Riga"),
                                                                        JName("salary") -> JNumber(2345.0)
                                                                    )
                                                                ),
                                                                JName("subunits") -> JArray(
                                                                    Vector(
                                                                        JObject(
                                                                            Vector(
                                                                                JName("type") -> JString("employee"),
                                                                                JName("name") -> JString("Joe"),
                                                                                JName("address") -> JString(
                                                                                    "Wifi City"
                                                                                ),
                                                                                JName("salary") -> JNumber(2344.0)
                                                                            )
                                                                        )
                                                                    )
                                                                )
                                                            )
                                                        )
                                                    )
                                                )
                                            )
                                        )
                                    )
                                )
                            )
                        )
                    )
                )
            )
        )

    test("total salary is correct") {
        total(company) shouldBe 399747.0
    }

    test("halving salaries in company works") {
        val newcompany =
            JObject(
                Vector(
                    JName("depts") -> JArray(
                        Vector(
                            JObject(
                                Vector(
                                    JName("name") -> JString("Research"),
                                    JName("manager") -> JObject(
                                        Vector(
                                            JName("name") -> JString("Craig"),
                                            JName("address") -> JString("Redmond"),
                                            JName("salary") -> JNumber(61728.0)
                                        )
                                    ),
                                    JName("subunits") -> JArray(
                                        Vector(
                                            JObject(
                                                Vector(
                                                    JName("type") -> JString("employee"),
                                                    JName("name") -> JString("Erik"),
                                                    JName("address") -> JString("Utrecht"),
                                                    JName("salary") -> JNumber(6172.5)
                                                )
                                            ),
                                            JObject(
                                                Vector(
                                                    JName("type") -> JString("employee"),
                                                    JName("name") -> JString("Ralf"),
                                                    JName("address") -> JString("Koblenz"),
                                                    JName("salary") -> JNumber(617.0)
                                                )
                                            )
                                        )
                                    )
                                )
                            ),
                            JObject(
                                Vector(
                                    JName("name") -> JString("Development"),
                                    JName("manager") -> JObject(
                                        Vector(
                                            JName("name") -> JString("Ray"),
                                            JName("address") -> JString("Redmond"),
                                            JName("salary") -> JNumber(117283.5)
                                        )
                                    ),
                                    JName("subunits") -> JArray(
                                        Vector(
                                            JObject(
                                                Vector(
                                                    JName("type") -> JString("department"),
                                                    JName("name") -> JString("Dev1"),
                                                    JName("manager") -> JObject(
                                                        Vector(
                                                            JName("name") -> JString("Klaus"),
                                                            JName("address") -> JString("Boston"),
                                                            JName("salary") -> JNumber(11728.0)
                                                        )
                                                    ),
                                                    JName("subunits") -> JArray(
                                                        Vector(
                                                            JObject(
                                                                Vector(
                                                                    JName("type") -> JString("department"),
                                                                    JName("name") -> JString("Dev1.1"),
                                                                    JName("manager") -> JObject(
                                                                        Vector(
                                                                            JName("name") -> JString("Karl"),
                                                                            JName("address") -> JString("Riga"),
                                                                            JName("salary") -> JNumber(1172.5)
                                                                        )
                                                                    ),
                                                                    JName("subunits") -> JArray(
                                                                        Vector(
                                                                            JObject(
                                                                                Vector(
                                                                                    JName("type") -> JString("employee"),
                                                                                    JName("name") -> JString("Joe"),
                                                                                    JName("address") -> JString(
                                                                                        "Wifi City"
                                                                                    ),
                                                                                    JName("salary") -> JNumber(1172.0)
                                                                                )
                                                                            )
                                                                        )
                                                                    )
                                                                )
                                                            )
                                                        )
                                                    )
                                                )
                                            )
                                        )
                                    )
                                )
                            )
                        )
                    )
                )
            )
        cut(company) shouldBe newcompany
    }

}
