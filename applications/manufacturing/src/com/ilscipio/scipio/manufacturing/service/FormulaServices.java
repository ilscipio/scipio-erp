/*
 * Scipio Commerce
 * Copyright (C) Ilscipio GmbH
 *
 * This file is part of Scipio Commerce. Scipio Commerce is free software: you
 * can redistribute it and modify it under the terms of the GNU Affero General
 * Public License, version 3, as published by the Free Software Foundation.
 * Scipio Commerce is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License
 * for more details. You should have received a copy of the license with this
 * work (file LICENSE). If not, see <https://www.gnu.org/licenses/agpl-3.0.html>.
 * A commercial license is available from Ilscipio GmbH.
 *
 * SPDX-License-Identifier: AGPL-3.0-only
 */
package com.ilscipio.scipio.manufacturing.service;

import com.ilscipio.scipio.service.def.*;

/**
 * Auto-generated annotation-based service definitions.
 *
 * <p>Generated from services.xml by convertXmlToAnnotation Gradle task.</p>
 *
 * <p>SCIPIO: 4.0.0: Auto-generated.</p>
 */
public class FormulaServices {

    @Service(
        name = "interfaceBomFormula",
        engine = "interface",
        attributes = {
            @Attribute(name = "arguments", type = "java.util.Map", mode = "IN"),
            @Attribute(name = "quantity", type = "BigDecimal", mode = "OUT")
        }
    )
    public interface InterfaceBomFormula {}

    /**
     * Example bom formula
     */
    @Service(
        name = "exampleComponentFormula",
        engine = "java",
        location = "com.ilscipio.scipio.manufacturing.event.BomFormulas",
        invoke = "exampleComponentFormula",
        description = "Example bom formula",
        auth = "true",
        implemented = {@Implements(service = "interfaceBomFormula")}
    )
    public interface ExampleComponentFormula {}

    /**
     * Formula that computes the quantity of linear component in bom
     */
    @Service(
        name = "linearComponentFormula",
        engine = "java",
        location = "com.ilscipio.scipio.manufacturing.event.BomFormulas",
        invoke = "linearComponentConsumptionFormula",
        description = "Formula that computes the quantity of linear component in bom",
        auth = "true",
        implemented = {@Implements(service = "interfaceBomFormula")}
    )
    public interface LinearComponentFormula {}

    @Service(
        name = "interfaceTaskFormula",
        engine = "interface",
        attributes = {
            @Attribute(name = "arguments", type = "java.util.Map", mode = "IN"),
            @Attribute(name = "totalTime", type = "BigDecimal", mode = "OUT")
        }
    )
    public interface InterfaceTaskFormula {}

    /**
     * Formula that computes the estimated manufacturing time of a given task
     */
    @Service(
        name = "exampleTaskFormula",
        engine = "java",
        location = "com.ilscipio.scipio.manufacturing.event.TaskFormulae",
        invoke = "exampleTaskFormula",
        description = "Formula that computes the estimated manufacturing time of a given task",
        implemented = {@Implements(service = "interfaceTaskFormula")}
    )
    public interface ExampleTaskFormula {}

}
