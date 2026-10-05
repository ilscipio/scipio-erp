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
package com.ilscipio.scipio.common.mca;

import com.ilscipio.scipio.service.def.mca.*;

/**
 * Mail condition action rules for the common component.
 *
 * <p>Generated from XML by the convertXmlToAnnotation Gradle task.</p>
 *
 * <p>SCIPIO: 4.0.0: Auto-generated.</p>
 */
public class TestMcas {

    /**
     * Mail condition action rule testRule1.
     */
    @Mca(
        name = "testRule1",
        conditions = {@McaCondition(fieldName = "to", operator = "matches", value = ".*@ofbiz\\.org"), @McaCondition(fieldName = "subject", operator = "matches", value = ".*Test.*")},
        actions = {@McaAction(service = "testMca")}
    )
    public interface TestRule1 {}

}
