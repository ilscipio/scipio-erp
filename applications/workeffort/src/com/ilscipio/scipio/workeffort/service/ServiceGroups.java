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
package com.ilscipio.scipio.workeffort.service;

import com.ilscipio.scipio.service.def.*;
import com.ilscipio.scipio.service.def.Service.GroupInvoke;

/**
 * Auto-generated annotation-based service definitions.
 *
 * <p>Generated from services.xml by convertXmlToAnnotation Gradle task.</p>
 *
 * <p>SCIPIO: 4.0.0: Auto-generated.</p>
 */
public class ServiceGroups {

    @Service(
        name = "updateWorkEffortAndAssoc",
        engine = "group",
        invokes = {@GroupInvoke(name = "updateWorkEffort", resultToContext = "false"), @GroupInvoke(name = "updateWorkEffortAssoc", resultToContext = "false")}
    )
    public interface UpdateWorkEffortAndAssoc {}

    @Service(
        name = "createWorkEffortRequestItemAndRequestItem",
        engine = "group",
        invokes = {@GroupInvoke(name = "checkCustRequestItemExists", resultToContext = "true"), @GroupInvoke(name = "createWorkEffortRequestItem", resultToContext = "false")},
        overrideAttributes = {
            @OverrideAttribute(name = "custRequestId", optional = "false"),
            @OverrideAttribute(name = "custRequestItemSeqId", optional = "false"),
            @OverrideAttribute(name = "workEffortId", optional = "false")
        }
    )
    public interface CreateWorkEffortRequestItemAndRequestItem {}

}
