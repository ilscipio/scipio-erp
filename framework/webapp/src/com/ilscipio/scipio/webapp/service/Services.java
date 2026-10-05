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
package com.ilscipio.scipio.webapp.service;

import com.ilscipio.scipio.service.def.*;
import com.ilscipio.scipio.service.def.Service.GroupInvoke;

/**
 * Auto-generated annotation-based service definitions.
 *
 * <p>Generated from services.xml by convertXmlToAnnotation Gradle task.</p>
 *
 * <p>SCIPIO: 4.0.0: Auto-generated.</p>
 */
public class Services {

    /**
     * SCIPIO: Expires all Visits with fromDate older than a certain cutoff date by setting their thruDate (added 2018-02-15)
     */
    @Service(
        name = "expireOldVisits",
        location = "org.ofbiz.webapp.WebAppServices",
        invoke = "expireOldVisits",
        description = "SCIPIO: Expires all Visits with fromDate older than a certain cutoff date by setting their thruDate (added 2018-02-15)",
        auth = "true",
        useTransaction = "false",
        semaphore = "wait",
        attributes = {
            @Attribute(name = "daysOld", type = "Integer", mode = "IN", optional = "true", description = "Age of Visits to consider expired, substracted from current time.\n                Default: value of serverstats.properties#stats.expire.visit.daysOld"),
            @Attribute(name = "olderThan", type = "Timestamp", mode = "IN", optional = "true", description = "Specific value to use as date cutoff, instead of daysOld."),
            @Attribute(name = "singleDbOp", type = "Boolean", mode = "IN", optional = "true", defaultValue = "true", description = "If true, try to run all expires in a single database statement;\n                if false, process row-by-row. Default: true (NOTE: service-specific default)"),
            @Attribute(name = "dryRun", type = "Boolean", mode = "IN", optional = "true", defaultValue = "false", description = "If true, don't perform actual updates.")
        },
        permissions = {@Permissions(joinType = "AND", permissions = {@Permission(permission = "SERVICE_INVOKE_ANY")})}
    )
    public interface ExpireOldVisits {}

    /**
     * SCIPIO: Deletes all Visits with fromDate older than a certain cutoff date (added 2018-02-15)
     */
    @Service(
        name = "purgeOldVisits",
        location = "org.ofbiz.webapp.WebAppServices",
        invoke = "purgeOldVisits",
        description = "SCIPIO: Deletes all Visits with fromDate older than a certain cutoff date (added 2018-02-15)",
        auth = "true",
        useTransaction = "false",
        semaphore = "wait",
        attributes = {
            @Attribute(name = "daysOld", type = "Integer", mode = "IN", optional = "true", description = "Age of Visits to consider removable, substracted from current time.\n                Default: value of serverstats.properties#stats.purge.visit.daysOld"),
            @Attribute(name = "olderThan", type = "Timestamp", mode = "IN", optional = "true", description = "Specific value to use as date cutoff, instead of daysOld."),
            @Attribute(name = "singleDbOp", type = "Boolean", mode = "IN", optional = "true", defaultValue = "false", description = "If true, try to run all purges in a single database statement (WARN: failure likely for this service);\n                if false, process row-by-row. Default: false (NOTE: service-specific default)"),
            @Attribute(name = "dryRun", type = "Boolean", mode = "IN", optional = "true", defaultValue = "false", description = "If true, don't perform actual updates.")
        },
        permissions = {@Permissions(joinType = "AND", permissions = {@Permission(permission = "SERVICE_INVOKE_ANY")})}
    )
    public interface PurgeOldVisits {}

}
