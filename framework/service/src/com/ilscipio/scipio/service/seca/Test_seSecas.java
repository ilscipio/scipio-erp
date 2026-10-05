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
package com.ilscipio.scipio.service.seca;

import com.ilscipio.scipio.service.def.seca.*;

/**
 * Auto-generated annotation-based service ECA definitions.
 *
 * <p>Generated from secas.xml by convertXmlToAnnotation Gradle task.</p>
 *
 * <p>SCIPIO: 4.0.0: Auto-generated.</p>
 */
public class Test_seSecas {

    /**
     * SECA for service testServiceEcaGlobalEventExec on event global-commit.
     */
    @Seca(
        service = "testServiceEcaGlobalEventExec",
        event = "global-commit",
        actions = {
            @SecaAction(
                service = "testServiceEcaGlobalEventExecOnCommit",
                mode = "sync"
            )
        }
    )
    public interface TestServiceEcaGlobalEventExecglobalcommitSeca1 {}

    /**
     * SECA for service testServiceEcaGlobalEventExecToRollback on event global-rollback.
     */
    @Seca(
        service = "testServiceEcaGlobalEventExecToRollback",
        event = "global-rollback",
        actions = {
            @SecaAction(
                service = "testServiceEcaGlobalEventExecOnRollback",
                mode = "sync"
            )
        }
    )
    public interface TestServiceEcaGlobalEventExecToRollbackglobalrollbackSeca2 {}

    /**
     * SECA for service testServiceEcaGlobalEventExec on event return.
     */
    @Seca(
        service = "testServiceEcaGlobalEventExec",
        event = "return",
        assignments = {
            @SecaSet(fieldName = "duration", value = "5000", format = "long")
        },
        actions = {
            @SecaAction(
                service = "blockingTestScv",
                mode = "sync"
            )
        }
    )
    public interface TestServiceEcaGlobalEventExecreturnSeca3 {}

}
