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
package com.ilscipio.scipio.common.seca;

import com.ilscipio.scipio.service.def.seca.*;

/**
 * Auto-generated annotation-based service ECA definitions.
 *
 * <p>Generated from secas.xml by convertXmlToAnnotation Gradle task.</p>
 *
 * <p>SCIPIO: 4.0.0: Auto-generated.</p>
 */
public class TestSecas {

    /**
     * SECA for service testScv on event invoke.
     */
    @Seca(
        service = "testScv",
        event = "invoke",
        condition = "message == 'auto'",
        assignments = {
            @SecaSet(fieldName = "message", value = "set message to auto message")
        }
    )
    public interface TestScvinvokeSeca1 {}

    /**
     * SECA for service testScv on event commit.
     */
    @Seca(
        service = "testScv",
        event = "commit",
        condition = "message == '12345'",
        actions = {
            @SecaAction(
                service = "testBsh",
                mode = "sync"
            )
        }
    )
    public interface TestScvcommitSeca2 {}

    /**
     * SECA for service testCommit on event global-commit.
     */
    @Seca(
        service = "testCommit",
        event = "global-commit",
        actions = {
            @SecaAction(
                service = "testScv",
                mode = "sync"
            )
        }
    )
    public interface TestCommitglobalcommitSeca3 {}

    /**
     * SECA for service testRollback on event global-rollback.
     */
    @Seca(
        service = "testRollback",
        event = "global-rollback",
        actions = {
            @SecaAction(
                service = "testScv",
                mode = "sync"
            )
        }
    )
    public interface TestRollbackglobalrollbackSeca4 {}

}
