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
package com.ilscipio.scipio.manufacturing.seca;

import com.ilscipio.scipio.service.def.seca.*;

/**
 * Auto-generated annotation-based service ECA definitions.
 *
 * <p>Generated from secas.xml by convertXmlToAnnotation Gradle task.</p>
 *
 * <p>SCIPIO: 4.0.0: Auto-generated.</p>
 */
public class Secas {

    /**
     * SECA for service updateRequirement on event commit.
     */
    @Seca(
        service = "updateRequirement",
        event = "commit",
        condition = "requirementTypeId == 'INTERNAL_REQUIREMENT' && statusId == 'REQ_APPROVED' && oldStatusId != 'REQ_APPROVED'",
        actions = {
            @SecaAction(
                service = "createProductionRunFromRequirement",
                mode = "sync"
            )
        }
    )
    public interface UpdateRequirementcommitSeca1 {}

    /**
     * SECA for service createRequirement on event commit.
     */
    @Seca(
        service = "createRequirement",
        event = "commit",
        condition = "requirementTypeId == 'INTERNAL_REQUIREMENT' && statusId == 'REQ_APPROVED'",
        actions = {
            @SecaAction(
                service = "createProductionRunFromRequirement",
                mode = "sync"
            )
        }
    )
    public interface CreateRequirementcommitSeca2 {}

    /**
     * SECA for service createBOMAssoc on event commit.
     */
    @Seca(
        service = "createBOMAssoc",
        event = "commit",
        condition = "productAssocTypeId == 'MANUF_COMPONENT'",
        actions = {
            @SecaAction(
                service = "updateLowLevelCode",
                mode = "sync"
            )
        }
    )
    public interface CreateBOMAssoccommitSeca3 {}

    /**
     * SECA for service deleteProductAssoc on event commit.
     */
    @Seca(
        service = "deleteProductAssoc",
        event = "commit",
        condition = "productAssocTypeId == 'MANUF_COMPONENT'",
        actions = {
            @SecaAction(
                service = "updateLowLevelCode",
                mode = "sync"
            )
        }
    )
    public interface DeleteProductAssoccommitSeca4 {}

    /**
     * SCIPIO: lot reservation upkeep: when issueInventoryItemToWorkEffort issues material from an inventory
     * item that carries an open ProductionRunLotReservation for the task, give the issued quantity back to
     * available-to-promise and lower the reservation by it, so the ATP is not taken down twice (once by the
     * reservation, once by the issuance). A no-op for any issuance that is not against a reserved lot.
     */
    @Seca(
        service = "issueInventoryItemToWorkEffort",
        event = "commit",
        actions = {
            @SecaAction(
                service = "adjustProductionRunLotReservation",
                mode = "sync"
            )
        }
    )
    public interface IssueInventoryItemToWorkEffortcommitSeca5 {}

    /**
     * SCIPIO: a production run's lot reservations are held for its tasks only while the run is open; once the
     * run is closed (changeProductionRunStatus sets newStatusId to PRUN_CLOSED), release every open reservation
     * of the run's tasks back to available-to-promise.
     */
    @Seca(
        service = "changeProductionRunStatus",
        event = "commit",
        condition = "newStatusId == 'PRUN_CLOSED'",
        actions = {
            @SecaAction(
                service = "releaseProductionRunReservations",
                mode = "sync"
            )
        }
    )
    public interface ChangeProductionRunStatuscommitSeca6 {}

}
