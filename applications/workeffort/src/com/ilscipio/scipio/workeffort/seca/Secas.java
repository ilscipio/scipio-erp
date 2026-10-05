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
package com.ilscipio.scipio.workeffort.seca;

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
     * SECA for service createWorkEffort on event commit.
     */
    @Seca(
        service = "createWorkEffort",
        event = "commit",
        condition = "!empty(quickAssignPartyId)",
        actions = {
            @SecaAction(
                service = "quickAssignPartyToWorkEffort",
                mode = "sync"
            )
        }
    )
    public interface CreateWorkEffortcommitSeca1 {}

    /**
     * SECA for service createWorkEffort on event commit.
     */
    @Seca(
        service = "createWorkEffort",
        event = "commit",
        condition = "!empty(communicationEventId) && empty(custRequestId)",
        actions = {
            @SecaAction(
                service = "makeCommunicationEventWorkEffort",
                mode = "sync"
            )
        }
    )
    public interface CreateWorkEffortcommitSeca2 {}

    /**
     * SECA for service updateWorkEffort on event commit.
     */
    @Seca(
        service = "updateWorkEffort",
        event = "commit",
        condition = "!empty(communicationEventId)",
        actions = {
            @SecaAction(
                service = "makeCommunicationEventWorkEffort",
                mode = "sync"
            )
        }
    )
    public interface UpdateWorkEffortcommitSeca3 {}

    /**
     * SECA for service createCommunicationEventWorkEff on event invoke.
     */
    @Seca(
        service = "createCommunicationEventWorkEff",
        event = "invoke",
        condition = "empty(communicationEventId)",
        actions = {
            @SecaAction(
                service = "createCommunicationEvent",
                mode = "sync"
            )
        }
    )
    public interface CreateCommunicationEventWorkEffinvokeSeca4 {}

    /**
     * SECA for service createWorkEffortRequest on event invoke.
     */
    @Seca(
        service = "createWorkEffortRequest",
        event = "invoke",
        condition = "empty(custRequestId)",
        actions = {
            @SecaAction(
                service = "createCustRequest",
                mode = "sync"
            )
        }
    )
    public interface CreateWorkEffortRequestinvokeSeca5 {}

    /**
     * SECA for service createWorkEffortRequestItem on event invoke.
     */
    @Seca(
        service = "createWorkEffortRequestItem",
        event = "invoke",
        condition = "empty(custRequestItemExists)",
        actions = {
            @SecaAction(
                service = "createCustRequestItem",
                mode = "sync"
            )
        }
    )
    public interface CreateWorkEffortRequestIteminvokeSeca6 {}

    /**
     * SECA for service createWorkEffortQuote on event invoke.
     */
    @Seca(
        service = "createWorkEffortQuote",
        event = "invoke",
        condition = "empty(quoteId)",
        actions = {
            @SecaAction(
                service = "createQuote",
                mode = "sync"
            )
        }
    )
    public interface CreateWorkEffortQuoteinvokeSeca7 {}

    /**
     * SECA for service createWorkRequirementFulfillment on event invoke.
     */
    @Seca(
        service = "createWorkRequirementFulfillment",
        event = "invoke",
        condition = "empty(requirementId)",
        actions = {
            @SecaAction(
                service = "createRequirement",
                mode = "sync"
            )
        }
    )
    public interface CreateWorkRequirementFulfillmentinvokeSeca8 {}

    /**
     * SECA for service createShoppingListWorkEffort on event invoke.
     */
    @Seca(
        service = "createShoppingListWorkEffort",
        event = "invoke",
        condition = "empty(shoppingListId)",
        actions = {
            @SecaAction(
                service = "createShoppingList",
                mode = "sync"
            )
        }
    )
    public interface CreateShoppingListWorkEffortinvokeSeca9 {}

    /**
     * SECA for service createOrderHeaderWorkEffort on event invoke.
     */
    @Seca(
        service = "createOrderHeaderWorkEffort",
        event = "invoke",
        condition = "empty(orderId)",
        actions = {
            @SecaAction(
                service = "createOrderHeader",
                mode = "sync"
            )
        }
    )
    public interface CreateOrderHeaderWorkEffortinvokeSeca10 {}

}
