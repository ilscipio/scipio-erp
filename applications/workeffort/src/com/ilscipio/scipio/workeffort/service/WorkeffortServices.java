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

/**
 * Auto-generated annotation-based service definitions.
 *
 * <p>Generated from services.xml by convertXmlToAnnotation Gradle task.</p>
 *
 * <p>SCIPIO: 4.0.0: Auto-generated.</p>
 */
public class WorkeffortServices {

    /**
     * Create a Deliverable record
     */
    @Service(
        name = "createDeliverable",
        engine = "entity-auto",
        invoke = "create",
        description = "Create a Deliverable record",
        defaultEntityName = "Deliverable",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "INOUT", include = "pk", optional = "true"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface CreateDeliverable {}

    /**
     * Update a Deliverable record
     */
    @Service(
        name = "updateDeliverable",
        engine = "entity-auto",
        invoke = "update",
        description = "Update a Deliverable record",
        defaultEntityName = "Deliverable",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateDeliverable {}

    /**
     * Delete a Deliverable record
     */
    @Service(
        name = "deleteDeliverable",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete a Deliverable record",
        defaultEntityName = "Deliverable",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteDeliverable {}

    /**
     * Create a Deliverable Type record
     */
    @Service(
        name = "createDeliverableType",
        engine = "entity-auto",
        invoke = "create",
        description = "Create a Deliverable Type record",
        defaultEntityName = "DeliverableType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "INOUT", include = "pk", optional = "true"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface CreateDeliverableType {}

    /**
     * Update a Deliverable Type record
     */
    @Service(
        name = "updateDeliverableType",
        engine = "entity-auto",
        invoke = "update",
        description = "Update a Deliverable Type record",
        defaultEntityName = "DeliverableType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateDeliverableType {}

    /**
     * Delete a Deliverable Type record
     */
    @Service(
        name = "deleteDeliverableType",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete a Deliverable Type record",
        defaultEntityName = "DeliverableType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteDeliverableType {}

    /**
     * Create a WorkEffortAssocAttribute record
     */
    @Service(
        name = "createWorkEffortAssocAttribute",
        engine = "entity-auto",
        invoke = "create",
        description = "Create a WorkEffortAssocAttribute record",
        defaultEntityName = "WorkEffortAssocAttribute",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface CreateWorkEffortAssocAttribute {}

    /**
     * Update a WorkEffortAssocAttribute record
     */
    @Service(
        name = "updateWorkEffortAssocAttribute",
        engine = "entity-auto",
        invoke = "update",
        description = "Update a WorkEffortAssocAttribute record",
        defaultEntityName = "WorkEffortAssocAttribute",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateWorkEffortAssocAttribute {}

    /**
     * Delete a WorkEffortAssocAttribute record
     */
    @Service(
        name = "deleteWorkEffortAssocAttribute",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete a WorkEffortAssocAttribute record",
        defaultEntityName = "WorkEffortAssocAttribute",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteWorkEffortAssocAttribute {}

    /**
     * Create a WorkEffortAssocType record
     */
    @Service(
        name = "createWorkEffortAssocType",
        engine = "entity-auto",
        invoke = "create",
        description = "Create a WorkEffortAssocType record",
        defaultEntityName = "WorkEffortAssocType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "INOUT", include = "pk", optional = "true"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface CreateWorkEffortAssocType {}

    /**
     * Update a WorkEffortAssocType record
     */
    @Service(
        name = "updateWorkEffortAssocType",
        engine = "entity-auto",
        invoke = "update",
        description = "Update a WorkEffortAssocType record",
        defaultEntityName = "WorkEffortAssocType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateWorkEffortAssocType {}

    /**
     * Delete a WorkEffortAssocType record
     */
    @Service(
        name = "deleteWorkEffortAssocType",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete a WorkEffortAssocType record",
        defaultEntityName = "WorkEffortAssocType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteWorkEffortAssocType {}

    /**
     * Create a WorkEffortAssocTypeAttr record
     */
    @Service(
        name = "createWorkEffortAssocTypeAttr",
        engine = "entity-auto",
        invoke = "create",
        description = "Create a WorkEffortAssocTypeAttr record",
        defaultEntityName = "WorkEffortAssocTypeAttr",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface CreateWorkEffortAssocTypeAttr {}

    /**
     * Update a WorkEffortAssocTypeAttr record
     */
    @Service(
        name = "updateWorkEffortAssocTypeAttr",
        engine = "entity-auto",
        invoke = "update",
        description = "Update a WorkEffortAssocTypeAttr record",
        defaultEntityName = "WorkEffortAssocTypeAttr",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateWorkEffortAssocTypeAttr {}

    /**
     * Delete a WorkEffortAssocTypeAttr record
     */
    @Service(
        name = "deleteWorkEffortAssocTypeAttr",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete a WorkEffortAssocTypeAttr record",
        defaultEntityName = "WorkEffortAssocTypeAttr",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteWorkEffortAssocTypeAttr {}

    /**
     * Create a WorkEffortBilling record
     */
    @Service(
        name = "createWorkEffortBilling",
        engine = "entity-auto",
        invoke = "create",
        description = "Create a WorkEffortBilling record",
        defaultEntityName = "WorkEffortBilling",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface CreateWorkEffortBilling {}

    /**
     * Update a WorkEffortBilling record
     */
    @Service(
        name = "updateWorkEffortBilling",
        engine = "entity-auto",
        invoke = "update",
        description = "Update a WorkEffortBilling record",
        defaultEntityName = "WorkEffortBilling",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateWorkEffortBilling {}

    /**
     * Delete a WorkEffortBilling record
     */
    @Service(
        name = "deleteWorkEffortBilling",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete a WorkEffortBilling record",
        defaultEntityName = "WorkEffortBilling",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteWorkEffortBilling {}

    /**
     * Create a WorkEffortContentType record
     */
    @Service(
        name = "createWorkEffortContentType",
        engine = "entity-auto",
        invoke = "create",
        description = "Create a WorkEffortContentType record",
        defaultEntityName = "WorkEffortContentType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "INOUT", include = "pk", optional = "true"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface CreateWorkEffortContentType {}

    /**
     * Update a WorkEffortContentType record
     */
    @Service(
        name = "updateWorkEffortContentType",
        engine = "entity-auto",
        invoke = "update",
        description = "Update a WorkEffortContentType record",
        defaultEntityName = "WorkEffortContentType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateWorkEffortContentType {}

    /**
     * Delete a WorkEffortContentType record
     */
    @Service(
        name = "deleteWorkEffortContentType",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete a WorkEffortContentType record",
        defaultEntityName = "WorkEffortContentType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteWorkEffortContentType {}

    /**
     * Create a WorkEffortGoodStandardType record
     */
    @Service(
        name = "createWorkEffortGoodStandardType",
        engine = "entity-auto",
        invoke = "create",
        description = "Create a WorkEffortGoodStandardType record",
        defaultEntityName = "WorkEffortGoodStandardType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "INOUT", include = "pk", optional = "true"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface CreateWorkEffortGoodStandardType {}

    /**
     * Update a WorkEffortGoodStandardType record
     */
    @Service(
        name = "updateWorkEffortGoodStandardType",
        engine = "entity-auto",
        invoke = "update",
        description = "Update a WorkEffortGoodStandardType record",
        defaultEntityName = "WorkEffortGoodStandardType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateWorkEffortGoodStandardType {}

    /**
     * Delete a WorkEffortGoodStandardType record
     */
    @Service(
        name = "deleteWorkEffortGoodStandardType",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete a WorkEffortGoodStandardType record",
        defaultEntityName = "WorkEffortGoodStandardType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteWorkEffortGoodStandardType {}

    /**
     * Create a WorkEffortPurposeType record
     */
    @Service(
        name = "createWorkEffortPurposeType",
        engine = "entity-auto",
        invoke = "create",
        description = "Create a WorkEffortPurposeType record",
        defaultEntityName = "WorkEffortPurposeType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "INOUT", include = "pk", optional = "true"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface CreateWorkEffortPurposeType {}

    /**
     * Update a WorkEffortPurposeType record
     */
    @Service(
        name = "updateWorkEffortPurposeType",
        engine = "entity-auto",
        invoke = "update",
        description = "Update a WorkEffortPurposeType record",
        defaultEntityName = "WorkEffortPurposeType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateWorkEffortPurposeType {}

    /**
     * Delete a WorkEffortPurposeType record
     */
    @Service(
        name = "deleteWorkEffortPurposeType",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete a WorkEffortPurposeType record",
        defaultEntityName = "WorkEffortPurposeType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteWorkEffortPurposeType {}

    /**
     * Create a WorkEffortType record
     */
    @Service(
        name = "createWorkEffortType",
        engine = "entity-auto",
        invoke = "create",
        description = "Create a WorkEffortType record",
        defaultEntityName = "WorkEffortType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "INOUT", include = "pk", optional = "true"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface CreateWorkEffortType {}

    /**
     * Update a WorkEffortType record
     */
    @Service(
        name = "updateWorkEffortType",
        engine = "entity-auto",
        invoke = "update",
        description = "Update a WorkEffortType record",
        defaultEntityName = "WorkEffortType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateWorkEffortType {}

    /**
     * Delete a WorkEffortType record
     */
    @Service(
        name = "deleteWorkEffortType",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete a WorkEffortType record",
        defaultEntityName = "WorkEffortType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteWorkEffortType {}

    /**
     * Create a WorkEffortTypeAttr record
     */
    @Service(
        name = "createWorkEffortTypeAttr",
        engine = "entity-auto",
        invoke = "create",
        description = "Create a WorkEffortTypeAttr record",
        defaultEntityName = "WorkEffortTypeAttr",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface CreateWorkEffortTypeAttr {}

    /**
     * Update a WorkEffortTypeAttr record
     */
    @Service(
        name = "updateWorkEffortTypeAttr",
        engine = "entity-auto",
        invoke = "update",
        description = "Update a WorkEffortTypeAttr record",
        defaultEntityName = "WorkEffortTypeAttr",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateWorkEffortTypeAttr {}

    /**
     * Delete a WorkEffortTypeAttr record
     */
    @Service(
        name = "deleteWorkEffortTypeAttr",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete a WorkEffortTypeAttr record",
        defaultEntityName = "WorkEffortTypeAttr",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteWorkEffortTypeAttr {}

    /**
     * Create ApplicationSandbox record
     */
    @Service(
        name = "createApplicationSandbox",
        engine = "entity-auto",
        invoke = "create",
        description = "Create ApplicationSandbox record",
        defaultEntityName = "ApplicationSandbox",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "INOUT", include = "pk", optional = "true"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface CreateApplicationSandbox {}

    /**
     * Update ApplicationSandbox record
     */
    @Service(
        name = "updateApplicationSandbox",
        engine = "entity-auto",
        invoke = "update",
        description = "Update ApplicationSandbox record",
        defaultEntityName = "ApplicationSandbox",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateApplicationSandbox {}

    /**
     * Delete ApplicationSandbox record
     */
    @Service(
        name = "deleteApplicationSandbox",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete ApplicationSandbox record",
        defaultEntityName = "ApplicationSandbox",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteApplicationSandbox {}

}
