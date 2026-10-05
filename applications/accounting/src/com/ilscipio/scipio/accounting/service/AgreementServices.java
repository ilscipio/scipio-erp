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
package com.ilscipio.scipio.accounting.service;

import com.ilscipio.scipio.service.def.*;
import com.ilscipio.scipio.service.def.Service.GroupInvoke;

/**
 * Auto-generated annotation-based service definitions.
 *
 * <p>Generated from services.xml by convertXmlToAnnotation Gradle task.</p>
 *
 * <p>SCIPIO: 4.0.0: Auto-generated.</p>
 */
public class AgreementServices {

    /**
     * Create an Agreement
     */
    @Service(
        name = "createAgreement",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/agreement/AgreementServices.xml",
        invoke = "createAgreement",
        description = "Create an Agreement",
        defaultEntityName = "Agreement",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true"),
            @EntityAttributes(mode = "OUT", include = "pk", optional = "true")
        },
        permissionService = @PermissionService(service = "acctgAgreementPermissionCheck", mainAction = "CREATE"),
        overrideAttributes = {
            @OverrideAttribute(name = "textData", allowHtml = "any")
        }
    )
    public interface CreateAgreement {}

    /**
     * Update an Agreement
     */
    @Service(
        name = "updateAgreement",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/agreement/AgreementServices.xml",
        invoke = "updateAgreement",
        description = "Update an Agreement",
        defaultEntityName = "Agreement",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "acctgAgreementPermissionCheck", mainAction = "UPDATE"),
        overrideAttributes = {
            @OverrideAttribute(name = "textData", allowHtml = "any")
        }
    )
    public interface UpdateAgreement {}

    /**
     * Cancel an Agreement (SCIPIO: 2018-09-10: Now simply a wrapper around expireAgreement)
     */
    @Service(
        name = "cancelAgreement",
        engine = "group",
        description = "Cancel an Agreement (SCIPIO: 2018-09-10: Now simply a wrapper around expireAgreement)",
        invokes = {@GroupInvoke(name = "expireAgreement", resultToContext = "false")}
    )
    public interface CancelAgreement {}

    /**
     * Expire an Agreement
     */
    @Service(
        name = "expireAgreement",
        engine = "entity-auto",
        invoke = "expire",
        description = "Expire an Agreement",
        defaultEntityName = "Agreement",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        },
        permissionService = @PermissionService(service = "acctgAgreementPermissionCheck", mainAction = "UPDATE")
    )
    public interface ExpireAgreement {}

    /**
     * Copy an Agreement
     */
    @Service(
        name = "copyAgreement",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/agreement/AgreementServices.xml",
        invoke = "copyAgreement",
        description = "Copy an Agreement",
        defaultEntityName = "Agreement",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "INOUT", include = "pk")
        },
        attributes = {
            @Attribute(name = "copyAgreementTerms", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "copyAgreementProducts", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "copyAgreementParties", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "copyAgreementFacilities", type = "String", mode = "IN", optional = "true")
        },
        permissionService = @PermissionService(service = "acctgAgreementPermissionCheck", mainAction = "CREATE")
    )
    public interface CopyAgreement {}

    /**
     * Create an AgreementItem
     */
    @Service(
        name = "createAgreementItem",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/agreement/AgreementServices.xml",
        invoke = "createAgreementItem",
        description = "Create an AgreementItem",
        defaultEntityName = "AgreementItem",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "INOUT", include = "pk", optional = "true"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "acctgAgreementPermissionCheck", mainAction = "CREATE"),
        overrideAttributes = {
            @OverrideAttribute(name = "agreementText", allowHtml = "any")
        }
    )
    public interface CreateAgreementItem {}

    /**
     * Update an AgreementItem
     */
    @Service(
        name = "updateAgreementItem",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/agreement/AgreementServices.xml",
        invoke = "updateAgreementItem",
        description = "Update an AgreementItem",
        defaultEntityName = "AgreementItem",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "acctgAgreementPermissionCheck", mainAction = "UPDATE"),
        overrideAttributes = {
            @OverrideAttribute(name = "agreementText", allowHtml = "any")
        }
    )
    public interface UpdateAgreementItem {}

    /**
     * Remove an AgreementItem
     */
    @Service(
        name = "removeAgreementItem",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/agreement/AgreementServices.xml",
        invoke = "removeAgreementItem",
        description = "Remove an AgreementItem",
        defaultEntityName = "AgreementItem",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        },
        permissionService = @PermissionService(service = "acctgAgreementPermissionCheck", mainAction = "DELETE")
    )
    public interface RemoveAgreementItem {}

    /**
     * Create an AgreementItemAttribute
     */
    @Service(
        name = "createAgreementItemAttribute",
        engine = "entity-auto",
        invoke = "create",
        description = "Create an AgreementItemAttribute",
        defaultEntityName = "AgreementItemAttribute",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "acctgAgreementPermissionCheck", mainAction = "CREATE")
    )
    public interface CreateAgreementItemAttribute {}

    /**
     * Update an AgreementItemAttribute
     */
    @Service(
        name = "updateAgreementItemAttribute",
        engine = "entity-auto",
        invoke = "update",
        description = "Update an AgreementItemAttribute",
        defaultEntityName = "AgreementItemAttribute",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "acctgAgreementPermissionCheck", mainAction = "UPDATE")
    )
    public interface UpdateAgreementItemAttribute {}

    /**
     * Delete an AgreementItemAttribute
     */
    @Service(
        name = "deleteAgreementItemAttribute",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete an AgreementItemAttribute",
        defaultEntityName = "AgreementItemAttribute",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        },
        permissionService = @PermissionService(service = "acctgAgreementPermissionCheck", mainAction = "DELETE")
    )
    public interface DeleteAgreementItemAttribute {}

    /**
     * Create an AgreementTerm
     */
    @Service(
        name = "createAgreementTerm",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/agreement/AgreementServices.xml",
        invoke = "createAgreementTerm",
        description = "Create an AgreementTerm",
        defaultEntityName = "AgreementTerm",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "INOUT", include = "pk", optional = "true"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "acctgAgreementPermissionCheck", mainAction = "CREATE"),
        overrideAttributes = {
            @OverrideAttribute(name = "textValue", allowHtml = "any")
        }
    )
    public interface CreateAgreementTerm {}

    /**
     * Update an AgreementTerm
     */
    @Service(
        name = "updateAgreementTerm",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/agreement/AgreementServices.xml",
        invoke = "updateAgreementTerm",
        description = "Update an AgreementTerm",
        defaultEntityName = "AgreementTerm",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "acctgAgreementPermissionCheck", mainAction = "UPDATE"),
        overrideAttributes = {
            @OverrideAttribute(name = "textValue", allowHtml = "any")
        }
    )
    public interface UpdateAgreementTerm {}

    /**
     * Delete an AgreementTerm
     */
    @Service(
        name = "deleteAgreementTerm",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/agreement/AgreementServices.xml",
        invoke = "deleteAgreementTerm",
        description = "Delete an AgreementTerm",
        defaultEntityName = "AgreementTerm",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "acctgAgreementPermissionCheck", mainAction = "DELETE")
    )
    public interface DeleteAgreementTerm {}

    /**
     * Create an AgreementPromoAppl
     */
    @Service(
        name = "createAgreementPromoAppl",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/agreement/AgreementServices.xml",
        invoke = "createAgreementPromoAppl",
        description = "Create an AgreementPromoAppl",
        defaultEntityName = "AgreementPromoAppl",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "acctgAgreementPermissionCheck", mainAction = "CREATE")
    )
    public interface CreateAgreementPromoAppl {}

    /**
     * Update an AgreementPromoAppl
     */
    @Service(
        name = "updateAgreementPromoAppl",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/agreement/AgreementServices.xml",
        invoke = "updateAgreementPromoAppl",
        description = "Update an AgreementPromoAppl",
        defaultEntityName = "AgreementPromoAppl",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "acctgAgreementPermissionCheck", mainAction = "UPDATE")
    )
    public interface UpdateAgreementPromoAppl {}

    /**
     * Remove an AgreementPromoAppl
     */
    @Service(
        name = "removeAgreementPromoAppl",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/agreement/AgreementServices.xml",
        invoke = "removeAgreementPromoAppl",
        description = "Remove an AgreementPromoAppl",
        defaultEntityName = "AgreementPromoAppl",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "acctgAgreementPermissionCheck", mainAction = "DELETE")
    )
    public interface RemoveAgreementPromoAppl {}

    /**
     * Create an AgreementProductAppl
     */
    @Service(
        name = "createAgreementProductAppl",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/agreement/AgreementServices.xml",
        invoke = "createAgreementProductAppl",
        description = "Create an AgreementProductAppl",
        defaultEntityName = "AgreementProductAppl",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "acctgAgreementPermissionCheck", mainAction = "CREATE")
    )
    public interface CreateAgreementProductAppl {}

    /**
     * Update an AgreementProductAppl
     */
    @Service(
        name = "updateAgreementProductAppl",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/agreement/AgreementServices.xml",
        invoke = "updateAgreementProductAppl",
        description = "Update an AgreementProductAppl",
        defaultEntityName = "AgreementProductAppl",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "acctgAgreementPermissionCheck", mainAction = "UPDATE")
    )
    public interface UpdateAgreementProductAppl {}

    /**
     * Remove an AgreementProductAppl
     */
    @Service(
        name = "removeAgreementProductAppl",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/agreement/AgreementServices.xml",
        invoke = "removeAgreementProductAppl",
        description = "Remove an AgreementProductAppl",
        defaultEntityName = "AgreementProductAppl",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "acctgAgreementPermissionCheck", mainAction = "DELETE")
    )
    public interface RemoveAgreementProductAppl {}

    /**
     * Create an AgreementFacilityAppl
     */
    @Service(
        name = "createAgreementFacilityAppl",
        engine = "entity-auto",
        invoke = "create",
        description = "Create an AgreementFacilityAppl",
        defaultEntityName = "AgreementFacilityAppl",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "acctgAgreementPermissionCheck", mainAction = "CREATE")
    )
    public interface CreateAgreementFacilityAppl {}

    /**
     * Update an AgreementFacilityAppl
     */
    @Service(
        name = "updateAgreementFacilityAppl",
        engine = "entity-auto",
        invoke = "update",
        description = "Update an AgreementFacilityAppl",
        defaultEntityName = "AgreementFacilityAppl",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "acctgAgreementPermissionCheck", mainAction = "UPDATE")
    )
    public interface UpdateAgreementFacilityAppl {}

    /**
     * Remove an AgreementFacilityAppl
     */
    @Service(
        name = "removeAgreementFacilityAppl",
        engine = "entity-auto",
        invoke = "delete",
        description = "Remove an AgreementFacilityAppl",
        defaultEntityName = "AgreementFacilityAppl",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "acctgAgreementPermissionCheck", mainAction = "DELETE")
    )
    public interface RemoveAgreementFacilityAppl {}

    /**
     * Create an AgreementPartyApplic
     */
    @Service(
        name = "createAgreementPartyApplic",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/agreement/AgreementServices.xml",
        invoke = "createAgreementPartyApplic",
        description = "Create an AgreementPartyApplic",
        defaultEntityName = "AgreementPartyApplic",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "acctgAgreementPermissionCheck", mainAction = "CREATE")
    )
    public interface CreateAgreementPartyApplic {}

    /**
     * Update an AgreementPartyApplic
     */
    @Service(
        name = "updateAgreementPartyApplic",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/agreement/AgreementServices.xml",
        invoke = "updateAgreementPartyApplic",
        description = "Update an AgreementPartyApplic",
        defaultEntityName = "AgreementPartyApplic",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "acctgAgreementPermissionCheck", mainAction = "UPDATE")
    )
    public interface UpdateAgreementPartyApplic {}

    /**
     * Remove an AgreementPartyApplic
     */
    @Service(
        name = "removeAgreementPartyApplic",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/agreement/AgreementServices.xml",
        invoke = "removeAgreementPartyApplic",
        description = "Remove an AgreementPartyApplic",
        defaultEntityName = "AgreementPartyApplic",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "acctgAgreementPermissionCheck", mainAction = "DELETE")
    )
    public interface RemoveAgreementPartyApplic {}

    /**
     * Create an AgreementGeographicalApplic
     */
    @Service(
        name = "createAgreementGeographicalApplic",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/agreement/AgreementServices.xml",
        invoke = "createAgreementGeographicalApplic",
        description = "Create an AgreementGeographicalApplic",
        defaultEntityName = "AgreementGeographicalApplic",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "acctgAgreementPermissionCheck", mainAction = "CREATE")
    )
    public interface CreateAgreementGeographicalApplic {}

    /**
     * Update an AgreementGeographicalApplic
     */
    @Service(
        name = "updateAgreementGeographicalApplic",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/agreement/AgreementServices.xml",
        invoke = "updateAgreementGeographicalApplic",
        description = "Update an AgreementGeographicalApplic",
        defaultEntityName = "AgreementGeographicalApplic",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "acctgAgreementPermissionCheck", mainAction = "UPDATE")
    )
    public interface UpdateAgreementGeographicalApplic {}

    /**
     * Remove an AgreementGeographicalApplic
     */
    @Service(
        name = "removeAgreementGeographicalApplic",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/agreement/AgreementServices.xml",
        invoke = "removeAgreementGeographicalApplic",
        description = "Remove an AgreementGeographicalApplic",
        defaultEntityName = "AgreementGeographicalApplic",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "acctgAgreementPermissionCheck", mainAction = "DELETE")
    )
    public interface RemoveAgreementGeographicalApplic {}

    /**
     * Create an Agreement Role
     */
    @Service(
        name = "createAgreementRole",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/agreement/AgreementServices.xml",
        invoke = "createAgreementRole",
        description = "Create an Agreement Role",
        defaultEntityName = "AgreementRole",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface CreateAgreementRole {}

    /**
     * Update an Agreement Role
     */
    @Service(
        name = "updateAgreementRole",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/agreement/AgreementServices.xml",
        invoke = "updateAgreementRole",
        description = "Update an Agreement Role",
        defaultEntityName = "AgreementRole",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateAgreementRole {}

    /**
     * Delete an Agreement Role
     */
    @Service(
        name = "deleteAgreementRole",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/agreement/AgreementServices.xml",
        invoke = "deleteAgreementRole",
        description = "Delete an Agreement Role",
        defaultEntityName = "AgreementRole",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteAgreementRole {}

    /**
     * Create a AgreementType record
     */
    @Service(
        name = "createAgreementType",
        engine = "entity-auto",
        invoke = "create",
        description = "Create a AgreementType record",
        defaultEntityName = "AgreementType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "INOUT", include = "pk", optional = "true"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface CreateAgreementType {}

    /**
     * Update a AgreementType record
     */
    @Service(
        name = "updateAgreementType",
        engine = "entity-auto",
        invoke = "update",
        description = "Update a AgreementType record",
        defaultEntityName = "AgreementType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateAgreementType {}

    /**
     * Remove a AgreementType record
     */
    @Service(
        name = "removeAgreementType",
        engine = "entity-auto",
        invoke = "delete",
        description = "Remove a AgreementType record",
        defaultEntityName = "AgreementType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface RemoveAgreementType {}

    /**
     * Create Agreement Content
     */
    @Service(
        name = "createAgreementContent",
        engine = "entity-auto",
        invoke = "create",
        description = "Create Agreement Content",
        defaultEntityName = "AgreementContent",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        overrideAttributes = {
            @OverrideAttribute(name = "fromDate", optional = "true")
        }
    )
    public interface CreateAgreementContent {}

    /**
     * Update Agreement Content
     */
    @Service(
        name = "updateAgreementContent",
        engine = "entity-auto",
        invoke = "update",
        description = "Update Agreement Content",
        defaultEntityName = "AgreementContent",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateAgreementContent {}

    /**
     * Remove Content From Agreement
     */
    @Service(
        name = "removeAgreementContent",
        engine = "entity-auto",
        invoke = "delete",
        description = "Remove Content From Agreement",
        defaultEntityName = "AgreementContent",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface RemoveAgreementContent {}

    /**
     * Upload and attach a file to an agreement
     */
    @Service(
        name = "uploadAgreementContentFile",
        engine = "group",
        description = "Upload and attach a file to an agreement",
        transactionTimeout = "300",
        invokes = {@GroupInvoke(name = "createContentFromUploadedFile", resultToContext = "true"), @GroupInvoke(name = "createAgreementContent", resultToContext = "false")}
    )
    public interface UploadAgreementContentFile {}

    /**
     * Get commission receiving parties and amounts for a product. <br/>             amount input is for the entire quantity. <br/><br/>             Returns a List of Maps each containing <br/>             partyIdFrom     String  commission paying party <br/>             partyIdTo       String  commission receiving party <br/>             commission      BigDecimal  Commission <br/>             days            Long    term days <br/>             currencyUomId   String  Currency <br/>             productId       String  Product Id <br/>             Will use the virtual product if no agreement is found for a variant product.  If no quantity is specified, defaults to one (1).         
     */
    @Service(
        name = "getCommissionForProduct",
        engine = "java",
        location = "org.ofbiz.accounting.agreement.AgreementServices",
        invoke = "getCommissionForProduct",
        description = "Get commission receiving parties and amounts for a product. <br/>\n            amount input is for the entire quantity. <br/><br/>\n            Returns a List of Maps each containing <br/>\n            partyIdFrom     String  commission paying party <br/>\n            partyIdTo       String  commission receiving party <br/>\n            commission      BigDecimal  Commission <br/>\n            days            Long    term days <br/>\n            currencyUomId   String  Currency <br/>\n            productId       String  Product Id <br/>\n            Will use the virtual product if no agreement is found for a variant product.  If no quantity is specified, defaults to one (1).\n        ",
        auth = "true",
        attributes = {
            @Attribute(name = "productId", type = "String", mode = "IN"),
            @Attribute(name = "invoiceId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "invoiceItemSeqId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "invoiceItemTypeId", type = "String", mode = "IN"),
            @Attribute(name = "amount", type = "BigDecimal", mode = "IN"),
            @Attribute(name = "quantity", type = "BigDecimal", mode = "IN", optional = "true"),
            @Attribute(name = "commissions", type = "List", mode = "OUT")
        },
        permissionService = @PermissionService(service = "acctgCommissionPermissionCheck", mainAction = "VIEW")
    )
    public interface GetCommissionForProduct {}

    /**
     *  Create AgreementWorkEffortApplic 
     */
    @Service(
        name = "createAgreementWorkEffortApplic",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/agreement/AgreementServices.xml",
        invoke = "createAgreementWorkEffortApplic",
        description = " Create AgreementWorkEffortApplic ",
        defaultEntityName = "AgreementWorkEffortApplic",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        },
        overrideAttributes = {
            @OverrideAttribute(name = "agreementItemSeqId", defaultValue = "_NA_")
        }
    )
    public interface CreateAgreementWorkEffortApplic {}

    /**
     * Delete AgreementWorkEffortApplic
     */
    @Service(
        name = "deleteAgreementWorkEffortApplic",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/agreement/AgreementServices.xml",
        invoke = "deleteAgreementWorkEffortApplic",
        description = "Delete AgreementWorkEffortApplic",
        defaultEntityName = "AgreementWorkEffortApplic",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteAgreementWorkEffortApplic {}

    /**
     * Create an AgreementTypeAttr
     */
    @Service(
        name = "createAgreementTypeAttr",
        engine = "entity-auto",
        invoke = "create",
        description = "Create an AgreementTypeAttr",
        defaultEntityName = "AgreementTypeAttr",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true"),
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface CreateAgreementTypeAttr {}

    /**
     * Update an AgreementTypeAttr
     */
    @Service(
        name = "updateAgreementTypeAttr",
        engine = "entity-auto",
        invoke = "update",
        description = "Update an AgreementTypeAttr",
        defaultEntityName = "AgreementTypeAttr",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateAgreementTypeAttr {}

    /**
     * Remove an AgreementTypeAttr
     */
    @Service(
        name = "removeAgreementTypeAttr",
        engine = "entity-auto",
        invoke = "delete",
        description = "Remove an AgreementTypeAttr",
        defaultEntityName = "AgreementTypeAttr",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface RemoveAgreementTypeAttr {}

}
