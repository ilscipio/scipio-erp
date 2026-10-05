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
package com.ilscipio.scipio.order.service;

import com.ilscipio.scipio.service.def.*;

/**
 * Auto-generated annotation-based service definitions.
 *
 * <p>Generated from services.xml by convertXmlToAnnotation Gradle task.</p>
 *
 * <p>SCIPIO: 4.0.0: Auto-generated.</p>
 */
public class QuoteServices {

    /**
     * Get the Next Quote ID According to Settings on the PartyAcctgPreference Entity for the given Party
     */
    @Service(
        name = "getNextQuoteId",
        engine = "simple",
        location = "component://order/script/org/ofbiz/order/quote/QuoteServices.xml",
        invoke = "getNextQuoteId",
        description = "Get the Next Quote ID According to Settings on the PartyAcctgPreference Entity for the given Party",
        implemented = {@Implements(service = "createQuote")},
        attributes = {
            @Attribute(name = "partyId", type = "String", mode = "IN"),
            @Attribute(name = "quoteId", type = "String", mode = "OUT")
        }
    )
    public interface GetNextQuoteId {}

    @Service(
        name = "quoteSequenceEnforced",
        engine = "simple",
        location = "component://order/script/org/ofbiz/order/quote/QuoteServices.xml",
        invoke = "quoteSequenceEnforced",
        implemented = {@Implements(service = "getNextQuoteId", optional = "true")},
        attributes = {
            @Attribute(name = "partyAcctgPreference", type = "org.ofbiz.entity.GenericValue", mode = "IN")
        },
        overrideAttributes = {
            @OverrideAttribute(name = "quoteId", type = "Long", mode = "OUT")
        }
    )
    public interface QuoteSequenceEnforced {}

    /**
     * Create an Quote
     */
    @Service(
        name = "createQuote",
        engine = "simple",
        location = "component://order/script/org/ofbiz/order/quote/QuoteServices.xml",
        invoke = "createQuote",
        description = "Create an Quote",
        defaultEntityName = "Quote",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true"),
            @EntityAttributes(mode = "OUT", include = "pk", optional = "true")
        }
    )
    public interface CreateQuote {}

    /**
     * Update a Quote
     */
    @Service(
        name = "updateQuote",
        engine = "simple",
        location = "component://order/script/org/ofbiz/order/quote/QuoteServices.xml",
        invoke = "updateQuote",
        description = "Update a Quote",
        defaultEntityName = "Quote",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateQuote {}

    /**
     * Copy a Quote
     */
    @Service(
        name = "copyQuote",
        engine = "simple",
        location = "component://order/script/org/ofbiz/order/quote/QuoteServices.xml",
        invoke = "copyQuote",
        description = "Copy a Quote",
        defaultEntityName = "Quote",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "INOUT", include = "pk")
        },
        attributes = {
            @Attribute(name = "copyQuoteRoles", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "copyQuoteAttributes", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "copyQuoteCoefficients", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "copyQuoteItems", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "copyQuoteAdjustments", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "copyQuoteTerms", type = "String", mode = "IN", optional = "true")
        }
    )
    public interface CopyQuote {}

    /**
     * Set the Quote status to ordered.
     */
    @Service(
        name = "checkUpdateQuoteStatus",
        engine = "simple",
        location = "component://order/script/org/ofbiz/order/quote/QuoteServices.xml",
        invoke = "checkUpdateQuoteStatus",
        description = "Set the Quote status to ordered.",
        defaultEntityName = "Quote",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface CheckUpdateQuoteStatus {}

    /**
     * Create a QuoteRole
     */
    @Service(
        name = "createQuoteRole",
        engine = "simple",
        location = "component://order/script/org/ofbiz/order/quote/QuoteServices.xml",
        invoke = "createQuoteRole",
        description = "Create a QuoteRole",
        defaultEntityName = "QuoteRole",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface CreateQuoteRole {}

    /**
     * Update a QuoteRole
     */
    @Service(
        name = "updateQuoteRole",
        engine = "entity-auto",
        invoke = "update",
        description = "Update a QuoteRole",
        defaultEntityName = "QuoteRole",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateQuoteRole {}

    /**
     * Remove a QuoteRole
     */
    @Service(
        name = "removeQuoteRole",
        engine = "simple",
        location = "component://order/script/org/ofbiz/order/quote/QuoteServices.xml",
        invoke = "removeQuoteRole",
        description = "Remove a QuoteRole",
        defaultEntityName = "QuoteRole",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface RemoveQuoteRole {}

    /**
     * Create a QuoteItem
     */
    @Service(
        name = "createQuoteItem",
        engine = "simple",
        location = "component://order/script/org/ofbiz/order/quote/QuoteServices.xml",
        invoke = "createQuoteItem",
        description = "Create a QuoteItem",
        defaultEntityName = "QuoteItem",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "INOUT", include = "pk", optional = "true"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface CreateQuoteItem {}

    /**
     * Update a QuoteItem
     */
    @Service(
        name = "updateQuoteItem",
        engine = "simple",
        location = "component://order/script/org/ofbiz/order/quote/QuoteServices.xml",
        invoke = "updateQuoteItem",
        description = "Update a QuoteItem",
        defaultEntityName = "QuoteItem",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk", optional = "true"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateQuoteItem {}

    /**
     * Remove a QuoteItem
     */
    @Service(
        name = "removeQuoteItem",
        engine = "simple",
        location = "component://order/script/org/ofbiz/order/quote/QuoteServices.xml",
        invoke = "removeQuoteItem",
        description = "Remove a QuoteItem",
        defaultEntityName = "QuoteItem",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "INOUT", include = "pk", optional = "true"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface RemoveQuoteItem {}

    /**
     * Copy a QuoteItem
     */
    @Service(
        name = "copyQuoteItem",
        engine = "simple",
        location = "component://order/script/org/ofbiz/order/quote/QuoteServices.xml",
        invoke = "copyQuoteItem",
        description = "Copy a QuoteItem",
        defaultEntityName = "QuoteItem",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        attributes = {
            @Attribute(name = "quoteIdTo", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "quoteItemSeqIdTo", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "copyQuoteAdjustments", type = "String", mode = "IN", optional = "true")
        }
    )
    public interface CopyQuoteItem {}

    /**
     * Create a QuoteAttribute
     */
    @Service(
        name = "createQuoteAttribute",
        engine = "simple",
        location = "component://order/script/org/ofbiz/order/quote/QuoteServices.xml",
        invoke = "createQuoteAttribute",
        description = "Create a QuoteAttribute",
        defaultEntityName = "QuoteAttribute",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface CreateQuoteAttribute {}

    /**
     * Update a QuoteAttribute
     */
    @Service(
        name = "updateQuoteAttribute",
        engine = "simple",
        location = "component://order/script/org/ofbiz/order/quote/QuoteServices.xml",
        invoke = "updateQuoteAttribute",
        description = "Update a QuoteAttribute",
        defaultEntityName = "QuoteAttribute",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk", optional = "true"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateQuoteAttribute {}

    /**
     * Remove a QuoteAttribute
     */
    @Service(
        name = "removeQuoteAttribute",
        engine = "simple",
        location = "component://order/script/org/ofbiz/order/quote/QuoteServices.xml",
        invoke = "removeQuoteAttribute",
        description = "Remove a QuoteAttribute",
        defaultEntityName = "QuoteAttribute",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk", optional = "true"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface RemoveQuoteAttribute {}

    /**
     * Create a QuoteCoefficient
     */
    @Service(
        name = "createQuoteCoefficient",
        engine = "simple",
        location = "component://order/script/org/ofbiz/order/quote/QuoteServices.xml",
        invoke = "createQuoteCoefficient",
        description = "Create a QuoteCoefficient",
        defaultEntityName = "QuoteCoefficient",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface CreateQuoteCoefficient {}

    /**
     * Update a QuoteCoefficient
     */
    @Service(
        name = "updateQuoteCoefficient",
        engine = "simple",
        location = "component://order/script/org/ofbiz/order/quote/QuoteServices.xml",
        invoke = "updateQuoteCoefficient",
        description = "Update a QuoteCoefficient",
        defaultEntityName = "QuoteCoefficient",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk", optional = "true"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateQuoteCoefficient {}

    /**
     * Remove a QuoteCoefficient
     */
    @Service(
        name = "removeQuoteCoefficient",
        engine = "simple",
        location = "component://order/script/org/ofbiz/order/quote/QuoteServices.xml",
        invoke = "removeQuoteCoefficient",
        description = "Remove a QuoteCoefficient",
        defaultEntityName = "QuoteCoefficient",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk", optional = "true"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface RemoveQuoteCoefficient {}

    /**
     * Create a new Quote and Quote Item for a CustRequest
     */
    @Service(
        name = "createQuoteAndQuoteItemForRequest",
        engine = "simple",
        location = "component://order/script/org/ofbiz/order/quote/QuoteServices.xml",
        invoke = "createQuoteAndQuoteItemForRequest",
        description = "Create a new Quote and Quote Item for a CustRequest",
        defaultEntityName = "QuoteItem",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true"),
            @EntityAttributes(mode = "OUT", include = "pk", optional = "true")
        },
        overrideAttributes = {
            @OverrideAttribute(name = "custRequestId", optional = "false")
        }
    )
    public interface CreateQuoteAndQuoteItemForRequest {}

    /**
     * Update the QuoteItem price with the passed value (if present) or automatically from the averageCost
     */
    @Service(
        name = "autoUpdateQuotePrice",
        engine = "simple",
        location = "component://order/script/org/ofbiz/order/quote/QuoteServices.xml",
        invoke = "autoUpdateQuotePrice",
        description = "Update the QuoteItem price with the passed value (if present) or automatically from the averageCost",
        defaultEntityName = "QuoteItem",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        },
        attributes = {
            @Attribute(name = "manualQuoteUnitPrice", type = "BigDecimal", mode = "IN", optional = "true"),
            @Attribute(name = "defaultQuoteUnitPrice", type = "BigDecimal", mode = "IN", optional = "true")
        }
    )
    public interface AutoUpdateQuotePrice {}

    /**
     * Remove all existing quote adjustments, recalc them and persist in QuoteAdjustment.
     */
    @Service(
        name = "autoCreateQuoteAdjustments",
        engine = "simple",
        location = "component://order/script/org/ofbiz/order/quote/QuoteServices.xml",
        invoke = "autoCreateQuoteAdjustments",
        description = "Remove all existing quote adjustments, recalc them and persist in QuoteAdjustment.",
        auth = "true",
        attributes = {
            @Attribute(name = "quoteId", type = "String", mode = "IN")
        }
    )
    public interface AutoCreateQuoteAdjustments {}

    /**
     * Creates a new quote adjustment record
     */
    @Service(
        name = "createQuoteAdjustment",
        engine = "simple",
        location = "component://order/script/org/ofbiz/order/quote/QuoteServices.xml",
        invoke = "createQuoteAdjustment",
        description = "Creates a new quote adjustment record",
        defaultEntityName = "QuoteAdjustment",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true"),
            @EntityAttributes(mode = "OUT", include = "pk")
        },
        overrideAttributes = {
            @OverrideAttribute(name = "quoteAdjustmentTypeId", optional = "false"),
            @OverrideAttribute(name = "quoteId", optional = "false")
        }
    )
    public interface CreateQuoteAdjustment {}

    /**
     * Update a QuoteAdjustment
     */
    @Service(
        name = "updateQuoteAdjustment",
        engine = "simple",
        location = "component://order/script/org/ofbiz/order/quote/QuoteServices.xml",
        invoke = "updateQuoteAdjustment",
        description = "Update a QuoteAdjustment",
        defaultEntityName = "QuoteAdjustment",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk", optional = "true"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateQuoteAdjustment {}

    /**
     * Remove a QuoteAdjustment
     */
    @Service(
        name = "removeQuoteAdjustment",
        engine = "simple",
        location = "component://order/script/org/ofbiz/order/quote/QuoteServices.xml",
        invoke = "removeQuoteAdjustment",
        description = "Remove a QuoteAdjustment",
        defaultEntityName = "QuoteAdjustment",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk", optional = "true"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface RemoveQuoteAdjustment {}

    /**
     * Creates a new QuoteWorkEffort record and WorkEffort record
     */
    @Service(
        name = "createQuoteWorkEffort",
        engine = "simple",
        location = "component://order/script/org/ofbiz/order/quote/QuoteServices.xml",
        invoke = "createQuoteWorkEffort",
        description = "Creates a new QuoteWorkEffort record and WorkEffort record",
        defaultEntityName = "QuoteWorkEffort",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true"),
            @EntityAttributes(mode = "INOUT", include = "pk", optional = "true"),
            @EntityAttributes(entityName = "WorkEffort", mode = "IN", include = "nonpk", optional = "true"),
            @EntityAttributes(entityName = "WorkEffort", mode = "INOUT", include = "pk", optional = "true")
        },
        overrideAttributes = {
            @OverrideAttribute(name = "quoteId", optional = "false"),
            @OverrideAttribute(name = "workEffortId", optional = "false")
        }
    )
    public interface CreateQuoteWorkEffort {}

    /**
     * Creates a new QuoteWorkEffort record
     */
    @Service(
        name = "deleteQuoteWorkEffort",
        engine = "simple",
        location = "component://order/script/org/ofbiz/order/quote/QuoteServices.xml",
        invoke = "deleteQuoteWorkEffort",
        description = "Creates a new QuoteWorkEffort record",
        defaultEntityName = "QuoteWorkEffort",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteQuoteWorkEffort {}

    /**
     * Creates a new quote from a shopping cart
     */
    @Service(
        name = "createQuoteFromCart",
        engine = "simple",
        location = "component://order/script/org/ofbiz/order/quote/QuoteServices.xml",
        invoke = "createQuoteFromCart",
        description = "Creates a new quote from a shopping cart",
        auth = "true",
        attributes = {
            @Attribute(name = "cart", type = "org.ofbiz.order.shoppingcart.ShoppingCart", mode = "IN"),
            @Attribute(name = "applyStorePromotions", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "quoteId", type = "String", mode = "OUT")
        }
    )
    public interface CreateQuoteFromCart {}

    /**
     * Creates a new quote from a shopping list
     */
    @Service(
        name = "createQuoteFromShoppingList",
        engine = "simple",
        location = "component://order/script/org/ofbiz/order/quote/QuoteServices.xml",
        invoke = "createQuoteFromShoppingList",
        description = "Creates a new quote from a shopping list",
        auth = "true",
        attributes = {
            @Attribute(name = "shoppingListId", type = "String", mode = "IN"),
            @Attribute(name = "applyStorePromotions", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "quoteId", type = "String", mode = "OUT")
        }
    )
    public interface CreateQuoteFromShoppingList {}

    /**
     * Creates a new quote from a customer request
     */
    @Service(
        name = "createQuoteFromCustRequest",
        engine = "simple",
        location = "component://order/script/org/ofbiz/order/quote/QuoteServices.xml",
        invoke = "createQuoteFromCustRequest",
        description = "Creates a new quote from a customer request",
        auth = "true",
        attributes = {
            @Attribute(name = "custRequestId", type = "String", mode = "IN"),
            @Attribute(name = "quoteTypeId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "quoteId", type = "String", mode = "OUT")
        }
    )
    public interface CreateQuoteFromCustRequest {}

    /**
     * Send a quote report mail
     */
    @Service(
        name = "sendQuoteReportMail",
        engine = "java",
        location = "org.ofbiz.order.quote.QuoteServices",
        invoke = "sendQuoteReportMail",
        description = "Send a quote report mail",
        requireNewTransaction = "true",
        maxRetry = "3",
        attributes = {
            @Attribute(name = "emailType", type = "String", mode = "INOUT"),
            @Attribute(name = "quoteId", type = "String", mode = "IN"),
            @Attribute(name = "sendTo", type = "String", mode = "IN"),
            @Attribute(name = "sendCc", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "note", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "body", type = "String", mode = "OUT", optional = "true"),
            @Attribute(name = "subject", type = "String", mode = "OUT", optional = "true"),
            @Attribute(name = "messageWrapper", type = "org.ofbiz.service.mail.MimeMessageWrapper", mode = "OUT", optional = "true"),
            @Attribute(name = "communicationEventId", type = "String", mode = "OUT", optional = "true")
        }
    )
    public interface SendQuoteReportMail {}

    /**
     * Creates quote entities
     */
    @Service(
        name = "storeQuote",
        engine = "java",
        location = "org.ofbiz.order.quote.QuoteServices",
        invoke = "storeQuote",
        description = "Creates quote entities",
        defaultEntityName = "Quote",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        attributes = {
            @Attribute(name = "quoteItems", type = "List", mode = "IN", optional = "true"),
            @Attribute(name = "quoteAttributes", type = "List", mode = "IN", optional = "true"),
            @Attribute(name = "quoteCoefficients", type = "List", mode = "IN", optional = "true"),
            @Attribute(name = "quoteRoles", type = "List", mode = "IN", optional = "true"),
            @Attribute(name = "quoteTerms", type = "List", mode = "IN", optional = "true"),
            @Attribute(name = "quoteTermAttributes", type = "List", mode = "IN", optional = "true"),
            @Attribute(name = "quoteWorkEfforts", type = "List", mode = "IN", optional = "true"),
            @Attribute(name = "quoteAdjustments", type = "List", mode = "IN", optional = "true"),
            @Attribute(name = "quoteId", type = "String", mode = "OUT")
        }
    )
    public interface StoreQuote {}

    /**
     * Create a note item and associate with a quote
     */
    @Service(
        name = "createQuoteNote",
        engine = "simple",
        location = "component://order/script/org/ofbiz/order/quote/QuoteServices.xml",
        invoke = "createQuoteNote",
        description = "Create a note item and associate with a quote",
        auth = "true",
        attributes = {
            @Attribute(name = "quoteId", type = "String", mode = "IN"),
            @Attribute(name = "noteInfo", type = "String", mode = "IN", allowHtml = "any"),
            @Attribute(name = "noteName", type = "String", mode = "IN", optional = "true")
        }
    )
    public interface CreateQuoteNote {}

    /**
     * Create a QuoteTermAttribute record
     */
    @Service(
        name = "createQuoteTermAttribute",
        engine = "entity-auto",
        invoke = "create",
        description = "Create a QuoteTermAttribute record",
        defaultEntityName = "QuoteTermAttribute",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface CreateQuoteTermAttribute {}

    /**
     * Update a QuoteTermAttribute record
     */
    @Service(
        name = "updateQuoteTermAttribute",
        engine = "entity-auto",
        invoke = "update",
        description = "Update a QuoteTermAttribute record",
        defaultEntityName = "QuoteTermAttribute",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateQuoteTermAttribute {}

    /**
     * Delete a QuoteTermAttribute record
     */
    @Service(
        name = "deleteQuoteTermAttribute",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete a QuoteTermAttribute record",
        defaultEntityName = "QuoteTermAttribute",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteQuoteTermAttribute {}

    /**
     * Create a QuoteType record
     */
    @Service(
        name = "createQuoteType",
        engine = "entity-auto",
        invoke = "create",
        description = "Create a QuoteType record",
        defaultEntityName = "QuoteType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "INOUT", include = "pk", optional = "true"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface CreateQuoteType {}

    /**
     * Update a QuoteType record
     */
    @Service(
        name = "updateQuoteType",
        engine = "entity-auto",
        invoke = "update",
        description = "Update a QuoteType record",
        defaultEntityName = "QuoteType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateQuoteType {}

    /**
     * Delete a QuoteType record
     */
    @Service(
        name = "deleteQuoteType",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete a QuoteType record",
        defaultEntityName = "QuoteType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteQuoteType {}

    /**
     * Create a QuoteTypeAttr record
     */
    @Service(
        name = "createQuoteTypeAttr",
        engine = "entity-auto",
        invoke = "create",
        description = "Create a QuoteTypeAttr record",
        defaultEntityName = "QuoteTypeAttr",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface CreateQuoteTypeAttr {}

    /**
     * Update a QuoteTypeAttr record
     */
    @Service(
        name = "updateQuoteTypeAttr",
        engine = "entity-auto",
        invoke = "update",
        description = "Update a QuoteTypeAttr record",
        defaultEntityName = "QuoteTypeAttr",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateQuoteTypeAttr {}

    /**
     * Delete a QuoteTypeAttr record
     */
    @Service(
        name = "deleteQuoteTypeAttr",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete a QuoteTypeAttr record",
        defaultEntityName = "QuoteTypeAttr",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteQuoteTypeAttr {}

}
