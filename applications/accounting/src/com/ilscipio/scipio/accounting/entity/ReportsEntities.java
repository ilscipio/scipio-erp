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
package com.ilscipio.scipio.accounting.entity;

import com.ilscipio.scipio.entity.def.*;

/**
 * Auto-generated annotation-based entity definitions.
 *
 * <p>Generated from entitymodel.xml by convertXmlToAnnotation Gradle task.</p>
 *
 * <p>SCIPIO: 4.0.0: Auto-generated.</p>
 */
public class ReportsEntities {

    @ViewEntity(
        name = "InvoiceItemProductSummary",
        packageName = "org.ofbiz.accounting.reports",
        members = {
            @MemberEntity(entityAlias = "INV", entityName = "Invoice"),
            @MemberEntity(entityAlias = "INITM", entityName = "InvoiceItem")
        },
        aliases = {
            @Alias(name = "statusId", entityAlias = "INV"),
            @Alias(name = "invoiceDate", entityAlias = "INV"),
            @Alias(name = "invoiceTypeId", entityAlias = "INV"),
            @Alias(name = "partyIdFrom", entityAlias = "INV"),
            @Alias(name = "partyId", entityAlias = "INV"),
            @Alias(name = "currencyUomId", entityAlias = "INV"),
            @Alias(name = "invoiceItemTypeId", entityAlias = "INITM"),
            @Alias(name = "productId", entityAlias = "INITM", groupBy = true),
            @Alias(name = "quantityTotal", entityAlias = "INITM", field = "quantity", function = AggregateFunction.SUM),
            @Alias(name = "amountTotal", entityAlias = "INITM", field = "amount", function = AggregateFunction.SUM)
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "INV",
                relEntityAlias = "INITM",
                keyMaps = {
                    @KeyMap(fieldName = "invoiceId")
                }
            )
        }
    )
    public interface InvoiceItemProductSummaryView {}

    @ViewEntity(
        name = "InvoiceItemCategorySummary",
        packageName = "org.ofbiz.accounting.reports",
        members = {
            @MemberEntity(entityAlias = "INV", entityName = "Invoice"),
            @MemberEntity(entityAlias = "INITM", entityName = "InvoiceItem"),
            @MemberEntity(entityAlias = "PCM", entityName = "ProductCategoryMember")
        },
        aliases = {
            @Alias(name = "statusId", entityAlias = "INV"),
            @Alias(name = "invoiceDate", entityAlias = "INV"),
            @Alias(name = "invoiceTypeId", entityAlias = "INV"),
            @Alias(name = "partyIdFrom", entityAlias = "INV"),
            @Alias(name = "partyId", entityAlias = "INV"),
            @Alias(name = "currencyUomId", entityAlias = "INV"),
            @Alias(name = "invoiceItemTypeId", entityAlias = "INITM"),
            @Alias(name = "productId", entityAlias = "INITM"),
            @Alias(name = "quantityTotal", entityAlias = "INITM", field = "quantity", function = AggregateFunction.SUM),
            @Alias(name = "amountTotal", entityAlias = "INITM", field = "amount", function = AggregateFunction.SUM),
            @Alias(name = "productCategoryId", entityAlias = "PCM", groupBy = true)
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "INV",
                relEntityAlias = "INITM",
                keyMaps = {
                    @KeyMap(fieldName = "invoiceId")
                }
            ),
            @ViewLink(
                entityAlias = "INITM",
                relEntityAlias = "PCM",
                keyMaps = {
                    @KeyMap(fieldName = "productId")
                }
            )
        }
    )
    public interface InvoiceItemCategorySummaryView {}

    @ViewEntity(
        name = "InvoiceExport",
        packageName = "org.ofbiz.accounting.reports",
        members = {
            @MemberEntity(entityAlias = "INV", entityName = "Invoice"),
            @MemberEntity(entityAlias = "ITM", entityName = "InvoiceItem"),
            @MemberEntity(entityAlias = "PFR", entityName = "PartyIdentification"),
            @MemberEntity(entityAlias = "PTO", entityName = "PartyIdentification"),
            @MemberEntity(entityAlias = "GI", entityName = "GoodIdentification")
        },
        aliases = {
            @Alias(name = "invoiceId", entityAlias = "INV"),
            @Alias(name = "invoiceDate", entityAlias = "INV"),
            @Alias(name = "invoiceTypeId", entityAlias = "INV"),
            @Alias(name = "description", entityAlias = "INV"),
            @Alias(name = "partyIdFrom", entityAlias = "INV"),
            @Alias(name = "partyIdFromTrans", entityAlias = "PFR", field = "idValue"),
            @Alias(name = "partyId", entityAlias = "INV"),
            @Alias(name = "partyIdTrans", entityAlias = "PTO", field = "idValue"),
            @Alias(name = "currencyUomId", entityAlias = "INV"),
            @Alias(name = "referenceNumber", entityAlias = "INV"),
            @Alias(name = "invoiceItemSeqId", entityAlias = "ITM"),
            @Alias(name = "invoiceItemTypeId", entityAlias = "ITM"),
            @Alias(name = "itemDescription", entityAlias = "ITM", field = "description"),
            @Alias(name = "productId", entityAlias = "ITM"),
            @Alias(name = "productIdTrans", entityAlias = "GI", field = "idValue"),
            @Alias(name = "quantity", entityAlias = "ITM"),
            @Alias(name = "amount", entityAlias = "ITM")
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "INV",
                relEntityAlias = "ITM",
                relOptional = true,
                keyMaps = {
                    @KeyMap(fieldName = "invoiceId")
                }
            ),
            @ViewLink(
                entityAlias = "INV",
                relEntityAlias = "PFR",
                relOptional = true,
                keyMaps = {
                    @KeyMap(fieldName = "partyIdFrom", relFieldName = "partyId")
                }
            ),
            @ViewLink(
                entityAlias = "INV",
                relEntityAlias = "PTO",
                relOptional = true,
                keyMaps = {
                    @KeyMap(fieldName = "partyId")
                }
            ),
            @ViewLink(
                entityAlias = "ITM",
                relEntityAlias = "GI",
                relOptional = true,
                keyMaps = {
                    @KeyMap(fieldName = "productId")
                }
            )
        }
    )
    public interface InvoiceExportView {}

}
