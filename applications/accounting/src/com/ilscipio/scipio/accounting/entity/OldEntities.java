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
public class OldEntities {

    /**
     * Value Link Fulfillment History
     */
    @Entity(
        name = "OldValueLinkFulfillment",
        packageName = "org.ofbiz.accounting.payment",
        tableName = "VALUE_LINK_FULFILLMENT",
        title = "Value Link Fulfillment History",
        fields = {
            @Field(name = "fulfillmentId", type = "id-ne"),
            @Field(name = "typeEnumId", type = "id-ne"),
            @Field(name = "merchantId", type = "id-vlong-ne"),
            @Field(name = "partyId", type = "id-ne"),
            @Field(name = "orderId", type = "id-ne"),
            @Field(name = "orderItemSeqId", type = "id-ne"),
            @Field(name = "surveyResponseId", type = "id-ne"),
            @Field(name = "cardNumber", type = "short-varchar"),
            @Field(name = "pinNumber", type = "short-varchar"),
            @Field(name = "amount", type = "currency-amount"),
            @Field(name = "responseCode", type = "short-varchar"),
            @Field(name = "referenceNum", type = "short-varchar"),
            @Field(name = "authCode", type = "short-varchar"),
            @Field(name = "fulfillmentDate", type = "date-time")
        },
        primaryKeys = {
            @PrimaryKey(field = "fulfillmentId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Enumeration",
                fkName = "VL_FILL_ENUM",
                keyMaps = {
                    @KeyMap(fieldName = "typeEnumId", relFieldName = "enumId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Party",
                fkName = "VL_FILL_PARTY",
                keyMaps = {
                    @KeyMap(fieldName = "partyId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "OrderHeader",
                fkName = "VL_FILL_ODRH",
                keyMaps = {
                    @KeyMap(fieldName = "orderId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "OrderItem",
                fkName = "VL_FILL_ODRI",
                keyMaps = {
                    @KeyMap(fieldName = "orderId"),
                    @KeyMap(fieldName = "orderItemSeqId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "SurveyResponse",
                fkName = "VL_FILL_SURVY",
                keyMaps = {
                    @KeyMap(fieldName = "surveyResponseId")
                }
            )
        }
    )
    public interface OldValueLinkFulfillmentEntity {}

    /**
     * Party Rate
     */
    @Entity(
        name = "OldPartyRate",
        packageName = "org.ofbiz.workeffort.timesheet",
        tableName = "PARTY_RATE",
        title = "Party Rate",
        fields = {
            @Field(name = "partyId", type = "id-ne"),
            @Field(name = "rateTypeId", type = "id-ne"),
            @Field(name = "currencyUomId", type = "id-ne"),
            @Field(name = "defaultRate", type = "indicator"),
            @Field(name = "fromDate", type = "date-time"),
            @Field(name = "thruDate", type = "date-time"),
            @Field(name = "rate", type = "currency-amount")
        },
        primaryKeys = {
            @PrimaryKey(field = "partyId"),
            @PrimaryKey(field = "rateTypeId"),
            @PrimaryKey(field = "currencyUomId"),
            @PrimaryKey(field = "fromDate")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Party",
                fkName = "OPRTY_RTE_PRTY",
                keyMaps = {
                    @KeyMap(fieldName = "partyId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "RateType",
                fkName = "OPRTY_RTE_RTTP",
                keyMaps = {
                    @KeyMap(fieldName = "rateTypeId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Uom",
                title = "Currency",
                fkName = "OPARTY_RATE_CUOM",
                keyMaps = {
                    @KeyMap(fieldName = "currencyUomId", relFieldName = "uomId")
                }
            )
        }
    )
    public interface OldPartyRateEntity {}

}
