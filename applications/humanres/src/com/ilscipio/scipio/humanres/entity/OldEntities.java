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
package com.ilscipio.scipio.humanres.entity;

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
     * EmplPosition Type Rate
     */
    @Entity(
        name = "OldEmplPositionTypeRate",
        packageName = "org.ofbiz.humanres.position",
        tableName = "EMPL_POSITION_TYPE_RATE",
        title = "EmplPosition Type Rate",
        fields = {
            @Field(name = "emplPositionTypeId", type = "id-ne"),
            @Field(name = "periodTypeId", type = "id-ne"),
            @Field(name = "payGradeId", type = "id"),
            @Field(name = "salaryStepSeqId", type = "id"),
            @Field(name = "rateTypeId", type = "id"),
            @Field(name = "fromDate", type = "date-time"),
            @Field(name = "thruDate", type = "date-time"),
            @Field(name = "rate", type = "id")
        },
        primaryKeys = {
            @PrimaryKey(field = "emplPositionTypeId"),
            @PrimaryKey(field = "periodTypeId"),
            @PrimaryKey(field = "fromDate")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "EmplPositionType",
                fkName = "EMPL_PSTPRT_EPTP",
                keyMaps = {
                    @KeyMap(fieldName = "emplPositionTypeId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "PeriodType",
                fkName = "EMPL_PSTPRT_PRDTYP",
                keyMaps = {
                    @KeyMap(fieldName = "periodTypeId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "SalaryStep",
                fkName = "EMPL_PSTPRT_SSTP",
                keyMaps = {
                    @KeyMap(fieldName = "salaryStepSeqId"),
                    @KeyMap(fieldName = "payGradeId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "RateType",
                fkName = "EMPL_PSTPRT_RTTYP",
                keyMaps = {
                    @KeyMap(fieldName = "rateTypeId")
                }
            )
        }
    )
    public interface OldEmplPositionTypeRateEntity {}

}
