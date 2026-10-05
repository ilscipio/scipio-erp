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
package com.ilscipio.scipio.party.entity;

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
     * Agreement WorkEffort Application
     * NOTE: this entity is deprecated by AgreementWorkEffortApplic
     */
    @Entity(
        name = "OldAgreementWorkEffortAppl",
        packageName = "org.ofbiz.party.agreement",
        tableName = "AGREEMENT_WORKEFFORT_APPL",
        title = "Agreement WorkEffort Application",
        description = "NOTE: this entity is deprecated by AgreementWorkEffortApplic",
        fields = {
            @Field(name = "agreementId", type = "id-ne"),
            @Field(name = "agreementItemSeqId", type = "id-ne"),
            @Field(name = "workEffortId", type = "id-ne")
        },
        primaryKeys = {
            @PrimaryKey(field = "agreementId"),
            @PrimaryKey(field = "agreementItemSeqId"),
            @PrimaryKey(field = "workEffortId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "Agreement",
                keyMaps = {
                    @KeyMap(fieldName = "agreementId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "AgreementItem",
                fkName = "AGRMNT_WEA_AITM",
                keyMaps = {
                    @KeyMap(fieldName = "agreementId"),
                    @KeyMap(fieldName = "agreementItemSeqId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "WorkEffort",
                fkName = "AGRMNT_WEA_WEFF",
                keyMaps = {
                    @KeyMap(fieldName = "workEffortId")
                }
            )
        }
    )
    public interface OldAgreementWorkEffortApplEntity {}

}
