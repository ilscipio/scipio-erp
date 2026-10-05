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
package com.ilscipio.scipio.catalina.entity;

import com.ilscipio.scipio.entity.def.*;

/**
 * Auto-generated annotation-based entity definitions.
 *
 * <p>Generated from entitymodel.xml by convertXmlToAnnotation Gradle task.</p>
 *
 * <p>SCIPIO: 4.0.0: Auto-generated.</p>
 */
public class Entities {

    /**
     * Catalina Session Store
     */
    @Entity(
        name = "CatalinaSession",
        packageName = "org.ofbiz.catalina.session",
        title = "Catalina Session Store",
        fields = {
            @Field(name = "sessionId", type = "id-long-ne"),
            @Field(name = "sessionSize", type = "numeric"),
            @Field(name = "sessionInfo", type = "blob"),
            @Field(name = "isValid", type = "indicator"),
            @Field(name = "maxIdle", type = "numeric"),
            @Field(name = "lastAccessed", type = "numeric")
        },
        primaryKeys = {
            @PrimaryKey(field = "sessionId")
        }
    )
    public interface CatalinaSessionEntity {}

}
