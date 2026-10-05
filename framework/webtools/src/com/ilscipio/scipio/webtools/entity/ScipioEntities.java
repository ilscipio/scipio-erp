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
package com.ilscipio.scipio.webtools.entity;

import com.ilscipio.scipio.entity.def.*;

/**
 * Auto-generated annotation-based entity definitions.
 *
 * <p>Generated from entitymodel.xml by convertXmlToAnnotation Gradle task.</p>
 *
 * <p>SCIPIO: 4.0.0: Auto-generated.</p>
 */
public class ScipioEntities {

    /**
     * Entity Export
     */
    @Entity(
        name = "EntityExport",
        packageName = "com.ilscipio.scipio.webtools.entity",
        title = "Entity Export",
        fields = {
            @Field(name = "exportId", type = "id-ne"),
            @Field(name = "fileData", type = "byte-array"),
            @Field(name = "file", type = "byte-array"),
            @Field(name = "fileSize", type = "numeric"),
            @Field(name = "description", type = "very-long"),
            @Field(name = "createdBy", type = "id"),
            @Field(name = "lastUpdatedBy", type = "id")
        },
        primaryKeys = {
            @PrimaryKey(field = "exportId")
        }
    )
    public interface EntityExportEntity {}

}
