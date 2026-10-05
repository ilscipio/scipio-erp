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
package com.ilscipio.scipio.commonext.entity;

import com.ilscipio.scipio.entity.def.*;

/**
 * Auto-generated annotation-based entity definitions.
 *
 * <p>Generated from entitymodel.xml by convertXmlToAnnotation Gradle task.</p>
 *
 * <p>SCIPIO: 4.0.0: Auto-generated.</p>
 */
public class Entities {

    @ExtendEntity(
        name = "NoteData",
        fields = {
            @Field(name = "moreInfoUrl", type = "value", description = "url to go to the related screen in the system"),
            @Field(name = "moreInfoItemId", type = "value", description = "The id of the item to be displayed i.e. custRequestId, commEventId etc"),
            @Field(name = "moreInfoItemName", type = "value", description = "The name of the item to be displayed i.e. custRequestId, commEventId etc")
        },
        indexes = {
            @Index(
                name = "systemInfo",
                fields = {
                    @IndexField(name = "noteName")
                }
            )
        }
    )
    public interface NoteDataExtension {}

}
