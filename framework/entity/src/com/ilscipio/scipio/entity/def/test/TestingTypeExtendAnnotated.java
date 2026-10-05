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
package com.ilscipio.scipio.entity.def.test;

import com.ilscipio.scipio.entity.def.ExtendEntity;
import com.ilscipio.scipio.entity.def.Field;
import com.ilscipio.scipio.entity.def.Index;
import com.ilscipio.scipio.entity.def.IndexField;

/**
 * Test extend-entity: Extends TestingType with additional field and index.
 *
 * <p>This demonstrates the @ExtendEntity annotation by adding a custom field
 * to the existing TestingType entity.</p>
 *
 * <p>SCIPIO: 4.0.0: Added for entity annotations support testing.</p>
 */
@ExtendEntity(
        name = "TestingType",
        fields = {
                @Field(name = "annotatedExtField", type = "description",
                        description = "Custom extension field added via @ExtendEntity annotation")
        },
        indexes = {
                @Index(name = "TST_TYPE_ANN_EXT",
                        fields = @IndexField(name = "annotatedExtField"),
                        description = "Index on annotated extension field")
        }
)
public interface TestingTypeExtendAnnotated {
}
