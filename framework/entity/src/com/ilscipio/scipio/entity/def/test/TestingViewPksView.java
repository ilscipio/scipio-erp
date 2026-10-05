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

import com.ilscipio.scipio.entity.def.AliasAll;
import com.ilscipio.scipio.entity.def.KeyMap;
import com.ilscipio.scipio.entity.def.MemberEntity;
import com.ilscipio.scipio.entity.def.ViewEntity;
import com.ilscipio.scipio.entity.def.ViewLink;

/**
 * Test view-entity: TestingViewPks - Testing And TestingSubtype View.
 *
 * <p>This is a test migration from XML to annotation-based definition.
 * The original XML definition was:</p>
 * <pre>
 * &lt;view-entity entity-name="TestingViewPks" package-name="org.ofbiz.entity.test" title="Testing And TestingSubtype View"&gt;
 *     &lt;member-entity entity-alias="TST" entity-name="TestingType" /&gt;
 *     &lt;member-entity entity-alias="TSTSUB" entity-name="TestingSubtype" /&gt;
 *     &lt;alias-all entity-alias="TST" /&gt;
 *     &lt;alias-all entity-alias="TSTSUB" /&gt;
 *     &lt;view-link entity-alias="TST" rel-entity-alias="TSTSUB"&gt;
 *         &lt;key-map field-name="testingTypeId" /&gt;
 *     &lt;/view-link&gt;
 * &lt;/view-entity&gt;
 * </pre>
 *
 * <p>SCIPIO: 4.0.0: Added for entity annotations support testing.</p>
 */
@ViewEntity(
        name = "TestingViewPksAnnotated",
        packageName = "org.ofbiz.entity.test",
        title = "Testing And TestingSubtype View (Annotation)",
        members = {
                @MemberEntity(entityAlias = "TST", entityName = "TestingType"),
                @MemberEntity(entityAlias = "TSTSUB", entityName = "TestingSubtype")
        },
        aliasAlls = {
                @AliasAll(entityAlias = "TST"),
                @AliasAll(entityAlias = "TSTSUB")
        },
        viewLinks = {
                @ViewLink(entityAlias = "TST", relEntityAlias = "TSTSUB",
                        keyMaps = @KeyMap(fieldName = "testingTypeId"))
        }
)
public interface TestingViewPksView {
}
