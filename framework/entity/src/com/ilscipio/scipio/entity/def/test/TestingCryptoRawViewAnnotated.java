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

import com.ilscipio.scipio.entity.def.Alias;
import com.ilscipio.scipio.entity.def.AliasAll;
import com.ilscipio.scipio.entity.def.ComplexAlias;
import com.ilscipio.scipio.entity.def.ComplexAliasField;
import com.ilscipio.scipio.entity.def.MemberEntity;
import com.ilscipio.scipio.entity.def.ViewEntity;

/**
 * Test view-entity: TestingCryptoRawView - TestingCrypto Raw View with complex-alias.
 *
 * <p>This is a test migration from XML to annotation-based definition.
 * The original XML definition was:</p>
 * <pre>
 * &lt;view-entity entity-name="TestingCryptoRawView"
 *         package-name="org.ofbiz.entity.test"
 *         title="TestingCrypto Raw View"&gt;
 *   &lt;member-entity entity-alias="TC" entity-name="TestingCrypto"/&gt;
 *   &lt;alias-all entity-alias="TC"/&gt;
 *   &lt;alias name="rawEncryptedValue"&gt;
 *     &lt;complex-alias operator="+"&gt;
 *       &lt;complex-alias-field entity-alias="TC" field="encryptedValue"/&gt;
 *     &lt;/complex-alias&gt;
 *   &lt;/alias&gt;
 *   &lt;alias name="rawSaltedEncryptedValue"&gt;
 *     &lt;complex-alias operator="+"&gt;
 *       &lt;complex-alias-field entity-alias="TC" field="saltedEncryptedValue"/&gt;
 *     &lt;/complex-alias&gt;
 *   &lt;/alias&gt;
 * &lt;/view-entity&gt;
 * </pre>
 *
 * <p>SCIPIO: 4.0.0: Added for entity annotations support testing.</p>
 */
@ViewEntity(
        name = "TestingCryptoRawViewAnnotated",
        packageName = "org.ofbiz.entity.test",
        title = "TestingCrypto Raw View (Annotation)",
        members = {
                @MemberEntity(entityAlias = "TC", entityName = "TestingCrypto")
        },
        aliasAlls = {
                @AliasAll(entityAlias = "TC")
        },
        aliases = {
                @Alias(name = "rawEncryptedValue",
                        complexAlias = @ComplexAlias(operator = "+",
                                fields = @ComplexAliasField(entityAlias = "TC", field = "encryptedValue"))),
                @Alias(name = "rawSaltedEncryptedValue",
                        complexAlias = @ComplexAlias(operator = "+",
                                fields = @ComplexAliasField(entityAlias = "TC", field = "saltedEncryptedValue")))
        }
)
public interface TestingCryptoRawViewAnnotated {
}
