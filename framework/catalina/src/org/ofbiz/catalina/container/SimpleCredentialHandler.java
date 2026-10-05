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

package org.ofbiz.catalina.container;

import org.apache.catalina.CredentialHandler;
import org.ofbiz.base.util.CredentialUtil;


public class SimpleCredentialHandler implements CredentialHandler {
    @Override
    public boolean matches(String inputCredentials, String storedCredentials) {
        return CredentialUtil.checkPassword(storedCredentials, false, inputCredentials);
    }

    @Override
    public String mutate(String inputCredentials) {
        // when password.encrypt=false, password is stored as cleartext in the database.
        // no need to encrypt this input password.
        return null;
    }
}
