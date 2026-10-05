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
package com.ilscipio.scipio.web;

import org.ofbiz.entity.GenericValue;
import org.ofbiz.security.Security;

import javax.servlet.http.HttpSession;
import javax.websocket.Session;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

/**
 * Simple permission security checker interface, which implicitly contains the permissions to check.
 * <p>SCIPIO: 3.0.0: Added for annotations support (generic interface).</p>
 */
public interface SocketPermissionVerifier {

    boolean hasPermission(Security security, GenericValue userLogin, HttpSession httpSession, Session socketSession);

    class EntityViewOr implements SocketPermissionVerifier {
        private final List<String> entities;

        public EntityViewOr(String... entities) {
            this.entities = new ArrayList<>(Arrays.asList(entities));
        }

        public List<String> getEntities() {
            return entities;
        }

        @Override
        public boolean hasPermission(Security security, GenericValue userLogin, HttpSession httpSession, Session socketSession) {
            for (String entity : getEntities()) {
                if (security.hasEntityPermission(entity, "_VIEW", userLogin)) {
                    return true;
                }
            }
            return false;
        }
    }

}
