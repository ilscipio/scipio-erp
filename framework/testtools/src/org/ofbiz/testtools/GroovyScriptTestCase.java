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
package org.ofbiz.testtools;

import org.ofbiz.entity.Delegator;
import org.ofbiz.security.Security;
import org.ofbiz.service.LocalDispatcher;

import groovy.util.GroovyTestCase;

public class GroovyScriptTestCase extends GroovyTestCase {

    public Delegator delegator;
    public LocalDispatcher dispatcher;
    public Security security;

    public void setDelegator(Delegator delegator) {
        this.delegator = delegator;
    }
    public Delegator getDelegator() {
        return delegator;
    }

    public LocalDispatcher getDispatcher() {
        return dispatcher;
    }
    public void setDispatcher(LocalDispatcher dispatcher) {
        this.dispatcher = dispatcher;
    }
    public void setSecurity(Security security) {
        this.security = security;
    }
    public Security getSecurity() {
        return security;
    }
}
