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
package com.ilscipio.scipio.ce.base.event;

import org.ofbiz.base.start.Config;
import org.ofbiz.base.start.ExtendedStartupLoader;
import org.ofbiz.base.start.StartupException;
import org.ofbiz.base.util.Debug;

public class TestStartupEvents implements ExtendedStartupLoader {

    @Override
    public void load(Config config, String[] args) throws StartupException {
        Debug.logInfo("Scipio: TestStartupEvents: load", TestStartupEvents.class.getName());
    }

    @Override
    public void start() throws StartupException {
        Debug.logInfo("Scipio: TestStartupEvents: start", TestStartupEvents.class.getName());
    }

    @Override
    public void unload() throws StartupException {
        Debug.logInfo("Scipio: TestStartupEvents: unload", TestStartupEvents.class.getName());
    }

    @Override
    public void execOnRunning() throws StartupException {
        Debug.logInfo("Scipio: TestStartupEventsS: execOnRunning", TestStartupEvents.class.getName());
    }

}
