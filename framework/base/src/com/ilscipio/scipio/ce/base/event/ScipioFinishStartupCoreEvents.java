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
import org.ofbiz.base.util.ScriptUtil;
import org.ofbiz.base.util.UtilCodec;

public class ScipioFinishStartupCoreEvents implements ExtendedStartupLoader {

    @Override
    public void load(Config config, String[] args) throws StartupException {
    }

    @Override
    public void start() throws StartupException {
        initScriptEngine();
        initCodecs();
    }

    @Override
    public void unload() throws StartupException {
    }

    @Override
    public void execOnRunning() throws StartupException {
    }

    /**
     * Pre-initialize the script engine to prevent various issues with,
     * and so the log is less cluttered/confusing afterward.
     */
    protected void initScriptEngine() throws StartupException {
        ScriptUtil.getScriptEngineManager();
    }

    protected void initCodecs() throws StartupException {
        UtilCodec.getEncoder("html-default");
    }
}
