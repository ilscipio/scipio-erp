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
package com.ilscipio.scipio.ce.webapp.ftl.doc;

import org.ofbiz.base.util.Debug;

/**
 * Message adapter for Ofbiz Debug class.
 * <p>
 * IMPORTANT: Keep separate from the other classes for now.
 */
public class OfbizDebugMsgHandler implements MsgHandler {

    protected final String targetModule;

    public OfbizDebugMsgHandler(String targetModule) {
        this.targetModule = targetModule;
    }

    @Override
    public void logInfo(String msg) {
        Debug.logInfo(msg, targetModule);
    }

    @Override
    public void logError(String msg) {
        Debug.logError(msg, targetModule);
    }

    @Override
    public void logDebug(String msg) {
        if (Debug.verboseOn()) {
            Debug.logVerbose(msg, targetModule);
        }
    }

    @Override
    public void logWarn(String msg) {
        Debug.logWarning(msg, targetModule);
    }

}
