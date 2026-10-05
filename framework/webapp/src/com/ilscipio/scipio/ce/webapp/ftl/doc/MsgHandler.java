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

/**
 * Message handler.
 * <p>
 * TODO: replace with log4j.
 */
public interface MsgHandler {

    public static final boolean DEBUG = false;

    public void logInfo(String msg);
    public void logError(String msg);
    public void logDebug(String msg);
    public void logWarn(String msg);

    public static class SysOutMsgHandler implements MsgHandler {

        @Override
        public void logInfo(String msg) {
            System.out.println(msg);
        }

        @Override
        public void logError(String msg) {
            System.out.println("ERROR: " + msg);
        }

        @Override
        public void logDebug(String msg) {
            if (DEBUG) {
                System.out.println(msg);
            }
        }

        @Override
        public void logWarn(String msg) {
            System.out.println("WARN: " + msg);
        }

    }

    public static class VoidMsgHandler implements MsgHandler {

        @Override
        public void logInfo(String msg) {
        }

        @Override
        public void logError(String msg) {
        }

        @Override
        public void logDebug(String msg) {
        }

        @Override
        public void logWarn(String msg) {
        }

    }
}