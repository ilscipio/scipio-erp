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
package org.ofbiz.base.start;

/**
 * SCIPIO: Extended startup loader with support for extra events
 * and callbacks.
 * <p>
 * Added 2018-05-23.
 */
public interface ExtendedStartupLoader extends StartupLoader {

    /**
     * Callback for post-startup events.
     * <p>
     * These execute once the server is in RUNNING state,
     * after the "FRAMEWORK IS LOADED" message has been printed.
     * <p>
     * In other words, it's functionally equivalent to adding
     * a startup service Job on eventId="SCH_EVENT_STARTUP",
     * but without need to modify data.
     *
     * @throws StartupException If an error was encountered.
     */
    public void execOnRunning() throws StartupException;

}
