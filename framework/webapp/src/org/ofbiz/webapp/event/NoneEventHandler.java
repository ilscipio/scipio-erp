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
package org.ofbiz.webapp.event;

import org.ofbiz.webapp.control.ConfigXMLReader;

import javax.servlet.ServletContext;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

/**
 * No-op event handler (SCIPIO).
 */
public class NoneEventHandler implements EventHandler {
    public static final NoneEventHandler DEFAULT = new NoneEventHandler();

    @Override
    public void init(ServletContext context) throws EventHandlerException {
    }

    @Override
    public String invoke(ConfigXMLReader.Event event, ConfigXMLReader.RequestMap requestMap, HttpServletRequest request, HttpServletResponse response) throws EventHandlerException {
        return "success";
    }
}
