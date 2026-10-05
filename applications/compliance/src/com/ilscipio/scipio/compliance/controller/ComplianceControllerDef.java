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
package com.ilscipio.scipio.compliance.controller;

import com.ilscipio.scipio.ce.webapp.control.def.View;
import com.ilscipio.scipio.ce.webapp.control.def.Request;
import com.ilscipio.scipio.ce.webapp.control.def.Response;
import com.ilscipio.scipio.ce.webapp.control.def.Event;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

/**
 * Controller definitions for the Compliance component.
 *
 * <p>SCIPIO: 4.0.0: Added by createComponent template.</p>
 */
public class ComplianceControllerDef {

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "main",
        type = "screen",
        page = "component://compliance/widget/ComplianceScreens.xml#main",
        controller = "compliance"
    )
    public static final String VIEW_MAIN = "main";

    @Request(
        uri = "main",
        controller = "compliance",
        secure = "true",
        auth = "true"
    )
    @Response(name = "success", type = "view", value = "main")
    public interface Main {}

    @Request(
        uri = "publishLegalDocument",
        controller = "compliance",
        secure = "true",
        auth = "true"
    )
    @Response(name = "success", type = "view", value = "main")
    @Response(name = "error", type = "view", value = "main")
    @Event(type = "service", invoke = "publishLegalDocument")
    public static String publishLegalDocument(HttpServletRequest request, HttpServletResponse response) {
        return "success"; // Event handled by @Event annotation
    }

}
