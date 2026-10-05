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
package com.ilscipio.scipio.commonext.controller;

import com.ilscipio.scipio.ce.webapp.control.def.Request;
import com.ilscipio.scipio.ce.webapp.control.def.Response;

/**
 * Controller definitions for the server root webapp ("/", controller "root"): the application launcher.
 *
 * <p>SCIPIO: 4.0.0: Added.</p>
 */
public class RootControllerDef {

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "main",
        type = "screen",
        page = "component://commonext/widget/RootScreens.xml#main",
        controller = "root"
    )
    public static final String VIEW_MAIN = "main";

    @Request(
        uri = "main",
        controller = "root",
        secure = "true",
        auth = "true"
    )
    @Response(name = "success", type = "view", value = "main")
    public interface Main {}

}
