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
package com.ilscipio.scipio.cms.controller;

import com.ilscipio.scipio.ce.webapp.control.def.View;
import com.ilscipio.scipio.ce.webapp.control.def.Request;
import com.ilscipio.scipio.ce.webapp.control.def.Response;
import com.ilscipio.scipio.ce.webapp.control.def.Responses;
import com.ilscipio.scipio.ce.webapp.control.def.Event;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

/**
 * Auto-generated annotation-based controller definitions.
 *
 * <p>Generated from controller.xml by convertXmlToAnnotation Gradle task.</p>
 *
 * <p>SCIPIO: 4.0.0: Auto-generated.</p>
 */
public class BackendsiteControllerDef {

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "main",
        type = "screen",
        page = "component://cms/widget/CommonScreens.xml#404",
        controller = "backendsite"
    )
    public static final String VIEW_MAIN = "main";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "pagenotfound",
        type = "screen",
        page = "component://cms/widget/CommonScreens.xml#404",
        controller = "backendsite"
    )
    public static final String VIEW_PAGENOTFOUND = "pagenotfound";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "editPage",
        type = "screen",
        page = "component://cms/widget/CMSScreens.xml#editPage",
        controller = "backendsite"
    )
    public static final String VIEW_EDITPAGE = "editPage";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "testView1",
        type = "screen",
        page = "component://cms/widget/CommonScreens.xml#404",
        controller = "backendsite"
    )
    public static final String VIEW_TESTVIEW1 = "testView1";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "testView2",
        type = "screen",
        page = "component://cms/widget/CommonScreens.xml#404",
        controller = "backendsite"
    )
    public static final String VIEW_TESTVIEW2 = "testView2";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "testView3",
        type = "screen",
        page = "component://cms/widget/CommonScreens.xml#404",
        controller = "backendsite"
    )
    public static final String VIEW_TESTVIEW3 = "testView3";

    @Request(
        uri = "error",
        controller = "backendsite",
        secure = "true"
    )
    @Response(name = "success", type = "view", value = "pagenotfound")
    public interface Error {}

    @Request(
        uri = "pagenotfound",
        controller = "backendsite",
        secure = "true"
    )
    @Response(name = "success", type = "view", value = "pagenotfound")
    public interface Pagenotfound {}

    @Request(
        uri = "main",
        controller = "backendsite",
        secure = "true",
        auth = "true"
    )
    @Response(name = "success", type = "view", value = "main")
    public interface Main {}

    @Request(
        uri = "testRequest1",
        controller = "backendsite",
        secure = "true",
        auth = "true"
    )
    @Response(name = "success", type = "view", value = "testView1")
    public interface TestRequest1 {}

    @Request(
        uri = "testRequest2",
        controller = "backendsite",
        secure = "true",
        auth = "true"
    )
    @Response(name = "success", type = "view", value = "testView2")
    public interface TestRequest2 {}

    @Request(
        uri = "testRequest3",
        controller = "backendsite",
        secure = "true",
        auth = "true"
    )
    @Response(name = "success", type = "view", value = "testView3")
    public interface TestRequest3 {}

}
