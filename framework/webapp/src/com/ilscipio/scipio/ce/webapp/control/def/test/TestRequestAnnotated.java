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
package com.ilscipio.scipio.ce.webapp.control.def.test;

import com.ilscipio.scipio.ce.webapp.control.def.Event;
import com.ilscipio.scipio.ce.webapp.control.def.EventProperty;
import com.ilscipio.scipio.ce.webapp.control.def.ParamToAttr;
import com.ilscipio.scipio.ce.webapp.control.def.RedirectParameter;
import com.ilscipio.scipio.ce.webapp.control.def.Request;
import com.ilscipio.scipio.ce.webapp.control.def.Response;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

/**
 * Test class-level @Request annotation with comprehensive features.
 *
 * <p>This demonstrates the complete annotation-based controller definition pattern including:</p>
 * <ul>
 * <li>Class-level @Request with security settings</li>
 * <li>Static @Response definitions for success/error routing</li>
 * <li>@Event with properties and param-to-attr mappings</li>
 * </ul>
 *
 * <p>Equivalent XML:</p>
 * <pre>
 * &lt;request-map uri="testAnnotatedRequest"&gt;
 *   &lt;description&gt;Test request defined via class-level @Request annotation&lt;/description&gt;
 *   &lt;security https="false" auth="false"/&gt;
 *   &lt;event type="java" path="...TestRequestAnnotated" invoke="handleRequest"&gt;
 *     &lt;param-to-attr name="productId" override="false"/&gt;
 *   &lt;/event&gt;
 *   &lt;response name="success" type="view" value="main"/&gt;
 *   &lt;response name="error" type="view" value="error"/&gt;
 * &lt;/request-map&gt;
 * </pre>
 *
 * <p>SCIPIO: 4.0.0: Added for controller annotations support testing.</p>
 */
@Request(
        uri = "testAnnotatedRequest",
        description = "Test request defined via class-level @Request annotation",
        auth = "false",
        secure = "false"
)
@Response(name = "success", type = "view", value = "main")
@Response(name = "error", type = "view", value = "error")
public class TestRequestAnnotated {

    /**
     * Event method for the test request with param-to-attr mapping.
     */
    @Event(
            paramToAttr = {
                    @ParamToAttr(name = "productId", override = false)
            }
    )
    public String handleRequest(HttpServletRequest request, HttpServletResponse response) {
        return "success";
    }
}

/**
 * Example of a service-based request using annotations.
 *
 * <p>Equivalent XML:</p>
 * <pre>
 * &lt;request-map uri="testServiceRequest"&gt;
 *   &lt;security https="true" auth="true"/&gt;
 *   &lt;event type="service" invoke="testService"/&gt;
 *   &lt;response name="success" type="request-redirect" value="main"&gt;
 *     &lt;redirect-parameter name="productId"/&gt;
 *   &lt;/response&gt;
 *   &lt;response name="error" type="view" value="error"/&gt;
 * &lt;/request-map&gt;
 * </pre>
 */
@Request(
        uri = "testServiceRequest",
        description = "Test service-based request",
        auth = "true",
        secure = "true"
)
@Response(name = "success", type = "request-redirect", value = "main",
        redirectParameters = {@RedirectParameter(name = "productId")})
@Response(name = "error", type = "view", value = "error")
class TestServiceRequestAnnotated {

    /**
     * Service event - invoke attribute specifies the service name.
     */
    @Event(type = "service", invoke = "testService")
    public void serviceEvent() {
        // This method body is not used - the service is invoked instead
    }
}

/**
 * Example of an async service request.
 *
 * <p>Equivalent XML:</p>
 * <pre>
 * &lt;request-map uri="testAsyncServiceRequest"&gt;
 *   &lt;event type="service" path="async" invoke="longRunningService"/&gt;
 *   &lt;response name="success" type="view" value="jobSubmitted"/&gt;
 * &lt;/request-map&gt;
 * </pre>
 */
@Request(uri = "testAsyncServiceRequest")
@Response(name = "success", type = "view", value = "jobSubmitted")
class TestAsyncServiceRequestAnnotated {

    @Event(type = "service", path = "async", invoke = "longRunningService")
    public void asyncServiceEvent() {
        // Async service invocation
    }
}
