/*******************************************************************************
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements.  See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership.  The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License.  You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations
 * under the License.
 *******************************************************************************/
package org.ofbiz.webtools.print;

import java.io.IOException;
import java.io.StringWriter;
import java.io.Writer;
import java.util.Locale;
import java.util.Map;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.commons.codec.binary.Base64;
import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.GeneralException;
import org.ofbiz.base.util.UtilHttp;
import org.ofbiz.base.util.UtilIO;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.entity.GenericEntityException;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.entity.util.EntityUtilProperties;
import org.ofbiz.service.DispatchContext;
import org.ofbiz.service.LocalDispatcher;
import org.ofbiz.widget.renderer.ScreenRenderer;
import org.ofbiz.widget.renderer.ScreenStringRenderer;
import org.ofbiz.widget.renderer.macro.MacroScreenRenderer;

/**
 * FoPrintServerEvents
 */

public class FoPrintServerEvents {

    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    public static String getXslFo(HttpServletRequest req, HttpServletResponse resp) {
        LocalDispatcher dispatcher = (LocalDispatcher) req.getAttribute("dispatcher");
        Map<String, Object> reqParams = UtilHttp.getParameterMap(req);
        reqParams.put("locale", UtilHttp.getLocale(req));

        String screenUri = (String) reqParams.remove("screenUri");
        if (UtilValidate.isNotEmpty(screenUri)) {
            String base64String = null;
            try {
                byte[] bytes = FoPrintServerEvents.getXslFo(dispatcher.getDispatchContext(), screenUri, reqParams);
                base64String = new String(Base64.encodeBase64(bytes), UtilIO.getUtf8()); // SCIPIO: UtilIO.getUtf8()
            } catch (GeneralException e) {
                Debug.logError(e, module);
                try {
                    resp.sendError(500);
                } catch (IOException e1) {
                    Debug.logError(e1, module);
                }
            }
            if (base64String != null) {
                try {
                    Writer out = resp.getWriter();
                    out.write(base64String);
                } catch (IOException e) {
                    try {
                        resp.sendError(500);
                    } catch (IOException e1) {
                        Debug.logError(e1, module);
                    }
                }
            }
        }

        return null;
    }

    public static byte[] getXslFo(DispatchContext dctx, String screen, Map<String, Object> parameters) throws GeneralException {
        // run as the system user
        GenericValue system = null;
        try {
            system = dctx.getDelegator().findOne("UserLogin", false, "userLoginId", "system");
        } catch (GenericEntityException e) {
            throw new GeneralException(e.getMessage(), e);
        }
        parameters.put("userLogin", system);
        if (!parameters.containsKey("locale")) {
            parameters.put("locale", Locale.getDefault());
        }

        // render and obtain the XSL-FO
        Writer writer = new StringWriter();
        try {
            // SCIPIO: NOTE: 2018-09-06: In old ofbiz code this used HTML renderer, can't find a reason why, but seems illogical.
            //ScreenStringRenderer screenStringRenderer = new MacroScreenRenderer(EntityUtilProperties.getPropertyValue("widget", "screen.name", dctx.getDelegator()),
            //        EntityUtilProperties.getPropertyValue("widget", "screen.screenrenderer", dctx.getDelegator()));
            ScreenStringRenderer screenStringRenderer = new MacroScreenRenderer(EntityUtilProperties.getPropertyValue("widget", "screenfop.name", dctx.getDelegator()),
                    EntityUtilProperties.getPropertyValue("widget", "screenfop.screenrenderer", dctx.getDelegator()));
            ScreenRenderer screens = ScreenRenderer.makeWithEnvAwareFetching(writer, null, screenStringRenderer);
            screens.populateContextForService(dctx, parameters);
            screens.render(screen);
        } catch (Throwable t) {
            throw new GeneralException("Problems rendering FOP XSL-FO", t);
        }
        return writer.toString().getBytes(UtilIO.getUtf8()); // SCIPIO: UtilIO.getUtf8()
    }
}
