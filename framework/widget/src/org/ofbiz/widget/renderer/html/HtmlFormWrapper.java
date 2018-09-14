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
package org.ofbiz.widget.renderer.html;

import java.io.IOException;
import java.io.StringWriter;
import java.util.HashMap;
import java.util.Map;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.xml.parsers.ParserConfigurationException;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilGenerics;
import org.ofbiz.base.util.UtilHttp;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.base.util.collections.MapStack;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.util.EntityUtilProperties;
import org.ofbiz.service.LocalDispatcher;
import org.ofbiz.widget.model.FormFactory;
import org.ofbiz.widget.model.ModelForm;
import org.ofbiz.widget.renderer.FormRenderer;
import org.ofbiz.widget.renderer.FormStringRenderer;
import org.ofbiz.widget.renderer.macro.MacroFormRenderer;
import org.xml.sax.SAXException;


/**
 * Widget Library - HTML Form Wrapper class - makes it easy to do the setup and render of a form
 * @deprecated SCIPIO: 2018: This class may not populate the context for the form renderer
 * correctly; use other form rendering facilities instead, such as
 * the Scipio Freemarker templating API <code>@render</code> macro (utilities.ftl).
 * <p>
 * SCIPIO: NOTE: 2016-09-15: This now renders using the Macro Freemarker renderer.
 */
@Deprecated
public class HtmlFormWrapper {

    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    protected String resourceName;
    protected String formName;
    protected HttpServletRequest request;
    protected HttpServletResponse response;
    protected ModelForm modelForm;
    protected FormStringRenderer renderer;
    protected Map<String, Object> context;
    /**
     * SCIPIO: For caller-specific context fields ({@link #putInContext(String, Object)}).
     */
    protected Map<String, Object> userContext = new HashMap<>();

    protected HtmlFormWrapper() {}

    public HtmlFormWrapper(String resourceName, String formName, HttpServletRequest request, HttpServletResponse response)
            throws IOException, SAXException, ParserConfigurationException {
        this.resourceName = resourceName;
        this.formName = formName;
        this.request = request;
        this.response = response;
        Delegator delegator = null;
        try {
            delegator = (Delegator) request.getAttribute("delegator");
            LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
            this.modelForm = FormFactory.getFormFromLocation(resourceName, formName, delegator.getModelReader(), dispatcher.getDispatchContext());
        } catch (IllegalArgumentException iae) {
            Debug.logWarning("Could not find form with name [" + formName + "] in class resource [" + resourceName + "], will try to load it using relative path syntax.", module);
            this.modelForm = FormFactory.getFormFromWebappContext(resourceName, formName, request);
        }

        // SCIPIO: 2016-09-15: use macro renderer, now available in request
        this.renderer = (FormStringRenderer) request.getAttribute("formStringRenderer");
        if (this.renderer == null) { // fallback (shouldn't happen)
            Debug.logWarning("No FormStringRenderer available in request - creating temporary html form renderer, but this may not work properly"
                    + " - please report this issue", module);
            try {
                this.renderer = new MacroFormRenderer(EntityUtilProperties.getPropertyValue("widget", "screen.name", delegator),
                        EntityUtilProperties.getPropertyValue("widget", "screen.formrenderer", delegator), request, response);
            } catch (Exception e) {
                throw new IOException("Could not create temporary MacroFormRenderer for HtmlFormWrapper", e);
            }
        }

        this.context = new HashMap<String, Object>();
        Map<String, Object> parameterMap = UtilHttp.getParameterMap(request);
        context.put("parameters", parameterMap);

        //make sure the locarenderFormStringle is in the context
        context.put("locale", UtilHttp.getLocale(request));
        //make sure the timeZone is in the context
        context.put("timeZone", UtilHttp.getTimeZone(request));

        // if there was an error message, this is an error
        if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_"))) {
            context.put("isError", Boolean.TRUE);
        } else {
            context.put("isError", Boolean.FALSE);
        }

        // if a parameter was passed saying this is an error, it is an error
        if ("true".equals(parameterMap.get("isError"))) {
            context.put("isError", Boolean.TRUE);
        }

        Map<String, String> uiLabelMap = UtilGenerics.cast(request.getAttribute("uiLabelMap"));
        if (UtilValidate.isNotEmpty(uiLabelMap) && context.get("uiLabelMap") == null) {
            Debug.logInfo("Got uiLabelMap: " + uiLabelMap, module);
            context.put("uiLabelMap", uiLabelMap);
        }
        if (UtilValidate.isNotEmpty(delegator) && context.get("delegator") == null) {
            context.put("delegator", delegator);
        }

        // SCIPIO: transfer the screens renderer object (REQUIRED)
        context.put("screens", request.getAttribute("screens"));
    }
    /**
     * renderFormString with unknown context type.
     * <p>
     * SCIPIO: NOTE: 2018-09-10: This method should never be called with a non MapStack context anymore,
     * and the context should always be the main render context. Any other context may suffer, such as the one
     * in this class's construction, may suffer from population issues and not guaranteed to work.
     */
    @SuppressWarnings("unchecked")
    public StringWriter renderFormString(Object contextStack) throws Exception {
        if (contextStack instanceof MapStack) {
            return renderFormString((MapStack<String>) contextStack);
        } else {
            Debug.logWarning("Call renderFormString with a non-MapStack: " + (contextStack == null ? "null" : contextStack.getClass().getName()), module);
            return renderFormString();
        }
    }
    /**
     * renderFormString with MapStack.
     * <p>
     * SCIPIO: NOTE: 2018-09-10: Although this whole class is deprecated by name, when this method is called with the
     * main render context passed, there will likely be no functional issues compared to using the Scipio Freemarker
     * templating API <code>@render</code> macro.
     */
    public StringWriter renderFormString(MapStack<String> contextStack) throws Exception {
        // create a new context with the current context on the bottom
        // SCIPIO: pushing this.context probably ruined the render context prior to this check
        // with variables from the constructor (though pushed, so did not wreck other renders)...
        if (contextStack.containsKey("delegator")) {
            contextStack.push(this.userContext);
        } else {
            Debug.logWarning("Scipio: Passed MapStack to HtmlFormWrapper.renderFormString contains no delegator"
                    + " - only the main render context (with delegator) should be passed to this method", module);
            contextStack.push(this.context);
        }
        StringWriter buffer = new StringWriter();
        FormRenderer formRenderer = new FormRenderer(modelForm, renderer);
        formRenderer.render(buffer, contextStack);
        contextStack.pop();
        return buffer;
    }
    /**
     * renderFormString using the context prepared and stored in this instance.
     * @deprecated SCIPIO: NOTE: 2018-09-10: This method uses a locally-built context
     * (from {@link #HtmlFormWrapper(String, String, HttpServletRequest, HttpServletResponse)})
     * which may contain insufficient data and the code for which may not be maintained.
     * It is recommended to simply replace usage of this class with Scipio Freemarker template API
     * utilities usage if possible; otherwise switch to {@link #renderFormString(MapStack)}
     * and push the context before adding your custom fields to it and pop it after the call.
     */
    @Deprecated
    public StringWriter renderFormString() throws Exception {
        Debug.logWarning("Scipio: Deprecated HtmlFormWrapper.renderFormString() call invoked; render context could be incomplete", module); // SCIPIO
        StringWriter buffer = new StringWriter();
        FormRenderer formRenderer = new FormRenderer(modelForm, renderer);
        formRenderer.render(buffer, context);
        return buffer;
    }

    /**
     * Tells the form library whether this is a response to an error or not.
     * Defaults on initialization according to the presense of an errorMessage
     * in the request or if an isError parameter was passed to the page with
     * the value "true". If true then the prefilled values will come from the
     * parameters Map instead of the value Map.
     * @deprecated SCIPIO: NOTE: 2018-09-10: This gets ignored by {@link #renderFormString(MapStack)}.
     */
    @Deprecated
    public void setIsError(boolean isError) {
        this.userContext.put("isError", Boolean.valueOf(isError)); // SCIPIO: callContext
    }

    /**
     * @deprecated SCIPIO: NOTE: 2018-09-10: this only reads from the locally-stored context, which should not be used.
     */
    @Deprecated
    public boolean getIsError() {
        Boolean isErrorBoolean = (Boolean) (userContext.containsKey("isError") ? userContext: context).get("isError"); // SCIPIO: callContext
        if (isErrorBoolean == null) {
            return false;
        } else {
            return isErrorBoolean.booleanValue();
        }
    }

    /**
     * The "useRequestParameters" value in the form context tells the form library
     * to use the request parameters to fill in values instead of the value map.
     * This is generally used when it is an empty form to pre-set inital values.
     * This is automatically set to false for list and multi forms. For related
     * functionality see the setIsError method.
     * @deprecated SCIPIO: NOTE: 2018-09-10: This gets ignored by {@link #renderFormString(MapStack)}.
     *
     * @param useRequestParameters
     */
    @Deprecated
    public void setUseRequestParameters(boolean useRequestParameters) {
        this.userContext.put("useRequestParameters", Boolean.valueOf(useRequestParameters)); // SCIPIO: callContext
    }

    /**
     * @deprecated SCIPIO: NOTE: 2018-09-10: this only reads from the locally-stored context, which should not be used.
     */
    @Deprecated
    public boolean getUseRequestParameters() {
        Boolean useRequestParametersBoolean = (Boolean) (userContext.containsKey("useRequestParameters") ? userContext: context).get("useRequestParameters"); // SCIPIO: callContext
        if (useRequestParametersBoolean == null) {
            return false;
        } else {
            return useRequestParametersBoolean.booleanValue();
        }
    }

    /**
     * @deprecated SCIPIO: NOTE: 2018-09-10: This method has NEVER worked correctly! Do not use!
     */
    @Deprecated
    public void setFormOverrideName(String formName) {
        Debug.logError("Scipio: Known broken stock HtmlFormWrapper method called, setFormOverrideName(string) - please fix or remove this call", module);
        this.userContext.put("formName", formName); // SCIPIO: callContext
    }

    public void putInContext(String name, Object value) {
        this.userContext.put(name, value); // SCIPIO: callContext
    }

    public Object getFromContext(String name) {
        return (userContext.containsKey(name) ? userContext: context).get(name); // SCIPIO: callContext
    }

    public ModelForm getModelForm() {
        return modelForm;
    }

    public FormStringRenderer getRenderer() {
        return renderer;
    }

    /**
     * @deprecated SCIPIO: 2018-09-10: Will give unpredictable results.
     */
    @Deprecated
    public void setRenderer(FormStringRenderer renderer) {
        this.renderer = renderer;
    }
}
