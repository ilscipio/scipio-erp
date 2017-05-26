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
package org.ofbiz.widget.renderer.macro;

import java.io.IOException;
import java.io.Reader;
import java.io.StringReader;
import java.io.StringWriter;
import java.rmi.server.UID;
import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Random;
import java.util.Map.Entry;
import java.util.Set;
import java.util.WeakHashMap;

import javax.servlet.ServletContext;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.StringUtil;
import org.ofbiz.base.util.UtilCodec;
import org.ofbiz.base.util.UtilFormatOut;
import org.ofbiz.base.util.UtilGenerics;
import org.ofbiz.base.util.UtilHttp;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.base.util.UtilProperties;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.base.util.string.FlexibleStringExpander;
import org.ofbiz.base.util.template.FreeMarkerWorker;
import org.ofbiz.base.util.template.FtlScriptFormatter;
import org.ofbiz.entity.Delegator;
import org.ofbiz.webapp.control.RequestHandler;
import org.ofbiz.webapp.taglib.ContentUrlTag;
import org.ofbiz.widget.WidgetWorker;
import org.ofbiz.widget.model.CommonWidgetModels;
import org.ofbiz.widget.model.FieldInfo;
import org.ofbiz.widget.model.ModelForm;
import org.ofbiz.widget.model.ModelFormField;
import org.ofbiz.widget.model.ModelFormField.CheckField;
import org.ofbiz.widget.model.ModelFormField.ContainerField;
import org.ofbiz.widget.model.ModelFormField.DateFindField;
import org.ofbiz.widget.model.ModelFormField.DateTimeField;
import org.ofbiz.widget.model.ModelFormField.DisplayEntityField;
import org.ofbiz.widget.model.ModelFormField.DisplayField;
import org.ofbiz.widget.model.ModelFormField.DropDownField;
import org.ofbiz.widget.model.ModelFormField.FieldInfoWithOptions;
import org.ofbiz.widget.model.ModelFormField.FileField;
import org.ofbiz.widget.model.ModelFormField.HiddenField;
import org.ofbiz.widget.model.ModelFormField.HyperlinkField;
import org.ofbiz.widget.model.ModelFormField.IgnoredField;
import org.ofbiz.widget.model.ModelFormField.ImageField;
import org.ofbiz.widget.model.ModelFormField.LookupField;
import org.ofbiz.widget.model.ModelFormField.PasswordField;
import org.ofbiz.widget.model.ModelFormField.RadioField;
import org.ofbiz.widget.model.ModelFormField.RangeFindField;
import org.ofbiz.widget.model.ModelFormField.ResetField;
import org.ofbiz.widget.model.ModelFormField.SubmitField;
import org.ofbiz.widget.model.ModelFormField.TextField;
import org.ofbiz.widget.model.ModelFormField.TextFindField;
import org.ofbiz.widget.model.ModelFormField.TextareaField;
import org.ofbiz.widget.model.ModelFormFieldBuilder;
import org.ofbiz.widget.model.ModelPageScript;
import org.ofbiz.widget.model.ModelScreenWidget;
import org.ofbiz.widget.model.ModelSingleForm;
import org.ofbiz.widget.model.ModelWidget;
import org.ofbiz.widget.renderer.FormRenderer;
import org.ofbiz.widget.renderer.FormStringRenderer;
import org.ofbiz.widget.renderer.Paginator;
import org.ofbiz.widget.renderer.UtilHelpText;

import com.ibm.icu.util.Calendar;
import com.ilscipio.scipio.ce.webapp.ftl.context.ContextFtlUtil;
import com.ilscipio.scipio.ce.webapp.ftl.lang.LangFtlUtil;

import freemarker.core.Environment;
import freemarker.template.Template;
import freemarker.template.TemplateException;
import freemarker.template.TemplateModelException;

/**
 * Widget Library - Form Renderer implementation based on Freemarker macros
 *
 */
public final class MacroFormRenderer implements FormStringRenderer {

    public static final String module = MacroFormRenderer.class.getName();
    private final Template macroLibrary;
    private final WeakHashMap<Appendable, Environment> environments = new WeakHashMap<Appendable, Environment>();
    //private final UtilCodec.SimpleEncoder internalEncoder; // SCIPIO: better off without this
    private final RequestHandler rh;
    private final HttpServletRequest request;
    private final HttpServletResponse response;
    private final boolean javaScriptEnabled;
    private boolean renderPagination = true;
    private boolean widgetCommentsEnabled = false;

    // SCIPIO: new
    private final FtlScriptFormatter ftlFmt = new FtlScriptFormatter();
    private ContextHandler contextHandler = new ContextHandler("form");
    private final String rendererName;
    
    /**
     * Constructor.
     * <p>
     * SCIPIO: modified to require name.
     */
    public MacroFormRenderer(String name, String macroLibraryPath, HttpServletRequest request, HttpServletResponse response) throws TemplateException, IOException {
        // SCIPIO: use abstracted template build
        this.macroLibrary = MacroScreenRenderer.getTemplate(name, macroLibraryPath);
        this.request = request;
        this.response = response;
        ServletContext ctx = (ServletContext) request.getAttribute("servletContext");
        this.rh = (RequestHandler) ctx.getAttribute("_REQUEST_HANDLER_");
        this.javaScriptEnabled = UtilHttp.isJavaScriptEnabled(request);
        // SCIPIO: better off without this
        //internalEncoder = UtilCodec.getEncoder("string");
        this.rendererName = name; // SCIPIO: new
    }

    /**
     * Constructor.
     * <p>
     * SCIPIO: modified to require rendererName.
     */
    @Deprecated
    public MacroFormRenderer(String name, String macroLibraryPath, Appendable writer, HttpServletRequest request, HttpServletResponse response) throws TemplateException, IOException {
        this(name, macroLibraryPath, request, response);
    }

    /**
     * SCIPIO: Returns macro library path used for this renderer. 
     */
    public String getMacroLibraryPath() {
        return macroLibrary.getName();
    }
    
    /**
     * SCIPIO: Returns the renderer name (html, xml, etc.).
     */
    public String getRendererName() {
        return rendererName;
    }
    
    public boolean getRenderPagination() {
        return this.renderPagination;
    }

    public void setRenderPagination(boolean renderPagination) {
        this.renderPagination = renderPagination;
    }

    private void executeMacro(Appendable writer, String macro) throws IOException {
        try {
            Environment environment = getEnvironment(writer);
            Reader templateReader = new StringReader(macro);
            Template template = new Template(new UID().toString(), templateReader, FreeMarkerWorker.getDefaultOfbizConfig());
            templateReader.close();
            FreeMarkerWorker.includeTemplate(template, environment);
        } catch (TemplateException e) {
            Debug.logError(e, "Error rendering screen thru ftl macro: " + macro, module);
            handleError(writer, e); // SCIPIO
        } catch (IOException e) {
            Debug.logError(e, "Error rendering screen thru ftl, macro: " + macro, module);
            handleError(writer, e); // SCIPIO
        }
    }

    /**
     * SCIPIO: makes exception handling decision for executeMacro exceptions.
     */
    private void handleError(Appendable writer, Throwable t) throws IOException, RuntimeException {
        MacroScreenRenderer.handleError(writer, contextHandler.getInitialContext(writer), t);
    }
    
    private Environment getEnvironment(Appendable writer) throws TemplateException, IOException {
        Environment environment = environments.get(writer);
        if (environment == null) {
            // SCIPIO: custom render context
            Map<String, Object> input = contextHandler.createRenderContext(writer, null, UtilMisc.toMap("key", null));
            environment = FreeMarkerWorker.renderTemplate(macroLibrary, input, writer);
            environments.put(writer, environment);
        }
        return environment;
    }
    
    private String encode(String value, ModelFormField modelFormField, Map<String, Object> context) {
        if (UtilValidate.isEmpty(value)) {
            return value;
        }
        // SCIPIO: simplified
        // NOTE: 2016-08-30: this and most other calls have been changed to use early encoder, which can then be disabled in widget.properties.
        UtilCodec.SimpleEncoder encoder = WidgetWorker.getEarlyEncoder(context);
        if (modelFormField.getEncodeOutput()) { // && encoder != null
            value = encoder.encode(value);
        //} else {
        //    value = internalEncoder.encode(value);
        }
        return value;
    }

    public void renderLabel(Appendable writer, Map<String, Object> context, ModelScreenWidget.Label label) throws IOException {
        String labelText = label.getText(context);
        if (UtilValidate.isEmpty(labelText)) {
            // nothing to render
            return;
        }
        StringWriter sr = new StringWriter();
        sr.append("<@renderLabel ");
        sr.append("text=");
        sr.append(ftlFmt.makeStringLiteral(labelText));
        sr.append(" />");
        executeMacro(writer, sr.toString());
    }

    public void renderDisplayField(Appendable writer, Map<String, Object> context, DisplayField displayField) throws IOException {
        ModelFormField modelFormField = displayField.getModelFormField();
        String idName = modelFormField.getCurrentContainerId(context);
        String description = displayField.getDescription(context);
        String type = displayField.getType();
        String imageLocation = displayField.getImageLocation(context);
        Integer size = Integer.valueOf("0");
        String title = "";
        if (UtilValidate.isNotEmpty(displayField.getSize())) {
            try {
                size = Integer.parseInt(displayField.getSize());
            } catch (NumberFormatException nfe) {
                Debug.logError(nfe, "Error reading size of a field fieldName=" + displayField.getModelFormField().getFieldName() + " FormName= " + displayField.getModelFormField().getModelForm().getName(), module);
                handleError(writer, nfe); // SCIPIO
            }
        }
        ModelFormField.InPlaceEditor inPlaceEditor = displayField.getInPlaceEditor();
        boolean ajaxEnabled = inPlaceEditor != null && this.javaScriptEnabled;
        if (UtilValidate.isNotEmpty(description) && size > 0 && description.length() > size) {
            title = description;
            description = description.substring(0, size - 8) + "..." + description.substring(description.length() - 5);
        }
        StringWriter sr = new StringWriter();
        sr.append("<@renderDisplayField ");
        appendFieldInfo(sr, context, modelFormField);
        sr.append("type=");
        sr.append(ftlFmt.makeStringLiteral(type));
        sr.append(" imageLocation=");
        sr.append(ftlFmt.makeStringLiteral(imageLocation));
        sr.append(" idName=");
        sr.append(ftlFmt.makeStringLiteral(idName));
        sr.append(" description=");
        sr.append(ftlFmt.makeStringLiteral(description)); // SCIPIO: redundant: FreeMarkerWorker.encodeDoubleQuotes(description)
        sr.append(" title=");
        sr.append(ftlFmt.makeStringLiteral(title));
        sr.append(" class=");
        sr.append(ftlFmt.makeStringLiteral(modelFormField.getWidgetStyle(context)));
        sr.append(" alert=");
        sr.append(ftlFmt.makeStringLiteral(modelFormField.shouldBeRed(context)));
        if (ajaxEnabled) {
            String url = inPlaceEditor.getUrl(context);
            // SCIPIO: FIXME?: the javascript string values should probably be escaped for javascript syntax
            String extraParameter = "{";
            Map<String, Object> fieldMap = inPlaceEditor.getFieldMap(context);
            if (fieldMap != null) {
                Set<Entry<String, Object>> fieldSet = fieldMap.entrySet();
                Iterator<Entry<String, Object>> fieldIterator = fieldSet.iterator();
                int count = 0;
                while (fieldIterator.hasNext()) {
                    count++;
                    Entry<String, Object> field = fieldIterator.next();
                    extraParameter += field.getKey() + ":'" + (String) field.getValue() + "'";
                    if (count < fieldSet.size()) {
                        extraParameter += ',';
                    }
                }

            }
            extraParameter += "}";
            sr.append(" inPlaceEditorUrl=");
            sr.append(ftlFmt.makeStringLiteral(url));
            sr.append(" inPlaceEditorParams=");
            StringWriter inPlaceEditorParams = new StringWriter();
            inPlaceEditorParams.append("{name: '");
            if (UtilValidate.isNotEmpty(inPlaceEditor.getParamName())) {
                inPlaceEditorParams.append(inPlaceEditor.getParamName());
            } else {
                inPlaceEditorParams.append(modelFormField.getFieldName());
            }
            inPlaceEditorParams.append("'");
            inPlaceEditorParams.append(", method: 'POST'");
            inPlaceEditorParams.append(", submitdata: " + extraParameter);
            inPlaceEditorParams.append(", type: 'textarea'");
            inPlaceEditorParams.append(", select: 'true'");
            inPlaceEditorParams.append(", onreset: function(){jQuery('#cc_" + idName + "').css('background-color', 'transparent');}");
            if (UtilValidate.isNotEmpty(inPlaceEditor.getCancelText())) {
                inPlaceEditorParams.append(", cancel: '" + inPlaceEditor.getCancelText() + "'");
            } else {
                inPlaceEditorParams.append(", cancel: 'Cancel'");
            }
            if (UtilValidate.isNotEmpty(inPlaceEditor.getClickToEditText())) {
                inPlaceEditorParams.append(", tooltip: '" + inPlaceEditor.getClickToEditText() + "'");
            }
            if (UtilValidate.isNotEmpty(inPlaceEditor.getFormClassName())) {
                inPlaceEditorParams.append(", cssclass: '" + inPlaceEditor.getFormClassName() + "'");
            } else {
                inPlaceEditorParams.append(", cssclass: 'inplaceeditor-form'");
            }
            if (UtilValidate.isNotEmpty(inPlaceEditor.getLoadingText())) {
                inPlaceEditorParams.append(", indicator: '" + inPlaceEditor.getLoadingText() + "'");
            }
            if (UtilValidate.isNotEmpty(inPlaceEditor.getOkControl())) {
                inPlaceEditorParams.append(", submit: ");
                if (!"false".equals(inPlaceEditor.getOkControl())) {
                    inPlaceEditorParams.append("'");
                }
                inPlaceEditorParams.append(inPlaceEditor.getOkControl());
                if (!"false".equals(inPlaceEditor.getOkControl())) {
                    inPlaceEditorParams.append("'");
                }
            } else {
                inPlaceEditorParams.append(", submit: 'OK'");
            }
            if (UtilValidate.isNotEmpty(inPlaceEditor.getRows())) {
                inPlaceEditorParams.append(", rows: '" + inPlaceEditor.getRows() + "'");
            }
            if (UtilValidate.isNotEmpty(inPlaceEditor.getCols())) {
                inPlaceEditorParams.append(", cols: '" + inPlaceEditor.getCols() + "'");
            }
            inPlaceEditorParams.append("}");
            sr.append(ftlFmt.makeStringLiteral(inPlaceEditorParams.toString()));
        }
        appendRequiredFieldParam(sr, context, modelFormField);
        sr.append(" />");
        executeMacro(writer, sr.toString());
        if (displayField instanceof DisplayEntityField) {
            makeHyperlinkString(writer, ((DisplayEntityField) displayField).getSubHyperlink(), context);
        }
        this.appendTooltip(writer, context, modelFormField);
    }

    public void renderHyperlinkField(Appendable writer, Map<String, Object> context, HyperlinkField hyperlinkField) throws IOException {
        this.request.setAttribute("image", hyperlinkField.getImageLocation(context));
        ModelFormField modelFormField = hyperlinkField.getModelFormField();
        String encodedAlternate = encode(hyperlinkField.getAlternate(context), modelFormField, context);
        String encodedImageTitle = encode(hyperlinkField.getImageTitle(context), modelFormField, context);
        this.request.setAttribute("alternate", encodedAlternate);
        this.request.setAttribute("imageTitle", encodedImageTitle);
        this.request.setAttribute("descriptionSize", hyperlinkField.getSize());
        makeHyperlinkByType(writer, hyperlinkField.getLinkType(), modelFormField.getWidgetStyle(context), hyperlinkField.getUrlMode(), hyperlinkField.getTarget(context), 
                hyperlinkField.getParameterMap(context, modelFormField.getEntityName(), modelFormField.getServiceName()), hyperlinkField.getDescription(context), hyperlinkField.getTargetWindow(context),
                hyperlinkField.getConfirmation(context), modelFormField, this.request, this.response, context);
        this.appendTooltip(writer, context, modelFormField);
        this.request.removeAttribute("image");
        this.request.removeAttribute("descriptionSize");
    }

    public void renderTextField(Appendable writer, Map<String, Object> context, TextField textField) throws IOException {
        ModelFormField modelFormField = textField.getModelFormField();
        String name = modelFormField.getParameterName(context);
        String className = "";
        String alert = "false";
        String mask = "";
        String placeholder = textField.getPlaceholder(context);
        if (UtilValidate.isNotEmpty(modelFormField.getWidgetStyle(context))) {
            className = modelFormField.getWidgetStyle(context);
            if (modelFormField.shouldBeRed(context)) {
                alert = "true";
            }
        }
        String value = modelFormField.getEntry(context, textField.getDefaultValue(context));
        String textSize = Integer.toString(textField.getSize());
        String maxlength = "";
        if (textField.getMaxlength() != null) {
            maxlength = Integer.toString(textField.getMaxlength());
        }
        String event = modelFormField.getEvent();
        String action = modelFormField.getAction(context);
        String id = modelFormField.getCurrentContainerId(context);
        String clientAutocomplete = "false";
        //check for required field style on single forms
        if ("single".equals(modelFormField.getModelForm().getType()) && modelFormField.getRequiredField()) {
            className = combineRequiredStyle(className, context, modelFormField); // SCIPIO
        }
        List<ModelForm.UpdateArea> updateAreas = modelFormField.getOnChangeUpdateAreas();
        boolean ajaxEnabled = updateAreas != null && this.javaScriptEnabled;
        if (textField.getClientAutocompleteField() || ajaxEnabled) {
            clientAutocomplete = "true";
        }
        if (UtilValidate.isNotEmpty(textField.getMask())) {
            mask = textField.getMask();
        }
        String tooltip = modelFormField.getTooltip(context); // SCIPIO: new arg
        if (UtilValidate.isEmpty(tooltip)) {
            tooltip = "";
        }
        String ajaxUrl = createAjaxParamsFromUpdateAreas(updateAreas, "", context);
        boolean disabled = textField.getDisabled();
        StringWriter sr = new StringWriter();
        sr.append("<@renderTextField ");
        appendFieldInfo(sr, context, modelFormField);
        sr.append("name=");
        sr.append(ftlFmt.makeStringLiteral(name));
        sr.append(" className=");
        sr.append(ftlFmt.makeStringLiteral(className));
        sr.append(" alert=");
        sr.append(ftlFmt.makeStringLiteral(alert));
        sr.append(" value=");
        sr.append(ftlFmt.makeStringLiteral(value));
        sr.append(" textSize=");
        sr.append(ftlFmt.makeStringLiteral(textSize));
        sr.append(" maxlength=");
        sr.append(ftlFmt.makeStringLiteral(maxlength));
        sr.append(" id=");
        sr.append(ftlFmt.makeStringLiteral(id));
        sr.append(" event=");
        sr.append(ftlFmt.makeStringLiteral(event));
        sr.append(" action=");
        sr.append(ftlFmt.makeStringLiteral(action));
        sr.append(" disabled=");
        sr.append(Boolean.toString(disabled));
        sr.append(" clientAutocomplete=");
        sr.append(ftlFmt.makeStringLiteral(clientAutocomplete));
        sr.append(" ajaxUrl=");
        sr.append(ftlFmt.makeStringLiteral(ajaxUrl));
        sr.append(" ajaxEnabled=");
        sr.append(Boolean.toString(ajaxEnabled));
        sr.append(" mask=");
        sr.append(ftlFmt.makeStringLiteral(mask));
        sr.append(" placeholder=");
        sr.append(ftlFmt.makeStringLiteral(placeholder));
        sr.append(" tooltip="); // SCIPIO: new arg
        sr.append(ftlFmt.makeStringLiteral(tooltip));
        appendRequiredFieldParam(sr, context, modelFormField);
        sr.append(" />");
        executeMacro(writer, sr.toString());
        ModelFormField.SubHyperlink subHyperlink = textField.getSubHyperlink();
        if (subHyperlink != null && subHyperlink.shouldUse(context)) {
            makeHyperlinkString(writer, subHyperlink, context);
        }
        this.addAsterisks(writer, context, modelFormField);
        this.appendTooltip(writer, context, modelFormField);
    }

    public void renderTextareaField(Appendable writer, Map<String, Object> context, TextareaField textareaField) throws IOException {
        ModelFormField modelFormField = textareaField.getModelFormField();
        String name = modelFormField.getParameterName(context);
        String cols = Integer.toString(textareaField.getCols());
        String rows = Integer.toString(textareaField.getRows());
        String id = modelFormField.getCurrentContainerId(context);
        String className = "";
        String alert = "false";
        String maxlength = ""; // SCIPIO: new
        if (UtilValidate.isNotEmpty(modelFormField.getWidgetStyle(context))) {
            className = modelFormField.getWidgetStyle(context);
            if (modelFormField.shouldBeRed(context)) {
                alert = "true";
            }
        }
        //check for required field style on single forms
        if ("single".equals(modelFormField.getModelForm().getType()) && modelFormField.getRequiredField()) {
            className = combineRequiredStyle(className, context, modelFormField); // SCIPIO
        }
        String visualEditorEnable = "";
        String buttons = "";
        if (textareaField.getVisualEditorEnable()) {
            visualEditorEnable = "true";
            buttons = textareaField.getVisualEditorButtons(context);
            if (UtilValidate.isEmpty(buttons)) {
                buttons = "maxi";
            }
        }
        String readonly = "";
        if (textareaField.isReadOnly()) {
            readonly = "readonly";
        }
        Map<String, Object> userLogin = UtilGenerics.checkMap(context.get("userLogin"));
        String language = "en";
        if (userLogin != null) {
            language = UtilValidate.isEmpty((String) userLogin.get("lastLocale")) ? "en" : (String) userLogin.get("lastLocale");
        }
        if (textareaField.getMaxlength() != null) {
            maxlength = Integer.toString(textareaField.getMaxlength());
        }
        String value = modelFormField.getEntry(context, textareaField.getDefaultValue(context));
        StringWriter sr = new StringWriter();
        sr.append("<@renderTextareaField ");
        appendFieldInfo(sr, context, modelFormField);
        sr.append("name=");
        sr.append(ftlFmt.makeStringLiteral(name));
        sr.append(" className=");
        sr.append(ftlFmt.makeStringLiteral(className));
        sr.append(" alert=");
        sr.append(ftlFmt.makeStringLiteral(alert));
        sr.append(" value=");
        sr.append(ftlFmt.makeStringLiteral(value));
        sr.append(" cols=");
        sr.append(ftlFmt.makeStringLiteral(cols));
        sr.append(" rows=");
        sr.append(ftlFmt.makeStringLiteral(rows));
        sr.append(" id=");
        sr.append(ftlFmt.makeStringLiteral(id));
        sr.append(" readonly=");
        sr.append(ftlFmt.makeStringLiteral(readonly));
        sr.append(" visualEditorEnable=");
        sr.append(ftlFmt.makeStringLiteral(visualEditorEnable));
        sr.append(" language=");
        sr.append(ftlFmt.makeStringLiteral(language));
        sr.append(" buttons=");
        sr.append(ftlFmt.makeStringLiteral(buttons));
        // SCIPIO: maxlength added
        sr.append(" maxlength=");
        sr.append(ftlFmt.makeStringLiteral(maxlength));
        appendRequiredFieldParam(sr, context, modelFormField);
        sr.append(" />");
        executeMacro(writer, sr.toString());
        this.addAsterisks(writer, context, modelFormField);
        this.appendTooltip(writer, context, modelFormField);
    }

    public void renderDateTimeField(Appendable writer, Map<String, Object> context, DateTimeField dateTimeField) throws IOException {
        // SCIPIO: NOTE: for compatibility only: this registerContext SHOULD not be needed but legacy code called directly
        contextHandler.registerContext(writer, context);
        ModelFormField modelFormField = dateTimeField.getModelFormField();
        String paramName = modelFormField.getParameterName(context);
        String defaultDateTimeString = dateTimeField.getDefaultDateTimeString(context);
        String className = "";
        String alert = "false";
        String name = "";
        String formattedMask = "";
        String event = modelFormField.getEvent();
        String action = modelFormField.getAction(context);
        if (UtilValidate.isNotEmpty(modelFormField.getWidgetStyle(context))) {
            className = modelFormField.getWidgetStyle(context);
            if (modelFormField.shouldBeRed(context)) {
                alert = "true";
            }
        }
        boolean useTimeDropDown = "time-dropdown".equals(dateTimeField.getInputMethod());
        String stepString = dateTimeField.getStep();
        int step = 1;
        StringBuilder timeValues = new StringBuilder();
        if (useTimeDropDown && UtilValidate.isNotEmpty(step)) {
            try {
                step = Integer.valueOf(stepString).intValue();
            } catch (IllegalArgumentException e) {
                Debug.logWarning("Invalid value for step property for field[" + paramName + "] with input-method=\"time-dropdown\" " + " Found Value [" + stepString + "]  " + e.getMessage(), module);
            }
            timeValues.append("[");
            for (int i = 0; i <= 59;) {
                if (i != 0) {
                    timeValues.append(", ");
                }
                timeValues.append(i);
                i += step;
            }
            timeValues.append("]");
        }
        Map<String, String> uiLabelMap = UtilGenerics.checkMap(context.get("uiLabelMap"));
        if (uiLabelMap == null) {
            Debug.logWarning("Could not find uiLabelMap in context", module);
        }
        String localizedInputTitle = "", localizedIconTitle = "";
        // whether the date field is short form, yyyy-mm-dd
        boolean shortDateInput = ("date".equals(dateTimeField.getType()) || useTimeDropDown ? true : false);
        if (useTimeDropDown) {
            name = UtilHttp.makeCompositeParam(paramName, "date");
        } else {
            name = paramName;
        }
        // the default values for a timestamp
        int size = 25;
        int maxlength = 30;
        if (shortDateInput) {
            size = maxlength = 10;
            if (uiLabelMap != null) {
                localizedInputTitle = uiLabelMap.get("CommonFormatDate");
            }
        } else if ("time".equals(dateTimeField.getType())) {
            size = maxlength = 8;
            if (uiLabelMap != null) {
                localizedInputTitle = uiLabelMap.get("CommonFormatTime");
            }
        } else {
            if (uiLabelMap != null) {
                localizedInputTitle = uiLabelMap.get("CommonFormatDateTime");
            }
        }
        /*
         * FIXME: Using a builder here is a hack. Replace the builder with appropriate code.
         */
        ModelFormFieldBuilder builder = new ModelFormFieldBuilder(modelFormField);
        boolean memEncodeOutput = modelFormField.getEncodeOutput();
        if (useTimeDropDown)
            // If time-dropdown deactivate encodingOutput for found hour and minutes
            // FIXME: Encoding should be controlled by the renderer, not by the model.
            builder.setEncodeOutput(false);
        // FIXME: modelFormField.getEntry ignores shortDateInput when converting Date objects to Strings.
        if (useTimeDropDown) {
            builder.setEncodeOutput(memEncodeOutput);
        }
        modelFormField = builder.build();
        
        // SCIPIO: any escaping must be done AFTER max length concat
        // TODO: review this entire encoding stuff
        //String contextValue = modelFormField.getEntry(context, dateTimeField.getDefaultValue(context));
        //String value = contextValue;
        //if (UtilValidate.isNotEmpty(value)) {
        //    if (value.length() > maxlength) {
        //        value = value.substring(0, maxlength);
        //    }
        //}
        String contextValue = modelFormField.getEntryRaw(context, dateTimeField.getDefaultValue(context));
        String value = contextValue;
        if (UtilValidate.isNotEmpty(value)) {
            if (value.length() > maxlength) {
                value = value.substring(0, maxlength);
            }
            // SCIPIO: NOW do escaping TODO: review encoding in general
            value = WidgetWorker.getEarlyEncoder(context).encode(value); // SCIPIO: simplified
        }
        
        String id = modelFormField.getCurrentContainerId(context);
        ModelForm modelForm = modelFormField.getModelForm();
        String formName = FormRenderer.getCurrentFormName(modelForm, context);
        String timeDropdown = dateTimeField.getInputMethod();
        String timeDropdownParamName = "";
        String classString = "";
        boolean isTwelveHour = false;
        String timeHourName = "";
        int hour2 = 0, hour1 = 0, minutes = 0;
        String timeMinutesName = "";
        String amSelected = "", pmSelected = "", ampmName = "";
        String compositeType = "";
        // search for a localized label for the icon
        if (uiLabelMap != null) {
            localizedIconTitle = uiLabelMap.get("CommonViewCalendar");
        }
        if (!"time".equals(dateTimeField.getType())) {
            String tempParamName;
            if (useTimeDropDown) {
                tempParamName = UtilHttp.makeCompositeParam(paramName, "date");
            } else {
                tempParamName = paramName;
            }
            timeDropdownParamName = tempParamName;
            defaultDateTimeString = UtilHttp.encodeBlanks(modelFormField.getEntry(context, defaultDateTimeString));
        }
        // if we have an input method of time-dropdown, then render two
        // dropdowns
        if (useTimeDropDown) {
            className = modelFormField.getWidgetStyle(context);
            classString = (className != null ? className : "");
            isTwelveHour = "12".equals(dateTimeField.getClock());
            // set the Calendar to the default time of the form or now()
            Calendar cal = null;
            // SCIPIO: WORKAROUND: the default date may be in short date format; extend if so
            String timeDropDownValue = contextValue;
            if (timeDropDownValue != null && timeDropDownValue.length() == 10) {
                timeDropDownValue += " 00:00:00.000";
            }
            // SCIPIO: don't print warn if empty
            if (UtilValidate.isNotEmpty(timeDropDownValue)){
                try {
                    Timestamp defaultTimestamp = Timestamp.valueOf(timeDropDownValue);
                    cal = Calendar.getInstance();
                    cal.setTime(defaultTimestamp);
                } catch (IllegalArgumentException e) {
                    Debug.logWarning("Form widget field [" + paramName + "] with input-method=\"time-dropdown\" was not able to understand the default time [" + 
                            timeDropDownValue + "]. The parsing error was: " + e.getMessage(), module);
                }
            }
            timeHourName = UtilHttp.makeCompositeParam(paramName, "hour");
            if (cal != null) {
                int hour = cal.get(Calendar.HOUR_OF_DAY);
                hour2 = hour;
                if (hour == 0) {
                    hour = 12;
                }
                if (hour > 12) {
                    hour -= 12;
                }
                hour1 = hour;
                minutes = cal.get(Calendar.MINUTE);
            }
            timeMinutesName = UtilHttp.makeCompositeParam(paramName, "minutes");
            compositeType = UtilHttp.makeCompositeParam(paramName, "compositeType");
            // if 12 hour clock, write the AM/PM selector
            if (isTwelveHour) {
                amSelected = ((cal != null && cal.get(Calendar.AM_PM) == Calendar.AM) ? "selected" : "");
                pmSelected = ((cal != null && cal.get(Calendar.AM_PM) == Calendar.PM) ? "selected" : "");
                ampmName = UtilHttp.makeCompositeParam(paramName, "ampm");
            }
        }
        //check for required field style on single forms
        if ("single".equals(modelFormField.getModelForm().getType()) && modelFormField.getRequiredField()) {
            className = combineRequiredStyle(className, context, modelFormField); // SCIPIO
        }
        String mask = dateTimeField.getMask();
        if ("Y".equals(mask)) {
            if ("date".equals(dateTimeField.getType())) {
                formattedMask = "9999-99-99";
            } else if ("time".equals(dateTimeField.getType())) {
                formattedMask = "99:99:99";
            } else if ("timestamp".equals(dateTimeField.getType())) {
                formattedMask = "9999-99-99 99:99:99";
            }
        }
        StringWriter sr = new StringWriter();
        sr.append("<@renderDateTimeField ");
        appendFieldInfo(sr, context, modelFormField);
        sr.append("name=");
        sr.append(ftlFmt.makeStringLiteral(name));
        sr.append(" className=");
        sr.append(ftlFmt.makeStringLiteral(className));
        sr.append(" alert=");
        sr.append(ftlFmt.makeStringLiteral(alert));
        sr.append(" value=");
        sr.append(ftlFmt.makeStringLiteral(value));
        sr.append(" title=");
        sr.append(ftlFmt.makeStringLiteral(localizedInputTitle));
        sr.append(" size=");
        sr.append(ftlFmt.makeStringLiteral(size));
        sr.append(" maxlength=");
        sr.append(ftlFmt.makeStringLiteral(maxlength));
        sr.append(" step=");
        sr.append(ftlFmt.makeStringLiteral(step));
        sr.append(" timeValues=\"");
        sr.append(timeValues.toString());
        sr.append("\" id=");
        sr.append(ftlFmt.makeStringLiteral(id));
        sr.append(" event=");
        sr.append(ftlFmt.makeStringLiteral(event));
        sr.append(" action=");
        sr.append(ftlFmt.makeStringLiteral(action));
        sr.append(" dateType=");
        sr.append(ftlFmt.makeStringLiteral(dateTimeField.getType()));
        sr.append(" shortDateInput=");
        sr.append(Boolean.toString(shortDateInput));
        sr.append(" timeDropdownParamName=");
        sr.append(ftlFmt.makeStringLiteral(timeDropdownParamName));
        sr.append(" defaultDateTimeString=");
        sr.append(ftlFmt.makeStringLiteral(defaultDateTimeString));
        sr.append(" localizedIconTitle=");
        sr.append(ftlFmt.makeStringLiteral(localizedIconTitle));
        sr.append(" timeDropdown=");
        sr.append(ftlFmt.makeStringLiteral(timeDropdown));
        sr.append(" timeHourName=");
        sr.append(ftlFmt.makeStringLiteral(timeHourName));
        sr.append(" classString=");
        sr.append(ftlFmt.makeStringLiteral(classString));
        sr.append(" hour1=");
        sr.append(Integer.toString(hour1));
        sr.append(" hour2=");
        sr.append(Integer.toString(hour2));
        sr.append(" timeMinutesName=");
        sr.append(ftlFmt.makeStringLiteral(timeMinutesName));
        sr.append(" minutes=");
        sr.append(Integer.toString(minutes));
        sr.append(" isTwelveHour=");
        sr.append(Boolean.toString(isTwelveHour));
        sr.append(" ampmName=");
        sr.append(ftlFmt.makeStringLiteral(ampmName));
        sr.append(" amSelected=");
        sr.append(ftlFmt.makeStringLiteral(amSelected));
        sr.append(" pmSelected=");
        sr.append(ftlFmt.makeStringLiteral(pmSelected));
        sr.append(" compositeType=");
        sr.append(ftlFmt.makeStringLiteral(compositeType));
        sr.append(" formName=");
        sr.append(ftlFmt.makeStringLiteral(formName));
        sr.append(" mask=");
        sr.append(ftlFmt.makeStringLiteral(formattedMask));
        appendRequiredFieldParam(sr, context, modelFormField);
        sr.append(" />");
        executeMacro(writer, sr.toString());
        this.addAsterisks(writer, context, modelFormField);
        this.appendTooltip(writer, context, modelFormField);
    }

    public void renderDropDownField(Appendable writer, Map<String, Object> context, DropDownField dropDownField) throws IOException {
        ModelFormField modelFormField = dropDownField.getModelFormField();
        ModelForm modelForm = modelFormField.getModelForm();
        String currentValue = modelFormField.getEntry(context);
        List<ModelFormField.OptionValue> allOptionValues = dropDownField.getAllOptionValues(context, WidgetWorker.getDelegator(context));
        ModelFormField.AutoComplete autoComplete = dropDownField.getAutoComplete();
        String event = modelFormField.getEvent();
        String action = modelFormField.getAction(context);
        Integer textSize = Integer.valueOf(0);
        if (UtilValidate.isNotEmpty(dropDownField.getTextSize())) {
            try {
                textSize = Integer.parseInt(dropDownField.getTextSize());
            } catch (NumberFormatException nfe) {
                Debug.logError(nfe, "Error reading size of a field fieldName=" + dropDownField.getModelFormField().getFieldName() + " FormName= " + dropDownField.getModelFormField().getModelForm().getName(), module);
                handleError(writer, nfe); // SCIPIO
            }
            if (textSize > 0 && UtilValidate.isNotEmpty(currentValue) && currentValue.length() > textSize) {
                currentValue = currentValue.substring(0, textSize - 8) + "..." + currentValue.substring(currentValue.length() - 5);
            }
        }
        boolean ajaxEnabled = autoComplete != null && this.javaScriptEnabled;
        String className = "";
        String alert = "false";
        String name = modelFormField.getParameterName(context);
        String id = modelFormField.getCurrentContainerId(context);
        String multiple = dropDownField.getAllowMultiple() ? "multiple" : "";
        String otherFieldName = "";
        String formName = modelForm.getName();
        String size = dropDownField.getSize();
        String dDFCurrent = dropDownField.getCurrent();
        String firstInList = "";
        String explicitDescription = "";
        String allowEmpty = "";
        StringBuilder options = new StringBuilder();
        StringBuilder ajaxOptions = new StringBuilder();
        if (UtilValidate.isNotEmpty(modelFormField.getWidgetStyle(context))) {
            className = modelFormField.getWidgetStyle(context);
            if (modelFormField.shouldBeRed(context)) {
                alert = "true";
            }
        }
        //check for required field style on single forms
        if ("single".equals(modelFormField.getModelForm().getType()) && modelFormField.getRequiredField()) {
            className = combineRequiredStyle(className, context, modelFormField); // SCIPIO
        }
        String currentDescription = null;
        if (UtilValidate.isNotEmpty(currentValue)) {
            for (ModelFormField.OptionValue optionValue : allOptionValues) {
                if (optionValue.getKey().equals(currentValue)) {
                    currentDescription = optionValue.getDescription();
                    break;
                }
            }
        }
        int otherFieldSize = dropDownField.getOtherFieldSize();
        if (otherFieldSize > 0) {
            otherFieldName = dropDownField.getParameterNameOther(context);
        }
        // if the current value should go first, stick it in
        if (UtilValidate.isNotEmpty(currentValue) && "first-in-list".equals(dropDownField.getCurrent())) {
            firstInList = "first-in-list";
        }
        explicitDescription = (currentDescription != null ? currentDescription : dropDownField.getCurrentDescription(context));
        if (UtilValidate.isEmpty(explicitDescription)) {
            explicitDescription = (FieldInfoWithOptions.getDescriptionForOptionKey(currentValue, allOptionValues));
        }
        if (textSize > 0 && UtilValidate.isNotEmpty(explicitDescription) && explicitDescription.length() > textSize) {
            explicitDescription = explicitDescription.substring(0, textSize - 8) + "..." + explicitDescription.substring(explicitDescription.length() - 5);
        }
        explicitDescription = encode(explicitDescription, modelFormField, context);
        // if allow empty is true, add an empty option
        if (dropDownField.getAllowEmpty()) {
            allowEmpty = "Y";
        }
        List<String> currentValueList = null;
        if (UtilValidate.isNotEmpty(currentValue) && dropDownField.getAllowMultiple()) {
            // If currentValue is Array, it will start with [
            if (currentValue.startsWith("[")) {
                currentValueList = StringUtil.toList(currentValue);
            } else {
                currentValueList = UtilMisc.toList(currentValue);
            }
        }
        options.append("[");
        Iterator<ModelFormField.OptionValue> optionValueIter = allOptionValues.iterator();
        int count = 0;
        while (optionValueIter.hasNext()) {
            ModelFormField.OptionValue optionValue = optionValueIter.next();
            if (options.length() > 1) {
                options.append(",");
            }
            options.append("{'key':");
            String key = encode(optionValue.getKey(), modelFormField, context);
            options.append(ftlFmt.makeStringLiteralSQ(key));
            options.append(",'description':");
            String description = optionValue.getDescription();
            if (textSize > 0 && description.length() > textSize) {
                description = description.substring(0, textSize - 8) + "..." + description.substring(description.length() - 5);
            }
            options.append(ftlFmt.makeStringLiteralSQ(encode(description, modelFormField, context)));

            if (UtilValidate.isNotEmpty(currentValueList)) {
                options.append(",'selected':");
                if (currentValueList.contains(optionValue.getKey())) {
                    options.append("'selected'");
                } else {
                    options.append("''");
                }
            }

            options.append("}");
            if (ajaxEnabled) {
                count++;
                // SCIPIO: FIXME?: these javascript string values should probably be escaped for javascript syntax
                ajaxOptions.append(optionValue.getKey()).append(": ");
                ajaxOptions.append(" '").append(optionValue.getDescription()).append("'");
                if (count != allOptionValues.size()) {
                    ajaxOptions.append(", ");
                }
            }
        }
        options.append("]");
        String noCurrentSelectedKey = dropDownField.getNoCurrentSelectedKey(context);
        String otherValue = "", fieldName = "";
        // Adapted from work by Yucca Korpela
        // http://www.cs.tut.fi/~jkorpela/forms/combo.html
        if (otherFieldSize > 0) {
            fieldName = modelFormField.getParameterName(context);
            Map<String, ? extends Object> dataMap = modelFormField.getMap(context);
            if (dataMap == null) {
                dataMap = context;
            }
            Object otherValueObj = dataMap.get(otherFieldName);
            otherValue = (otherValueObj == null) ? "" : otherValueObj.toString();
        }
        String frequency = "";
        String minChars = "";
        String choices = "";
        String autoSelect = "";
        String partialSearch = "";
        String partialChars = "";
        String ignoreCase = "";
        String fullSearch = "";
        if (ajaxEnabled) {
            frequency = autoComplete.getFrequency();
            minChars = autoComplete.getMinChars();
            choices = autoComplete.getChoices();
            autoSelect = autoComplete.getAutoSelect();
            partialSearch = autoComplete.getPartialSearch();
            partialChars = autoComplete.getPartialChars();
            ignoreCase = autoComplete.getIgnoreCase();
            fullSearch = autoComplete.getFullSearch();
        }
        StringWriter sr = new StringWriter();
        sr.append("<@renderDropDownField ");
        appendFieldInfo(sr, context, modelFormField);
        sr.append("name=");
        sr.append(ftlFmt.makeStringLiteral(name));
        sr.append(" className=");
        sr.append(ftlFmt.makeStringLiteral(className));
        sr.append(" alert=");
        sr.append(ftlFmt.makeStringLiteral(alert));
        sr.append(" id=");
        sr.append(ftlFmt.makeStringLiteral(id));
        sr.append(" multiple=");
        sr.append(ftlFmt.makeStringLiteral(multiple));
        sr.append(" formName=");
        sr.append(ftlFmt.makeStringLiteral(formName));
        sr.append(" otherFieldName=");
        sr.append(ftlFmt.makeStringLiteral(otherFieldName));
        sr.append(" event=");
        sr.append(ftlFmt.makeStringLiteral(event));
        sr.append(" action=");
        sr.append(ftlFmt.makeStringLiteral(action));
        sr.append(" size=");
        sr.append(ftlFmt.makeStringLiteral(size));
        sr.append(" firstInList=");
        sr.append(ftlFmt.makeStringLiteral(firstInList));
        sr.append(" currentValue=");
        sr.append(ftlFmt.makeStringLiteral(currentValue));
        sr.append(" explicitDescription=");
        sr.append(ftlFmt.makeStringLiteral(explicitDescription));
        sr.append(" allowEmpty=");
        sr.append(ftlFmt.makeStringLiteral(allowEmpty));
        sr.append(" options=");
        sr.append(options.toString());
        sr.append(" fieldName=");
        sr.append(ftlFmt.makeStringLiteral(fieldName));
        sr.append(" otherFieldName=");
        sr.append(ftlFmt.makeStringLiteral(otherFieldName));
        sr.append(" otherValue=");
        sr.append(ftlFmt.makeStringLiteral(otherValue));
        sr.append(" otherFieldSize=");
        sr.append(Integer.toString(otherFieldSize));
        sr.append(" dDFCurrent=");
        sr.append(ftlFmt.makeStringLiteral(dDFCurrent));
        sr.append(" ajaxEnabled=");
        sr.append(Boolean.toString(ajaxEnabled));
        sr.append(" noCurrentSelectedKey=");
        sr.append(ftlFmt.makeStringLiteral(noCurrentSelectedKey));
        sr.append(" ajaxOptions=");
        sr.append(ftlFmt.makeStringLiteral(ajaxOptions.toString()));
        sr.append(" frequency=");
        sr.append(ftlFmt.makeStringLiteral(frequency));
        sr.append(" minChars=");
        sr.append(ftlFmt.makeStringLiteral(minChars));
        sr.append(" choices=");
        sr.append(ftlFmt.makeStringLiteral(choices));
        sr.append(" autoSelect=");
        sr.append(ftlFmt.makeStringLiteral(autoSelect));
        sr.append(" partialSearch=");
        sr.append(ftlFmt.makeStringLiteral(partialSearch));
        sr.append(" partialChars=");
        sr.append(ftlFmt.makeStringLiteral(partialChars));
        sr.append(" ignoreCase=");
        sr.append(ftlFmt.makeStringLiteral(ignoreCase));
        sr.append(" fullSearch=");
        sr.append(ftlFmt.makeStringLiteral(fullSearch));
        appendRequiredFieldParam(sr, context, modelFormField);
        sr.append(" />");
        executeMacro(writer, sr.toString());
        ModelFormField.SubHyperlink subHyperlink = dropDownField.getSubHyperlink();
        if (subHyperlink != null && subHyperlink.shouldUse(context)) {
            makeHyperlinkString(writer, subHyperlink, context);
        }
        this.appendTooltip(writer, context, modelFormField);
    }

    public void renderCheckField(Appendable writer, Map<String, Object> context, CheckField checkField) throws IOException {
        ModelFormField modelFormField = checkField.getModelFormField();
        modelFormField.getModelForm();
        String currentValue = modelFormField.getEntry(context);
        Boolean allChecked = checkField.isAllChecked(context);
        String id = modelFormField.getCurrentContainerId(context);
        String className = "";
        String alert = "false";
        String name = modelFormField.getParameterName(context);
        String event = modelFormField.getEvent();
        String action = modelFormField.getAction(context);
        StringBuilder items = new StringBuilder();
        if (UtilValidate.isNotEmpty(modelFormField.getWidgetStyle(context))) {
            className = modelFormField.getWidgetStyle(context);
            if (modelFormField.shouldBeRed(context)) {
                alert = "true";
            }
        }
        List<ModelFormField.OptionValue> allOptionValues = checkField.getAllOptionValues(context, WidgetWorker.getDelegator(context));
        items.append("[");
        for (ModelFormField.OptionValue optionValue : allOptionValues) {
            if (items.length() > 1) {
                items.append(",");
            }
            items.append("{");
            if (optionValue.getKey() != null) {
                items.append("'value':");
                items.append(ftlFmt.makeStringLiteralSQ(optionValue.getKey()));
            }
            // SCIPIO: 2017-04-20: alt-value support
            if (optionValue.getAltKey() != null) {
                if (items.length() > 1) items.append(",");
                items.append("'altValue':");
                items.append(ftlFmt.makeStringLiteralSQ(optionValue.getAltKey()));
            }
            String description = optionValue.getDescription();
            // SCIPIO: 2017-04-20: special one-char empty-title prevention
            if (description != null && !" ".equals(description)) {
                if (items.length() > 1) items.append(",");
                items.append("'description':");
                items.append(ftlFmt.makeStringLiteralSQ(encode(optionValue.getDescription(), modelFormField, context)));
            }
            items.append("}");
        }
        items.append("]");
        // SCIPIO: 2017-04-20: new
        String key = checkField.getKey(context);
        String altKey = checkField.getAltKey(context);
        String noCurrentSelectedKey = checkField.getNoCurrentSelectedKey(context);
        Boolean useHidden = checkField.getUseHidden(context);
        StringWriter sr = new StringWriter();
        sr.append("<@renderCheckField ");
        appendFieldInfo(sr, context, modelFormField);
        sr.append("items=");
        sr.append(items.toString());
        sr.append(" className=");
        sr.append(ftlFmt.makeStringLiteral(className));
        sr.append(" alert=");
        sr.append(ftlFmt.makeStringLiteral(alert));
        sr.append(" id=");
        sr.append(ftlFmt.makeStringLiteral(id));
        sr.append(" allChecked=");
        sr.append((allChecked != null ? Boolean.toString(allChecked) : "\"\""));
        sr.append(" currentValue=");
        sr.append(ftlFmt.makeStringLiteral(currentValue));
        sr.append(" name=");
        sr.append(ftlFmt.makeStringLiteral(name));
        sr.append(" event=");
        sr.append(ftlFmt.makeStringLiteral(event));
        sr.append(" action=");
        sr.append(ftlFmt.makeStringLiteral(action));
        if (noCurrentSelectedKey != null) { // SCIPIO: NOTE: by doing this, we let macro decide how to handle default
            sr.append(" noCurrentSelectedKey=");
            sr.append(ftlFmt.makeStringLiteral(noCurrentSelectedKey));
        }
        if (key != null) {
            sr.append(" key=");
            sr.append(ftlFmt.makeStringLiteral(key));
        }
        if (altKey != null) {
            sr.append(" altKey=");
            sr.append(ftlFmt.makeStringLiteral(altKey));
        }
        sr.append(" useHidden=");
        sr.append(ftlFmt.makeTernaryBooleanLiteral(useHidden));
        appendRequiredFieldParam(sr, context, modelFormField);
        sr.append(" />");
        executeMacro(writer, sr.toString());
        this.appendTooltip(writer, context, modelFormField);
    }

    public void renderRadioField(Appendable writer, Map<String, Object> context, RadioField radioField) throws IOException {
        ModelFormField modelFormField = radioField.getModelFormField();
        modelFormField.getModelForm();
        List<ModelFormField.OptionValue> allOptionValues = radioField.getAllOptionValues(context, WidgetWorker.getDelegator(context));
        String currentValue = modelFormField.getEntry(context);
        String className = "";
        String alert = "false";
        String name = modelFormField.getParameterName(context);
        String event = modelFormField.getEvent();
        String action = modelFormField.getAction(context);
        StringBuilder items = new StringBuilder();
        if (UtilValidate.isNotEmpty(modelFormField.getWidgetStyle(context))) {
            className = modelFormField.getWidgetStyle(context);
            if (modelFormField.shouldBeRed(context)) {
                alert = "true";
            }
        }
        String noCurrentSelectedKey = radioField.getNoCurrentSelectedKey(context);
        items.append("[");
        for (ModelFormField.OptionValue optionValue : allOptionValues) {
            if (items.length() > 1) {
                items.append(",");
            }
            items.append("{'key':");
            items.append(ftlFmt.makeStringLiteralSQ(optionValue.getKey()));
            String description = optionValue.getDescription();
            // SCIPIO: 2017-04-20: special one-char empty-title prevention
            if (description != null && !" ".equals(description)) {
                items.append(", 'description':");
                // SCIPIO: 2017-02
                items.append(ftlFmt.makeStringLiteralSQ(encode(optionValue.getDescription(), modelFormField, context)));
            }
            items.append("}");
        }
        items.append("]");
        StringWriter sr = new StringWriter();
        sr.append("<@renderRadioField ");
        appendFieldInfo(sr, context, modelFormField);
        sr.append("items=");
        sr.append(items.toString());
        sr.append(" className=");
        sr.append(ftlFmt.makeStringLiteral(className));
        sr.append(" alert=");
        sr.append(ftlFmt.makeStringLiteral(alert));
        sr.append(" currentValue=");
        sr.append(ftlFmt.makeStringLiteral(currentValue));
        sr.append(" noCurrentSelectedKey=");
        sr.append(ftlFmt.makeStringLiteral(noCurrentSelectedKey));
        sr.append(" name=");
        sr.append(ftlFmt.makeStringLiteral(name));
        sr.append(" event=");
        sr.append(ftlFmt.makeStringLiteral(event));
        sr.append(" action=");
        sr.append(ftlFmt.makeStringLiteral(action));
        appendRequiredFieldParam(sr, context, modelFormField);
        sr.append(" />");
        executeMacro(writer, sr.toString());
        this.appendTooltip(writer, context, modelFormField);
    }

    public void renderSubmitField(Appendable writer, Map<String, Object> context, SubmitField submitField) throws IOException {
        ModelFormField modelFormField = submitField.getModelFormField();
        ModelForm modelForm = modelFormField.getModelForm();
        String event = modelFormField.getEvent();
        String action = modelFormField.getAction(context);
        String title = modelFormField.getTitle(context);
        String name = modelFormField.getParameterName(context);
        String buttonType = submitField.getButtonType();
        String formName = FormRenderer.getCurrentFormName(modelForm, context);
        String imgSrc = submitField.getImageLocation(context);
        String confirmation = submitField.getConfirmation(context);
        String className = "";
        String alert = "false";
        if (UtilValidate.isNotEmpty(modelFormField.getWidgetStyle(context))) {
            className = modelFormField.getWidgetStyle(context);
            if (modelFormField.shouldBeRed(context)) {
                alert = "true";
            }
        }
        String formId = FormRenderer.getCurrentContainerId(modelForm, context);
        List<ModelForm.UpdateArea> updateAreas = modelForm.getOnSubmitUpdateAreas();
        // This is here for backwards compatibility. Use on-event-update-area
        // elements instead.
        String backgroundSubmitRefreshTarget = submitField.getBackgroundSubmitRefreshTarget(context);
        if (UtilValidate.isNotEmpty(backgroundSubmitRefreshTarget)) {
            if (updateAreas == null) {
                updateAreas = new LinkedList<ModelForm.UpdateArea>();
            }
            updateAreas.add(new ModelForm.UpdateArea("submit", formId, backgroundSubmitRefreshTarget));
        }
        boolean ajaxEnabled = (UtilValidate.isNotEmpty(updateAreas) || UtilValidate.isNotEmpty(backgroundSubmitRefreshTarget)) && this.javaScriptEnabled;
        String ajaxUrl = "";
        if (ajaxEnabled) {
            ajaxUrl = createAjaxParamsFromUpdateAreas(updateAreas, "", context);
        }
        
        // SCIPIO: Getting the id of the element from the context
        String id = null;
        if (submitField.getModelFormField().getCurrentContainerId(context) != null) {
            id = submitField.getModelFormField().getCurrentContainerId(context);
        }        
        
        StringWriter sr = new StringWriter();
        sr.append("<@renderSubmitField ");
        appendFieldInfo(sr, context, modelFormField);
        sr.append("buttonType=");
        sr.append(ftlFmt.makeStringLiteral(buttonType));
        sr.append(" className=");
        sr.append(ftlFmt.makeStringLiteral(className));
        sr.append(" alert=");
        sr.append(ftlFmt.makeStringLiteral(alert));
        sr.append(" formName=");
        sr.append(ftlFmt.makeStringLiteral(formName));
        sr.append(" title=");
        sr.append(ftlFmt.makeStringLiteral(encode(title, modelFormField, context)));
        sr.append(" name=");
        sr.append(ftlFmt.makeStringLiteral(name));
        sr.append(" event=");
        sr.append(ftlFmt.makeStringLiteral(event));
        sr.append(" action=");
        sr.append(ftlFmt.makeStringLiteral(action));
        sr.append(" imgSrc=");
        sr.append(ftlFmt.makeStringLiteral(imgSrc));
        sr.append(" containerId=");
        sr.append(ftlFmt.makeStringLiteral(ajaxEnabled ? formId : null));
        sr.append(" confirmation =");
        sr.append(ftlFmt.makeStringLiteral(confirmation));
        sr.append(" ajaxUrl=");
        sr.append(ftlFmt.makeStringLiteral(ajaxEnabled ? ajaxUrl : null));
        sr.append(" id=");
        sr.append(ftlFmt.makeStringLiteral(id));      
        sr.append(" />");
        executeMacro(writer, sr.toString());
        this.appendTooltip(writer, context, modelFormField);
    }

    public void renderResetField(Appendable writer, Map<String, Object> context, ResetField resetField) throws IOException {
        ModelFormField modelFormField = resetField.getModelFormField();
        String name = modelFormField.getParameterName(context);
        String className = "";
        String alert = "false";
        if (UtilValidate.isNotEmpty(modelFormField.getWidgetStyle(context))) {
            className = modelFormField.getWidgetStyle(context);
            if (modelFormField.shouldBeRed(context)) {
                alert = "true";
            }
        }
        String title = modelFormField.getTitle(context);
        StringWriter sr = new StringWriter();
        sr.append("<@renderResetField ");
        appendFieldInfo(sr, context, modelFormField);
        sr.append(" className=");
        sr.append(ftlFmt.makeStringLiteral(className));
        sr.append(" alert=");
        sr.append(ftlFmt.makeStringLiteral(alert));
        sr.append(" name=");
        sr.append(ftlFmt.makeStringLiteral(name));
        sr.append(" title=");
        sr.append(ftlFmt.makeStringLiteral(title));
        sr.append(" />");
        executeMacro(writer, sr.toString());
        this.appendTooltip(writer, context, modelFormField);
    }

    public void renderHiddenField(Appendable writer, Map<String, Object> context, HiddenField hiddenField) throws IOException {
        ModelFormField modelFormField = hiddenField.getModelFormField();
        String value = hiddenField.getValue(context);
        this.renderHiddenField(writer, context, modelFormField, value);
    }

    public void renderHiddenField(Appendable writer, Map<String, Object> context, ModelFormField modelFormField, String value) throws IOException {
        String name = modelFormField.getParameterName(context);
        String action = modelFormField.getAction(context);
        String event = modelFormField.getEvent();
        String id = modelFormField.getCurrentContainerId(context);
        StringWriter sr = new StringWriter();
        sr.append("<@renderHiddenField ");
        sr.append(" name=");
        sr.append(ftlFmt.makeStringLiteral(name));
        sr.append(" value=");
        sr.append(ftlFmt.makeStringLiteral(value));
        sr.append(" id=");
        sr.append(ftlFmt.makeStringLiteral(id));
        sr.append(" event=");
        sr.append(ftlFmt.makeStringLiteral(event));
        sr.append(" action=");
        sr.append(ftlFmt.makeStringLiteral(action));
        sr.append(" />");
        executeMacro(writer, sr.toString());
    }

    public void renderIgnoredField(Appendable writer, Map<String, Object> context, IgnoredField ignoredField) {
        // do nothing, it's an ignored field; could add a comment or something if we wanted to
    }

    public void renderFieldTitle(Appendable writer, Map<String, Object> context, ModelFormField modelFormField) throws IOException {
        String titleText = modelFormField.getTitle(context);
        String style = modelFormField.getTitleStyle();
        String id = modelFormField.getCurrentContainerId(context);
        StringBuilder sb = new StringBuilder();
        if (UtilValidate.isNotEmpty(titleText)) {
            if (" ".equals(titleText)) {
                // SCIPIO: 2017-01-13: delegate
                //executeMacro(writer, "<@renderFormatEmptySpace />");
                renderFormatEmptySpace(writer, context, modelFormField.getModelForm(), "field-title");
            } else {
                titleText = UtilHttp.encodeAmpersands(titleText);
                titleText = encode(titleText, modelFormField, context);
                if (UtilValidate.isNotEmpty(modelFormField.getHeaderLink())) {
                    StringBuilder targetBuffer = new StringBuilder();
                    FlexibleStringExpander target = FlexibleStringExpander.getInstance(modelFormField.getHeaderLink());
                    String fullTarget = target.expandString(context);
                    targetBuffer.append(fullTarget);
                    String targetType = CommonWidgetModels.Link.DEFAULT_URL_MODE;
                    if (UtilValidate.isNotEmpty(targetBuffer.toString()) && targetBuffer.toString().toLowerCase().startsWith("javascript:")) {
                        targetType = "plain";
                    }
                    StringWriter sr = new StringWriter();
                    makeHyperlinkString(sr, modelFormField.getHeaderLinkStyle(), targetType, targetBuffer.toString(), null, titleText, "", modelFormField, this.request, this.response, context, "");
                    String title = sr.toString().replace("\"", "\'"); // SCIPIO: FIXME?: this should now be redundant, but leaving in for compabitility/legacy behavior for now
                    sr = new StringWriter();
                    sr.append("<@renderHyperlinkTitle ");
                    sr.append(" name=");
                    sr.append(ftlFmt.makeStringLiteral(modelFormField.getModelForm().getName()));
                    sr.append(" title=");
                    sr.append(ftlFmt.makeStringLiteral(title)); // SCIPIO: redundant: FreeMarkerWorker.encodeDoubleQuotes(title)
                    sr.append(" />");
                    executeMacro(writer, sr.toString());
                } else if (modelFormField.isSortField()) {
                    renderSortField(writer, context, modelFormField, titleText);
                } else if (modelFormField.isRowSubmit()) {
                    StringWriter sr = new StringWriter();
                    sr.append("<@renderHyperlinkTitle ");
                    sr.append(" name=");
                    sr.append(ftlFmt.makeStringLiteral(modelFormField.getModelForm().getName()));
                    sr.append(" title=");
                    sr.append(ftlFmt.makeStringLiteral(titleText));
                    sr.append(" showSelectAll=\"Y\"/>");
                    executeMacro(writer, sr.toString());
                } else {
                    sb.append(titleText);
                }
            }
        }
        if (!sb.toString().isEmpty()) {
            //check for required field style on single forms
            if ("single".equals(modelFormField.getModelForm().getType()) && modelFormField.getRequiredField()) {
                style = combineRequiredStyle(style, false, context, modelFormField); // SCIPIO
            }
            StringWriter sr = new StringWriter();
            sr.append("<@renderFieldTitle ");
            sr.append(" style=");
            sr.append(ftlFmt.makeStringLiteral(style));
            String displayHelpText = UtilProperties.getPropertyValue("widget.properties", "widget.form.displayhelpText");
            if ("Y".equals(displayHelpText)) {
                Delegator delegator = WidgetWorker.getDelegator(context);
                Locale locale = (Locale) context.get("locale");
                String entityName = modelFormField.getEntityName();
                String fieldName = modelFormField.getFieldName();
                String helpText = UtilHelpText.getEntityFieldDescription(entityName, fieldName, delegator, locale);

                sr.append(" fieldHelpText=");
                sr.append(ftlFmt.makeStringLiteral(helpText)); // SCIPIO: redundant: FreeMarkerWorker.encodeDoubleQuotes(helpText)
            }
            sr.append(" title=");
            sr.append(ftlFmt.makeStringLiteral(sb.toString()));
            if (UtilValidate.isNotEmpty(id)) {
                sr.append(" id=");
                sr.append(ftlFmt.makeStringLiteral(id + "_title"));
                // Render "for"
                sr.append(" for=");
                sr.append(ftlFmt.makeStringLiteral(id));
            }
            sr.append(" />");
            executeMacro(writer, sr.toString());
        }
    }

    public void renderSingleFormFieldTitle(Appendable writer, Map<String, Object> context, ModelFormField modelFormField) throws IOException {
        renderFieldTitle(writer, context, modelFormField);
    }

    public void renderFormOpen(Appendable writer, Map<String, Object> context, ModelForm modelForm) throws IOException {
        contextHandler.registerContext(writer, context);
        this.widgetCommentsEnabled = ModelWidget.widgetBoundaryCommentsEnabled(context);
        if (modelForm instanceof ModelSingleForm) {
            renderBeginningBoundaryComment(writer, "Form Widget - Form Element", modelForm);
        } else {
            renderBeginningBoundaryComment(writer, "Grid Widget - Grid Element", modelForm);
        }
        String targetType = modelForm.getTargetType();
        String targ = modelForm.getTarget(context, targetType);
        StringBuilder linkUrl = new StringBuilder();
        if (UtilValidate.isNotEmpty(targ)) {
            //this.appendOfbizUrl(writer, "/" + targ);
            WidgetWorker.buildHyperlinkUrl(linkUrl, targ, targetType, null, null, null, null, null, request, response, context);
        }
        String formType = modelForm.getType();
        String targetWindow = modelForm.getTargetWindow(context);
        String containerId = FormRenderer.getCurrentContainerId(modelForm, context);
        String containerStyle = modelForm.getContainerStyle();
        String autocomplete = "";
        String name = FormRenderer.getCurrentFormName(modelForm, context);
        String viewIndexField = modelForm.getMultiPaginateIndexField(context);
        String viewSizeField = modelForm.getMultiPaginateSizeField(context);
        int viewIndex = Paginator.getViewIndex(modelForm, context);
        int viewSize = Paginator.getViewSize(modelForm, context);
        boolean useRowSubmit = modelForm.getUseRowSubmit();
        if (!modelForm.getClientAutocompleteFields()) {
            autocomplete = "off";
        }
        
        // SCIPIO: extra attribs map (json-like)
        String attribs = modelForm.getAttribsExpr().compile(context);
        
        // SCIPIO: form method
        String method = modelForm.getMethod(context);
        
        // SCIPIO: special flags
        String formScope = (String) context.get("renderForm_formScope");
        if (UtilValidate.isEmpty(formScope)) {
            formScope = "general";
        }
        String formSpread = (String) context.get("renderForm_formSpread");
        if (UtilValidate.isEmpty(formSpread)) {
            formSpread = "general";
        }

        StringWriter sr = new StringWriter();
        sr.append("<@renderFormOpen ");
        sr.append(" linkUrl=");
        sr.append(ftlFmt.makeStringLiteral(linkUrl.toString()));
        sr.append(" formType=");
        sr.append(ftlFmt.makeStringLiteral(formType));
        sr.append(" targetWindow=");
        sr.append(ftlFmt.makeStringLiteral(targetWindow));
        sr.append(" containerId=");
        sr.append(ftlFmt.makeStringLiteral(containerId));
        sr.append(" containerStyle=");
        sr.append(ftlFmt.makeStringLiteral(containerStyle));
        sr.append(" autocomplete=");
        sr.append(ftlFmt.makeStringLiteral(autocomplete));
        sr.append(" name=");
        sr.append(ftlFmt.makeStringLiteral(name));
        sr.append(" viewIndexField=");
        sr.append(ftlFmt.makeStringLiteral(viewIndexField));
        sr.append(" viewSizeField=");
        sr.append(ftlFmt.makeStringLiteral(viewSizeField));
        sr.append(" viewIndex=");
        sr.append(ftlFmt.makeStringLiteral(viewIndex));
        sr.append(" viewSize=");
        sr.append(ftlFmt.makeStringLiteral(viewSize));
        sr.append(" useRowSubmit=");
        sr.append(Boolean.toString(useRowSubmit));
        sr.append(" attribs=(");
        sr.append(attribs);
        sr.append(") method=");
        sr.append(ftlFmt.makeStringLiteral(method));
        sr.append(" formScope=");
        sr.append(ftlFmt.makeStringLiteral(formScope));
        sr.append(" formSpread=");
        sr.append(ftlFmt.makeStringLiteral(formSpread));
        sr.append(" />");
        
        executeMacro(writer, sr.toString());
    }

    public void renderFormClose(Appendable writer, Map<String, Object> context, ModelForm modelForm) throws IOException {
        String focusFieldName = modelForm.getFocusFieldName();
        String formName = FormRenderer.getCurrentFormName(modelForm, context);
        String containerId = FormRenderer.getCurrentContainerId(modelForm, context);
        String hasRequiredField = "";
        for (ModelFormField formField : modelForm.getFieldList()) {
            if (formField.getRequiredField()) {
                hasRequiredField = "Y";
                break;
            }
        }
        
        StringWriter sr = new StringWriter();
        sr.append("<@renderFormClose ");
        sr.append(" focusFieldName=");
        sr.append(ftlFmt.makeStringLiteral(focusFieldName));
        sr.append(" formName=");
        sr.append(ftlFmt.makeStringLiteral(formName));
        sr.append(" containerId=");
        sr.append(ftlFmt.makeStringLiteral(containerId));
        sr.append(" hasRequiredField=");
        sr.append(ftlFmt.makeStringLiteral(hasRequiredField));
        sr.append(" />");
        executeMacro(writer, sr.toString());
        
        // SCIPIO: same as the multi type form, I think this is the cleanest way to do it
        // see if there is anything that needs to be added outside of the list-form
        Map<String, Object> wholeFormContext = UtilGenerics.checkMap(context.get("wholeFormContext"));
        Appendable postMultiFormWriter = wholeFormContext != null ? (Appendable) wholeFormContext.get("postMultiFormWriter") : null;
        if (postMultiFormWriter != null) {
            writer.append(postMultiFormWriter.toString());
        }

        if (modelForm instanceof ModelSingleForm) {
            renderEndingBoundaryComment(writer, "Form Widget - Form Element", modelForm);
        } else {
            renderEndingBoundaryComment(writer, "Grid Widget - Grid Element", modelForm);
        }
    }

    public void renderMultiFormClose(Appendable writer, Map<String, Object> context, ModelForm modelForm) throws IOException {
        //FIXME copy from HtmlFormRenderer.java (except for the closing form tag itself, that is now converted)
        StringWriter sr = new StringWriter();
        sr.append("<@renderMultiFormClose />");
        executeMacro(writer, sr.toString());
        // see if there is anything that needs to be added outside of the multi-form
        Map<String, Object> wholeFormContext = UtilGenerics.checkMap(context.get("wholeFormContext"));
        Appendable postMultiFormWriter = wholeFormContext != null ? (Appendable) wholeFormContext.get("postMultiFormWriter") : null;
        if (postMultiFormWriter != null) {
            writer.append(postMultiFormWriter.toString());
        }
        if (modelForm instanceof ModelSingleForm) {
            renderEndingBoundaryComment(writer, "Form Widget - Form Element", modelForm);
        } else {
            renderEndingBoundaryComment(writer, "Grid Widget - Grid Element", modelForm);
        }
    }

    public void renderFormatListWrapperOpen(Appendable writer, Map<String, Object> context, ModelForm modelForm) throws IOException {
        contextHandler.registerContext(writer, context);
        Map<String, Object> inputFields = UtilGenerics.checkMap(context.get("requestParameters"));
        Map<String, Object> queryStringMap = UtilGenerics.toMap(context.get("queryStringMap"));
        if (UtilValidate.isNotEmpty(queryStringMap)) {
            inputFields.putAll(queryStringMap);
        }
        if (modelForm.getType().equals("multi")) {
            inputFields = UtilHttp.removeMultiFormParameters(inputFields);
        }
        String queryString = UtilHttp.urlEncodeArgs(inputFields);
        context.put("_QBESTRING_", queryString);
        if (modelForm instanceof ModelSingleForm) {
            renderBeginningBoundaryComment(writer, "Form Widget - Form Element", modelForm);
        } else {
            renderBeginningBoundaryComment(writer, "Grid Widget - Grid Element", modelForm);
        }
        if (this.renderPagination) {
            this.renderNextPrev(writer, context, modelForm, "top");
        }
        List<ModelFormField> childFieldList = modelForm.getFieldList();
        List<String> columnStyleList = new LinkedList<String>();
        List<String> fieldNameList = new LinkedList<String>();
        for (ModelFormField childField : childFieldList) {
            int childFieldType = childField.getFieldInfo().getFieldType();
            if (childFieldType == FieldInfo.HIDDEN || childFieldType == FieldInfo.IGNORED) {
                continue;
            }
            String areaStyle = childField.getTitleAreaStyle();
            if (UtilValidate.isEmpty(areaStyle)) {
                areaStyle = "";
            }
            if (fieldNameList.contains(childField.getName())) {
                if (UtilValidate.isNotEmpty(areaStyle)) {
                    columnStyleList.set(fieldNameList.indexOf(childField.getName()), areaStyle);
                }
            } else {
                columnStyleList.add(areaStyle);
                fieldNameList.add(childField.getName());
            }
        }
        // SCIPIO: this is not good enough
        //columnStyleList = StringUtil.quoteStrList(columnStyleList);
        List<String> columnStyleListEscaped = new ArrayList<>(columnStyleList.size());
        for(String style : columnStyleList) {
            columnStyleListEscaped.add(ftlFmt.makeStringLiteral(style));
        }
        String columnStyleListString = StringUtil.join(columnStyleListEscaped, ", ");
        
        // SCIPIO: extra attribs map (json-like)
        String attribs = modelForm.getAttribsExpr().compile(context);
        
        StringWriter sr = new StringWriter();
        sr.append("<@renderFormatListWrapperOpen ");
        sr.append(" formName=");
        sr.append(ftlFmt.makeStringLiteral(modelForm.getName()));
        sr.append(" style=");
        sr.append(ftlFmt.makeStringLiteral(FlexibleStringExpander.expandString(modelForm.getDefaultTableStyle(), context)));
        sr.append(" columnStyles=[");
        if (UtilValidate.isNotEmpty(columnStyleListString)) {
            // this is a fix for forms with no fields
            sr.append(columnStyleListString);
        }
        // SCIPIO: also pass formType, to remove all ambiguity (so macro doesn't have to assume)
        sr.append("] formType=");
        sr.append(ftlFmt.makeStringLiteral(modelForm.getType()));        
        sr.append(" attribs=(");
        sr.append(attribs);
        sr.append(") ");
        sr.append("/>");
        executeMacro(writer, sr.toString());
    }

    public void renderFormatListWrapperClose(Appendable writer, Map<String, Object> context, ModelForm modelForm) throws IOException {
        StringWriter sr = new StringWriter();
        sr.append("<@renderFormatListWrapperClose");
        sr.append(" formName=");
        sr.append(ftlFmt.makeStringLiteral(modelForm.getName()));
        sr.append(" />");
        executeMacro(writer, sr.toString());
        if (this.renderPagination) {
            this.renderNextPrev(writer, context, modelForm, "bottom");
        }
        if (modelForm instanceof ModelSingleForm) {
            renderEndingBoundaryComment(writer, "Form Widget - Form Element", modelForm);
        } else {
            renderEndingBoundaryComment(writer, "Grid Widget - Grid Element", modelForm);
        }
    }

    public void renderFormatHeaderRowOpen(Appendable writer, Map<String, Object> context, ModelForm modelForm) throws IOException {
        contextHandler.registerContext(writer, context);
        String headerStyle = FlexibleStringExpander.expandString(modelForm.getHeaderRowStyle(), context);
        StringWriter sr = new StringWriter();
        sr.append("<@renderFormatHeaderRowOpen ");
        sr.append(" style=");
        sr.append(ftlFmt.makeStringLiteral(headerStyle));
        sr.append(" />");
        executeMacro(writer, sr.toString());
    }

    public void renderFormatHeaderRowClose(Appendable writer, Map<String, Object> context, ModelForm modelForm) throws IOException {
        StringWriter sr = new StringWriter();
        sr.append("<@renderFormatHeaderRowClose />");
        executeMacro(writer, sr.toString());
    }

    public void renderFormatHeaderRowCellOpen(Appendable writer, Map<String, Object> context, ModelForm modelForm, ModelFormField modelFormField, int positionSpan) throws IOException {
        String areaStyle = modelFormField.getTitleAreaStyle();
        String areaInlineStyle = modelFormField.getTitleAreaInlineStyle();
        StringWriter sr = new StringWriter();
        sr.append("<@renderFormatHeaderRowCellOpen ");
        sr.append(" style=");
        sr.append(ftlFmt.makeStringLiteral(areaStyle));
        sr.append(" positionSpan=");
        sr.append(Integer.toString(positionSpan));
        sr.append(" inlineStyle=");
        sr.append(ftlFmt.makeStringLiteral(areaInlineStyle));
        sr.append(" />");
        executeMacro(writer, sr.toString());
    }

    public void renderFormatHeaderRowCellClose(Appendable writer, Map<String, Object> context, ModelForm modelForm, ModelFormField modelFormField) throws IOException {
        StringWriter sr = new StringWriter();
        sr.append("<@renderFormatHeaderRowCellClose />");
        executeMacro(writer, sr.toString());
    }

    public void renderFormatHeaderRowFormCellOpen(Appendable writer, Map<String, Object> context, ModelForm modelForm) throws IOException {
        String areaStyle = modelForm.getFormTitleAreaStyle();
        StringWriter sr = new StringWriter();
        sr.append("<@renderFormatHeaderRowFormCellOpen ");
        sr.append(" style=");
        sr.append(ftlFmt.makeStringLiteral(areaStyle));
        sr.append(" />");
        executeMacro(writer, sr.toString());
    }

    public void renderFormatHeaderRowFormCellClose(Appendable writer, Map<String, Object> context, ModelForm modelForm) throws IOException {
        StringWriter sr = new StringWriter();
        sr.append("<@renderFormatHeaderRowFormCellClose />");
        executeMacro(writer, sr.toString());
    }

    public void renderFormatHeaderRowFormCellTitleSeparator(Appendable writer, Map<String, Object> context, ModelForm modelForm, ModelFormField modelFormField, boolean isLast) throws IOException {
        String titleStyle = modelFormField.getTitleStyle();
        StringWriter sr = new StringWriter();
        sr.append("<@renderFormatHeaderRowFormCellTitleSeparator ");
        sr.append(" style=");
        sr.append(ftlFmt.makeStringLiteral(titleStyle));
        sr.append(" isLast=");
        sr.append(Boolean.toString(isLast));
        sr.append(" />");
        executeMacro(writer, sr.toString());
    }

    public void renderFormatItemRowOpen(Appendable writer, Map<String, Object> context, ModelForm modelForm) throws IOException {
        Integer itemIndex = (Integer) context.get("itemIndex");
        String altRowStyles = "";
        String evenRowStyle = "";
        String oddRowStyle = "";
        if (itemIndex != null) {
            altRowStyles = modelForm.getStyleAltRowStyle(context);
            // SCIPIO: this is silly, pass both and let FTL make the check so it has more info
            //if (itemIndex.intValue() % 2 == 0) {
            //    evenRowStyle = modelForm.getEvenRowStyle();
            //} else {
            //    oddRowStyle = FlexibleStringExpander.expandString(modelForm.getOddRowStyle(), context);
            //}
            evenRowStyle = FlexibleStringExpander.expandString(modelForm.getEvenRowStyle(), context);
            oddRowStyle = FlexibleStringExpander.expandString(modelForm.getOddRowStyle(), context);
        }
        StringWriter sr = new StringWriter();
        sr.append("<@renderFormatItemRowOpen ");
        sr.append(" formName=");
        sr.append(ftlFmt.makeStringLiteral(modelForm.getName()));
        sr.append(" itemIndex=");
        sr.append(Integer.toString(itemIndex));
        sr.append(" altRowStyles=");
        sr.append(ftlFmt.makeStringLiteral(altRowStyles));
        sr.append(" evenRowStyle=");
        sr.append(ftlFmt.makeStringLiteral(evenRowStyle));
        sr.append(" oddRowStyle=");
        sr.append(ftlFmt.makeStringLiteral(oddRowStyle));
        sr.append(" />");
        executeMacro(writer, sr.toString());
    }

    public void renderFormatItemRowClose(Appendable writer, Map<String, Object> context, ModelForm modelForm) throws IOException {
        StringWriter sr = new StringWriter();
        sr.append("<@renderFormatItemRowClose ");
        sr.append(" formName=");
        sr.append(ftlFmt.makeStringLiteral(modelForm.getName()));
        sr.append("/>");
        executeMacro(writer, sr.toString());
    }

    public void renderFormatItemRowCellOpen(Appendable writer, Map<String, Object> context, ModelForm modelForm, ModelFormField modelFormField, int positionSpan) throws IOException {
        String areaStyle = (modelFormField != null) ? modelFormField.getWidgetAreaStyle() : "";
        StringWriter sr = new StringWriter();
        sr.append("<@renderFormatItemRowCellOpen ");
        sr.append(" fieldName=");
        sr.append(ftlFmt.makeStringLiteral((modelFormField != null) ? modelFormField.getName() : ""));
        sr.append(" style=");
        sr.append(ftlFmt.makeStringLiteral(areaStyle));
        sr.append(" positionSpan=");
        sr.append(Integer.toString(positionSpan));
        sr.append(" />");
        executeMacro(writer, sr.toString());
    }

    public void renderFormatItemRowCellClose(Appendable writer, Map<String, Object> context, ModelForm modelForm, ModelFormField modelFormField) throws IOException {
        StringWriter sr = new StringWriter();
        sr.append("<@renderFormatItemRowCellClose");
        sr.append(" fieldName=");
        sr.append(ftlFmt.makeStringLiteral(modelFormField.getName()));
        sr.append("/>");
        executeMacro(writer, sr.toString());
    }

    public void renderFormatItemRowFormCellOpen(Appendable writer, Map<String, Object> context, ModelForm modelForm) throws IOException {
        String areaStyle = modelForm.getFormTitleAreaStyle();
        StringWriter sr = new StringWriter();
        sr.append("<@renderFormatItemRowFormCellOpen ");
        sr.append(" style=");
        sr.append(ftlFmt.makeStringLiteral(areaStyle));
        sr.append(" />");
        executeMacro(writer, sr.toString());
    }

    public void renderFormatItemRowFormCellClose(Appendable writer, Map<String, Object> context, ModelForm modelForm) throws IOException {
        StringWriter sr = new StringWriter();
        sr.append("<@renderFormatItemRowFormCellClose />");
        executeMacro(writer, sr.toString());
    }

    public void renderFormatSingleWrapperOpen(Appendable writer, Map<String, Object> context, ModelForm modelForm) throws IOException {
        String style = FlexibleStringExpander.expandString(modelForm.getDefaultTableStyle(), context);
        StringWriter sr = new StringWriter();
        sr.append("<@renderFormatSingleWrapperOpen ");
        sr.append(" formName=");
        sr.append(ftlFmt.makeStringLiteral(modelForm.getName()));
        sr.append(" style=");
        sr.append(ftlFmt.makeStringLiteral(style));
        sr.append(" />");
        executeMacro(writer, sr.toString());
    }

    public void renderFormatSingleWrapperClose(Appendable writer, Map<String, Object> context, ModelForm modelForm) throws IOException {
        StringWriter sr = new StringWriter();
        sr.append("<@renderFormatSingleWrapperClose");
        sr.append(" formName=");
        sr.append(ftlFmt.makeStringLiteral(modelForm.getName()));
        sr.append("/>");
        executeMacro(writer, sr.toString());
    }
    
    public void renderFormatFieldRowOpen(Appendable writer, Map<String, Object> context, ModelForm modelForm) throws IOException {
        Integer positions = (Integer) context.get("formFieldRender_positions");
        StringWriter sr = new StringWriter();
        sr.append("<@renderFormatFieldRowOpen");
        sr.append(" positions=" + (positions != null ? positions.toString() : "\"\""));
        sr.append(" />");
        executeMacro(writer, sr.toString());
    }

    public void renderFormatFieldRowClose(Appendable writer, Map<String, Object> context, ModelForm modelForm) throws IOException {
        StringWriter sr = new StringWriter();
        sr.append("<@renderFormatFieldRowClose />");
        executeMacro(writer, sr.toString());
    }
    
    /**
     * SCIPIO: appends position params.
     */
    private void appendPositionParams(Appendable sr, Map<String, Object> context, ModelFormField modelFormField) throws IOException {
        Integer positions = (Integer) context.get("formFieldRender_positions");
        Integer position = (Integer) context.get("formFieldRender_position");
        Integer positionSpan = (Integer) context.get("formFieldRender_positionSpan");
        Integer nextPositionInRow = (Integer) context.get("formFieldRender_nextPositionInRow");
        Integer lastPositionInRow = (Integer) context.get("formFieldRender_lastPositionInRow");
        sr.append(" positions=" + (positions != null ? positions.toString() : "\"\""));
        sr.append(" position=" + (position != null ? position.toString() : "\"\""));
        sr.append(" positionSpan=" + (positionSpan != null ? positionSpan.toString() : "\"\""));
        sr.append(" nextPositionInRow=" + (nextPositionInRow != null ? nextPositionInRow.toString() : "\"\""));
        sr.append(" lastPositionInRow=" + (lastPositionInRow != null ? lastPositionInRow.toString() : "\"\""));
    }

    // SCIPIO: new
    private void appendFieldType(Appendable sr, Map<String, Object> context, ModelFormField modelFormField) throws IOException {
        String fieldType = null;
        if (modelFormField.getFieldInfo() != null) {
            fieldType = modelFormField.getFieldInfo().getFieldTypeName();
        }
        if (fieldType == null) {
            fieldType = "";
        }
        sr.append(" fieldType=" + ftlFmt.makeStringLiteral(fieldType) + " ");
    }
    
    // SCIPIO: new
    private void appendFieldInfo(Appendable sr, Map<String, Object> context, ModelFormField modelFormField) throws IOException {
        appendFieldType(sr, context, modelFormField);
        boolean fieldTitleBlank = modelFormField.isBlankTitle(context);
        sr.append(" fieldTitleBlank=" + fieldTitleBlank + " ");
    }
    
    public void renderFormatFieldRowTitleCellOpen(Appendable writer, Map<String, Object> context, ModelFormField modelFormField) throws IOException {
        String style = modelFormField.getTitleAreaStyle();
        
        // SCIPIO: extra attribs map (json-like)
        String attribs = modelFormField.getAttribsExpr().compile(context);
        
        StringWriter sr = new StringWriter();
        sr.append("<@renderFormatFieldRowTitleCellOpen ");
        sr.append(" style=");
        sr.append(ftlFmt.makeStringLiteral(style));
        sr.append("");
        appendPositionParams(sr, context, modelFormField);
        appendFieldInfo(sr, context, modelFormField);
        appendAsterisksParams(sr, context, modelFormField);
        sr.append(" attribs=(");
        sr.append(attribs);
        sr.append(") />");
        executeMacro(writer, sr.toString());
    }

    public void renderFormatFieldRowTitleCellClose(Appendable writer, Map<String, Object> context, ModelFormField modelFormField) throws IOException {
        StringWriter sr = new StringWriter();
        sr.append("<@renderFormatFieldRowTitleCellClose ");
        appendFieldInfo(sr, context, modelFormField);
        sr.append(" />");
        executeMacro(writer, sr.toString());
    }

    public void renderFormatFieldRowSpacerCell(Appendable writer, Map<String, Object> context, ModelFormField modelFormField) throws IOException {
    }

    public void renderFormatFieldRowWidgetCellOpen(Appendable writer, Map<String, Object> context, ModelFormField modelFormField, int positions, int positionSpan, Integer nextPositionInRow) throws IOException {
        String areaStyle = modelFormField.getWidgetAreaStyle();
        
        // SCIPIO: extra attribs map (json-like)
        String attribs = modelFormField.getAttribsExpr().compile(context);
        
        StringWriter sr = new StringWriter();
        sr.append("<@renderFormatFieldRowWidgetCellOpen ");
        //sr.append(" positionSpan=");
        //sr.append(Integer.toString(positionSpan));
        sr.append(" style=");
        sr.append(ftlFmt.makeStringLiteral(areaStyle));
        sr.append("");
        appendPositionParams(sr, context, modelFormField);
        appendFieldInfo(sr, context, modelFormField);
        appendAsterisksParams(sr, context, modelFormField);
        sr.append(" attribs=(");
        sr.append(attribs);
        sr.append(") />");
        executeMacro(writer, sr.toString());
    }

    public void renderFormatFieldRowWidgetCellClose(Appendable writer, Map<String, Object> context, ModelFormField modelFormField, int positions, int positionSpan, Integer nextPositionInRow) throws IOException {
        StringWriter sr = new StringWriter();
        sr.append("<@renderFormatFieldRowWidgetCellClose ");
        appendFieldInfo(sr, context, modelFormField);
        sr.append(" />");
        executeMacro(writer, sr.toString());
    }

    public void renderFormatEmptySpace(Appendable writer, Map<String, Object> context, ModelForm modelForm) throws IOException {
        renderFormatEmptySpace(writer, context, modelForm, (String) context.get("renderFormatEmptySpace_role"));
    }
    
    // SCIPIO: 2017-01-13: new overloads for new role parameter
    public void renderFormatEmptySpace(Appendable writer, Map<String, Object> context, ModelForm modelForm, String role) throws IOException {
        StringWriter sr = new StringWriter();

        sr.append("<@renderFormatEmptySpace");
        sr.append(" role=");
        sr.append(ftlFmt.makeStringLiteral(role != null ? role : ""));
        sr.append(" formType=");
        sr.append(ftlFmt.makeStringLiteral(modelForm.getType()));
        sr.append("/>");
        
        executeMacro(writer, sr.toString());
    }

    public void renderTextFindField(Appendable writer, Map<String, Object> context, TextFindField textFindField) throws IOException {
        ModelFormField modelFormField = textFindField.getModelFormField();
        String defaultOption = textFindField.getDefaultOption(context);
        String className = "";
        String alert = "false";
        String opEquals = "";
        String opBeginsWith = "";
        String opContains = "";
        String opIsEmpty = "";
        String opNotEqual = "";
        String name = modelFormField.getParameterName(context);
        String size = Integer.toString(textFindField.getSize());
        String maxlength = "";
        String autocomplete = "";
        if (UtilValidate.isNotEmpty(modelFormField.getWidgetStyle(context))) {
            className = modelFormField.getWidgetStyle(context);
            if (modelFormField.shouldBeRed(context)) {
                alert = "true";
            }
        }
        Locale locale = (Locale) context.get("locale");
        // SCIPIO: pass options hide setting to macro
        boolean hideOptions = textFindField.getHideOptions();
        if (!hideOptions) {
            opEquals = UtilProperties.getMessage("conditional", "equals", locale);
            opBeginsWith = UtilProperties.getMessage("conditional", "begins_with", locale);
            opContains = UtilProperties.getMessage("conditional", "contains", locale);
            opIsEmpty = UtilProperties.getMessage("conditional", "is_empty", locale);
            opNotEqual = UtilProperties.getMessage("conditional", "not_equal", locale);
        }
        String value = modelFormField.getEntry(context, textFindField.getDefaultValue(context));
        if (value == null) {
            value = "";
        }
        if (textFindField.getMaxlength() != null) {
            maxlength = textFindField.getMaxlength().toString();
        }
        if (!textFindField.getClientAutocompleteField()) {
            autocomplete = "off";
        }
        String titleStyle = "";
        if (UtilValidate.isNotEmpty(modelFormField.getTitleStyle())) {
            titleStyle = modelFormField.getTitleStyle();
        }
        String ignoreCase = UtilProperties.getMessage("conditional", "ignore_case", locale);
        boolean ignCase = textFindField.getIgnoreCase(context);
        boolean hideIgnoreCase = textFindField.getHideIgnoreCase();
        StringWriter sr = new StringWriter();
        sr.append("<@renderTextFindField ");
        sr.append(" name=");
        sr.append(ftlFmt.makeStringLiteral(name));
        sr.append(" value=");
        sr.append(ftlFmt.makeStringLiteral(value));
        sr.append(" defaultOption=");
        sr.append(ftlFmt.makeStringLiteral(defaultOption));
        sr.append(" opEquals=");
        sr.append(ftlFmt.makeStringLiteral(opEquals));
        sr.append(" opBeginsWith=");
        sr.append(ftlFmt.makeStringLiteral(opBeginsWith));
        sr.append(" opContains=");
        sr.append(ftlFmt.makeStringLiteral(opContains));
        sr.append(" opIsEmpty=");
        sr.append(ftlFmt.makeStringLiteral(opIsEmpty));
        sr.append(" opNotEqual=");
        sr.append(ftlFmt.makeStringLiteral(opNotEqual));
        sr.append(" className=");
        sr.append(ftlFmt.makeStringLiteral(className));
        sr.append(" alert=");
        sr.append(ftlFmt.makeStringLiteral(alert));
        sr.append(" size=");
        sr.append(ftlFmt.makeStringLiteral(size));
        sr.append(" maxlength=");
        sr.append(ftlFmt.makeStringLiteral(maxlength));
        sr.append(" autocomplete=");
        sr.append(ftlFmt.makeStringLiteral(autocomplete));
        sr.append(" titleStyle=");
        sr.append(ftlFmt.makeStringLiteral(titleStyle));
        sr.append(" hideIgnoreCase=");
        sr.append(Boolean.toString(hideIgnoreCase));
        sr.append(" ignCase=");
        sr.append(Boolean.toString(ignCase));
        sr.append(" ignoreCase=");
        sr.append(ftlFmt.makeStringLiteral(ignoreCase));
        sr.append("");
        // SCIPIO: new args
        sr.append(" hideOptions=");
        sr.append(Boolean.toString(hideOptions));
        appendRequiredFieldParam(sr, context, modelFormField);
        sr.append(" />");
        executeMacro(writer, sr.toString());
        this.appendTooltip(writer, context, modelFormField);
    }

    public void renderRangeFindField(Appendable writer, Map<String, Object> context, RangeFindField rangeFindField) throws IOException {
        ModelFormField modelFormField = rangeFindField.getModelFormField();
        Locale locale = (Locale) context.get("locale");
        String opEquals = UtilProperties.getMessage("conditional", "equals", locale);
        String opGreaterThan = UtilProperties.getMessage("conditional", "greater_than", locale);
        String opGreaterThanEquals = UtilProperties.getMessage("conditional", "greater_than_equals", locale);
        String opLessThan = UtilProperties.getMessage("conditional", "less_than", locale);
        String opLessThanEquals = UtilProperties.getMessage("conditional", "less_than_equals", locale);
        //String opIsEmpty = UtilProperties.getMessage("conditional", "is_empty", locale);
        String className = "";
        String alert = "false";
        if (UtilValidate.isNotEmpty(modelFormField.getWidgetStyle(context))) {
            className = modelFormField.getWidgetStyle(context);
            if (modelFormField.shouldBeRed(context)) {
                alert = "true";
            }
        }
        String name = modelFormField.getParameterName(context);
        String size = Integer.toString(rangeFindField.getSize());
        String value = modelFormField.getEntry(context, rangeFindField.getDefaultValue(context));
        if (value == null) {
            value = "";
        }
        Integer maxlength = rangeFindField.getMaxlength();
        String autocomplete = "";

        if (!rangeFindField.getClientAutocompleteField()) {
            autocomplete = "off";
        }
        String titleStyle = modelFormField.getTitleStyle();

        if (titleStyle == null) {
            titleStyle = "";
        }
        String defaultOptionFrom = rangeFindField.getDefaultOptionFrom();
        String value2 = modelFormField.getEntry(context);
        if (value2 == null) {
            value2 = "";
        }
        String defaultOptionThru = rangeFindField.getDefaultOptionThru();
        StringWriter sr = new StringWriter();
        sr.append("<@renderRangeFindField ");
        sr.append(" className=");
        sr.append(ftlFmt.makeStringLiteral(className));
        sr.append(" alert=");
        sr.append(ftlFmt.makeStringLiteral(alert));
        sr.append(" name=");
        sr.append(ftlFmt.makeStringLiteral(name));
        sr.append(" value=");
        sr.append(ftlFmt.makeStringLiteral(value));
        sr.append(" size=");
        sr.append(ftlFmt.makeStringLiteral(size));
        sr.append(" maxlength=");
        sr.append(ftlFmt.makeStringLiteral(maxlength));
        sr.append(" autocomplete=");
        sr.append(ftlFmt.makeStringLiteral(autocomplete));
        sr.append(" titleStyle=");
        sr.append(ftlFmt.makeStringLiteral(titleStyle));
        sr.append(" defaultOptionFrom=");
        sr.append(ftlFmt.makeStringLiteral(defaultOptionFrom));
        sr.append(" opEquals=");
        sr.append(ftlFmt.makeStringLiteral(opEquals));
        sr.append(" opGreaterThan=");
        sr.append(ftlFmt.makeStringLiteral(opGreaterThan));
        sr.append(" opGreaterThanEquals=");
        sr.append(ftlFmt.makeStringLiteral(opGreaterThanEquals));
        sr.append(" opLessThan=");
        sr.append(ftlFmt.makeStringLiteral(opLessThan));
        sr.append(" opLessThanEquals=");
        sr.append(ftlFmt.makeStringLiteral(opLessThanEquals));
        sr.append(" value2=");
        sr.append(ftlFmt.makeStringLiteral(value2));
        sr.append(" defaultOptionThru=");
        sr.append(ftlFmt.makeStringLiteral(defaultOptionThru));
        appendRequiredFieldParam(sr, context, modelFormField);
        sr.append(" />");
        executeMacro(writer, sr.toString());
        this.appendTooltip(writer, context, modelFormField);
    }

    public void renderDateFindField(Appendable writer, Map<String, Object> context, DateFindField dateFindField) throws IOException {
        ModelFormField modelFormField = dateFindField.getModelFormField();
        Locale locale = (Locale) context.get("locale");
        String opEquals = UtilProperties.getMessage("conditional", "equals", locale);
        String opGreaterThan = UtilProperties.getMessage("conditional", "greater_than", locale);
        String opSameDay = UtilProperties.getMessage("conditional", "same_day", locale);
        String opGreaterThanFromDayStart = UtilProperties.getMessage("conditional", "greater_than_from_day_start", locale);
        String opLessThan = UtilProperties.getMessage("conditional", "less_than", locale);
        String opUpToDay = UtilProperties.getMessage("conditional", "up_to_day", locale);
        String opUpThruDay = UtilProperties.getMessage("conditional", "up_thru_day", locale);
        String opIsEmpty = UtilProperties.getMessage("conditional", "is_empty", locale);
        Map<String, String> uiLabelMap = UtilGenerics.checkMap(context.get("uiLabelMap"));
        if (uiLabelMap == null) {
            Debug.logWarning("Could not find uiLabelMap in context", module);
        }
        String localizedInputTitle = "", localizedIconTitle = "";
        String className = "";
        String alert = "false";
        if (UtilValidate.isNotEmpty(modelFormField.getWidgetStyle(context))) {
            className = modelFormField.getWidgetStyle(context);
            if (modelFormField.shouldBeRed(context)) {
                alert = "true";
            }
        }
        String name = modelFormField.getParameterName(context);
        // the default values for a timestamp
        int size = 25;
        int maxlength = 30;
        String dateType = dateFindField.getType();
        if ("date".equals(dateType)) {
            size = maxlength = 10;
            if (uiLabelMap != null) {
                localizedInputTitle = uiLabelMap.get("CommonFormatDate");
            }
        } else if ("time".equals(dateFindField.getType())) {
            size = maxlength = 8;
            if (uiLabelMap != null) {
                localizedInputTitle = uiLabelMap.get("CommonFormatTime");
            }
        } else {
            if (uiLabelMap != null) {
                localizedInputTitle = uiLabelMap.get("CommonFormatDateTime");
            }
        }
        String value = modelFormField.getEntry(context, dateFindField.getDefaultValue(context));
        if (value == null) {
            value = "";
        }
        // search for a localized label for the icon
        if (uiLabelMap != null) {
            localizedIconTitle = uiLabelMap.get("CommonViewCalendar");
        }
        String formName = "";
        String defaultDateTimeString = "";
        StringBuilder imgSrc = new StringBuilder();
        // add calendar pop-up button and seed data IF this is not a "time" type date-find
        if (!"time".equals(dateFindField.getType())) {
            ModelForm modelForm = modelFormField.getModelForm();
            formName = FormRenderer.getCurrentFormName(modelForm, context);
            defaultDateTimeString = UtilHttp.encodeBlanks(modelFormField.getEntry(context, dateFindField.getDefaultDateTimeString(context)));
            this.appendContentUrl(imgSrc, "/images/cal.gif");
        }
        String defaultOptionFrom = dateFindField.getDefaultOptionFrom(context);
        String defaultOptionThru = dateFindField.getDefaultOptionThru(context);
        String value2 = modelFormField.getEntry(context);
        if (value2 == null) {
            value2 = "";
        }
        if (context.containsKey("parameters")) {
            Map<String, Object> parameters = UtilGenerics.checkMap(context.get("parameters"));
            if (parameters.containsKey(name + "_fld0_value")) {
                value = (String) parameters.get(name + "_fld0_value");
            }
            if (parameters.containsKey(name + "_fld1_value")) {
                value2 = (String) parameters.get(name + "_fld1_value");
            }
        }
        
        String titleStyle = "";
        if (UtilValidate.isNotEmpty(modelFormField.getTitleStyle())) {
            titleStyle = modelFormField.getTitleStyle();
        }
        StringWriter sr = new StringWriter();
        sr.append("<@renderDateFindField ");
        sr.append(" className=");
        sr.append(ftlFmt.makeStringLiteral(className));
        sr.append(" alert=");
        sr.append(ftlFmt.makeStringLiteral(alert));
        sr.append(" name=");
        sr.append(ftlFmt.makeStringLiteral(name));
        sr.append(" localizedInputTitle=");
        sr.append(ftlFmt.makeStringLiteral(localizedInputTitle));
        sr.append(" value=");
        sr.append(ftlFmt.makeStringLiteral(value));
        sr.append(" value2=");
        sr.append(ftlFmt.makeStringLiteral(value2));
        sr.append(" size=");
        sr.append(ftlFmt.makeStringLiteral(Integer.toString(size)));
        sr.append(" maxlength=");
        sr.append(ftlFmt.makeStringLiteral(Integer.toString(maxlength)));
        sr.append(" dateType=");
        sr.append(ftlFmt.makeStringLiteral(dateType));
        sr.append(" formName=");
        sr.append(ftlFmt.makeStringLiteral(formName));
        sr.append(" defaultDateTimeString=");
        sr.append(ftlFmt.makeStringLiteral(defaultDateTimeString));
        sr.append(" imgSrc=");
        sr.append(ftlFmt.makeStringLiteral(imgSrc.toString()));
        sr.append(" localizedIconTitle=");
        sr.append(ftlFmt.makeStringLiteral(localizedIconTitle));
        sr.append(" titleStyle=");
        sr.append(ftlFmt.makeStringLiteral(titleStyle));
        sr.append(" defaultOptionFrom=");
        sr.append(ftlFmt.makeStringLiteral(defaultOptionFrom));
        sr.append(" defaultOptionThru=");
        sr.append(ftlFmt.makeStringLiteral(defaultOptionThru));
        sr.append(" opEquals=");
        sr.append(ftlFmt.makeStringLiteral(opEquals));
        sr.append(" opSameDay=");
        sr.append(ftlFmt.makeStringLiteral(opSameDay));
        sr.append(" opGreaterThanFromDayStart=");
        sr.append(ftlFmt.makeStringLiteral(opGreaterThanFromDayStart));
        sr.append(" opGreaterThan=");
        sr.append(ftlFmt.makeStringLiteral(opGreaterThan));
        sr.append(" opGreaterThan=");
        sr.append(ftlFmt.makeStringLiteral(opGreaterThan));
        sr.append(" opLessThan=");
        sr.append(ftlFmt.makeStringLiteral(opLessThan));
        sr.append(" opUpToDay=");
        sr.append(ftlFmt.makeStringLiteral(opUpToDay));
        sr.append(" opUpThruDay=");
        sr.append(ftlFmt.makeStringLiteral(opUpThruDay));
        sr.append(" opIsEmpty=");
        sr.append(ftlFmt.makeStringLiteral(opIsEmpty));
        appendRequiredFieldParam(sr, context, modelFormField);
        sr.append(" />");
        executeMacro(writer, sr.toString());
        this.appendTooltip(writer, context, modelFormField);
    }

    public void renderLookupField(Appendable writer, Map<String, Object> context, LookupField lookupField) throws IOException {
        ModelFormField modelFormField = lookupField.getModelFormField();
        String lookupFieldFormName = lookupField.getFormName(context);
        String className = "";
        String alert = "false";
        if (UtilValidate.isNotEmpty(modelFormField.getWidgetStyle(context))) {
            className = modelFormField.getWidgetStyle(context);
            if (modelFormField.shouldBeRed(context)) {
                alert = "true";
            }
        }
        //check for required field style on single forms
        if ("single".equals(modelFormField.getModelForm().getType()) && modelFormField.getRequiredField()) {
            className = combineRequiredStyle(className, context, modelFormField); // SCIPIO
        }
        String name = modelFormField.getParameterName(context);
        String value = modelFormField.getEntry(context, lookupField.getDefaultValue(context));
        if (value == null) {
            value = "";
        }
        String size = Integer.toString(lookupField.getSize());
        Integer maxlength = lookupField.getMaxlength();
        String id = modelFormField.getCurrentContainerId(context);
        List<ModelForm.UpdateArea> updateAreas = modelFormField.getOnChangeUpdateAreas();
        //add default ajax auto completer to all lookup fields
        if (UtilValidate.isEmpty(updateAreas) && UtilValidate.isNotEmpty(lookupFieldFormName)) {
            String autoCompleterTarget = null;
            if (lookupFieldFormName.indexOf('?') == -1) {
                autoCompleterTarget = lookupFieldFormName + "?";
            } else {
                autoCompleterTarget = lookupFieldFormName + "&amp;amp;";
            }
            autoCompleterTarget = autoCompleterTarget + "ajaxLookup=Y";
            updateAreas = new LinkedList<ModelForm.UpdateArea>();
            //SCIPIO: ugly hack but seems to work for now, so the lookup field can find the proper field
            String updateAreaId = id;
            if (modelFormField.getModelForm().getType().equals("list"))
                updateAreaId = name;
            updateAreas.add(new ModelForm.UpdateArea("change", updateAreaId, autoCompleterTarget));
        }
        boolean ajaxEnabled = UtilValidate.isNotEmpty(updateAreas) && this.javaScriptEnabled;
        String autocomplete = "";
        if (!lookupField.getClientAutocompleteField() || !ajaxEnabled) {
            autocomplete = "off";
        }
        String event = modelFormField.getEvent();
        String action = modelFormField.getAction(context);
        boolean readonly = lookupField.getReadonly();
        // add lookup pop-up button
        String descriptionFieldName = lookupField.getDescriptionFieldName();
        ModelForm modelForm = modelFormField.getModelForm();
        String formName = modelFormField.getParentFormName();
        if (UtilValidate.isEmpty(formName)) {
            formName = FormRenderer.getCurrentFormName(modelForm, context);
        }        
        StringBuilder targetParameterIter = new StringBuilder();
        StringBuilder imgSrc = new StringBuilder();
        // FIXME: refactor using the StringUtils methods
        List<String> targetParameterList = lookupField.getTargetParameterList();
        targetParameterIter.append("[");
        for (String targetParameter : targetParameterList) {
            if (targetParameterIter.length() > 1) {
                targetParameterIter.append(",");
            }
            targetParameterIter.append(ftlFmt.makeStringLiteralSQ(targetParameter));
        }
        targetParameterIter.append("]");
        this.appendContentUrl(imgSrc, "/images/fieldlookup.gif");
        String ajaxUrl = "";
        if (ajaxEnabled) {
            ajaxUrl = createAjaxParamsFromUpdateAreas(updateAreas, "", context);
        }
        String lookupPresentation = lookupField.getLookupPresentation();
        if (UtilValidate.isEmpty(lookupPresentation)) {
            lookupPresentation = "";
        }
        String lookupHeight = lookupField.getLookupHeight();
        if (UtilValidate.isEmpty(lookupHeight)) {
            lookupHeight = "";
        }
        String lookupWidth = lookupField.getLookupWidth();
        if (UtilValidate.isEmpty(lookupWidth)) {
            lookupWidth = "";
        }
        String lookupPosition = lookupField.getLookupPosition();
        if (UtilValidate.isEmpty(lookupPosition)) {
            lookupPosition = "";
        }
        String fadeBackground = lookupField.getFadeBackground();
        if (UtilValidate.isEmpty(fadeBackground)) {
            fadeBackground = "false";
        }
        String tooltip = modelFormField.getTooltip(context); // SCIPIO: new arg
        if (UtilValidate.isEmpty(tooltip)) {
            tooltip = "";
        }
        Boolean isInitiallyCollapsed = lookupField.getInitiallyCollapsed();
        String clearText = "";
        Map<String, Object> uiLabelMap = UtilGenerics.checkMap(context.get("uiLabelMap"));
        if (uiLabelMap != null) {
            clearText = (String) uiLabelMap.get("CommonClear");
        } else {
            Debug.logWarning("Could not find uiLabelMap in context", module);
        }
        Boolean showDescription = lookupField.getShowDescription();
        if (showDescription == null) {
            showDescription = "Y".equals(UtilProperties.getPropertyValue("widget", "widget.lookup.showDescription", "Y"));
        }
        // lastViewName, used by lookup to remember the real last view name
        String lastViewName = request.getParameter("_LAST_VIEW_NAME_"); // Try to get it from parameters firstly
        if (UtilValidate.isEmpty(lastViewName)) { // get from session
            lastViewName = (String) request.getSession().getAttribute("_LAST_VIEW_NAME_");
        }
        if (UtilValidate.isEmpty(lastViewName)) {
            lastViewName = "";
        }
        StringWriter sr = new StringWriter();
        sr.append("<@renderLookupField ");
        appendFieldInfo(sr, context, modelFormField);
        sr.append(" className=");
        sr.append(ftlFmt.makeStringLiteral(className));
        sr.append(" alert=");
        sr.append(ftlFmt.makeStringLiteral(alert));
        sr.append(" name=");
//        if (modelForm.getType().equals("list"))
//            sr.append(makeFtlStringLit(id));
//        else
            sr.append(ftlFmt.makeStringLiteral(name));
        sr.append(" value=");
        sr.append(ftlFmt.makeStringLiteral(value));
        sr.append(" size=");
        sr.append(ftlFmt.makeStringLiteral(size));
        sr.append(" maxlength=");
        sr.append(ftlFmt.makeStringLiteral(maxlength));
        sr.append(" id=");
        sr.append(ftlFmt.makeStringLiteral(id));
        sr.append(" event=");
        sr.append(ftlFmt.makeStringLiteral(event));
        sr.append(" action=");
        sr.append(ftlFmt.makeStringLiteral(action));
        sr.append(" readonly=");
        sr.append(Boolean.toString(readonly));
        sr.append(" autocomplete=");
        sr.append(ftlFmt.makeStringLiteral(autocomplete));
        sr.append(" descriptionFieldName=");
        sr.append(ftlFmt.makeStringLiteral(descriptionFieldName));
        sr.append(" formName=");
        sr.append(ftlFmt.makeStringLiteral(formName));
        sr.append(" fieldFormName=");
        sr.append(ftlFmt.makeStringLiteral(lookupFieldFormName));
        sr.append(" targetParameterIter=");
        sr.append(targetParameterIter.toString());
        sr.append(" imgSrc=");
        sr.append(ftlFmt.makeStringLiteral(imgSrc.toString()));
        sr.append(" ajaxUrl=");
        sr.append(ftlFmt.makeStringLiteral(ajaxUrl));
        sr.append(" ajaxEnabled=");
        sr.append(Boolean.toString(ajaxEnabled));
        sr.append(" presentation=");
        sr.append(ftlFmt.makeStringLiteral(lookupPresentation));
        sr.append(" height=");
        sr.append(ftlFmt.makeStringLiteral(lookupHeight));
        sr.append(" width=");
        sr.append(ftlFmt.makeStringLiteral(lookupWidth));
        sr.append(" position=");
        sr.append(ftlFmt.makeStringLiteral(lookupPosition));
        sr.append(" fadeBackground=");
        sr.append(ftlFmt.makeStringLiteral(fadeBackground));
        sr.append(" clearText=");
        sr.append(ftlFmt.makeStringLiteral(clearText));
        sr.append(" showDescription=");
        sr.append(ftlFmt.makeStringLiteral(showDescription));
        sr.append(" initiallyCollapsed=");
        sr.append(ftlFmt.makeStringLiteral(isInitiallyCollapsed));
        sr.append(" lastViewName=");
        sr.append(ftlFmt.makeStringLiteral(lastViewName));
        sr.append(" tooltip=");
        sr.append(ftlFmt.makeStringLiteral(tooltip)); // SCIPIO: new arg
        appendRequiredFieldParam(sr, context, modelFormField);
        sr.append(" />");
        executeMacro(writer, sr.toString());
        this.addAsterisks(writer, context, modelFormField);
        this.makeHyperlinkString(writer, lookupField.getSubHyperlink(), context);
        this.appendTooltip(writer, context, modelFormField);
    }

    protected String appendExternalLoginKey(String target) {
        String result = target;
        String sessionId = ";jsessionid=" + request.getSession().getId();
        int questionIndex = target.indexOf("?");
        if (questionIndex == -1) {
            result += sessionId;
        } else {
            result = result.replace("?", sessionId + "?");
        }
        return result;
    }

    // SCIPIO: new param: position
    public void renderNextPrev(Appendable writer, Map<String, Object> context, ModelForm modelForm, String position) throws IOException {
        boolean ajaxEnabled = false;
        List<ModelForm.UpdateArea> updateAreas = modelForm.getOnPaginateUpdateAreas();
        String targetService = modelForm.getPaginateTarget(context);
        if (this.javaScriptEnabled) {
            if (UtilValidate.isNotEmpty(updateAreas)) {
                ajaxEnabled = true;
            }
        }
        if (targetService == null) {
            targetService = "${targetService}";
        }
        if (UtilValidate.isEmpty(targetService) && updateAreas == null) {
            Debug.logWarning("Cannot paginate because TargetService is empty for the form: " + modelForm.getName(), module);
            return;
        }
        // get the parameterized pagination index and size fields
        int paginatorNumber = WidgetWorker.getPaginatorNumber(context);
        String viewIndexParam = modelForm.getMultiPaginateIndexField(context);
        String viewSizeParam = modelForm.getMultiPaginateSizeField(context);
        int viewIndex = Paginator.getViewIndex(modelForm, context);
        int viewSize = Paginator.getViewSize(modelForm, context);
        int listSize = Paginator.getListSize(context);
        int lowIndex = Paginator.getLowIndex(context);
        int highIndex = Paginator.getHighIndex(context);
        int actualPageSize = Paginator.getActualPageSize(context);
        // needed for the "Page" and "rows" labels
        Map<String, String> uiLabelMap = UtilGenerics.checkMap(context.get("uiLabelMap"));
        String pageLabel = "";
        String commonDisplaying = "";
        if (uiLabelMap == null) {
            Debug.logWarning("Could not find uiLabelMap in context", module);
        } else {
            pageLabel = uiLabelMap.get("CommonPage");
            Map<String, Integer> messageMap = UtilMisc.toMap("lowCount", Integer.valueOf(lowIndex + 1), "highCount", Integer.valueOf(lowIndex + actualPageSize), "total", Integer.valueOf(listSize));
            commonDisplaying = UtilProperties.getMessage("CommonUiLabels", "CommonDisplaying", messageMap, (Locale) context.get("locale"));
        }
        
        // SCIPIO: realHighIndex needed for macro
        int realHighIndex = Integer.valueOf(lowIndex + actualPageSize);
        
        // for legacy support, the viewSizeParam is VIEW_SIZE and viewIndexParam is VIEW_INDEX when the fields are "viewSize" and "viewIndex"
        if (viewIndexParam.equals("viewIndex" + "_" + paginatorNumber))
            viewIndexParam = "VIEW_INDEX" + "_" + paginatorNumber;
        if (viewSizeParam.equals("viewSize" + "_" + paginatorNumber))
            viewSizeParam = "VIEW_SIZE" + "_" + paginatorNumber;
        String str = (String) context.get("_QBESTRING_");
        // strip legacy viewIndex/viewSize params from the query string
        String queryString = UtilHttp.stripViewParamsFromQueryString(str, "" + paginatorNumber);
        // strip parameterized index/size params from the query string
        HashSet<String> paramNames = new HashSet<String>();
        paramNames.add(viewIndexParam);
        paramNames.add(viewSizeParam);
        queryString = UtilHttp.stripNamedParamsFromQueryString(queryString, paramNames);
        String anchor = "";
        String paginateAnchor = modelForm.getPaginateTargetAnchor();
        if (UtilValidate.isNotEmpty(paginateAnchor))
            anchor = "#" + paginateAnchor;
        // Create separate url path String and request parameters String,
        // add viewIndex/viewSize parameters to request parameter String
        String urlPath = UtilHttp.removeQueryStringFromTarget(targetService);
        String prepLinkText = UtilHttp.getQueryStringFromTarget(targetService);
        
        // SCIPIO: Also prevent duplicate params between queryString and targetService
        if (!UtilValidate.isEmpty(queryString) && !queryString.equals("null") &&
            !UtilValidate.isEmpty(prepLinkText)) {
            // get queryString param names
            String[] qsParamPairs = queryString.split("&(amp;)?");
            Set<String> qsParamNames = new HashSet<String>(qsParamPairs.length);
            for(String paramPair : qsParamPairs) {
                String[] pair = paramPair.split("=",2);
                if (pair[0] != null && pair[0].length() > 0) {
                    qsParamNames.add(pair[0]);
                }
            }
            String newText;
            String rawTsParams;
            if (prepLinkText.startsWith("?")) {
                newText = "?";
                rawTsParams = prepLinkText.substring(1);
            }
            else {
                newText = "";
                rawTsParams = prepLinkText;
            }
            // remove params already in queryString from targetService params
            newText += UtilHttp.stripNamedParamsFromQueryString(rawTsParams, qsParamNames);
            prepLinkText = newText;
        }  
        
        String prepLinkSizeText;
        if (UtilValidate.isNotEmpty(queryString)) {
            queryString = UtilHttp.encodeAmpersands(queryString);
        }
        if (prepLinkText == null) {
            prepLinkText = "";
        }
        if (prepLinkText.indexOf("?") < 0) {
            prepLinkText += "?";
        } else if (!prepLinkText.endsWith("?")) {
            prepLinkText += "&amp;";
        }
        if (!UtilValidate.isEmpty(queryString) && !queryString.equals("null")) {
            prepLinkText += queryString + "&amp;";
        }
        prepLinkSizeText = prepLinkText + viewSizeParam + "='+this.value+'" + "&amp;" + viewIndexParam + "=0";
        prepLinkText += viewSizeParam + "=" + viewSize + "&amp;" + viewIndexParam + "=";
        if (ajaxEnabled) {
            // Prepare params for prototype.js
            prepLinkText = prepLinkText.replace("?", "");
            prepLinkText = prepLinkText.replace("&amp;", "&");
        }
        String linkText;
        String paginateStyle = modelForm.getPaginateStyle();
        String paginateFirstStyle = modelForm.getPaginateFirstStyle();
        String paginateFirstLabel = modelForm.getPaginateFirstLabel(context);
        String firstUrl = "";
        String ajaxFirstUrl = "";
        String paginatePreviousStyle = modelForm.getPaginatePreviousStyle();
        String paginatePreviousLabel = modelForm.getPaginatePreviousLabel(context);
        String previousUrl = "";
        String ajaxPreviousUrl = "";
        String selectUrl = "";
        String ajaxSelectUrl = "";
        String paginateViewSizeLabel = modelForm.getPaginateViewSizeLabel(context);
        String selectSizeUrl = "";
        String ajaxSelectSizeUrl = "";
        String paginateNextStyle = modelForm.getPaginateNextStyle();
        String paginateNextLabel = modelForm.getPaginateNextLabel(context);
        String nextUrl = "";
        String ajaxNextUrl = "";
        String paginateLastStyle = modelForm.getPaginateLastStyle();
        String paginateLastLabel = modelForm.getPaginateLastLabel(context);
        String lastUrl = "";
        String ajaxLastUrl = "";
        if (viewIndex > 0) {
            if (ajaxEnabled) {
                ajaxFirstUrl = createAjaxParamsFromUpdateAreas(updateAreas, prepLinkText + 0 + anchor, context);
            } else {
                linkText = prepLinkText + 0 + anchor;
                firstUrl = rh.makeLink(this.request, this.response, urlPath + linkText);
            }
        }
        if (viewIndex > 0) {
            if (ajaxEnabled) {
                ajaxPreviousUrl = createAjaxParamsFromUpdateAreas(updateAreas, prepLinkText + (viewIndex - 1) + anchor, context);
            } else {
                linkText = prepLinkText + (viewIndex - 1) + anchor;
                previousUrl = rh.makeLink(this.request, this.response, urlPath + linkText);
            }
        }
        // Page select dropdown
        if (listSize > 0) {
            if (ajaxEnabled) {
                ajaxSelectUrl = createAjaxParamsFromUpdateAreas(updateAreas, prepLinkText + "' + this.value + '", context);
            } else {
                linkText = prepLinkText;
                if (linkText.startsWith("/")) {
                    linkText = linkText.substring(1);
                }
                selectUrl = rh.makeLink(this.request, this.response, urlPath + linkText);
            }
        }
        // Next button
        if (highIndex < listSize) {
            if (ajaxEnabled) {
                ajaxNextUrl = createAjaxParamsFromUpdateAreas(updateAreas, prepLinkText + (viewIndex + 1) + anchor, context);
            } else {
                linkText = prepLinkText + (viewIndex + 1) + anchor;
                nextUrl = rh.makeLink(this.request, this.response, urlPath + linkText);
            }
        }
        // Last button
        if (highIndex < listSize) {
            int lastIndex = UtilMisc.getViewLastIndex(listSize, viewSize);
            if (ajaxEnabled) {
                ajaxLastUrl = createAjaxParamsFromUpdateAreas(updateAreas, prepLinkText + lastIndex + anchor, context);
            } else {
                linkText = prepLinkText + lastIndex + anchor;
                lastUrl = rh.makeLink(this.request, this.response, urlPath + linkText);
            }
        }
        // Page size select dropdown
        if (listSize > 0) {
            if (ajaxEnabled) {
                ajaxSelectSizeUrl = createAjaxParamsFromUpdateAreas(updateAreas, prepLinkSizeText + anchor, context);
            } else {
                linkText = prepLinkSizeText;
                if (linkText.startsWith("/")) {
                    linkText = linkText.substring(1);
                }
                selectSizeUrl = rh.makeLink(this.request, this.response, urlPath + linkText);
            }
        }
        boolean paginate = modelForm.getPaginate(context);
        StringWriter sr = new StringWriter();
        sr.append("<@renderNextPrev ");
        sr.append(" paginateStyle=");
        sr.append(ftlFmt.makeStringLiteral(paginateStyle));
        sr.append(" paginateFirstStyle=");
        sr.append(ftlFmt.makeStringLiteral(paginateFirstStyle));
        sr.append(" viewIndex=");
        sr.append(Integer.toString(viewIndex));
        sr.append(" highIndex=");
        sr.append(Integer.toString(highIndex));
        sr.append(" listSize=");
        sr.append(Integer.toString(listSize));
        sr.append(" viewSize=");
        sr.append(Integer.toString(viewSize));
        sr.append(" ajaxEnabled=");
        sr.append(Boolean.toString(ajaxEnabled));
        sr.append(" javaScriptEnabled=");
        sr.append(Boolean.toString(javaScriptEnabled));
        sr.append(" ajaxFirstUrl=");
        sr.append(ftlFmt.makeStringLiteral(ajaxFirstUrl));
        sr.append(" ajaxFirstUrl=");
        sr.append(ftlFmt.makeStringLiteral(ajaxFirstUrl));
        sr.append(" ajaxFirstUrl=");
        sr.append(ftlFmt.makeStringLiteral(ajaxFirstUrl));
        sr.append(" firstUrl=");
        sr.append(ftlFmt.makeStringLiteral(firstUrl));
        sr.append(" paginateFirstLabel=");
        sr.append(ftlFmt.makeStringLiteral(paginateFirstLabel));
        sr.append(" paginatePreviousStyle=");
        sr.append(ftlFmt.makeStringLiteral(paginatePreviousStyle));
        sr.append(" ajaxPreviousUrl=");
        sr.append(ftlFmt.makeStringLiteral(ajaxPreviousUrl));
        sr.append(" previousUrl=");
        sr.append(ftlFmt.makeStringLiteral(previousUrl));
        sr.append(" paginatePreviousLabel=");
        sr.append(ftlFmt.makeStringLiteral(paginatePreviousLabel));
        sr.append(" pageLabel=");
        sr.append(ftlFmt.makeStringLiteral(pageLabel));
        sr.append(" ajaxSelectUrl=");
        sr.append(ftlFmt.makeStringLiteral(ajaxSelectUrl));
        sr.append(" selectUrl=");
        sr.append(ftlFmt.makeStringLiteral(selectUrl));
        sr.append(" ajaxSelectSizeUrl=");
        sr.append(ftlFmt.makeStringLiteral(ajaxSelectSizeUrl));
        sr.append(" selectSizeUrl=");
        sr.append(ftlFmt.makeStringLiteral(selectSizeUrl));
        sr.append(" commonDisplaying=");
        sr.append(ftlFmt.makeStringLiteral(commonDisplaying));
        sr.append(" paginateNextStyle=");
        sr.append(ftlFmt.makeStringLiteral(paginateNextStyle));
        sr.append(" ajaxNextUrl=");
        sr.append(ftlFmt.makeStringLiteral(ajaxNextUrl));
        sr.append(" nextUrl=");
        sr.append(ftlFmt.makeStringLiteral(nextUrl));
        sr.append(" paginateNextLabel=");
        sr.append(ftlFmt.makeStringLiteral(paginateNextLabel));
        sr.append(" paginateLastStyle=");
        sr.append(ftlFmt.makeStringLiteral(paginateLastStyle));
        sr.append(" ajaxLastUrl=");
        sr.append(ftlFmt.makeStringLiteral(ajaxLastUrl));
        sr.append(" lastUrl=");
        sr.append(ftlFmt.makeStringLiteral(lastUrl));
        sr.append(" paginateLastLabel=");
        sr.append(ftlFmt.makeStringLiteral(paginateLastLabel));
        sr.append(" paginateViewSizeLabel=");
        sr.append(ftlFmt.makeStringLiteral(paginateViewSizeLabel));
        // SCIPIO: new params
        sr.append(" paginate=");
        sr.append(Boolean.toString(paginate));
        sr.append(" lowIndex=");
        sr.append(Integer.toString(lowIndex));
        sr.append(" realHighIndex=");
        sr.append(Integer.toString(realHighIndex));
        sr.append(" position=");
        sr.append(ftlFmt.makeStringLiteral(position != null ? position : ""));
        sr.append(" />");
        executeMacro(writer, sr.toString());
    }

    public void renderFileField(Appendable writer, Map<String, Object> context, FileField textField) throws IOException {
        ModelFormField modelFormField = textField.getModelFormField();
        String className = "";
        String alert = "false";
        String name = modelFormField.getParameterName(context);
        String value = modelFormField.getEntry(context, textField.getDefaultValue(context));
        String size = Integer.toString(textField.getSize());
        String maxlength = "";
        String autocomplete = "";
        if (UtilValidate.isNotEmpty(modelFormField.getWidgetStyle(context))) {
            className = modelFormField.getWidgetStyle(context);
            if (modelFormField.shouldBeRed(context)) {
                alert = "true";
            }
        }
        if (UtilValidate.isEmpty(value)) {
            value = "";
        }
        if (textField.getMaxlength() != null) {
            maxlength = textField.getMaxlength().toString();
        }
        if (!textField.getClientAutocompleteField()) {
            autocomplete = "off";
        }
        StringWriter sr = new StringWriter();
        sr.append("<@renderFileField ");
        appendFieldInfo(sr, context, modelFormField);
        sr.append(" className=");
        sr.append(ftlFmt.makeStringLiteral(className));
        sr.append(" alert=");
        sr.append(ftlFmt.makeStringLiteral(alert));
        sr.append(" name=");
        sr.append(ftlFmt.makeStringLiteral(name));
        sr.append(" value=");
        sr.append(ftlFmt.makeStringLiteral(value));
        sr.append(" size=");
        sr.append(ftlFmt.makeStringLiteral(size));
        sr.append(" maxlength=");
        sr.append(ftlFmt.makeStringLiteral(maxlength));
        sr.append(" autocomplete=");
        sr.append(ftlFmt.makeStringLiteral(autocomplete));
        appendRequiredFieldParam(sr, context, modelFormField);
        sr.append(" />");
        executeMacro(writer, sr.toString());
        this.makeHyperlinkString(writer, textField.getSubHyperlink(), context);
        this.appendTooltip(writer, context, modelFormField);
    }

    public void renderPasswordField(Appendable writer, Map<String, Object> context, PasswordField passwordField) throws IOException {
        ModelFormField modelFormField = passwordField.getModelFormField();
        String className = "";
        String alert = "false";
        String name = modelFormField.getParameterName(context);
        String size = Integer.toString(passwordField.getSize());
        String maxlength = "";
        String id = modelFormField.getCurrentContainerId(context);
        String autocomplete = "";
        if (UtilValidate.isNotEmpty(modelFormField.getWidgetStyle(context))) {
            className = modelFormField.getWidgetStyle(context);
            if (modelFormField.shouldBeRed(context)) {
                alert = "true";
            }
        }
        String value = modelFormField.getEntry(context, passwordField.getDefaultValue(context));
        if (value == null) {
            value = "";
        }
        if (passwordField.getMaxlength() != null) {
            maxlength = passwordField.getMaxlength().toString();
        }
        if (id == null) {
            id = "";
        }
        if (!passwordField.getClientAutocompleteField()) {
            autocomplete = "off";
        }
        //check for required field style on single forms
        if ("single".equals(modelFormField.getModelForm().getType()) && modelFormField.getRequiredField()) {
            className = combineRequiredStyle(className, context, modelFormField); // SCIPIO
        }
        StringWriter sr = new StringWriter();
        sr.append("<@renderPasswordField ");
        appendFieldInfo(sr, context, modelFormField);
        sr.append(" className=");
        sr.append(ftlFmt.makeStringLiteral(className));
        sr.append(" alert=");
        sr.append(ftlFmt.makeStringLiteral(alert));
        sr.append(" name=");
        sr.append(ftlFmt.makeStringLiteral(name));
        sr.append(" value=");
        sr.append(ftlFmt.makeStringLiteral(value));
        sr.append(" size=");
        sr.append(ftlFmt.makeStringLiteral(size));
        sr.append(" maxlength=");
        sr.append(ftlFmt.makeStringLiteral(maxlength));
        sr.append(" id=");
        sr.append(ftlFmt.makeStringLiteral(id));
        sr.append(" autocomplete=");
        sr.append(ftlFmt.makeStringLiteral(autocomplete));
        appendRequiredFieldParam(sr, context, modelFormField);
        sr.append(" />");
        executeMacro(writer, sr.toString());
        this.addAsterisks(writer, context, modelFormField);
        this.makeHyperlinkString(writer, passwordField.getSubHyperlink(), context);
        this.appendTooltip(writer, context, modelFormField);
    }

    public void renderImageField(Appendable writer, Map<String, Object> context, ImageField imageField) throws IOException {
        ModelFormField modelFormField = imageField.getModelFormField();
        String value = modelFormField.getEntry(context, imageField.getValue(context));
        String description = imageField.getDescription(context);
        String alternate = imageField.getAlternate(context);
        String style = imageField.getStyle(context);
        if (UtilValidate.isEmpty(description)) {
            description = imageField.getModelFormField().getTitle(context);
        }
        if (UtilValidate.isEmpty(alternate)) {
            alternate = description;
        }
        if (UtilValidate.isNotEmpty(value)) {
            if (!value.startsWith("http")) {
                StringBuilder buffer = new StringBuilder();
                ContentUrlTag.appendContentPrefix(request, buffer);
                buffer.append(value);
                value = buffer.toString();
            }
        } else if (value == null) {
            value = "";
        }
        String event = modelFormField.getEvent();
        String action = modelFormField.getAction(context);
        StringWriter sr = new StringWriter();
        sr.append("<@renderImageField ");
        appendFieldInfo(sr, context, modelFormField);
        sr.append(" value=");
        sr.append(ftlFmt.makeStringLiteral(value));
        sr.append(" description=");
        sr.append(ftlFmt.makeStringLiteral(encode(description, modelFormField, context)));
        sr.append(" alternate=");
        sr.append(ftlFmt.makeStringLiteral(encode(alternate, modelFormField, context)));
        sr.append(" style=");
        sr.append(ftlFmt.makeStringLiteral(style));
        sr.append(" event=");
        sr.append(ftlFmt.makeStringLiteral(event == null ? "" : event));
        sr.append(" action=");
        sr.append(ftlFmt.makeStringLiteral(action == null ? "" : action));
        sr.append(" />");
        executeMacro(writer, sr.toString());
        this.makeHyperlinkString(writer, imageField.getSubHyperlink(), context);
        this.appendTooltip(writer, context, modelFormField);
    }

    public void renderFieldGroupOpen(Appendable writer, Map<String, Object> context, ModelForm.FieldGroup fieldGroup) throws IOException {
        String style = fieldGroup.getStyle();
        String id = fieldGroup.getId();
        FlexibleStringExpander titleNotExpanded = FlexibleStringExpander.getInstance(fieldGroup.getTitle());
        String title = titleNotExpanded.expandString(context);
        Boolean collapsed = fieldGroup.initiallyCollapsed();
        String collapsibleAreaId = fieldGroup.getId() + "_body";
        Boolean collapsible = fieldGroup.collapsible();
        String expandToolTip = "";
        String collapseToolTip = "";
        if (UtilValidate.isNotEmpty(style) || UtilValidate.isNotEmpty(id) || UtilValidate.isNotEmpty(title)) {
            if (fieldGroup.collapsible()) {
                Map<String, Object> uiLabelMap = UtilGenerics.checkMap(context.get("uiLabelMap"));
                //Map<String, Object> paramMap = UtilGenerics.checkMap(context.get("requestParameters"));
                if (uiLabelMap != null) {
                    expandToolTip = (String) uiLabelMap.get("CommonExpand");
                    collapseToolTip = (String) uiLabelMap.get("CommonCollapse");
                }
            }
        }
        StringWriter sr = new StringWriter();
        sr.append("<@renderFieldGroupOpen ");
        sr.append(" style=");
        sr.append(ftlFmt.makeStringLiteral(style));
        sr.append(" id=");
        sr.append(ftlFmt.makeStringLiteral(id));
        sr.append(" title=");
        sr.append(ftlFmt.makeStringLiteral(title));
        sr.append(" collapsed=");
        sr.append(Boolean.toString(collapsed));
        sr.append(" collapsibleAreaId=");
        sr.append(ftlFmt.makeStringLiteral(collapsibleAreaId));
        sr.append(" collapsible=");
        sr.append(Boolean.toString(collapsible));
        sr.append(" expandToolTip=");
        sr.append(ftlFmt.makeStringLiteral(expandToolTip));
        sr.append(" collapseToolTip=");
        sr.append(ftlFmt.makeStringLiteral(collapseToolTip));
        sr.append(" />");
        executeMacro(writer, sr.toString());
    }

    public void renderFieldGroupClose(Appendable writer, Map<String, Object> context, ModelForm.FieldGroup fieldGroup) throws IOException {
        String style = fieldGroup.getStyle();
        String id = fieldGroup.getId();
        FlexibleStringExpander titleNotExpanded = FlexibleStringExpander.getInstance(fieldGroup.getTitle());
        String title = titleNotExpanded.expandString(context);
        StringWriter sr = new StringWriter();
        sr.append("<@renderFieldGroupClose ");
        sr.append(" style=");
        sr.append(ftlFmt.makeStringLiteral(style));
        sr.append(" id=");
        sr.append(ftlFmt.makeStringLiteral(id));
        sr.append(" title=");
        sr.append(ftlFmt.makeStringLiteral(title));
        sr.append(" />");
        executeMacro(writer, sr.toString());
    }

    public void renderBanner(Appendable writer, Map<String, Object> context, ModelForm.Banner banner) throws IOException {
        String style = banner.getStyle(context);
        String leftStyle = banner.getLeftTextStyle(context);
        if (UtilValidate.isEmpty(leftStyle))
            leftStyle = style;
        String rightStyle = banner.getRightTextStyle(context);
        if (UtilValidate.isEmpty(rightStyle))
            rightStyle = style;
        String leftText = banner.getLeftText(context);
        if (leftText == null) {
            leftText = "";
        }
        String text = banner.getText(context);
        if (text == null) {
            text = "";
        }
        String rightText = banner.getRightText(context);
        if (rightText == null) {
            rightText = "";
        }
        StringWriter sr = new StringWriter();
        sr.append("<@renderBanner ");
        sr.append(" style=");
        sr.append(ftlFmt.makeStringLiteral(style));
        sr.append(" leftStyle=");
        sr.append(ftlFmt.makeStringLiteral(leftStyle));
        sr.append(" rightStyle=");
        sr.append(ftlFmt.makeStringLiteral(rightStyle));
        sr.append(" leftText=");
        sr.append(ftlFmt.makeStringLiteral(leftText));
        sr.append(" text=");
        sr.append(ftlFmt.makeStringLiteral(text));
        sr.append(" rightText=");
        sr.append(ftlFmt.makeStringLiteral(rightText));
        sr.append(" />");
        executeMacro(writer, sr.toString());
    }

    /**
     * Renders the beginning boundary comment string.
     * @param writer The writer to write to
     * @param widgetType The widget type: "Screen Widget", "Form Widget", etc.
     * @param modelWidget The widget
     */
    public void renderBeginningBoundaryComment(Appendable writer, String widgetType, ModelWidget modelWidget) throws IOException {
        if (this.widgetCommentsEnabled) {
            StringWriter sr = new StringWriter();
            sr.append("<@formatBoundaryComment ");
            sr.append(" boundaryType=");
            sr.append(ftlFmt.makeStringLiteral("Begin"));
            sr.append(" widgetType=");
            sr.append(ftlFmt.makeStringLiteral(widgetType));
            sr.append(" widgetName=");
            sr.append(ftlFmt.makeStringLiteral(modelWidget.getBoundaryCommentName()));
            sr.append(" />");
            executeMacro(writer, sr.toString());
        }
    }

    /**
     * Renders the ending boundary comment string.
     * @param writer The writer to write to
     * @param widgetType The widget type: "Screen Widget", "Form Widget", etc.
     * @param modelWidget The widget
     */
    public void renderEndingBoundaryComment(Appendable writer, String widgetType, ModelWidget modelWidget) throws IOException {
        if (this.widgetCommentsEnabled) {
            StringWriter sr = new StringWriter();
            sr.append("<@formatBoundaryComment ");
            sr.append(" boundaryType=");
            sr.append(ftlFmt.makeStringLiteral("End"));
            sr.append(" widgetType=");
            sr.append(ftlFmt.makeStringLiteral(widgetType));
            sr.append(" widgetName=");
            sr.append(ftlFmt.makeStringLiteral(modelWidget.getBoundaryCommentName()));
            sr.append(" />");
            executeMacro(writer, sr.toString());
        }
    }

    public void renderSortField(Appendable writer, Map<String, Object> context, ModelFormField modelFormField, String titleText) throws IOException {
        boolean ajaxEnabled = false;
        ModelForm modelForm = modelFormField.getModelForm();
        List<ModelForm.UpdateArea> updateAreas = modelForm.getOnSortColumnUpdateAreas();
        if (updateAreas == null) {
            // For backward compatibility.
            updateAreas = modelForm.getOnPaginateUpdateAreas();
        }
        if (this.javaScriptEnabled) {
            if (UtilValidate.isNotEmpty(updateAreas)) {
                ajaxEnabled = true;
            }
        }
        String paginateTarget = modelForm.getPaginateTarget(context);
        if (paginateTarget.isEmpty() && updateAreas == null) {
            Debug.logWarning("Cannot sort because the paginate target URL is empty for the form: " + modelForm.getName(), module);
            return;
        }
        String oldSortField = modelForm.getSortField(context);
        String sortFieldStyle = modelFormField.getSortFieldStyle();
        // if the entry-name is defined use this instead of field name
        String columnField = modelFormField.getEntryName();
        if (UtilValidate.isEmpty(columnField)) {
            columnField = modelFormField.getFieldName();
        }
        // switch between asc/desc order
        String newSortField = columnField;
        if (UtilValidate.isNotEmpty(oldSortField)) {
            if (oldSortField.equals(columnField)) {
                newSortField = "-" + columnField;
                sortFieldStyle = modelFormField.getSortFieldStyleDesc();
            } else if (oldSortField.equals("-" + columnField)) {
                newSortField = columnField;
                sortFieldStyle = modelFormField.getSortFieldStyleAsc();
            }
        }
        String queryString = UtilHttp.getQueryStringFromTarget(paginateTarget).replace("?", "");
        Map<String, Object> paramMap = UtilHttp.getQueryStringOnlyParameterMap(queryString);
        String qbeString = (String) context.get("_QBESTRING_");
        if (qbeString != null) {
            qbeString = qbeString.replaceAll("&amp;", "&");
            paramMap.putAll(UtilHttp.getQueryStringOnlyParameterMap(qbeString));
        }
        paramMap.put(modelForm.getSortFieldParameterName(), newSortField);
        UtilHttp.canonicalizeParameterMap(paramMap);
        String linkUrl = null;
        if (ajaxEnabled) {
            linkUrl = createAjaxParamsFromUpdateAreas(updateAreas, paramMap, null, context);
        } else {
            StringBuilder sb = new StringBuilder("?");
            Iterator<Map.Entry<String, Object>> iter = paramMap.entrySet().iterator();
            while (iter.hasNext()) {
                Map.Entry<String, Object> entry = iter.next();
                sb.append(entry.getKey()).append("=").append(entry.getValue());
                if (iter.hasNext()) {
                    sb.append("&amp;");
                }
            }
            String newQueryString = sb.toString();
            String urlPath = UtilHttp.removeQueryStringFromTarget(paginateTarget);
            linkUrl = rh.makeLink(this.request, this.response, urlPath.concat(newQueryString));
        }
        StringWriter sr = new StringWriter();
        sr.append("<@renderSortField ");
        sr.append(" style=");
        sr.append(ftlFmt.makeStringLiteral(sortFieldStyle));
        sr.append(" title=");
        sr.append(ftlFmt.makeStringLiteral(titleText));
        sr.append(" linkUrl=");
        sr.append(ftlFmt.makeStringLiteral(linkUrl));
        sr.append(" ajaxEnabled=");
        sr.append(Boolean.toString(ajaxEnabled));
        String tooltip = modelFormField.getSortFieldHelpText(context);
        if (!tooltip.isEmpty()) {
            sr.append(" tooltip=").append(ftlFmt.makeStringLiteral(tooltip));
        }
        sr.append(" />");
        executeMacro(writer, sr.toString());
    }

    /** Create an ajaxXxxx JavaScript CSV string from a list of UpdateArea objects. See
     * <code>selectall.js</code>.
     * @param updateAreas
     * @param extraParams Renderer-supplied additional target parameters
     * @param context
     * @return Parameter string or empty string if no UpdateArea objects were found
     */
    private String createAjaxParamsFromUpdateAreas(List<ModelForm.UpdateArea> updateAreas, Map<String, Object> extraParams, String anchor, Map<String, ? extends Object> context) {
        StringBuilder sb = new StringBuilder();
        Iterator<ModelForm.UpdateArea> updateAreaIter = updateAreas.iterator();
        while (updateAreaIter.hasNext()) {
            ModelForm.UpdateArea updateArea = updateAreaIter.next();
            sb.append(updateArea.getAreaId()).append(",");
            String ajaxTarget = updateArea.getAreaTarget(context);
            String urlPath = UtilHttp.removeQueryStringFromTarget(ajaxTarget);
            sb.append(this.rh.makeLink(this.request, this.response,urlPath)).append(",");
            String queryString = UtilHttp.getQueryStringFromTarget(ajaxTarget).replace("?", "");
            Map<String, Object> parameters = UtilHttp.getQueryStringOnlyParameterMap(queryString);
            Map<String, Object> ctx = UtilGenerics.checkMap(context);
            Map<String, Object> updateParams = UtilGenerics.checkMap(updateArea.getParameterMap(ctx));
            parameters.putAll(updateParams);
            UtilHttp.canonicalizeParameterMap(parameters);
            parameters.putAll(extraParams);
            Iterator<Map.Entry<String, Object>> paramIter = parameters.entrySet().iterator();
            while (paramIter.hasNext()) {
                Map.Entry<String, Object> entry = paramIter.next();
                sb.append(entry.getKey()).append("=").append(entry.getValue());
                if (paramIter.hasNext()) {
                    sb.append("&");
                }
            }
            if (anchor != null) {
                sb.append("#").append(anchor);
            }
            if (updateAreaIter.hasNext()) {
                sb.append(",");
            }
        }
        Locale locale = UtilMisc.ensureLocale(context.get("locale"));
        return FlexibleStringExpander.expandString(sb.toString(), context, locale);
    }

    /** Create an ajaxXxxx JavaScript CSV string from a list of UpdateArea objects. See
     * <code>selectall.js</code>.
     * @param updateAreas
     * @param extraParams Renderer-supplied additional target parameters
     * @param context
     * @return Parameter string or empty string if no UpdateArea objects were found
     */
    public String createAjaxParamsFromUpdateAreas(List<ModelForm.UpdateArea> updateAreas, String extraParams, Map<String, ? extends Object> context) {
        //FIXME copy from HtmlFormRenderer.java
        if (updateAreas == null) {
            return "";
        }
        String ajaxUrl = "";
        boolean firstLoop = true;
        for (ModelForm.UpdateArea updateArea : updateAreas) {
            if (firstLoop) {
                firstLoop = false;
            } else {
                ajaxUrl += ",";
            }
            Map<String, Object> ctx = UtilGenerics.checkMap(context);
            Map<String, String> parameters = updateArea.getParameterMap(ctx);
            String targetUrl = updateArea.getAreaTarget(context);
            String ajaxParams = getAjaxParamsFromTarget(targetUrl);
            //add first parameters from updateArea parameters
            if (UtilValidate.isNotEmpty(parameters)) {
                if (UtilValidate.isEmpty(ajaxParams)) {
                    ajaxParams = "";
                }
                for (Map.Entry<String, String> entry : parameters.entrySet()) {
                    String key = entry.getKey();
                    String value = entry.getValue();
                    //test if ajax parameters are not already into extraParams, if so do not add it
                    if (UtilValidate.isNotEmpty(extraParams) && extraParams.contains(value)) {
                        continue;
                    }
                    if (ajaxParams.length() > 0 && ajaxParams.indexOf(key) < 0) {
                        ajaxParams += "&";
                    }
                    if (ajaxParams.indexOf(key) < 0) {
                        ajaxParams += key + "=" + value;
                    }
                }
            }
            //then add parameters from request. Those parameters could end with an anchor so we must set ajax parameters first
            if (UtilValidate.isNotEmpty(extraParams)) {
                if (ajaxParams.length() > 0 && !extraParams.startsWith("&")) {
                    ajaxParams += "&";
                }
                ajaxParams += extraParams;
            }
            ajaxUrl += updateArea.getAreaId() + ",";
            ajaxUrl += this.rh.makeLink(this.request, this.response, UtilHttp.removeQueryStringFromTarget(targetUrl));
            ajaxUrl += "," + ajaxParams;
        }
        Locale locale = UtilMisc.ensureLocale(context.get("locale"));
        return FlexibleStringExpander.expandString(ajaxUrl, context, locale);
    }

    /** Extracts parameters from a target URL string, prepares them for an Ajax
     * JavaScript call. This method is currently set to return a parameter string
     * suitable for the Prototype.js library.
     * @param target Target URL string
     * @return Parameter string
     */
    public static String getAjaxParamsFromTarget(String target) {
        String targetParams = UtilHttp.getQueryStringFromTarget(target);
        targetParams = targetParams.replace("?", "");
        targetParams = targetParams.replace("&amp;", "&");
        return targetParams;
    }

    public void appendTooltip(Appendable writer, Map<String, Object> context, ModelFormField modelFormField) throws IOException {
        // render the tooltip, in other methods too
        String tooltip = modelFormField.getTooltip(context);
        StringWriter sr = new StringWriter();
        sr.append("<@renderTooltip ");
        sr.append("tooltip=");
        sr.append(ftlFmt.makeStringLiteral(tooltip)); // SCIPIO: redundant: FreeMarkerWorker.encodeDoubleQuotes(tooltip)
        sr.append(" tooltipStyle=");
        sr.append(ftlFmt.makeStringLiteral(modelFormField.getTooltipStyle()));
        sr.append(" />");
        executeMacro(writer, sr.toString());
    }

    public void makeHyperlinkString(Appendable writer, ModelFormField.SubHyperlink subHyperlink, Map<String, Object> context) throws IOException {
        if (subHyperlink == null) {
            return;
        }
        if (subHyperlink.shouldUse(context)) {
            writer.append(' ');
            makeHyperlinkByType(writer, subHyperlink.getLinkType(), subHyperlink.getStyle(context), subHyperlink.getUrlMode(),
                    subHyperlink.getTarget(context), subHyperlink.getParameterMap(context, subHyperlink.getModelFormField().getEntityName(), 
                    subHyperlink.getModelFormField().getServiceName()), subHyperlink.getDescription(context),
                    subHyperlink.getTargetWindow(context), "", subHyperlink.getModelFormField(), this.request, this.response,
                    context);
        }
    }

    public void addAsterisks(Appendable writer, Map<String, Object> context, ModelFormField modelFormField) throws IOException {
        StringWriter sr = new StringWriter();
        sr.append("<@renderAsterisks");
        appendAsterisksParams(sr, context, modelFormField);
        sr.append(" />");
        executeMacro(writer, sr.toString());
    }
    
    /**
     * SCIPIO: Factors out renderAstericks macro args.
     */
    public void appendAsterisksParams(Appendable sr, Map<String, Object> context, ModelFormField modelFormField) throws IOException {
        String requiredField = "false";
        String requiredStyle = "";
        if (modelFormField.getRequiredField()) {
            requiredField = "true";
            requiredStyle = modelFormField.getRequiredFieldStyle();
        }
        sr.append(" requiredField=");
        sr.append(ftlFmt.makeStringLiteral(requiredField));
        sr.append(" requiredStyle=");
        sr.append(ftlFmt.makeStringLiteral(requiredStyle));
        sr.append("");
    }

    public void appendContentUrl(Appendable writer, String location) throws IOException {
        StringBuilder buffer = new StringBuilder();
        ContentUrlTag.appendContentPrefix(this.request, buffer);
        writer.append(buffer.toString());
        writer.append(location);
    }

    public void makeHyperlinkByType(Appendable writer, String linkType, String linkStyle, String targetType, String target, Map<String, String> parameterMap, String description, String targetWindow, String confirmation, ModelFormField modelFormField, HttpServletRequest request,
            HttpServletResponse response, Map<String, Object> context) throws IOException {
        String realLinkType = WidgetWorker.determineAutoLinkType(linkType, target, targetType, request);
        String encodedDescription = encode(description, modelFormField, context);
        // get the parameterized pagination index and size fields
        int paginatorNumber = WidgetWorker.getPaginatorNumber(context);
        ModelForm modelForm = modelFormField.getModelForm();
        String viewIndexField = modelForm.getMultiPaginateIndexField(context);
        String viewSizeField = modelForm.getMultiPaginateSizeField(context);
        int viewIndex = Paginator.getViewIndex(modelForm, context);
        int viewSize = Paginator.getViewSize(modelForm, context);
        if (viewIndexField.equals("viewIndex" + "_" + paginatorNumber)) {
            viewIndexField = "VIEW_INDEX" + "_" + paginatorNumber;
        }
        if (viewSizeField.equals("viewSize" + "_" + paginatorNumber)) {
            viewSizeField = "VIEW_SIZE" + "_" + paginatorNumber;
        }
        if ("hidden-form".equals(realLinkType)) {
            parameterMap.put(viewIndexField, Integer.toString(viewIndex));
            parameterMap.put(viewSizeField, Integer.toString(viewSize));
            if (modelFormField != null && "multi".equals(modelForm.getType())) {
                WidgetWorker.makeHiddenFormLinkAnchor(writer, linkStyle, encodedDescription, confirmation, modelFormField, request, response, context);
                // this is a bit trickier, since we can't do a nested form we'll have to put the link to submit the form in place, but put the actual form def elsewhere, ie after the big form is closed
                Map<String, Object> wholeFormContext = UtilGenerics.checkMap(context.get("wholeFormContext"));
                Appendable postMultiFormWriter = wholeFormContext != null ? (Appendable) wholeFormContext.get("postMultiFormWriter") : null;
                if (postMultiFormWriter == null) {
                    postMultiFormWriter = new StringWriter();
                    wholeFormContext.put("postMultiFormWriter", postMultiFormWriter);
                }
                WidgetWorker.makeHiddenFormLinkForm(postMultiFormWriter, target, targetType, targetWindow, parameterMap, modelFormField, request, response, context);
            } else {
                WidgetWorker.makeHiddenFormLinkForm(writer, target, targetType, targetWindow, parameterMap, modelFormField, request, response, context);
                WidgetWorker.makeHiddenFormLinkAnchor(writer, linkStyle, encodedDescription, confirmation, modelFormField, request, response, context);
            }
        } else {
            makeHyperlinkString(writer, linkStyle, targetType, target, parameterMap, encodedDescription, confirmation, modelFormField, request, response, context, targetWindow);
        }

    }

    public void makeHyperlinkString(Appendable writer, String linkStyle, String targetType, String target, Map<String, String> parameterMap, String description, String confirmation, ModelFormField modelFormField, HttpServletRequest request, HttpServletResponse response, Map<String, Object> context,
            String targetWindow) throws IOException {
        if ( description != null || UtilValidate.isNotEmpty(request.getAttribute("image"))) {
            StringBuilder linkUrl = new StringBuilder();
            WidgetWorker.buildHyperlinkUrl(linkUrl, target, targetType, parameterMap, null, null, null, null, request, response, context);
            String event = "";
            String action = "";
            String imgSrc = "";
            String alt = "";
            String imgTitle = "";
            String hiddenFormName = WidgetWorker.makeLinkHiddenFormName(context, modelFormField);
            if (UtilValidate.isNotEmpty(modelFormField.getEvent()) && UtilValidate.isNotEmpty(modelFormField.getAction(context))) {
                event = modelFormField.getEvent();
                action = modelFormField.getAction(context);
            }
            if (UtilValidate.isNotEmpty(request.getAttribute("image"))) {
                imgSrc = request.getAttribute("image").toString();
            }
            if (UtilValidate.isNotEmpty(request.getAttribute("alternate"))) {
                alt = request.getAttribute("alternate").toString();
            }
            if (UtilValidate.isNotEmpty(request.getAttribute("imageTitle"))) {
                imgTitle = request.getAttribute("imageTitle").toString();
            }
            Integer size = Integer.valueOf("0");
            if (UtilValidate.isNotEmpty(request.getAttribute("descriptionSize"))) {
                size = Integer.valueOf(request.getAttribute("descriptionSize").toString());
            }
            if (UtilValidate.isNotEmpty(description) && size > 0 && description.length() > size) {
                imgTitle = description;
                description = description.substring(0, size - 8) + "..." + description.substring(description.length() - 5);
            }
            if (UtilValidate.isEmpty(imgTitle)) {
                imgTitle = modelFormField.getTitle(context);
            }
            StringWriter sr = new StringWriter();
            sr.append("<@makeHyperlinkString ");
            sr.append("linkStyle=");
            sr.append(ftlFmt.makeStringLiteral(linkStyle == null ? "" : linkStyle));
            sr.append(" hiddenFormName=");
            sr.append(ftlFmt.makeStringLiteral(hiddenFormName == null ? "" : hiddenFormName));
            sr.append(" event=");
            sr.append(ftlFmt.makeStringLiteral(event));
            sr.append(" action=");
            sr.append(ftlFmt.makeStringLiteral(action));
            sr.append(" imgSrc=");
            sr.append(ftlFmt.makeStringLiteral(imgSrc));
            sr.append(" title=");
            sr.append(ftlFmt.makeStringLiteral(imgTitle));
            sr.append(" alternate=");
            sr.append(ftlFmt.makeStringLiteral(alt));
            sr.append(" linkUrl=");
            sr.append(ftlFmt.makeStringLiteral(linkUrl.toString()));
            sr.append(" targetWindow=");
            sr.append(ftlFmt.makeStringLiteral(targetWindow));
            sr.append(" description=");
            sr.append(ftlFmt.makeStringLiteral(description));
            sr.append(" confirmation =");
            sr.append(ftlFmt.makeStringLiteral(confirmation));
            sr.append(" />");
            executeMacro(writer, sr.toString());
        }
    }

    public void makeHiddenFormLinkAnchor(Appendable writer, String linkStyle, String description, String confirmation, ModelFormField modelFormField, HttpServletRequest request, HttpServletResponse response, Map<String, Object> context) throws IOException {
        if (UtilValidate.isNotEmpty(description) || UtilValidate.isNotEmpty(request.getAttribute("image"))) {
            String hiddenFormName = WidgetWorker.makeLinkHiddenFormName(context, modelFormField);
            String event = "";
            String action = "";
            String imgSrc = "";
            if (UtilValidate.isNotEmpty(modelFormField.getEvent()) && UtilValidate.isNotEmpty(modelFormField.getAction(context))) {
                event = modelFormField.getEvent();
                action = modelFormField.getAction(context);
            }
            if (UtilValidate.isNotEmpty(request.getAttribute("image"))) {
                imgSrc = request.getAttribute("image").toString();
            }
            StringWriter sr = new StringWriter();
            sr.append("<@makeHiddenFormLinkAnchor ");
            sr.append("linkStyle=");
            sr.append(ftlFmt.makeStringLiteral(linkStyle == null ? "" : linkStyle));
            sr.append(" hiddenFormName=");
            sr.append(ftlFmt.makeStringLiteral(hiddenFormName == null ? "" : hiddenFormName));
            sr.append(" event=");
            sr.append(ftlFmt.makeStringLiteral(event));
            sr.append(" action=");
            sr.append(ftlFmt.makeStringLiteral(action));
            sr.append(" imgSrc=");
            sr.append(ftlFmt.makeStringLiteral(imgSrc));
            sr.append(" description=");
            sr.append(ftlFmt.makeStringLiteral(description));
            sr.append(" confirmation =");
            sr.append(ftlFmt.makeStringLiteral(confirmation));
            sr.append(" />");
            executeMacro(writer, sr.toString());
        }
    }

    public void makeHiddenFormLinkForm(Appendable writer, String target, String targetType, String targetWindow, List<CommonWidgetModels.Parameter> parameterList, ModelFormField modelFormField, HttpServletRequest request, HttpServletResponse response, Map<String, Object> context) throws IOException {
        StringBuilder actionUrl = new StringBuilder();
        WidgetWorker.buildHyperlinkUrl(actionUrl, target, targetType, null, null, null, null, null, request, response, context);
        String name = WidgetWorker.makeLinkHiddenFormName(context, modelFormField);
        StringBuilder parameters = new StringBuilder();
        parameters.append("[");
        for (CommonWidgetModels.Parameter parameter : parameterList) {
            if (parameters.length() > 1) {
                parameters.append(",");
            }
            parameters.append("{'name':");
            parameters.append(ftlFmt.makeStringLiteralSQ(parameter.getName()));
            parameters.append(",'value':");
            parameters.append(ftlFmt.makeStringLiteralSQ(encode(parameter.getValue(context), modelFormField, context))); // SCIPIO: unhardcoded html encode call here
            parameters.append("}");
        }
        parameters.append("]");
        StringWriter sr = new StringWriter();
        sr.append("<@makeHiddenFormLinkForm ");
        sr.append("actionUrl=");
        sr.append(ftlFmt.makeStringLiteral(actionUrl.toString()));
        sr.append(" name=");
        sr.append(ftlFmt.makeStringLiteral(name));
        sr.append(" parameters=");
        sr.append(parameters.toString());
        sr.append(" targetWindow=");
        sr.append(ftlFmt.makeStringLiteral(targetWindow));
        sr.append(" />");
        executeMacro(writer, sr.toString());
    }

    public void renderContainerFindField(Appendable writer, Map<String, Object> context, ContainerField containerField) throws IOException {
        String id = containerField.getModelFormField().getIdName();
        String className = UtilFormatOut.checkNull(containerField.getModelFormField().getWidgetStyle(context));
        StringWriter sr = new StringWriter();
        sr.append("<@renderContainerField ");
        sr.append("id=");
        sr.append(ftlFmt.makeStringLiteral(id));
        sr.append(" className=");
        sr.append(ftlFmt.makeStringLiteral(className));
        sr.append(" />");
        executeMacro(writer, sr.toString());
    }

    public void renderAlternateText(Appendable writer, Map<String, Object> context, ModelForm modelForm, boolean wrapperOpened, boolean headerRendered, int numOfColumns) throws IOException {
        contextHandler.registerContext(writer, context);
        String className = UtilFormatOut.checkNull(modelForm.getAlternateTextStyle(context));
        String text = UtilFormatOut.checkNull(modelForm.getAlternateText(context));
        StringWriter sr = new StringWriter();
        sr.append("<@renderAlternateText ");
        sr.append("className=");
        sr.append(ftlFmt.makeStringLiteral(className));
        sr.append(" text=");
        sr.append(ftlFmt.makeStringLiteral(text));
        sr.append(" wrapperOpened=");
        sr.append(Boolean.toString(wrapperOpened));
        sr.append(" headerRendered=");
        sr.append(Boolean.toString(headerRendered));
        sr.append(" numOfColumns=");
        sr.append(Integer.toString(numOfColumns));
        sr.append(" />");
        executeMacro(writer, sr.toString());
    }

    @Override
    public void renderSubmitForm(Appendable writer, Map<String, Object> context, ModelForm modelForm) throws IOException {        
        // get the parameterized pagination index and size fields
        int paginatorNumber = WidgetWorker.getPaginatorNumber(context);

        String viewIndexField = modelForm.getMultiPaginateIndexField(context);
        String viewSizeField = modelForm.getMultiPaginateSizeField(context);
        int viewIndex = Paginator.getViewIndex(modelForm, context);
        int viewSize = Paginator.getViewSize(modelForm, context);
        if (viewIndexField.equals("viewIndex" + "_" + paginatorNumber)) {
            viewIndexField = "VIEW_INDEX" + "_" + paginatorNumber;
        }
        if (viewSizeField.equals("viewSize" + "_" + paginatorNumber)) {
            viewSizeField = "VIEW_SIZE" + "_" + paginatorNumber;
        }
        
        HashMap<String, String> parameterMap = new HashMap<String, String>();
        parameterMap.put(viewIndexField, Integer.toString(viewIndex));
        parameterMap.put(viewSizeField, Integer.toString(viewSize));

        // this has to be rendered OUTSIDE/AFTER the multi form stuff; handled by FormRenderer
        //Map<String, Object> wholeFormContext = UtilGenerics.checkMap(context.get("wholeFormContext"));
        //Appendable effWriter = wholeFormContext != null ? (Appendable) wholeFormContext.get("postMultiFormWriter") : null;
        //if (modelForm.getType().equals("multi")) {        
        //    if (effWriter == null) {
        //        effWriter = new StringWriter();
        //        if (wholeFormContext != null)
        //            wholeFormContext.put("postMultiFormWriter", effWriter);
        //    }
        //} else {
        //effWriter = writer;
        //}
        Appendable effWriter = writer;
        
        // SCIPIO: now use a macro instead
        //WidgetWorker.makeHiddenFormSubmitForm(postMultiFormWriter, modelForm.getTarget(context, modelForm.getTargetType()), modelForm.getTargetType(), modelForm.getTargetWindow(), parameterMap, request, response, modelForm, context);

        // SCIPIO: WARN: the FTL must encode everything passed below!
        
        String hiddenFormName = WidgetWorker.makeLinkHiddenFormName(context, modelForm,
                modelForm.getSubmitHiddenFormName(Integer.toString(getNextRenderSubmitFormIdNum(writer, context, modelForm)))); 
        
        StringWriter targetUrlSw = new StringWriter();
        String target = modelForm.getTarget(context, modelForm.getTargetType());
        if (UtilValidate.isNotEmpty(target)) {
            WidgetWorker.buildHyperlinkUrl(targetUrlSw, target, 
                modelForm.getTargetType(), null, null, null, null, null, request, response, context);
        }
        String targetUrl = targetUrlSw.toString();
        
        List<Map<String, Object>> submitEntries = new ArrayList<>();
        if (modelForm.getUseRowSubmit()) {
            List<ModelFormField> rowSubmitFields = modelForm.getMultiSubmitFields();
            if (rowSubmitFields != null) {
                for (ModelFormField rowSubmitField : rowSubmitFields) {
                    Map<String, Object> map = new HashMap<String, Object>();
                    map.put("submitFieldId", rowSubmitField.getCurrentContainerId(context));
                    map.put("submitFieldName", rowSubmitField.getParameterName(context));
                    map.put("selectFieldNamePrefix", modelForm.getRowSubmitSelectFieldParamNamePrefix());
                    submitEntries.add(map);
                }
            }
        } else {
            List<ModelFormField> rowSubmitFields = modelForm.getMultiSubmitFields();
            if (rowSubmitFields != null) {
                for (ModelFormField rowSubmitField : rowSubmitFields) {
                    Map<String, Object> map = new HashMap<String, Object>();
                    map.put("submitFieldId", rowSubmitField.getCurrentContainerId(context));
                    map.put("submitFieldName", rowSubmitField.getParameterName(context));
                    submitEntries.add(map);
                }
            }
        }
        
        StringWriter sr = new StringWriter();
        sr.append("<@renderSubmitForm");
        sr.append(" hiddenFormName=");
        sr.append(ftlFmt.makeStringLiteral(hiddenFormName));
        sr.append(" formId=");
        sr.append(ftlFmt.makeStringLiteral(modelForm.getContainerId()));
        sr.append(" formName=");
        sr.append(ftlFmt.makeStringLiteral(modelForm.getName()));
        sr.append(" formType=");
        sr.append(ftlFmt.makeStringLiteral(modelForm.getType()));
        sr.append(" targetUrl=");
        sr.append(ftlFmt.makeStringLiteral(targetUrl));
        sr.append(" targetWindow=");
        sr.append(ftlFmt.makeStringLiteral(modelForm.getTargetWindow()));
        sr.append(" params=");
        sr.append(ftlFmt.makeLiteralSQ(parameterMap));
        sr.append(" useRowSubmit=");
        sr.append(modelForm.getUseRowSubmit() ? "true" : "false");
        sr.append(" useMasterSubmitField=");
        sr.append(modelForm.getUseMasterSubmitField() ? "true" : "false");
        sr.append(" submitEntries=");
        sr.append(ftlFmt.makeLiteralSQ(submitEntries));
        sr.append(" />");
        executeMacro(effWriter, sr.toString());
    }

    /**
     * SCIPIO: returns a new submit form hidden form unique count.
     */
    private int getNextRenderSubmitFormIdNum(Appendable writer, Map<String, Object> context, ModelForm modelForm) throws IOException {
        try {
            Object numObj = ContextFtlUtil.getRequestVar("renderSubmitForm_formIdNum", request, contextHandler.getRenderContext(writer, context));
            Integer num = (Integer) LangFtlUtil.unwrap(numObj);
            
            if (num == null) {
                num = 1;
            } else {
                num = num + 1;
            }
            ContextFtlUtil.setRequestVar("renderSubmitForm_formIdNum", num, request, contextHandler.getRenderContext(writer, context));
            return num;
        } catch (TemplateModelException e) {
            throw new IOException(e);
        } 
    }
    
    @Override
    public void renderFormatFooterRowOpen(Appendable writer, Map<String, Object> context, ModelForm modelForm) throws IOException {
        contextHandler.registerContext(writer, context);
        String headerStyle = FlexibleStringExpander.expandString(modelForm.getHeaderRowStyle(), context);
        StringWriter sr = new StringWriter();
        sr.append("<@renderFormatFooterRowOpen ");
        sr.append(" style=");
        sr.append(ftlFmt.makeStringLiteral(headerStyle));
        sr.append(" />");
        executeMacro(writer, sr.toString());
    }

    @Override
    public void renderFormatFooterRowClose(Appendable writer, Map<String, Object> context, ModelForm modelForm) throws IOException {
        StringWriter sr = new StringWriter();
        sr.append("<@renderFormatFooterRowClose />");
        executeMacro(writer, sr.toString());
    }
    
    /**
     * SCIPIO: helper to append requiredField param.
     * @throws IOException 
     */
    protected void appendRequiredFieldParam(Appendable sr, Map<String, Object> context, ModelFormField modelFormField) throws IOException {
        sr.append(" requiredField=" + (modelFormField.getRequiredField() ? "\"true\"" : "\"false\""));
    }

    @Override
    public void renderFormPageScripts(Appendable writer, Map<String, Object> context, ModelForm modelForm)
            throws IOException {
        if (modelForm.getPageScripts().size() <= 0) return;
        // SCIPIO: 2017-04-21: new
        StringWriter sr = new StringWriter();
        sr.append("<@renderFormPageScripts ");
        appendFormPageScripts(writer, sr, context, modelForm);
        sr.append("/>");
        executeMacro(writer, sr.toString());
    }
    
    /**
     * SCIPIO: appends special page scripts to macro call.
     * New 2017-04-21.
     */
    private void appendFormPageScripts(Appendable writer, Appendable sr, Map<String, Object> context, ModelForm modelForm) throws IOException {
        // SCIPIO: 2017-04-21: special pageScripts
        List<Object> pageScripts = new ArrayList<>();
        for(ModelPageScript pageScript : modelForm.getPageScripts()) {
            pageScripts.add(pageScript.getScript(context));
        }
        Environment env;
        try {
            env = getEnvironment(writer);
            freemarker.template.TemplateModel pageScriptsModel = env.getObjectWrapper().wrap(pageScripts);
            env.setGlobalVariable("_scpFormRenPageScripts", pageScriptsModel);
        } catch (TemplateException e) {
            throw new IOException(e);
        }
        sr.append(" pageScripts=_scpFormRenPageScripts");
    }
    
    // SCIPIO: new 2017-04-21 - FIXME: hardcoded 'required' style added
    String combineRequiredStyle(String className, boolean allowDefault, Map<String, Object> context, ModelFormField modelFormField) {
        // ORIGINAL:
//        String requiredStyle = modelFormField.getRequiredFieldStyle();
//        if (UtilValidate.isEmpty(requiredStyle))
//            requiredStyle = "required";
//        if (UtilValidate.isEmpty(className))
//            className = requiredStyle;
//        else
//            className = requiredStyle + " " + className;
        String requiredStyle = modelFormField.getRequiredFieldStyle();
        if (UtilValidate.isNotEmpty(requiredStyle)) {
            // SCIPIO: NOTE: we expand as a bugfix to original ofbiz
            requiredStyle = FlexibleStringExpander.expandString(requiredStyle, context);
        } else {
            if (allowDefault) {
                // SCIPIO: FIXME: the 'required' style is currently hardcoded! 
                // this should be gotten from global styles (ftl) instead...
                // but this is currently not done from the target macros...
                requiredStyle = "required";
            } else {
                requiredStyle = "";
            }
        }
        return ModelForm.combineExtraStyle(className, requiredStyle);
    }
    
    String combineRequiredStyle(String className, Map<String, Object> context, ModelFormField modelFormField) {
        return combineRequiredStyle(className, true, context, modelFormField);
    }
}
