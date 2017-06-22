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
import java.math.BigDecimal;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.servlet.ServletContext;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.StringUtil;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.base.util.UtilProperties;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.base.util.string.FlexibleStringExpander;
import org.ofbiz.base.util.template.FreeMarkerWorker;
import org.ofbiz.base.util.template.FtlScriptFormatter;
import org.ofbiz.webapp.control.RequestHandler;
import org.ofbiz.webapp.taglib.ContentUrlTag;
import org.ofbiz.widget.WidgetWorker;
import org.ofbiz.widget.model.CommonWidgetModels.Image;
import org.ofbiz.widget.model.MenuRenderState;
import org.ofbiz.widget.model.MenuRenderState.MenuItemState;
import org.ofbiz.widget.model.ModelMenu;
import org.ofbiz.widget.model.ModelMenu.MenuAndItem;
import org.ofbiz.widget.model.ModelMenuItem;
import org.ofbiz.widget.model.ModelMenuItem.MenuLink;
import org.ofbiz.widget.model.ModelSubMenu;
import org.ofbiz.widget.model.ModelWidget;
import org.ofbiz.widget.renderer.MenuStringRenderer;

import freemarker.core.Environment;
import freemarker.template.Template;
import freemarker.template.TemplateException;

public class MacroMenuRenderer implements MenuStringRenderer {

    public static final String module = MacroMenuRenderer.class.getName();
    
    /**
     * SCIPIO: NOTE: This should always be true. Scipio no longer supports the old rendering method.
     */
    private static final boolean oneShotMacrosEnabled = UtilProperties.getPropertyAsBoolean("scipioWebapp", "scipio.templating.widget.oneshotmacros", true);
    
    /**
     * SCIPIO: Maps traditional Ofbiz macro names to one-shot macro render entries.
     */
    static final Map<String, OneShotMacro.Entry> renderEntryMacroNameMap;
    static {
        Map<String, OneShotMacro.Entry> map = new HashMap<String, OneShotMacro.Entry>();
        map.put("renderMenuBegin", new OneShotMacro.BeginEntry(OneShotMacro.VarType.SINGLE, "menuArgs"));
        map.put("renderMenuEnd", new OneShotMacro.EndEntry());
        map.put("renderMenuItemBegin", new OneShotMacro.BeginEntry(OneShotMacro.VarType.LIST, "items"));
        map.put("renderMenuItemEnd", new OneShotMacro.EndEntry());
        map.put("renderLink", new OneShotMacro.SingleEntry(OneShotMacro.VarType.SINGLE, "linkArgs"));
        map.put("renderImage", new OneShotMacro.SingleEntry(OneShotMacro.VarType.SINGLE, "imgArgs"));
        map.put("renderSubMenuBegin", new OneShotMacro.BeginEntry(OneShotMacro.VarType.LIST, "subMenuList"));
        map.put("renderSubMenuEnd", new OneShotMacro.EndEntry());
        renderEntryMacroNameMap = map;
    }
    
    private int macroCount = 999;
    private final Map<Appendable, Environment> environments = new HashMap<Appendable, Environment>();
    private final Template macroLibrary;
    private final HttpServletRequest request;
    private final HttpServletResponse response;

    // SCIPIO: new
    private final FtlScriptFormatter ftlFmt = new FtlScriptFormatter();
    private ContextHandler contextHandler = new ContextHandler("menu");
    private final String rendererName;
    
    /**
     * SCIPIO: One-shot macro helper class. Controls whether render macros piecemeal or
     * in one invocation upon close.
     * <p>
     * NOTE: 2017-04-25: removed final, in some case might need to switch it out temporarily.
     */
    private OneShotMacro oneShotMacro = makeOneShotMacroInst();
    
    /**
     * Constructor.
     * <p>
     * SCIPIO: modified to require name.
     */
    public MacroMenuRenderer(String name, String macroLibraryPath, HttpServletRequest request, HttpServletResponse response) throws TemplateException, IOException {
        // SCIPIO: use abstracted template build
        this.macroLibrary = MacroScreenRenderer.getTemplate(name, macroLibraryPath);
        this.request = request;
        this.response = response;
        this.rendererName = name; // SCIPIO: new
    }

    private OneShotMacro makeOneShotMacroInst() { // SCIPIO
        return new OneShotMacro(oneShotMacrosEnabled, "renderMenuFull", renderEntryMacroNameMap, ftlFmt);
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
    
    // Made this a separate method so it can be externalized and reused.
    private Map<String, Object> createImageParameters(Map<String, Object> context, Image image) {
        Map<String, Object> parameters = new HashMap<String, Object>();
        parameters.put("id", image.getId(context));
        parameters.put("style", image.getStyle(context));
        parameters.put("width", image.getWidth(context));
        parameters.put("height", image.getHeight(context));
        parameters.put("border", image.getBorder(context));
        String src = image.getSrc(context);
        if (UtilValidate.isNotEmpty(src) && request != null && response != null) {
            String urlMode = image.getUrlMode();
            if ("ofbiz".equalsIgnoreCase(urlMode)) {
                Boolean fullPath = null; // SCIPIO: changed from boolean to Boolean
                Boolean secure = null; // SCIPIO: changed from boolean to Boolean
                Boolean encode = false; // SCIPIO: changed from boolean to Boolean
                ServletContext ctx = (ServletContext) request.getAttribute("servletContext");
                RequestHandler rh = (RequestHandler) ctx.getAttribute("_REQUEST_HANDLER_");
                src = rh.makeLink(request, response, src, fullPath, secure, encode);
            } else if ("content".equalsIgnoreCase(urlMode)) {
                StringBuilder newURL = new StringBuilder();
                ContentUrlTag.appendContentPrefix(request, newURL);
                newURL.append(src);
                src = newURL.toString();
            }
        }
        parameters.put("src", src);
        return parameters;
    }

    private void executeMacro(Appendable writer, String macro) throws IOException, TemplateException {
        Environment environment = getEnvironment(writer);
        Reader templateReader = new StringReader(macro);
        macroCount++;
        String templateName = toString().concat("_") + macroCount;
        Template template = new Template(templateName, templateReader, FreeMarkerWorker.getDefaultOfbizConfig());
        templateReader.close();
        FreeMarkerWorker.includeTemplate(template, environment);
    }

    /**
     * SCIPIO: This is the original executeMacro.
     * <p>
     * NOTE: To prevent auto-enclosing String in quotes, pass a StringBuilder or other non-String wrapper instead.
     */
    private void executeMacroReal(Appendable writer, String macroName, Map<String, Object> macroParameters) throws IOException, TemplateException {
        StringBuilder sb = new StringBuilder("<@");
        sb.append(macroName);
        if (macroParameters != null) {
            for (Map.Entry<String, Object> parameter : macroParameters.entrySet()) {
                sb.append(' ');
                sb.append(parameter.getKey());
                sb.append("=");
                Object value = parameter.getValue();
                if (value instanceof String) {
                    sb.append(ftlFmt.makeStringLiteral((String) value));
                } else {
                    sb.append(value);
                }
            }
        }
        sb.append(" />");
        if (Debug.verboseOn()) {
            Debug.logVerbose("Executing macro: " + sb, module);
        }
        executeMacro(writer, sb.toString());
    }
    
    /**
     * SCIPIO: Modified executeMacro.
     */
    private void executeMacro(Appendable writer, String macroName, Map<String, Object> macroParameters) throws IOException, TemplateException {
        if (oneShotMacro.isEnabled()) {
            oneShotMacro.appendData(writer, macroName, macroParameters);
        } else {
            executeMacroReal(writer, macroName, macroParameters);
        }
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

    
    /**
     * SCIPIO: NOTE: this now also considers the straight "disabled" attribute new in Scipio.
     */
    private boolean isDisableIfEmpty(ModelMenuItem menuItem, Map<String, Object> context) {
        // SCIPIO: check the direct attribute first
        Boolean disabledDirect = menuItem.getDisabled(context);
        if (disabledDirect != null) {
            return disabledDirect;
        }
        
        boolean disabled = false;
        String disableIfEmpty = menuItem.getDisableIfEmpty();
        if (UtilValidate.isNotEmpty(disableIfEmpty)) {
            List<String> keys = StringUtil.split(disableIfEmpty, "|");
            for (String key : keys) {
                Object obj = context.get(key);
                if (obj == null) {
                    disabled = true;
                    break;
                }
            }
        }
        return disabled;
    }

    private boolean isHideIfSelected(ModelMenuItem menuItem, Map<String, Object> context) {
        // SCIPIO: this is obsolete
        //ModelMenu menu = menuItem.getFuncModelMenu();
        //String currentMenuItemName = menu.getSelectedMenuItemContextFieldName(context);
        //String currentItemName = menuItem.getName();
        //Boolean hideIfSelected = menuItem.getHideIfSelected();
        //return (hideIfSelected != null && hideIfSelected.booleanValue() && currentMenuItemName != null && currentMenuItemName.equals(currentItemName));
        Boolean hideIfSelected = menuItem.getHideIfSelected();
        MenuItemState itemState = MenuRenderState.retrieve(context).getItemState();
        return (hideIfSelected != null && hideIfSelected.booleanValue() && itemState.isSelected());
    }
    
    /**
     * SCIPIO: Renders full menu in one macro call using data previously collected in buffer.
     */
    protected void renderMenuFull(Appendable writer, Map<String, Object> context, ModelMenu menu, StringBuffer sb) throws IOException {
        try {
            executeMacro(writer, sb.toString());
        } catch (TemplateException e) {
            throw new IOException(e);
        }
    }
    
    @Override
    public void renderFormatSimpleWrapperClose(Appendable writer, Map<String, Object> context, ModelMenu menu) throws IOException {
        // Nothing to do.
    }

    @Override
    public void renderFormatSimpleWrapperOpen(Appendable writer, Map<String, Object> context, ModelMenu menu) throws IOException {
        contextHandler.registerContext(writer, context);
        // Nothing to do.
    }

    @Override
    public void renderFormatSimpleWrapperRows(Appendable writer, Map<String, Object> context, Object menu) throws IOException {
        contextHandler.registerContext(writer, context);
        List<ModelMenuItem> menuItemList = ((ModelMenu) menu).getOrderedMenuItemList(context);
        for (ModelMenuItem currentMenuItem : menuItemList) {
            // SCIPIO: this is WRONG, must go through the menu item's method, otherwise missing implementation
            //renderMenuItem(writer, context, currentMenuItem);
            currentMenuItem.renderMenuItemString(writer, context, (MenuStringRenderer) context.get("menuStringRenderer"));
        }
    }

    @Override
    public void renderImage(Appendable writer, Map<String, Object> context, Image image) throws IOException {
        Map<String, Object> parameters = createImageParameters(context, image);
        
        parameters.put("menuCtxRole", MenuRenderState.retrieve(context).getMenuCtxRoleOrEmpty());
        
        try {
            executeMacro(writer, "renderImage", parameters);
        } catch (TemplateException e) {
            throw new IOException(e);
        }
    }

    @Override
    public void renderLink(Appendable writer, Map<String, Object> context, MenuLink link) throws IOException {
        Map<String, Object> parameters = new HashMap<String, Object>();
        String target = link.getTarget(context);
        ModelMenuItem menuItem = link.getLinkMenuItem();
        // SCIPIO: Let macro decide what to do when disabled.
        //if (isDisableIfEmpty(menuItem, context)) {
        //    target = null;
        //}
        boolean disabled = isDisableIfEmpty(menuItem, context);
        
        // SCIPIO: tell macro which selected and disabled
        MenuRenderState renderState = MenuRenderState.retrieve(context);
        MenuAndItem selectedMenuAndItem = renderState.getSelectedMenuAndItem(context);
        ModelMenuItem selectedMenuItem = selectedMenuAndItem.getMenuItem();
        
        boolean selected = menuItem.isSame(selectedMenuItem);
        boolean selectedAncestor = !selected && menuItem.isAncestorOf(selectedMenuAndItem.getSubMenu());
        parameters.put("id", link.getId(context));
        parameters.put("style", link.getStyle(context));
        parameters.put("name", link.getName(context));
        parameters.put("text", link.getText(context));
        parameters.put("targetWindow", link.getTargetWindow(context));
        String uniqueItemName = menuItem.getTopMenu().getName() + "_" + menuItem.getName() + "_LF_" + UtilMisc.<String> addToBigDecimalInMap(context, "menuUniqueItemIndex", BigDecimal.ONE);
        if(menuItem.getModelMenu().getExtraIndex(context) != null){
            uniqueItemName += "_" + menuItem.getTopMenu().getExtraIndex(context);
        }
        // SCIPIO: make uniqueItemName actually globally unique; is NOT unique in stock ofbiz!
        uniqueItemName += "_" + MacroScreenRenderer.getNextUniqueItemNameIdNum(context);
        parameters.put("uniqueItemName", uniqueItemName);
        String linkType = "";
        if (UtilValidate.isNotEmpty(target)) {
            linkType = WidgetWorker.determineAutoLinkType(link.getLinkType(), target, link.getUrlMode(), request);
        }
        parameters.put("linkType", linkType);
        String linkUrl = "";
        String actionUrl = "";
        StringBuilder targetParameters = new StringBuilder();
        if ("hidden-form".equals(linkType) || "ajax-window".equals(linkType)) {
            StringBuilder sb = new StringBuilder();
            WidgetWorker.buildHyperlinkUrl(sb, target, link.getUrlMode(), null, link.getPrefix(context), link.getFullPath(), link.getSecure(), link.getEncode(), request, response, context);
            actionUrl = sb.toString();
            targetParameters.append("[");
            for (Map.Entry<String, String> parameter : link.getParameterMap(context).entrySet()) {
                if (targetParameters.length() > 1) {
                    targetParameters.append(",");
                }
                targetParameters.append("{'name':");
                targetParameters.append(ftlFmt.makeStringLiteralSQ(parameter.getKey()));
                targetParameters.append(",'value':");
                targetParameters.append(ftlFmt.makeStringLiteralSQ(parameter.getValue()));
                targetParameters.append("}");
            }
            targetParameters.append("]");

        }
        if (targetParameters.length() == 0) {
            targetParameters.append("\"\"");
        }
        if (UtilValidate.isNotEmpty(target)) {
            if (!"hidden-form".equals(linkType)) {
                StringBuilder sb = new StringBuilder();
                WidgetWorker.buildHyperlinkUrl(sb, target, link.getUrlMode(), link.getParameterMap(context), link.getPrefix(context), link.getFullPath(), link.getSecure(), link.getEncode(), request, response, context);
                linkUrl = sb.toString();
            }
        }
        parameters.put("linkUrl", linkUrl);
        parameters.put("actionUrl", actionUrl);
        parameters.put("parameterList", targetParameters); // SCIPIO: NOTE: this must NOT be a String; explicitly pass StringBuilder (obscure workaround)
        String imgStr = "";
        Image img = link.getImage();
        if (img != null) {
            StringWriter sw = new StringWriter();
            renderImage(sw, context, img);
            imgStr = sw.toString();
        }
        parameters.put("imgStr", imgStr);
        
        parameters.put("menuCtxRole", MenuRenderState.retrieve(context).getMenuCtxRoleOrEmpty());
        
        // SCIPIO: add disabled and selected
        parameters.put("disabled", disabled);
        parameters.put("selected", selected);
        parameters.put("selectedAncestor", selectedAncestor);
        
        try {
            executeMacro(writer, "renderLink", parameters);
        } catch (TemplateException e) {
            throw new IOException(e);
        }
    }

    @Override
    public void renderMenuClose(Appendable writer, Map<String, Object> context, ModelMenu menu) throws IOException {
        Map<String, Object> parameters = new HashMap<String, Object>();

        // SCIPIO: new entries
        parameters.put("style", menu.getMenuContainerStyle(context));
        parameters.put("inlineEntries", MenuRenderState.retrieve(context).isInlineEntries());
        parameters.put("menuCtxRole", MenuRenderState.retrieve(context).getMenuCtxRoleOrEmpty());
        
        if (ModelWidget.widgetBoundaryCommentsEnabled(context)) {
            StringBuilder sb = new StringBuilder("End Menu Widget ");
            sb.append(menu.getBoundaryCommentName());
            parameters.put("boundaryComment", sb.toString());
        }
        try {
            executeMacro(writer, "renderMenuEnd", parameters);
        } catch (TemplateException e) {
            throw new IOException(e);
        }
        // SCIPIO: reset one-shot macro buffer
        if (oneShotMacro.isReady()) {
            renderMenuFull(writer, context, menu, oneShotMacro.getBuffer());
            oneShotMacro.resetState();
        }
    }

    @Override
    public void renderMenuItem(Appendable writer, Map<String, Object> context, ModelMenuItem menuItem) throws IOException {
        contextHandler.registerContext(writer, context);
        if (isHideIfSelected(menuItem, context))
            return;
        Map<String, Object> parameters = new HashMap<String, Object>();
        String style = menuItem.getWidgetStyle();
        
        // SCIPIO: tell macro which selected and disabled
        MenuRenderState renderState = MenuRenderState.retrieve(context);
        MenuAndItem selectedMenuAndItem = renderState.getSelectedMenuAndItem(context);
        //ModelMenuItem selectedMenuItem = selectedMenuAndItem.getMenuItem();
        ModelSubMenu selectedSubMenu = selectedMenuAndItem.getSubMenu();
        MenuItemState menuItemState = renderState.getItemState();
        
        boolean selected = menuItemState.isSelected();
        boolean selectedAncestor = menuItemState.isSelectedAncestor();
        if (selected) {
            String selectedStyle = menuItem.getSelectedStyle();
            // SCIPIO: Must use new combination logic
            //if (UtilValidate.isEmpty(selectedStyle)) {
            //    selectedStyle = "selected";
            //}
            //if (UtilValidate.isNotEmpty(style)) {
            //    style += " " ;
            //}
            //style += selectedStyle ;
            // SCIPIO: fallback default does not work well here anymore, so now managed by ftl impl.
            //if (UtilValidate.isEmpty(selectedStyle)) {
            //    selectedStyle = "+selected";
            //} 
            style = ModelMenu.combineExtraStyle(style, selectedStyle);
        } else {
            // SCIPIO: support selected-ancestor
            if (selectedAncestor) {
                String selectedStyle = menuItem.getSelectedAncestorStyle();
                // SCIPIO: fallback default does not work well here anymore, so now managed by ftl impl.
                //if (UtilValidate.isEmpty(selectedStyle)) {
                //    selectedStyle = "+selected-ancestor";
                //} 
                style = ModelMenu.combineExtraStyle(style, selectedStyle);
            }
        }
        boolean disabled = this.isDisableIfEmpty(menuItem, context);
        if (disabled) {
            // SCIPIO: Must use new combination logic
            //style = menuItem.getDisabledTitleStyle();
            style = ModelMenu.combineExtraStyle(style, menuItem.getDisabledTitleStyle());
        }
        if (style == null) {
            style = "";
        }
        String alignStyle = menuItem.getAlignStyle();
        if (UtilValidate.isNotEmpty(alignStyle)) {
            // SCIPIO: Must use new combination logic
            //style = style.concat(" ").concat(alignStyle);
            style = ModelMenu.combineExtraStyle(style, alignStyle);
        }
        
        // SCIPIO: expand the style here (not done previously, and _may_ expand on its own through FTL, but
        // may not produce expected results!)
        style = FlexibleStringExpander.expandString(style, context).trim();
                
        parameters.put("style", style);
        parameters.put("toolTip", menuItem.getTooltip(context));
        String linkStr = "";
        MenuLink link = menuItem.getLink();
        // SCIPIO: NEW: implement use-when on link
        boolean useLink = (link != null && !Boolean.FALSE.equals(link.getUseWhen(context)));
        if (useLink) {
            StringWriter sw = new StringWriter();
            renderLink(sw, context, link);
            linkStr = sw.toString();
        } else {
            linkStr = menuItem.getTitle(context);
            linkStr = WidgetWorker.getEarlyEncoder(context).encode(linkStr); // SCIPIO: simplified
        }
        parameters.put("linkStr", linkStr);
        // SCIPIO: we have a better check now
        //boolean containsNestedMenus = !menuItem.getMenuItemList().isEmpty();
        boolean containsNestedMenus = menuItem.hasSubMenu();
        
        // SCIPIO: 2016-08-29: max depth check
        //if (renderState == null) {
        //    Debug.logWarning("No MenuRenderState present in context; no depth checks possible", module);
        //} else {
        if (renderState.hasReachedMaxDepth()) {
            containsNestedMenus = false;
        }
        //}
        
        parameters.put("containsNestedMenus", containsNestedMenus);
        
        
        // SCIPIO: menu context role
        String menuCtxRole = MenuRenderState.retrieve(context).getMenuCtxRoleOrEmpty();
        parameters.put("menuCtxRole", menuCtxRole);
        
        // SCIPIO: sub menu style
        // NOTE: there is another "getSubMenu" (for "sub-menu" attribute), but I don't know what it was intended for.
        String subMenuStyle = menuItem.getSubMenuStyle(context);
        parameters.put("subMenuStyle", subMenuStyle); // SCIPIO: 2016-08-26: NOTE: DEPRECATED
        
        // SCIPIO: sub menu id
        String subMenuId = menuItem.getSubMenuId(context);
        parameters.put("subMenuId", subMenuId); // SCIPIO: 2016-08-26: NOTE: DEPRECATED
        
        // SCIPIO: sub menu title
        String subMenuTitle = menuItem.getSubMenuTitle(context);
        parameters.put("subMenuTitle", subMenuTitle); // SCIPIO: 2016-08-26: NOTE: DEPRECATED
        
        // SCIPIO: disabled and selected
        parameters.put("selected", selected);
        parameters.put("disabled", disabled);
        parameters.put("selectedAncestor", selectedAncestor);
        
        // SCIPIO: 2017-02-17: new
        parameters.put("name", menuItem.getName());
        
        try {
            executeMacro(writer, "renderMenuItemBegin", parameters);
        } catch (TemplateException e) {
            throw new IOException(e);
        }

        if (containsNestedMenus) {
            // SCIPIO: 2016-08-26: This is now obsoleted in favor of explicit sub-menus.
            // We could have provided compatibility for macros, but all it will do is
            // significantly slow down the code due to rendering the sub-items twice.
            //for (ModelMenuItem childMenuItem : menuItem.getMenuItemList()) {
            //    childMenuItem.renderMenuItemString(writer, context, this);
            //}
            if (renderState != null) {
                renderState.increaseCurrentDepth();
            }
            try {
                for(ModelSubMenu childSubMenu : menuItem.getSubMenuList()) {
                    Boolean expandOvrd = childSubMenu.getExpand(context);
                    // TODO: expand flag functionality is incomplete - this does not expand parents -
                    // need functionality from org.ofbiz.widget.model.ModelMenu.FlaggedMenuNodes
                    if (!Boolean.FALSE.equals(expandOvrd)) { // false expandOvrd prevents expand, true guarantees expand
                        if (!(renderState != null && renderState.isCurrentSubMenusOnly()) || childSubMenu.isSameOrAncestorOf(selectedSubMenu) || Boolean.TRUE.equals(expandOvrd)) {
                            // 2017-04-25: if this is the separate sub-menu match, we have to render it to a different output
                            if (renderState != null && renderState.checkUpdateSeparateMenuTargetSelected(context, childSubMenu)) {
                                // SPECIAL: to extract the sub-menu definition, we put a marker, output, and copy the output
                                String subMenuOutput;
                                oneShotMacro.setMarker();
                                try {
                                    childSubMenu.renderSubMenuString(writer, context, this);
     
                                    subMenuOutput = oneShotMacro.getBufferFromFirstValueSinceMarker();
                                    
                                    String origAction = renderState.getSeparateMenuConfig().getTargetOriginalAction();
                                    if (origAction.startsWith("remove")) {
                                        // simply delete the text output we made
                                        oneShotMacro.resetToMarker();
                                    } else { // preserve
                                        // nothing to do, we leave the original alone 
                                    }
                                } finally {
                                    oneShotMacro.clearMarker();
                                }
                                
                                if (UtilValidate.isNotEmpty(subMenuOutput)) {
                                    oneShotMacro.addExtraRootMacroArgRaw("sepMenuDef", subMenuOutput);
                                }
                            } else if (renderState != null && renderState.isSeparateMenuTargetStaticNonSelected(context, childSubMenu) &&
                                    "remove-all".equals(renderState.getSeparateMenuConfig().getTargetOriginalAction())) {
                                ; // do not render - removing all candidates
                            } else {
                                // Regular case
                                childSubMenu.renderSubMenuString(writer, context, this);
                            }
                        }
                    }
                }
            } finally {
                if (renderState != null) {
                    renderState.decreaseCurrentDepth();
                }
            }
        }
 
        parameters.clear();
        parameters.put("containsNestedMenus", containsNestedMenus);
        
        parameters.put("menuCtxRole", menuCtxRole);
        
        try {
            executeMacro(writer, "renderMenuItemEnd", parameters);
        } catch (TemplateException e) {
            throw new IOException(e);
        }
    }

    @Override
    public void renderMenuOpen(Appendable writer, Map<String, Object> context, ModelMenu menu) throws IOException {
        contextHandler.registerContext(writer, context);
        Map<String, Object> parameters = new HashMap<String, Object>();
        if (ModelWidget.widgetBoundaryCommentsEnabled(context)) {
            StringBuilder sb = new StringBuilder("Begin Menu Widget ");
            sb.append(menu.getBoundaryCommentName());
            parameters.put("boundaryComment", sb.toString());
        }
        MenuRenderState renderState = MenuRenderState.retrieve(context);
        parameters.put("id", menu.getId());
        parameters.put("style", menu.getMenuContainerStyle(context));
        parameters.put("title", menu.getTitle(context));
        parameters.put("titleStyle", menu.getTitleStyle(context)); // SCIPIO: new
        parameters.put("inlineEntries", renderState.isInlineEntries());
        parameters.put("menuCtxRole", renderState.getMenuCtxRoleOrEmpty());
        
        MenuAndItem selectedMenuAndItem = renderState.getSelectedMenuAndItem(context);
        ModelMenuItem selectedMenuItem = selectedMenuAndItem.getMenuItem();
        ModelSubMenu selectedSubMenu = selectedMenuAndItem.getSubMenu();
        
        boolean selected = selectedSubMenu == null && menu.isParentOf(selectedMenuItem);
        boolean selectedAncestor = !selected && (selectedSubMenu != null || (selectedMenuItem != null && !menu.isParentOf(selectedMenuItem)));
        parameters.put("selected", selected);
        parameters.put("selectedAncestor", selectedAncestor);
        
        // SCIPIO: 2017-02-17: new
        parameters.put("name", menu.getName());
        
        // SCIPIO: 2017-04-25: new
        parameters.put("sepMenuType", renderState.getSeparateMenuConfig().getType());
        // NOTE: sepMenuDef is added using special code further below
        
        try {
            executeMacro(writer, "renderMenuBegin", parameters);
        } catch (TemplateException e) {
            throw new IOException(e);
        }
    }

    @Override
    public void renderSubMenuOpen(Appendable writer, Map<String, Object> context, ModelSubMenu subMenu)
            throws IOException {
        // SCIPIO: new method
        
        contextHandler.registerContext(writer, context);
        
        Map<String, Object> parameters = new HashMap<String, Object>();
        
        parameters.put("id", subMenu.getId(context));
        parameters.put("style", subMenu.getStyle(context));
        parameters.put("title", subMenu.getTitle(context));
        parameters.put("effectiveName", subMenu.getEffectiveName());
        
        MenuRenderState renderState = MenuRenderState.retrieve(context);
        
        // SCIPIO: menu context role
        parameters.put("menuCtxRole", renderState.getMenuCtxRoleOrEmpty());

        MenuAndItem selectedMenuAndItem = renderState.getSelectedMenuAndItem(context);
        
        boolean selected = subMenu.isSame(selectedMenuAndItem.getSubMenu());
        boolean selectedAncestor = !selected && subMenu.isSameOrAncestorOf(selectedMenuAndItem.getSubMenu());
        parameters.put("selected", selected);
        parameters.put("selectedAncestor", selectedAncestor);
        
        // SCIPIO: 2017-02-17: new
        parameters.put("name", subMenu.getName());
        
        try {
            executeMacro(writer, "renderSubMenuBegin", parameters);
        } catch (TemplateException e) {
            throw new IOException(e);
        }
    }
    
    @Override
    public void renderSubMenuClose(Appendable writer, Map<String, Object> context, ModelSubMenu subMenu)
            throws IOException {
        // SCIPIO: new method

        Map<String, Object> parameters = new HashMap<String, Object>();
        
        // SCIPIO: menu context role
        parameters.put("menuCtxRole", MenuRenderState.retrieve(context).getMenuCtxRoleOrEmpty());
        
        try {
            executeMacro(writer, "renderSubMenuEnd", parameters);
        } catch (TemplateException e) {
            throw new IOException(e);
        }
    }

    
}
