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
package org.ofbiz.widget.model;

import java.io.IOException;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilGenerics;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.base.util.UtilXml;
import org.ofbiz.base.util.string.FlexibleStringExpander;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.widget.model.CommonWidgetModels.AutoEntityParameters;
import org.ofbiz.widget.model.CommonWidgetModels.AutoServiceParameters;
import org.ofbiz.widget.model.CommonWidgetModels.Image;
import org.ofbiz.widget.model.CommonWidgetModels.Link;
import org.ofbiz.widget.model.CommonWidgetModels.Parameter;
import org.ofbiz.widget.model.MenuRenderState.MenuItemState;
import org.ofbiz.widget.model.ModelMenu.CurrentMenuDefBuildArgs;
import org.ofbiz.widget.model.ModelMenu.GeneralBuildArgs;
import org.ofbiz.widget.model.ModelMenuItem.MenuLink;
import org.ofbiz.widget.model.ModelMenuNode.ModelMenuItemNode;
import org.ofbiz.widget.portal.PortalPageWorker;
import org.ofbiz.widget.renderer.MenuStringRenderer;
import org.w3c.dom.Element;

/**
 * Models the &lt;menu-item&gt; element.
 * 
 * @see <code>widget-menu.xsd</code>
 */
@SuppressWarnings("serial")
public class ModelMenuItem extends ModelWidget implements ModelMenuItemNode {

    /*
     * ----------------------------------------------------------------------- *
     *                     DEVELOPERS PLEASE READ
     * ----------------------------------------------------------------------- *
     * 
     * This model is intended to be a read-only data structure that represents
     * an XML element. Outside of object construction, the class should not
     * have any behaviors.
     * 
     * Instances of this class will be shared by multiple threads - therefore
     * it is immutable. DO NOT CHANGE THE OBJECT'S STATE AT RUN TIME!
     * 
     */

    public static final String module = ModelMenuItem.class.getName();
    
    // SCIPIO: special/keyword menu and item names
    public static final String PARENT_MENU_ITEM_NAME = "PARENT";
    public static final String PARENT_NOSUB_MENU_ITEM_NAME = "PARENT-NOSUB";
    public static final String NONE_MENU_ITEM_NAME = "NONE";
    static final Set<String> specialMenuItemNames = UtilMisc.toHashSet(NONE_MENU_ITEM_NAME, PARENT_MENU_ITEM_NAME, PARENT_NOSUB_MENU_ITEM_NAME);
    static final Set<String> parentMenuItemNames = UtilMisc.toHashSet(PARENT_MENU_ITEM_NAME, PARENT_NOSUB_MENU_ITEM_NAME);
    
    private final List<ModelAction> actions;
    private final String align;
    private final String alignStyle;
    private final FlexibleStringExpander associatedContentId;
    private final String cellWidth;
    private final ModelMenuCondition condition;
    private final String disabledTitleStyle;
    private final String disableIfEmpty;
    private final String entityName;
    private final Boolean hideIfSelected;
    private final MenuLink link;
    //@Deprecated
    //private final List<ModelMenuItem> menuItemList; // SCIPIO: This is replaced by ModelSubMenu. This var doesn't contain the sub-menu menu-item elements.
    private final ModelMenu modelMenu;  
    private final String overrideName;
    //private final ModelMenuItem parentMenuItem; // SCIPIO: must go through parent sub menu now
    private final ModelSubMenu parentSubMenu;   // SCIPIO: new
    private final FlexibleStringExpander parentPortalPageId;
    private final Integer position;
    private final String selectedStyle;
    private final String selectedAncestorStyle; // SCIPIO: new
    @Deprecated
    private final String subMenu; // SCIPIO: This should not be used.
    private final FlexibleStringExpander title;
    private final String titleStyle;
    private final FlexibleStringExpander tooltip;
    private final String tooltipStyle;
    private final String widgetStyle;
    private final String linkStyle;
    
    private final String overrideMode; // SCIPIO: override mode
    private final String sortMode; // SCIPIO: sort mode
    
    @Deprecated
    private final String subMenuModel;  // SCIPIO: DEPRECATED - use ModelSubMenu instead (no relation to subMenu)
    @Deprecated
    private final String subMenuStyle;  // SCIPIO: DEPRECATED - use ModelSubMenu instead (no relation to subMenu)
    @Deprecated
    private final String subMenuTitle;  // SCIPIO: DEPRECATED - use ModelSubMenu instead (no relation to subMenu)

    private transient ModelMenu styleModelMenu = null; // SCIPIO: records which model menu should be used for style fields (NOTE: doesn't need synchronizing)
    private transient ModelMenu funcModelMenu = null; // SCIPIO: records which model menu should be used for functional fields
 
    private final Map<String, ModelSubMenu> subMenuMap; // SCIPIO: new sub-menu models (order preserved)
    private final List<ModelSubMenu> subMenuList; // SCIPIO: new sub-menu models
    
    private final FlexibleStringExpander disabled; // SCIPIO: new
    
    private final Boolean alwaysExpandSelectedOrAncestor; // SCIPIO: new (override)
    private final FlexibleStringExpander selected; // SCIPIO: new (override)

    
    // ===== CONSTRUCTORS =====

    public ModelMenuItem(Element menuItemElement, ModelMenu modelMenu, BuildArgs buildArgs) {
        this(menuItemElement, modelMenu, null, buildArgs);
    }
    
    public ModelMenuItem(Element menuItemElement, ModelSubMenu parentSubMenu, BuildArgs buildArgs) {
        this(menuItemElement, parentSubMenu.getTopModelMenu(), parentSubMenu, buildArgs);
    }
    
    // SCIPIO: constructor modified to take parentInfo and buildArgs.
    // Presence of parentInfo.menuItem indicates this item is part of a sub-menu.
    private ModelMenuItem(Element menuItemElement, ModelMenu modelMenu, ModelSubMenu parentSubMenu, BuildArgs buildArgs) {
        super(menuItemElement);
        buildArgs.genBuildArgs.totalMenuItemCount++;
        this.modelMenu = modelMenu;
        //this.parentMenuItem = parentMenuItem;
        this.parentSubMenu = parentSubMenu; // SCIPIO: new
        this.entityName = menuItemElement.getAttribute("entity-name");
        this.title = FlexibleStringExpander.getInstance(menuItemElement.getAttribute("title"));
        this.tooltip = FlexibleStringExpander.getInstance(menuItemElement.getAttribute("tooltip"));
        this.parentPortalPageId = FlexibleStringExpander.getInstance(menuItemElement.getAttribute("parent-portal-page-value"));
        this.titleStyle = menuItemElement.getAttribute("title-style");
        this.disabledTitleStyle = menuItemElement.getAttribute("disabled-title-style");
        this.widgetStyle = menuItemElement.getAttribute("widget-style");
        this.linkStyle = menuItemElement.getAttribute("link-style");
        this.overrideMode = menuItemElement.getAttribute("override-mode");
        this.sortMode = menuItemElement.getAttribute("sort-mode");
        this.tooltipStyle = menuItemElement.getAttribute("tooltip-style");
        this.selectedStyle = menuItemElement.getAttribute("selected-style");
        this.selectedAncestorStyle = menuItemElement.getAttribute("selected-ancestor-style");
        String hideIfSelected = menuItemElement.getAttribute("hide-if-selected");
        if (!hideIfSelected.isEmpty())
            if (hideIfSelected.equalsIgnoreCase("true"))
                this.hideIfSelected = Boolean.TRUE;
            else
                this.hideIfSelected = Boolean.FALSE;
        else
            this.hideIfSelected = null;
        this.disableIfEmpty = menuItemElement.getAttribute("disable-if-empty");
        this.disabled = FlexibleStringExpander.getInstance(menuItemElement.getAttribute("disabled"));
        this.align = menuItemElement.getAttribute("align");
        this.alignStyle = menuItemElement.getAttribute("align-style");
        Integer position = null;
        String positionStr = menuItemElement.getAttribute("position");
        if (!positionStr.isEmpty()) {
            try {
                position = Integer.valueOf(positionStr);
            } catch (Exception e) {
                Debug.logError(e, "Could not convert position attribute of the field element to an integer: [" + positionStr
                        + "], using the default of the menu renderer", module);
                position = null;
            }
        }
        this.position = position;
        this.associatedContentId = FlexibleStringExpander.getInstance(menuItemElement.getAttribute("associated-content-id"));
        this.cellWidth = menuItemElement.getAttribute("cell-width");
        this.subMenu = menuItemElement.getAttribute("sub-menu");
        Element linkElement = UtilXml.firstChildElement(menuItemElement, "link");
        if (linkElement != null) {
            this.link = new MenuLink(linkElement, this);
        } else {
            this.link = null;
        }
        
        if (!menuItemElement.getAttribute("always-expand-selected-or-ancestor").isEmpty()) 
            alwaysExpandSelectedOrAncestor = "true".equals(menuItemElement.getAttribute("always-expand-selected-or-ancestor"));
        else 
            alwaysExpandSelectedOrAncestor = null;
        this.selected = FlexibleStringExpander.getInstance(menuItemElement.getAttribute("selected"));

        // SCIPIO: legacy inlined menu-items
        if (buildArgs.omitSubMenus) {
            this.subMenuModel = "";
            this.subMenuStyle = "";
            this.subMenuTitle = "";
            
            this.subMenuList = Collections.emptyList();
            this.subMenuMap = Collections.emptyMap();
        } else {
            List<? extends Element> itemElements = UtilXml.childElementList(menuItemElement, "menu-item");

            // old sub-menu-xxx attributes (deprecated)
            this.subMenuModel = menuItemElement.getAttribute("sub-menu-model");
            this.subMenuStyle = menuItemElement.getAttribute("sub-menu-style");
            this.subMenuTitle = menuItemElement.getAttribute("sub-menu-title");

            // the sub-menu elements
            List<Element> subMenuElements = UtilGenerics.checkList(UtilXml.childElementList(menuItemElement, "sub-menu"));
            if (subMenuElements.isEmpty()) {
                // we must generate an element for legacy support if there are any inlined menu-items
                if (!itemElements.isEmpty()) {
                    Element subMenuElem = menuItemElement.getOwnerDocument().createElement("sub-menu");
                    if (!this.subMenuModel.isEmpty()) {
                        subMenuElem.setAttribute("model", this.subMenuModel);
                    }
                    if (!this.subMenuStyle.isEmpty()) {
                        subMenuElem.setAttribute("style", this.subMenuStyle);
                    }
                    if (!this.subMenuTitle.isEmpty()) {
                        subMenuElem.setAttribute("title", this.subMenuTitle);
                    }
                    subMenuElements.add(subMenuElem);
                }
            }
            
            List<ModelSubMenu> subMenuList = new ArrayList<>();
            Map<String, ModelSubMenu> subMenuMap = new LinkedHashMap<>();
            
            int i = 0;
            for(Element subMenuElement : subMenuElements) {
                List<? extends Element> extraMenuItems = null;
                if (i == 0) {
                    // Add the legacy items as extras to the first sub-menu
                    extraMenuItems = itemElements;
                }
                ModelSubMenu.BuildArgs subBuildArgs = new ModelSubMenu.BuildArgs(buildArgs);
                subBuildArgs.extraMenuItems = extraMenuItems;
                
                ModelSubMenu modelSubMenu = new ModelSubMenu(subMenuElement, buildArgs.currResource, 
                        this, subBuildArgs);
                
                subMenuMap.put(modelSubMenu.getEffectiveName(), modelSubMenu);
                subMenuList.add(modelSubMenu);
                
                i++;
            }
            this.subMenuList = Collections.unmodifiableList(subMenuList);
            this.subMenuMap = Collections.unmodifiableMap(subMenuMap);
        }

        // SCIPIO: NOTE: reading of legacy menu-items (menuItemList) is done by ModelSubMenu.
        // The code that was here was moved/removed.
        
        // read condition under the "condition" element
        Element conditionElement = UtilXml.firstChildElement(menuItemElement, "condition");
        if (conditionElement != null) {
            conditionElement = UtilXml.firstChildElement(conditionElement);
            this.condition = new ModelMenuCondition(this, conditionElement);
        } else {
            this.condition = null;
        }
        // read all actions under the "actions" element
        Element actionsElement = UtilXml.firstChildElement(conditionElement, "actions");
        if (actionsElement != null) {
            this.actions = AbstractModelAction.readSubActions(this, actionsElement);
        } else {
            this.actions = Collections.emptyList();
        }
        this.overrideName = "";
    }

    // Portal constructor
    private ModelMenuItem(GenericValue portalPage, ModelMenuItem parentMenuItem, Locale locale) {
        super(portalPage.getString("portalPageId"));
        this.actions = Collections.emptyList();
        this.align = "";
        this.alignStyle = "";
        this.associatedContentId = FlexibleStringExpander.getInstance("");
        this.cellWidth = "";
        this.condition = null;
        this.disabledTitleStyle = "";
        this.disableIfEmpty = "";
        this.disabled = FlexibleStringExpander.getInstance("");
        this.entityName = "";
        this.hideIfSelected = null;
        //this.menuItemList = Collections.emptyList(); // SCIPIO: moved to ModelSubMenu
        this.overrideName = "";
        //this.parentMenuItem = null;
        this.parentSubMenu = null; // SCIPIO: new
        this.parentPortalPageId = FlexibleStringExpander.getInstance(portalPage.getString("parentPortalPageId"));
        this.position = null;
        this.selectedStyle = "";
        this.selectedAncestorStyle = "";
        this.subMenu = "";
        this.title = FlexibleStringExpander.getInstance((String) portalPage.get("portalPageName", locale));
        this.titleStyle = "";
        this.tooltip = FlexibleStringExpander.getInstance("");
        this.tooltipStyle = "";
        this.widgetStyle = "";
        this.linkStyle = "";
        this.overrideMode = "";
        this.sortMode = "";
        this.alwaysExpandSelectedOrAncestor = null;
        this.selected = FlexibleStringExpander.getInstance("");
        this.link = new MenuLink(portalPage, parentMenuItem, locale);
        this.modelMenu = parentMenuItem.modelMenu;
        this.subMenuList = Collections.emptyList();
        this.subMenuMap = Collections.emptyMap();
        this.subMenuModel = "";
        this.subMenuStyle = "";
        this.subMenuTitle = "";
    }

    // SCIPIO: copy constructor
    private ModelMenuItem(ModelMenuItem existingMenuItem, 
            ModelMenu modelMenu, ModelSubMenu parentSubMenu, BuildArgs buildArgs) {
        super(existingMenuItem.getName());
        buildArgs.genBuildArgs.totalMenuItemCount++;
        // SCIPIO: this is an error; when we retried the modelMenu we want the effective one,
        // not the original one
        //this.modelMenu = existingMenuItem.modelMenu;
        this.modelMenu = (modelMenu != null ? modelMenu : existingMenuItem.modelMenu);
        this.overrideName = existingMenuItem.getName();
        this.entityName = existingMenuItem.entityName;
        this.parentPortalPageId = existingMenuItem.parentPortalPageId;
        this.title = existingMenuItem.title;
 
        this.tooltip = existingMenuItem.tooltip;
        this.titleStyle = existingMenuItem.titleStyle;
        this.selectedStyle = existingMenuItem.selectedStyle;

        this.selectedAncestorStyle = existingMenuItem.selectedAncestorStyle;
        this.widgetStyle = existingMenuItem.widgetStyle;
        this.linkStyle = existingMenuItem.linkStyle;
        this.position = existingMenuItem.position;
        this.sortMode = existingMenuItem.sortMode;
        
        this.subMenuModel = existingMenuItem.subMenuModel;
        this.subMenuStyle = existingMenuItem.subMenuStyle;
        this.subMenuTitle = existingMenuItem.subMenuTitle;
  
        ArrayList<ModelSubMenu> subMenuList = new ArrayList<>();
        Map<String, ModelSubMenu> subMenuMap = new HashMap<>();
        if (!buildArgs.omitSubMenus) {
            ModelSubMenu.cloneModelSubMenus(existingMenuItem.getSubMenuList(), 
                    subMenuList, subMenuMap, getModelMenu(), this,
                    new ModelSubMenu.BuildArgs(buildArgs));
            subMenuList.trimToSize();
        }
        this.subMenuList = Collections.unmodifiableList(subMenuList);
        this.subMenuMap = Collections.unmodifiableMap(subMenuMap);

        // SCIPIO: some of the assignments below have been changed to take
        // into account overrideMenuItem.
        
        List<ModelAction> actions = new ArrayList<>();
        actions.addAll(existingMenuItem.actions);
        this.actions = Collections.unmodifiableList(actions);
        this.align = existingMenuItem.align;

        this.alignStyle = existingMenuItem.alignStyle;
        this.associatedContentId = existingMenuItem.associatedContentId;
        this.cellWidth = existingMenuItem.cellWidth;
        this.condition = existingMenuItem.condition;
        this.disabledTitleStyle = existingMenuItem.disabledTitleStyle;
        this.disableIfEmpty = existingMenuItem.disableIfEmpty;
        this.disabled = existingMenuItem.disabled;
        this.hideIfSelected = existingMenuItem.hideIfSelected;
        
        //this.menuItemList = existingMenuItem.menuItemList; // SCIPIO: moved to ModelSubMenu
        // SCIPIO: this should ALWAYS use the overriding parent item
        //this.parentMenuItem = existingMenuItem.parentMenuItem;
        // SCIPIO: too problematic, keeping sub menu only instead
        //this.parentMenuItem = overrideMenuItem.parentMenuItem; 
        this.parentSubMenu = (parentSubMenu != null) ? parentSubMenu : existingMenuItem.parentSubMenu; // SCIPIO: should always be ours
        this.subMenu = existingMenuItem.subMenu;
    
        this.tooltipStyle = existingMenuItem.tooltipStyle;
        this.link = existingMenuItem.link;
 
        this.overrideMode = existingMenuItem.overrideMode;
        this.alwaysExpandSelectedOrAncestor = existingMenuItem.alwaysExpandSelectedOrAncestor;
        this.selected = existingMenuItem.selected;
    }
    
    // Merge constructor
    // SCIPIO: enhanced for inserting new backreferences
    private ModelMenuItem(ModelMenuItem existingMenuItem, ModelMenuItem overrideMenuItem, 
            BuildArgs buildArgs) {
        super(overrideMenuItem.getName());
        buildArgs.genBuildArgs.totalMenuItemCount++;
        // SCIPIO: this is an error; when we retried the modelMenu we want the effective one,
        // not the original one
        //this.modelMenu = existingMenuItem.modelMenu;
        this.modelMenu = overrideMenuItem.modelMenu;
        this.parentSubMenu = overrideMenuItem.parentSubMenu; // SCIPIO: should always be ours
        if (UtilValidate.isNotEmpty(overrideMenuItem.getName())) {
            this.overrideName = overrideMenuItem.getName();
        } else {
            this.overrideName = existingMenuItem.getName();
        }
        if (UtilValidate.isNotEmpty(overrideMenuItem.entityName)) {
            this.entityName = overrideMenuItem.entityName;
        } else {
            this.entityName = existingMenuItem.entityName;
        }
        if (UtilValidate.isNotEmpty(overrideMenuItem.parentPortalPageId)) {
            this.parentPortalPageId = overrideMenuItem.parentPortalPageId;
        } else {
            this.parentPortalPageId = existingMenuItem.parentPortalPageId;
        }
        if (UtilValidate.isNotEmpty(overrideMenuItem.title)) {
            this.title = overrideMenuItem.title;
        } else {
            this.title = existingMenuItem.title;
        }
        if (UtilValidate.isNotEmpty(overrideMenuItem.tooltip)) {
            this.tooltip = overrideMenuItem.tooltip;
        } else {
            this.tooltip = existingMenuItem.tooltip;
        }
        if (UtilValidate.isNotEmpty(overrideMenuItem.titleStyle)) {
            this.titleStyle = overrideMenuItem.titleStyle;
        } else {
            this.titleStyle = existingMenuItem.titleStyle;
        }
        if (UtilValidate.isNotEmpty(overrideMenuItem.selectedStyle)) {
            this.selectedStyle = overrideMenuItem.selectedStyle;
        } else {
            this.selectedStyle = existingMenuItem.selectedStyle;
        }
        if (UtilValidate.isNotEmpty(overrideMenuItem.selectedAncestorStyle)) {
            this.selectedAncestorStyle = overrideMenuItem.selectedAncestorStyle;
        } else {
            this.selectedAncestorStyle = existingMenuItem.selectedAncestorStyle;
        }
        if (UtilValidate.isNotEmpty(overrideMenuItem.widgetStyle)) {
            this.widgetStyle = overrideMenuItem.widgetStyle;
        } else {
            this.widgetStyle = existingMenuItem.widgetStyle;
        }
        if (UtilValidate.isNotEmpty(overrideMenuItem.linkStyle)) {
            this.linkStyle = overrideMenuItem.linkStyle;
        } else {
            this.linkStyle = existingMenuItem.linkStyle;
        }
        if (overrideMenuItem.position != null) {
            this.position = overrideMenuItem.position;
        } else {
            this.position = existingMenuItem.position;
        }
        if (UtilValidate.isNotEmpty(overrideMenuItem.sortMode)) {
            this.sortMode = overrideMenuItem.sortMode;
        } else {
            this.sortMode = existingMenuItem.sortMode;
        }
        if (overrideMenuItem.alwaysExpandSelectedOrAncestor != null) {
            this.alwaysExpandSelectedOrAncestor = overrideMenuItem.alwaysExpandSelectedOrAncestor;
        } else {
            this.alwaysExpandSelectedOrAncestor = existingMenuItem.alwaysExpandSelectedOrAncestor;
        }
        if (UtilValidate.isNotEmpty(overrideMenuItem.selected.getOriginal())) {
            this.selected = overrideMenuItem.selected;
        } else {
            this.selected = existingMenuItem.selected;
        }
        
        if (UtilValidate.isNotEmpty(overrideMenuItem.subMenuModel)) {
            this.subMenuModel = overrideMenuItem.subMenuModel;
        } else {
            this.subMenuModel = existingMenuItem.subMenuModel;
        }
        if (UtilValidate.isNotEmpty(overrideMenuItem.subMenuStyle)) {
            this.subMenuStyle = overrideMenuItem.subMenuStyle;
        } else {
            this.subMenuStyle = existingMenuItem.subMenuStyle;
        }
        if (UtilValidate.isNotEmpty(overrideMenuItem.subMenuTitle)) {
            this.subMenuTitle = overrideMenuItem.subMenuTitle;
        } else {
            this.subMenuTitle = existingMenuItem.subMenuTitle;
        }

        List<ModelSubMenu> srcSubMenuList;
        boolean subMenusFromOverriding = overrideMenuItem.hasSubMenu();
        if (subMenusFromOverriding) {
            // WARN: cloning the overriding is probably excessive and stupidly slow because overrideMenuItem was just created,
            // but we need to update all the backreferences to this item in the children (and other things if caller passed), 
            // so we have no choice.
            srcSubMenuList = overrideMenuItem.getSubMenuList();
        } else {
            srcSubMenuList = existingMenuItem.getSubMenuList();
        }
        ArrayList<ModelSubMenu> subMenuList = new ArrayList<>();
        Map<String, ModelSubMenu> subMenuMap = new HashMap<>();
        // SCIPIO: NOTE: the omit logic here is for completeness but it is not really used in practice.
        // the first (non-recursive) call to processIncludeMenuItems method actually always passes omitSubMenus=false,
        // and instead the existingMenuItem was recursively built with omitSubMenus true in the XML constructor.
        if (!buildArgs.omitSubMenus || subMenusFromOverriding) { // NOTE: the omit flag is intended only for the existing, not the overriding
            // TODO?: in the future we could support recursive merging here, but it's a can of worms.
            ModelSubMenu.cloneModelSubMenus(srcSubMenuList, 
                    subMenuList, subMenuMap, getModelMenu(), this,
                    new ModelSubMenu.BuildArgs(buildArgs));
            subMenuList.trimToSize();
        }
        this.subMenuList = Collections.unmodifiableList(subMenuList);
        this.subMenuMap = Collections.unmodifiableMap(subMenuMap);

        // SCIPIO: some of the assignments below have been changed to take
        // into account overrideMenuItem.
        
        List<ModelAction> actions = new ArrayList<>();
        actions.addAll(existingMenuItem.actions);
        actions.addAll(overrideMenuItem.actions);
        this.actions = Collections.unmodifiableList(actions);
        if (UtilValidate.isNotEmpty(overrideMenuItem.align)) {
            this.align = overrideMenuItem.align;
        } else {
            this.align = existingMenuItem.align;
        }
        if (UtilValidate.isNotEmpty(overrideMenuItem.alignStyle)) {
            this.alignStyle = overrideMenuItem.alignStyle;
        } else {
            this.alignStyle = existingMenuItem.alignStyle;
        }
        if (UtilValidate.isNotEmpty(overrideMenuItem.associatedContentId.getOriginal())) {
            this.associatedContentId = overrideMenuItem.associatedContentId;
        } else {
            this.associatedContentId = existingMenuItem.associatedContentId;
        }
        if (UtilValidate.isNotEmpty(overrideMenuItem.cellWidth)) {
            this.cellWidth = overrideMenuItem.cellWidth;
        } else {
            this.cellWidth = existingMenuItem.cellWidth;
        }
        if (overrideMenuItem.condition != null) {
            this.condition = overrideMenuItem.condition;
        } else {
            this.condition = existingMenuItem.condition;
        }
        if (UtilValidate.isNotEmpty(overrideMenuItem.disabledTitleStyle)) {
            this.disabledTitleStyle = overrideMenuItem.disabledTitleStyle;
        } else {
            this.disabledTitleStyle = existingMenuItem.disabledTitleStyle;
        }
        if (UtilValidate.isNotEmpty(overrideMenuItem.disableIfEmpty)) {
            this.disableIfEmpty = overrideMenuItem.disableIfEmpty;
        } else {
            this.disableIfEmpty = existingMenuItem.disableIfEmpty;
        }
        if (!overrideMenuItem.disabled.getOriginal().isEmpty()) {
            this.disabled = overrideMenuItem.disabled;
        } else {
            this.disabled = existingMenuItem.disabled;
        }
        
        if (overrideMenuItem.hideIfSelected != null) {
            this.hideIfSelected = overrideMenuItem.hideIfSelected;
        } else {
            this.hideIfSelected = existingMenuItem.hideIfSelected;
        }
        //this.menuItemList = existingMenuItem.menuItemList; // SCIPIO: moved to ModelSubMenu
        // SCIPIO: this should ALWAYS use the overriding parent item
        //this.parentMenuItem = existingMenuItem.parentMenuItem;
        // SCIPIO: too problematic, keeping sub menu only instead
        //this.parentMenuItem = overrideMenuItem.parentMenuItem; 
        
        if (UtilValidate.isNotEmpty(overrideMenuItem.subMenu)) {
            this.subMenu = overrideMenuItem.subMenu;
        } else {
            this.subMenu = existingMenuItem.subMenu;
        }
        if (UtilValidate.isNotEmpty(overrideMenuItem.tooltipStyle)) {
            this.tooltipStyle = overrideMenuItem.tooltipStyle;
        } else {
            this.tooltipStyle = existingMenuItem.tooltipStyle;
        }
        if (overrideMenuItem.link != null) {
            this.link = overrideMenuItem.link;
        } else {
            this.link = existingMenuItem.link;
        }
        this.overrideMode = overrideMenuItem.overrideMode;
    }
    
    @Override
    public void accept(ModelWidgetVisitor visitor) throws Exception {
        visitor.visit(this);
    }

    @SuppressWarnings("unused")
    private void addUpdateMenuItem(ModelMenuItem modelMenuItem, List<ModelMenuItem> menuItemList,
            Map<String, ModelMenuItem> menuItemMap, BuildArgs buildArgs) {
        // SCIPIO: this is a copy of the ModelMenu method, so delegate
        modelMenu.addUpdateMenuItem(modelMenuItem, menuItemList, menuItemMap, buildArgs);
    }

    public List<ModelAction> getActions() {
        return actions;
    }

    public String getAlign() {
        if (!this.align.isEmpty()) {
            return this.align;
        } else if (parentSubMenu != null) {
            return getParentMenuItem().getAlign();
        } else {
            return this.modelMenu.getDefaultAlign();
        }
    }

    public String getAlignStyle() {
        return getStyle("align", this.alignStyle, getStyleModelMenu().getDefaultAlignStyle());
    }

    public FlexibleStringExpander getAssociatedContentId() {
        return associatedContentId;
    }

    public String getAssociatedContentId(Map<String, Object> context) {
        String retStr = null;
        if (this.associatedContentId != null) {
            retStr = associatedContentId.expandString(context);
        }
        if (retStr.isEmpty()) {
            retStr = this.modelMenu.getDefaultAssociatedContentId(context);
        }
        return retStr;
    }

    public String getCellWidth() {
        if (!this.cellWidth.isEmpty()) {
            return this.cellWidth;
        } else {
            return this.modelMenu.getDefaultCellWidth();
        }
    }

    public ModelMenuCondition getCondition() {
        return condition;
    }

    public String getDisabledTitleStyle() {
        return getStyle("disabled", this.disabledTitleStyle, getStyleModelMenu().getDefaultDisabledTitleStyle());
    }

    public String getDisableIfEmpty() {
        return this.disableIfEmpty;
    }
    
    public FlexibleStringExpander getDisabled() {
        return this.disabled;
    }
    
    public Boolean getDisabled(Map<String, Object> context) {
        return UtilMisc.booleanValue(this.disabled.expandString(context));
    }
    
    /**
     * SCIPIO: Returns if has selected expr defined.
     */
    public boolean hasSelectedExpr() {
        return !this.selected.getOriginal().isEmpty();
    }
    
    /**
     * SCIPIO: Returns expression determining if selected style override is enabled.
     */
    public FlexibleStringExpander getSelected() {
        return this.selected;
    }
    
    public String getEntityName() {
        if (!this.entityName.isEmpty()) {
            return this.entityName;
        } else if (parentSubMenu != null) {
            return getParentMenuItem().getEntityName();
        } else {
            return this.modelMenu.getDefaultEntityName();
        }
    }

    public Boolean getHideIfSelected() {
        if (hideIfSelected != null) {
            return this.hideIfSelected;
        } else {
            return this.modelMenu.getDefaultHideIfSelected();
        }
    }

    public MenuLink getLink() {
        return this.link;
    }

    /**
     * SCIPIO: This is deprecated. Should go through ModelSubMenu instead.
     */
    @Deprecated
    public List<ModelMenuItem> getMenuItemList() {
        ModelSubMenu subMenu = getDefaultSubMenu();
        if (subMenu != null) {
            return subMenu.getMenuItemList();
        } else {
            return Collections.emptyList();
        }
    }
    
    /**
     * SCIPIO: This is a variant of getMenuItemList that only returns the legacy inlined
     * menu-items, but not the ones under sub-menu elements.
     */
    @Deprecated
    public List<ModelMenuItem> getLegacyMenuItemList() {
        ModelSubMenu subMenu = getDefaultSubMenu();
        if (subMenu != null) {
            return subMenu.getExtraMenuItemList();
        } else {
            return Collections.emptyList();
        }
    }

    public ModelMenu getModelMenu() {
        return modelMenu;
    }

    @Override
    public String getName() {
        if (!this.overrideName.isEmpty()) {
            return this.overrideName;
        }
        return super.getName();
    }

    public String getOverrideName() {
        return overrideName;
    }

    public ModelMenuItem getParentMenuItem() {
        if (parentSubMenu != null) {
            return parentSubMenu.getParentMenuItem();
        } else {
            return null;
        }
    }
    
    public ModelMenuItem getTopParentMenuItem() { // SCIPIO: new
        ModelMenuItem curr = this;
        while(curr.getParentMenuItem() != null) {
            curr = curr.getParentMenuItem();
        }
        return curr;
    }

    public FlexibleStringExpander getParentPortalPageId() {
        return parentPortalPageId;
    }

    public String getParentPortalPageId(Map<String, Object> context) {
        return this.parentPortalPageId.expandString(context);
    }

    public int getPosition() {
        if (this.position == null) {
            return 1;
        } else {
            return position.intValue();
        }
    }

    public String getSelectedStyle() {
        return getStyle("selected", this.selectedStyle, getStyleModelMenu().getDefaultSelectedStyle());
    }
    
    public String getSelectedAncestorStyle() {
        return getStyle("selectedAncestor", this.selectedAncestorStyle, getStyleModelMenu().getDefaultSelectedAncestorStyle());
    }

    /**
     * SCIPIO: NOTE: This should not be used.
     */
    @Deprecated
    public String getSubMenu() {
        return subMenu;
    }

    public FlexibleStringExpander getTitle() {
        return title;
    }

    public String getTitle(Map<String, Object> context) {
        return title.expandString(context);
    }

    public String getTitleStyle() {
        return getStyle("title", this.titleStyle, getStyleModelMenu().getDefaultTitleStyle());
    }

    public FlexibleStringExpander getTooltip() {
        return tooltip;
    }

    public String getTooltip(Map<String, Object> context) {
        if (UtilValidate.isNotEmpty(tooltip)) {
            return tooltip.expandString(context);
        } else {
            return "";
        }
    }

    public String getTooltipStyle() {
        return getStyle("tooltip", this.tooltipStyle, getStyleModelMenu().getDefaultTooltipStyle());
    }

    public String getWidgetStyle() {
        return getStyle("widget", this.widgetStyle, getStyleModelMenu().getDefaultWidgetStyle());
    }
    
    /**
     * SCIPIO: Gets the logical link style. The style on <link> element has priority
     * over the link-style on <menu-item>.
     */
    public String getLinkStyle() {
        // Check the style directly on the <link> element first
        String style = this.link.getStyleExdr().getOriginal();
        // If not there, use link-style on <menu-item>
        if (style.isEmpty()) {
            style = this.linkStyle;
        }
        return getStyle("link", style, getStyleModelMenu().getDefaultLinkStyle());
    }
    
    /**
     * SCIPIO: Gets style.
     * <p>
     * TODO?: this could probably cache based on passed name for faster access, but not certain
     * if safe. OR it could be integrated into constructor,
     * but I'm not sure that safe either (because of the "link" element).
     */
    String getStyle(String name, String style, String defaultStyle) {
        return ModelMenu.buildStyle(style, null, defaultStyle);
    }
    
    public String getOverrideMode() {
        return this.overrideMode;
    }
    
    public String getSortMode() {
        return this.sortMode;
    }
    
    public List<ModelSubMenu> getSubMenuList() {
        return subMenuList;
    }
    
    public Map<String, ModelSubMenu> getSubMenuMap() {
        return subMenuMap;
    }
    
    public boolean hasSubMenu() {
        return !subMenuList.isEmpty();
    }
    
    public ModelSubMenu getDefaultSubMenu() {
        if (subMenuList.size() > 0) {
            return subMenuList.get(0);
        } else {
            return null;
        }
    }
    
    public ModelMenuItem getModelMenuItemByTrail(String[] nameList) {
        return getModelMenuItemByTrail(nameList[0], nameList, 1);
    }
    
    public ModelMenuItem getModelMenuItemByTrail(String name, String[] nameList, int nextNameIndex) {
        String[] currLevelRef = name.split(":", 2);
        String nextItemName;
        ModelSubMenu subMenu;
        if (currLevelRef.length >= 2) {
            subMenu = this.subMenuMap.get(currLevelRef[0]);
            nextItemName = currLevelRef[1];
        } else {
            subMenu = this.getDefaultSubMenu();
            nextItemName = currLevelRef[0];
        }
        
        if (subMenu != null) {
            return subMenu.getModelMenuItemByTrail(nextItemName, nameList, nextNameIndex + 1);
        } else {
            return null;
        }
    }
    
    
    @Deprecated
    public String getSubMenuId(Map<String, Object> context) {
        ModelSubMenu subMenu = getDefaultSubMenu();
        if (subMenu != null) {
            return subMenu.getId(context);
        } else {
            return "";
        }
    }
    
    @Deprecated
    public String getSubMenuStyle(Map<String, Object> context) {
        ModelSubMenu subMenu = getDefaultSubMenu();
        if (subMenu != null) {
            return subMenu.getStyle(context);
        } else {
            return "";
        }
    }
    
    @Deprecated
    public String getSubMenuStyle() {
        ModelSubMenu subMenu = getDefaultSubMenu();
        if (subMenu != null) {
            return subMenu.getStyle();
        } else {
            return "";
        }
    }
    
    @Deprecated
    public String getSubMenuTitle(Map<String, Object> context) {
        ModelSubMenu subMenu = getDefaultSubMenu();
        if (subMenu != null) {
            return subMenu.getTitle(context);
        } else {
            return "";
        }
    }
    
    
    /**
     * SCIPIO: Returns THIS item's alt model menu for style fields.
     */
    public ModelMenu getStyleModelMenu() {
        // WARN: special fast thread-safe read pattern in use here (single atomic read of instance variable (which is immutable object)
        // using local variable); no synchronized block used because single calculation/assignment not important.
        ModelMenu result = this.styleModelMenu;
        if (result == null) {
            result = (parentSubMenu != null) ? parentSubMenu.getStyleModelMenu() : this.getModelMenu();
            this.styleModelMenu = result;
        }
        return result;
    }
    
    /**
     * SCIPIO: Returns THIS item's alt model menu for functional/logic fields.
     */
    public ModelMenu getFuncModelMenu() {
        // WARN: special fast thread-safe read pattern in use here (single atomic read of instance variable (which is immutable object)
        // using local variable); no synchronized block used because single calculation/assignment not important.
        ModelMenu result = this.funcModelMenu;
        if (result == null) {
            result = (parentSubMenu != null) ? parentSubMenu.getFuncModelMenu() : this.getModelMenu();
            this.funcModelMenu = result;
        }
        return result;
    }

    /**
     * Checks if selected.
     * <p>
     * @deprecated SCIPIO: use getModelMenu().getSelectedMenuItem and compare instead
     */
    @Deprecated
    public boolean isSelected(Map<String, Object> context) {
        // SCIPIO: This is modified to heavily simplify and centralize
        ModelMenuItem selMenuItem = getModelMenu().getSelectedMenuAndItem(context).getMenuItem();
        return (isSame(selMenuItem)); // WARN: this is hackish but it should currently work
    }

    /**
     * SCIPIO: NOTE: only valid if the items were part of the same ModelMenu instance.
     */
    public boolean isSame(ModelMenuItem menuItem) {
        return this == menuItem; // SCIPIO: NOTE: this works because we know how the ModelMenu was built
    }
    
    public boolean isSameOrAncestorOf(ModelMenuItem menuItem) {
        if (menuItem == null) {
            return false;
        }
        else if (isSame(menuItem)) {
            return true;
        } else {
            return isSameOrAncestorOf(menuItem.getParentMenuItem());
        }
    }
    
    public boolean isAncestorOf(ModelSubMenu subMenu) {
        if (subMenu == null) {
            return false;
        }
        else {
            return isSameOrAncestorOf(subMenu.getParentMenuItem());
        }
    }
    
    /**
     * Merges items.
     * <p>
     * SCIPIO: modified for better in-depth cloning.
     * <p>
     * NOTE: the backreferences are always taken from overrideMenuItem.
     */
    public ModelMenuItem mergeOverrideModelMenuItem(ModelMenuItem overrideMenuItem, BuildArgs buildArgs) {
        return new ModelMenuItem(this, overrideMenuItem, buildArgs);
    }
    
    /**
     * SCIPIO: Clones item.
     * <p>
     * NOTE: if modelMenu/parentSubMenu are null, they are taken from this item. 
     */
    public ModelMenuItem cloneModelMenuItem(ModelMenu modelMenu, ModelSubMenu parentSubMenu, BuildArgs buildArgs) {
        return new ModelMenuItem(this, modelMenu, parentSubMenu, buildArgs);
    }
    
    public static void cloneModelMenuItems(List<ModelMenuItem> menuItemList, 
            List<ModelMenuItem> targetList, Map<String, ModelMenuItem> targetMap, 
            ModelMenu modelMenu, ModelSubMenu parentSubMenu, BuildArgs buildArgs) {
        for(ModelMenuItem menuItem : menuItemList) {
            ModelMenuItem clonedItem = menuItem.cloneModelMenuItem(modelMenu, parentSubMenu, buildArgs);
            targetList.add(clonedItem);
            targetMap.put(clonedItem.getName(), clonedItem);
        }
    }
    
    public boolean isTopMenuItem() {
        return parentSubMenu == null;
    }
    
    public ModelSubMenu getParentSubMenu() {
        return parentSubMenu;
    }
    
    public String getDisplayText(Map<String, Object> context) {
        String res;
        MenuLink link = getLink();
        if (link != null) {
            res = link.getTextExdr().expandString(context);
            if (UtilValidate.isNotEmpty(res)) {
                return res;
            }
        }
        res = getTitle(context);
        if (UtilValidate.isNotEmpty(res)) {
            return res;
        }
        return getName();
    }

    void addAllSubMenus(Map<String, ModelSubMenu> subMenuMap) {
        for(ModelSubMenu subMenu : getSubMenuList()) {
            String name = subMenu.getEffectiveName();
            if (subMenuMap.containsKey(name)) {
                Debug.logError("Duplicate resolved sub-menu name '" + name + "' in menu " +
                        modelMenu.getMenuLocation() + "#" + modelMenu.getName() + "; "
                                + "you may have to disable auto-sub-menu-names on the menu "
                                + "and/or explicitly use the sub-menu element 'name' "
                                + "attribute to resolve this situation", module);   
            } else {
                subMenuMap.put(name, subMenu);
            }
            subMenu.addAllSubMenus(subMenuMap);
        }
    }
    
    public void renderMenuItemString(Appendable writer, Map<String, Object> context, MenuStringRenderer menuStringRenderer)
            throws IOException {
        // SCIPIO: figure out this logic early so the condition can use it
        MenuRenderState renderState = MenuRenderState.retrieve(context);
        Object prevItemContext = prepareItemContext(context, renderState);

        if (shouldBeRendered(context, renderState)) {
            AbstractModelAction.runSubActions(actions, context);
            String parentPortalPageId = getParentPortalPageId(context);
            if (UtilValidate.isNotEmpty(parentPortalPageId)) {
                List<GenericValue> portalPages = PortalPageWorker.getPortalPages(parentPortalPageId, context);
                if (UtilValidate.isNotEmpty(portalPages)) {
                    Locale locale = (Locale) context.get("locale");
                    for (GenericValue portalPage : portalPages) {
                        if (UtilValidate.isNotEmpty(portalPage.getString("portalPageName"))) {
                            ModelMenuItem localItem = new ModelMenuItem(portalPage, this, locale);
                            menuStringRenderer.renderMenuItem(writer, context, localItem);
                        }
                    }
                }
            } else {
                menuStringRenderer.renderMenuItem(writer, context, this);
            }
        }
        
        // SCIPIO: restore previous
        restoreItemContext(context, prevItemContext, renderState);
    }

    /**
     * SCIPIO: prepares context for an item render and returns the previous
     * item's state (if any).
     */
    public Object prepareItemContext(Map<String, Object> context) {
        return prepareItemContext(context, MenuRenderState.retrieve(context));
    }
    
    public Object prepareItemContext(Map<String, Object> context, MenuRenderState renderState) {
        MenuItemState prevItemState = renderState.getItemState();
        renderState.setItemState(MenuItemState.fromCurrent(this, context, renderState));
        return prevItemState;
    }
    
    /**
     * SCIPIO: restores item context, after a call to prepareItemContext.
     */
    public void restoreItemContext(Map<String, Object> context, Object prevItemContext) {
        restoreItemContext(context, prevItemContext, MenuRenderState.retrieve(context));
    }
    
    public void restoreItemContext(Map<String, Object> context, Object prevItemContext, MenuRenderState renderState) {
        renderState.setItemState((MenuItemState) prevItemContext);
    }
    
    
    /**
     * Determines if item should be rendered, basic on condition and context.
     * <p>
     * SCIPIO: NOTE: This is ONLY valid if the context was prepared for the item
     * using {@link #prepareItemContext(Map)}!
     */
    public boolean shouldBeRendered(Map<String, Object> context) {
        // SCIPIO: now delegated
        return shouldBeRendered(context, MenuRenderState.retrieve(context));
    }
    
    public boolean shouldBeRendered(Map<String, Object> context, MenuRenderState renderState) {
        // SCIPIO: first check if must always render selected or ancestor
        Boolean result = null;
        MenuItemState itemState = renderState.getItemState();
        if (Boolean.TRUE.equals(this.alwaysExpandSelectedOrAncestor) ||
                (this.alwaysExpandSelectedOrAncestor == null && this.getModelMenu().isAlwaysExpandSelectedOrAncestor()) &&
            itemState.isSelectedOrAncestor()) {
            result = Boolean.TRUE;
        }
        if (this.condition != null) {
            // SCIPIO: only assign this if always-expand not set, but always store the result.
            boolean conditionResult = this.condition.getCondition().eval(context);
            if (result == null) {
                result = conditionResult;
            }
            itemState.setConditionResult(conditionResult);
        }
        if (result != null) {
            return result;
        } else {
            return true;
        }
    }

    public static class MenuLink implements Serializable {
        private final ModelMenuItem linkMenuItem;
        private final Link link;
        
        public MenuLink(Element linkElement, ModelMenuItem parentMenuItem) {
            this.linkMenuItem = parentMenuItem;
            if (linkElement.getAttribute("text").isEmpty()) {
                linkElement.setAttribute("text", parentMenuItem.getTitle().getOriginal());
            }
            // SCIPIO: This whole block was removed in ofbiz branch 14.12 r1720933, AFTER we already removed the code inside the block
            //if (linkElement.getAttribute("style").isEmpty()) {
                // SCIPIO: this was changed by us...
                // WARN: removing this effectively changed the behavior of menu-item's widget-style for all widgets!
                //linkElement.setAttribute("style", parentMenuItem.getWidgetStyle());
                // The following was added by us instead of previous line, but we don't need to do this here anymore.
                // we modify the getter below instead.
                //linkElement.setAttribute("style", parentMenuItem.getLinkStyle());
            //}
            this.link = new Link(linkElement);
        }

        public MenuLink(GenericValue portalPage, ModelMenuItem parentMenuItem, Locale locale) {
            this.linkMenuItem = parentMenuItem;
            ArrayList<Parameter> parameterList = new ArrayList<Parameter>();
            if (parentMenuItem.link != null) {
                parameterList.addAll(parentMenuItem.link.getParameterList());
            }
            parameterList.add(new Parameter("portalPageId", portalPage.getString("portalPageId"), false));
            parameterList.add(new Parameter("parentPortalPageId", portalPage.getString("parentPortalPageId"), false));
            String target = "showPortalPage";
            if (parentMenuItem.link != null) {
                target = parentMenuItem.link.getTargetExdr().getOriginal();
            }
            this.link = new Link(portalPage, parameterList, target, locale);
        }

        public AutoEntityParameters getAutoEntityParameters() {
            return link.getAutoEntityParameters();
        }

        public AutoServiceParameters getAutoServiceParameters() {
            return link.getAutoServiceParameters();
        }

        public Boolean getEncode() { // SCIPIO: changed from boolean to Boolean
            return link.getEncode();
        }

        public Boolean getFullPath() { // SCIPIO: changed from boolean to Boolean
            return link.getFullPath();
        }

        public String getHeight() {
            return link.getHeight();
        }

        public String getId(Map<String, Object> context) {
            return link.getId(context);
        }

        public FlexibleStringExpander getIdExdr() {
            return link.getIdExdr();
        }

        public Image getImage() {
            return link.getImage();
        }

        public String getLinkType() {
            return link.getLinkType();
        }

        public String getName() {
            return link.getName();
        }

        public String getName(Map<String, Object> context) {
            return link.getName(context);
        }

        public FlexibleStringExpander getNameExdr() {
            return link.getNameExdr();
        }

        public List<Parameter> getParameterList() {
            return link.getParameterList();
        }

        public Map<String, String> getParameterMap(Map<String, Object> context) {
            return link.getParameterMap(context);
        }

        public String getPrefix(Map<String, Object> context) {
            return link.getPrefix(context);
        }

        public FlexibleStringExpander getPrefixExdr() {
            return link.getPrefixExdr();
        }

        public Boolean getSecure() { // SCIPIO: changed from boolean to Boolean
            return link.getSecure();
        }

        public String getStyle(Map<String, Object> context) {
            // SCIPIO: use more advanced style inheritance
            //return link.getStyle(context);
            // We can simply delegate to the parent menu item. it will fetch back the style from us on its own.
            // This makes sure getLinkStyle will be logical.
            return FlexibleStringExpander.expandString(linkMenuItem.getLinkStyle(), context).trim();
        }

        public FlexibleStringExpander getStyleExdr() {
            return link.getStyleExdr();
        }

        public String getTarget(Map<String, Object> context) {
            return link.getTarget(context);
        }

        public FlexibleStringExpander getTargetExdr() {
            return link.getTargetExdr();
        }

        public String getTargetWindow(Map<String, Object> context) {
            return link.getTargetWindow(context);
        }

        public FlexibleStringExpander getTargetWindowExdr() {
            return link.getTargetWindowExdr();
        }

        public String getText(Map<String, Object> context) {
            return link.getText(context);
        }

        public FlexibleStringExpander getTextExdr() {
            return link.getTextExdr();
        }

        public String getUrlMode() {
            return link.getUrlMode();
        }

        public String getWidth() {
            return link.getWidth();
        }

        public ModelMenuItem getLinkMenuItem() {
            return linkMenuItem;
        }

        public Link getLink() {
            return link;
        }

        public FlexibleStringExpander getUseWhenExdr() { // SCIPIO: new
            return link.getUseWhenExdr();
        }

        public Boolean getUseWhen(Map<String, Object> context) { // SCIPIO: new
            return link.getUseWhen(context);
        }

        public void renderLinkString(Appendable writer, Map<String, Object> context, MenuStringRenderer menuStringRenderer)
                throws IOException {
            menuStringRenderer.renderLink(writer, context, this);
        }
    }
    
    /**
     * SCIPIO: Basic structure of extra options for menu item construction.
     */
    public static class BuildArgs {
        public final GeneralBuildArgs genBuildArgs;
        public final CurrentMenuDefBuildArgs currentMenuDefBuildArgs;
        public String currResource;
        public String forceSubMenuModelScope;

        public boolean omitSubMenus;

        /**
         * Specify-all-essentials constructor.
         */
        public BuildArgs(GeneralBuildArgs genBuildArgs, CurrentMenuDefBuildArgs currentMenuDefBuildArgs,
                    String currResource, String forceSubMenuModelScope) {
            this.genBuildArgs = genBuildArgs;
            this.currentMenuDefBuildArgs = currentMenuDefBuildArgs;
            this.currResource = currResource;
            this.forceSubMenuModelScope = forceSubMenuModelScope;
            this.omitSubMenus = false;
        }
        
        /**
         * Preserve-all-essentials constructor.
         */
        public BuildArgs(ModelSubMenu.BuildArgs subBuildArgs) {
            this.genBuildArgs = subBuildArgs.genBuildArgs;
            this.currentMenuDefBuildArgs = subBuildArgs.currentMenuDefBuildArgs;
            this.currResource = subBuildArgs.currResource;
            this.forceSubMenuModelScope = subBuildArgs.forceSubMenuModelScope;
            this.omitSubMenus = false;
        }
    }

    // SCIPIO: ModelMenuNode methods
    
    @Override
    public ModelMenuItemGroupNode getParentNode() {
        return getParentSubMenu();
    }

    @Override
    public List<ModelSubMenu> getChildrenNodes() {
        return subMenuList;
    }

    @Override
    public FlexibleStringExpander getExpanded() {
        return null;
    }

    @Override
    public String getContainerLocation() { // SCIPIO: new
        // NOTE: this may not give the orig code location due to extends/merging... but can track down
        return getModelMenu().getFullLocationAndName(); 
    }
    
    @Override
    public String getWidgetType() { // SCIPIO: new
        return "menu-item";
    }

}
