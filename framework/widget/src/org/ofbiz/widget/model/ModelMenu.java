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
import java.net.URL;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.ofbiz.base.location.FlexibleLocation;
import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.base.util.UtilXml;
import org.ofbiz.base.util.collections.FlexibleMapAccessor;
import org.ofbiz.base.util.string.FlexibleStringExpander;
import org.ofbiz.widget.model.ModelMenuItem.MenuLink;
import org.ofbiz.widget.model.ModelMenuItem.ModelMenuItemAlias;
import org.ofbiz.widget.renderer.MenuStringRenderer;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NamedNodeMap;

/**
 * Models the &lt;menu&gt; element.
 *
 * @see <code>widget-menu.xsd</code>
 */
@SuppressWarnings("serial")
public class ModelMenu extends ModelMenuCommon implements ModelWidget.IdAttrWidget { // SCIPIO: new comon base class to share with sub-menu

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

    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    // SCIPIO: special/keyword menu and item names
    public static final String TOP_MENU_NAME = "TOP";
    static final Set<String> specialMenuNames = UtilMisc.toHashSet(TOP_MENU_NAME);

    private final List<ModelAction> actions;
    private final String defaultAlign;
    private final String defaultAlignStyle;
    private final FlexibleStringExpander defaultAssociatedContentId;
    private final String defaultCellWidth;
    private final String defaultDisabledTitleStyle;
    private final String defaultEntityName;
    private final Boolean defaultHideIfSelected;
    private final String defaultMenuItemName;
    private final String defaultPermissionEntityAction;
    private final String defaultPermissionOperation;
    private final String defaultSelectedStyle;
    private final String defaultSelectedAncestorStyle; // SCIPIO: new
    private final String defaultTitleStyle;
    private final String defaultTooltipStyle;
    private final String defaultWidgetStyle;
    private final String defaultLinkStyle; // SCIPIO
    private final FlexibleStringExpander extraIndex;
    private final String fillStyle;
    private final String id;
    private final FlexibleStringExpander menuContainerStyleExdr;
    /** This List will contain one copy of each item for each item name in the order
     * they were encountered in the service, entity, or menu definition; item definitions
     * with constraints will also be in this list but may appear multiple times for the same
     * item name.
     *
     * When rendering the menu the order in this list should be following and it should not be
     * necessary to use the Map. The Map is used when loading the menu definition to keep the
     * list clean and implement the override features for item definitions.
     */
    private final List<ModelMenuItem> menuItemList;
    /** This Map is keyed with the item name and has a ModelMenuItem for the value; items
     * with conditions will not be put in this Map so item definition overrides for items
     * with conditions is not possible.
     */
    private final Map<String, ModelMenuItem> menuItemMap;
    private final String menuLocation;
    private final String menuWidth;
    private final String orientation;
    private final ModelMenu parentMenu;
    /**
     * SCIPIO: List of selected menu item context field names (replaces single stock selectedMenuItemContextFieldName field).
     * <p>
     * We now support multiple lookups instead of only one.
     */
    private final List<FlexibleMapAccessor<String>> selectedMenuItemContextFieldName;
    //private final FlexibleMapAccessor<String> selectedMenuItemContextFieldName;
    private final FlexibleMapAccessor<String> selectedMenuContextFieldName; // SCIPIO: sub-menu locator
    private final String selectedMenuItemContextFieldNameStr;
    private final String target;
    private final FlexibleStringExpander title;
    private final String tooltip;
    private final String type;

    // SCIPIO: (other) new fields
    private final String itemsSortMode;

    private final Map<String, ModelSubMenu> subMenuMap; // SCIPIO: map of unique sub-menu names to sub-menus NOTE: only valid post-construction
    private final String autoSubMenuNames;
    private final String defaultSubMenuModelScope;
    private final String defaultSubMenuInstanceScope;

    private final String forceExtendsSubMenuModelScope;
    private final String forceAllSubMenuModelScope;

    private final boolean alwaysExpandSelectedOrAncestor;

    private final FlexibleStringExpander titleStyle;
    private final List<ModelMenuNode> manualSelectedNodes; // SCIPIO: cache of potentially manual selected items
    private final List<ModelMenuNode> manualExpandedNodes; // SCIPIO: cache of potentially manual expanded items

    private final Map<String, ModelMenuItemAlias> menuItemAliasMap;
    private final Map<String, String> menuItemNameAliasMap;

    // SCIPIO: 2017-04-25: new separated menu options
    private final FlexibleStringExpander separateMenuType;
    private final FlexibleStringExpander separateMenuTargetStyle;
    private final FlexibleStringExpander separateMenuTargetPreference;
    private final FlexibleStringExpander separateMenuTargetOriginalAction;

    /** XML Constructor */
    public ModelMenu(Element menuElement, String menuLocation) {
        super(menuElement);
        // SCIPIO: This MUST be set early so the menu item constructor can get the location!
        this.menuLocation = menuLocation;
        GeneralBuildArgs genBuildArgs = new GeneralBuildArgs();

        ArrayList<ModelAction> actions = new ArrayList<>();
        String defaultAlign = "";
        String defaultAlignStyle = "";
        FlexibleStringExpander defaultAssociatedContentId = FlexibleStringExpander.getInstance("");
        String defaultCellWidth = "";
        String defaultDisabledTitleStyle = "";
        String defaultEntityName = "";
        Boolean defaultHideIfSelected = Boolean.FALSE;
        String defaultMenuItemName = "";
        String defaultPermissionEntityAction = "";
        String defaultPermissionOperation = "";
        String defaultSelectedStyle = "";
        String defaultSelectedAncestorStyle = ""; // SCIPIO
        String defaultTitleStyle = "";
        String defaultTooltipStyle = "";
        String defaultWidgetStyle = "";
        String defaultLinkStyle = ""; // SCIPIO
        FlexibleStringExpander extraIndex = FlexibleStringExpander.getInstance("");
        String fillStyle = "";
        String id = "";
        FlexibleStringExpander menuContainerStyleExdr = FlexibleStringExpander.getInstance("");
        ArrayList<ModelMenuItem> menuItemList = new ArrayList<>();
        Map<String, ModelMenuItem> menuItemMap = new HashMap<>();
        String menuWidth = "";
        String orientation = "horizontal";
        // SCIPIO: now using list
        //FlexibleMapAccessor<String> selectedMenuItemContextFieldName = FlexibleMapAccessor.getInstance("");
        List<FlexibleMapAccessor<String>> selectedMenuItemContextFieldName = new ArrayList<>();
        String selectedMenuItemContextFieldNameStr = "";
        FlexibleMapAccessor<String> selectedMenuContextFieldName = FlexibleMapAccessor.getInstance("");
        String target = "";
        FlexibleStringExpander title = FlexibleStringExpander.getInstance("");
        FlexibleStringExpander titleStyle = FlexibleStringExpander.getInstance(""); // SCIPIO
        String tooltip = "";
        String type = "";
        // SCIPIO: (other) new fields
        String itemsSortMode = "";
        String autoSubMenuNames = "";
        String defaultSubMenuModelScope = "";
        String defaultSubMenuInstanceScope = "";
        String forceExtendsSubMenuModelScope = "";
        String forceAllSubMenuModelScope = "";
        boolean alwaysExpandSelectedOrAncestor = false;
        FlexibleStringExpander separateMenuType = FlexibleStringExpander.getInstance("");
        FlexibleStringExpander separateMenuTargetStyle = FlexibleStringExpander.getInstance("");
        FlexibleStringExpander separateMenuTargetPreference = FlexibleStringExpander.getInstance("");
        FlexibleStringExpander separateMenuTargetOriginalAction = FlexibleStringExpander.getInstance("");

        // check if there is a parent menu to inherit from
        ModelMenu parent = null;
        String parentResource = menuElement.getAttribute("extends-resource");
        String parentMenu = menuElement.getAttribute("extends");
        if (!parentMenu.isEmpty()) {
            parent = getMenuDefinition(parentResource, parentMenu, menuElement, genBuildArgs); // SCIPIO: this is now gotten from menuElement in safer way: menuLocation
            if (parent != null) {
                type = parent.type;
                itemsSortMode = parent.itemsSortMode;
                target = parent.target;
                id = parent.id;
                title = parent.title;
                titleStyle = parent.titleStyle;
                tooltip = parent.tooltip;
                defaultEntityName = parent.defaultEntityName;
                defaultTitleStyle = parent.defaultTitleStyle;
                defaultSelectedStyle = parent.defaultSelectedStyle;
                defaultSelectedAncestorStyle = parent.defaultSelectedAncestorStyle;
                defaultWidgetStyle = parent.defaultWidgetStyle;
                defaultLinkStyle = parent.defaultLinkStyle;
                defaultTooltipStyle = parent.defaultTooltipStyle;
                defaultMenuItemName = parent.defaultMenuItemName;

                defaultPermissionOperation = parent.defaultPermissionOperation;
                defaultPermissionEntityAction = parent.defaultPermissionEntityAction;
                defaultAssociatedContentId = parent.defaultAssociatedContentId;
                defaultHideIfSelected = parent.defaultHideIfSelected;
                orientation = parent.orientation;
                menuWidth = parent.menuWidth;
                defaultCellWidth = parent.defaultCellWidth;
                defaultDisabledTitleStyle = parent.defaultDisabledTitleStyle;
                defaultAlign = parent.defaultAlign;
                defaultAlignStyle = parent.defaultAlignStyle;
                fillStyle = parent.fillStyle;
                extraIndex = parent.extraIndex;
                autoSubMenuNames = parent.autoSubMenuNames;
                defaultSubMenuModelScope = parent.defaultSubMenuModelScope;
                defaultSubMenuInstanceScope = parent.defaultSubMenuInstanceScope;
                // SCIPIO: NOT inherited
                //forceExtendsSubMenuModelScope = parent.forceExtendsSubMenuModelScope;
                //forceAllSubMenuModelScope = parent.forceAllSubMenuModelScope;
                // SCIPIO: copy list
                //selectedMenuItemContextFieldName = parent.selectedMenuItemContextFieldName;
                selectedMenuItemContextFieldName = new ArrayList<FlexibleMapAccessor<String>>(parent.selectedMenuItemContextFieldName);
                selectedMenuItemContextFieldNameStr = parent.selectedMenuItemContextFieldNameStr;
                selectedMenuContextFieldName = parent.selectedMenuContextFieldName;
                menuContainerStyleExdr = parent.menuContainerStyleExdr;
                alwaysExpandSelectedOrAncestor = parent.alwaysExpandSelectedOrAncestor;

                separateMenuType = parent.separateMenuType;
                separateMenuTargetStyle = parent.separateMenuTargetStyle;
                separateMenuTargetPreference = parent.separateMenuTargetPreference;
                separateMenuTargetOriginalAction = parent.separateMenuTargetOriginalAction;
            }
        }
        if (!menuElement.getAttribute("type").isEmpty()) {
            type = menuElement.getAttribute("type");
        }
        if (!menuElement.getAttribute("items-sort-mode").isEmpty()) {
            itemsSortMode = menuElement.getAttribute("items-sort-mode");
        }
        if (!menuElement.getAttribute("target").isEmpty()) {
            target = menuElement.getAttribute("target");
        }
        if (!menuElement.getAttribute("id").isEmpty()) {
            id = menuElement.getAttribute("id");
        }
        if (!menuElement.getAttribute("title").isEmpty()) {
            title = FlexibleStringExpander.getInstance(menuElement.getAttribute("title"));
        }
        if (!menuElement.getAttribute("title-style").isEmpty()) {
            titleStyle = FlexibleStringExpander.getInstance(menuElement.getAttribute("title-style"));
        }
        if (!menuElement.getAttribute("tooltip").isEmpty()) {
            tooltip = menuElement.getAttribute("tooltip");
        }
        if (!menuElement.getAttribute("default-entity-name").isEmpty()) {
            defaultEntityName = menuElement.getAttribute("default-entity-name");
        }
        // SCIPIO: MUST pass all the -style attributes through buildStyle to combine with parent values (modifies stock)
        if (!menuElement.getAttribute("default-title-style").isEmpty()) {
            defaultTitleStyle = buildStyle(menuElement.getAttribute("default-title-style"), parent != null ? parent.defaultTitleStyle : null, "");
        }
        if (!menuElement.getAttribute("default-selected-style").isEmpty()) {
            defaultSelectedStyle = buildStyle(menuElement.getAttribute("default-selected-style"), parent != null ? parent.defaultSelectedStyle : null, "");
        }
        if (!menuElement.getAttribute("default-selected-ancestor-style").isEmpty()) {
            defaultSelectedAncestorStyle = buildStyle(menuElement.getAttribute("default-selected-ancestor-style"), parent != null ? parent.defaultSelectedAncestorStyle : null, "");
        }
        if (!menuElement.getAttribute("default-widget-style").isEmpty()) {
            defaultWidgetStyle = buildStyle(menuElement.getAttribute("default-widget-style"), parent != null ? parent.defaultWidgetStyle : null, "");
        }
        if (!menuElement.getAttribute("default-link-style").isEmpty()) { // SCIPIO
            defaultLinkStyle = buildStyle(menuElement.getAttribute("default-link-style"), parent != null ? parent.defaultLinkStyle : null, "");
        }
        if (!menuElement.getAttribute("default-tooltip-style").isEmpty()) {
            defaultTooltipStyle = buildStyle(menuElement.getAttribute("default-tooltip-style"), parent != null ? parent.defaultTooltipStyle : null, "");
        }
        if (!menuElement.getAttribute("default-menu-item-name").isEmpty()) {
            defaultMenuItemName = menuElement.getAttribute("default-menu-item-name");
        }
        if (!menuElement.getAttribute("default-permission-operation").isEmpty()) {
            defaultPermissionOperation = menuElement.getAttribute("default-permission-operation");
        }
        if (!menuElement.getAttribute("default-permission-entity-action").isEmpty()) {
            defaultPermissionEntityAction = menuElement.getAttribute("default-permission-entity-action");
        }
        if (!menuElement.getAttribute("default-associated-content-id").isEmpty()) {
            defaultAssociatedContentId = FlexibleStringExpander.getInstance(menuElement
                    .getAttribute("default-associated-content-id"));
        }
        if (!menuElement.getAttribute("orientation").isEmpty()) {
            orientation = menuElement.getAttribute("orientation");
        }
        if (!menuElement.getAttribute("menu-width").isEmpty()) {
            menuWidth = menuElement.getAttribute("menu-width");
        }
        if (!menuElement.getAttribute("default-cell-width").isEmpty()) {
            defaultCellWidth = menuElement.getAttribute("default-cell-width");
        }
        if (!menuElement.getAttribute("default-hide-if-selected").isEmpty()) {
            defaultHideIfSelected = "true".equals(menuElement.getAttribute("default-hide-if-selected"));
        }
        if (!menuElement.getAttribute("default-disabled-title-style").isEmpty()) {
            defaultDisabledTitleStyle = buildStyle(menuElement.getAttribute("default-disabled-title-style"), parent != null ? parent.defaultDisabledTitleStyle : null, "");
        }
        if (!menuElement.getAttribute("selected-menuitem-context-field-name").isEmpty()) {
            selectedMenuItemContextFieldNameStr = menuElement.getAttribute("selected-menuitem-context-field-name");
            selectedMenuItemContextFieldName = makeAccessorList(selectedMenuItemContextFieldNameStr);
        }
        if (!menuElement.getAttribute("selected-menu-context-field-name").isEmpty()) { // SCIPIO
            String selectedMenuContextFieldNameStr = menuElement.getAttribute("selected-menu-context-field-name");
            selectedMenuContextFieldName = FlexibleMapAccessor.getInstance(selectedMenuContextFieldNameStr);
        }
        if (!menuElement.getAttribute("menu-container-style").isEmpty()) {
            menuContainerStyleExdr = FlexibleStringExpander.getInstance(buildStyle(menuElement.getAttribute("menu-container-style"), parent != null ? parent.menuContainerStyleExdr.getOriginal(): null, ""));
        }
        if (!menuElement.getAttribute("default-align").isEmpty()) {
            defaultAlign = menuElement.getAttribute("default-align");
        }
        if (!menuElement.getAttribute("default-align-style").isEmpty()) {
            defaultAlignStyle = buildStyle(menuElement.getAttribute("default-align-style"), parent != null ? parent.defaultAlignStyle : null, "");
        }
        if (!menuElement.getAttribute("fill-style").isEmpty()) {
            fillStyle = buildStyle(menuElement.getAttribute("fill-style"), parent != null ? parent.fillStyle : null, "");
        }
        if (!menuElement.getAttribute("extra-index").isEmpty()) {
            extraIndex = FlexibleStringExpander.getInstance(menuElement.getAttribute("extra-index"));
        }
        if (!menuElement.getAttribute("auto-sub-menu-names").isEmpty()) {
            autoSubMenuNames = menuElement.getAttribute("auto-sub-menu-names");
        }
        if (!menuElement.getAttribute("default-sub-menu-model-scope").isEmpty()) {
            defaultSubMenuModelScope = menuElement.getAttribute("default-sub-menu-model-scope");
        }
        if (!menuElement.getAttribute("default-sub-menu-include-scope").isEmpty()) {
            defaultSubMenuInstanceScope = menuElement.getAttribute("default-sub-menu-include-scope");
        }
        if (!menuElement.getAttribute("force-extends-sub-menu-model-scope").isEmpty()) {
            forceExtendsSubMenuModelScope = menuElement.getAttribute("force-extends-sub-menu-model-scope");
        }
        if (!menuElement.getAttribute("force-all-sub-menu-model-scope").isEmpty()) {
            forceAllSubMenuModelScope = menuElement.getAttribute("force-all-sub-menu-model-scope");
        }
        if (!menuElement.getAttribute("always-expand-selected-or-ancestor").isEmpty()) {
            alwaysExpandSelectedOrAncestor = "true".equals(menuElement.getAttribute("always-expand-selected-or-ancestor"));
        }
        separateMenuType = getExpander(menuElement, "separate-menu-type", separateMenuType);
        separateMenuTargetStyle = getExpander(menuElement, "separate-menu-target-style", separateMenuTargetStyle);
        separateMenuTargetPreference = getExpander(menuElement, "separate-menu-target-preference", separateMenuTargetPreference);
        if (separateMenuTargetPreference.isEmpty()) separateMenuTargetPreference = FlexibleStringExpander.getInstance("greatest-ancestor");
        separateMenuTargetOriginalAction = getExpander(menuElement, "separate-menu-target-original-action", separateMenuTargetOriginalAction);

        this.autoSubMenuNames = autoSubMenuNames;
        this.defaultSubMenuModelScope = defaultSubMenuModelScope;
        this.defaultSubMenuInstanceScope = defaultSubMenuInstanceScope;
        this.forceExtendsSubMenuModelScope = forceExtendsSubMenuModelScope;
        this.forceAllSubMenuModelScope = forceAllSubMenuModelScope;

        this.defaultAlign = defaultAlign;
        this.defaultAlignStyle = defaultAlignStyle;
        this.defaultAssociatedContentId = defaultAssociatedContentId;
        this.defaultCellWidth = defaultCellWidth;
        this.defaultDisabledTitleStyle = defaultDisabledTitleStyle;
        this.defaultEntityName = defaultEntityName;
        this.defaultHideIfSelected = defaultHideIfSelected;
        this.defaultMenuItemName = defaultMenuItemName;
        this.defaultPermissionEntityAction = defaultPermissionEntityAction;
        this.defaultPermissionOperation = defaultPermissionOperation;
        this.defaultSelectedStyle = defaultSelectedStyle;
        this.defaultSelectedAncestorStyle = defaultSelectedAncestorStyle; // SCIPIO
        this.defaultTitleStyle = defaultTitleStyle;
        this.defaultTooltipStyle = defaultTooltipStyle;
        this.defaultWidgetStyle = defaultWidgetStyle;
        this.defaultLinkStyle = defaultLinkStyle; // SCIPIO
        this.extraIndex = extraIndex;
        this.fillStyle = fillStyle;
        this.id = id;
        this.menuContainerStyleExdr = menuContainerStyleExdr;

        this.menuWidth = menuWidth;
        this.orientation = orientation;
        this.parentMenu = parent;
        this.selectedMenuItemContextFieldName = Collections.unmodifiableList(selectedMenuItemContextFieldName); // SCIPIO
        this.selectedMenuItemContextFieldNameStr = selectedMenuItemContextFieldNameStr; // SCIPIO
        this.selectedMenuContextFieldName = selectedMenuContextFieldName; // SCIPIO
        this.target = target;
        this.title = title;
        this.titleStyle = titleStyle; // SCIPIO
        this.tooltip = tooltip;
        this.type = type;

        // SCIPIO: (other) new fields and new processing code
        this.itemsSortMode = itemsSortMode;
        this.alwaysExpandSelectedOrAncestor = alwaysExpandSelectedOrAncestor;

        this.separateMenuType = separateMenuType;
        this.separateMenuTargetStyle = separateMenuTargetStyle;
        this.separateMenuTargetPreference = separateMenuTargetPreference;
        this.separateMenuTargetOriginalAction = separateMenuTargetOriginalAction;

        CurrentMenuDefBuildArgs currentMenuDefBuildArgs = new CurrentMenuDefBuildArgs(this);
        Map<String, ModelMenuItemAlias> menuItemAliasMap = new HashMap<>();

        // SCIPIO: this is delayed so the cloning can read some fields from this model instance
        if (parent != null) {
            if (parent.actions != null) {
                actions.addAll(parent.actions);
            }
            if (parent.menuItemAliasMap != null) {
                menuItemAliasMap.putAll(parent.menuItemAliasMap);
            }

            // SCIPIO: we must CLONE the parent's items with updated backreferences
            //menuItemList.addAll(parent.menuItemList);
            //menuItemMap.putAll(parent.menuItemMap);
            String extendedForceSubMenuModelScope = null;
            if (UtilValidate.isNotEmpty(this.forceAllSubMenuModelScope)) {
                extendedForceSubMenuModelScope = this.forceAllSubMenuModelScope;
            } else if (UtilValidate.isNotEmpty(this.forceExtendsSubMenuModelScope)) {
                extendedForceSubMenuModelScope = this.forceExtendsSubMenuModelScope;
            }
            ModelMenuItem.BuildArgs itemBuildArgs = new ModelMenuItem.BuildArgs(genBuildArgs, currentMenuDefBuildArgs,
                    menuLocation, extendedForceSubMenuModelScope);

            ModelMenuItem.cloneModelMenuItems(parent.menuItemList,
                    menuItemList, menuItemMap, this, null, itemBuildArgs);
        }

        // SCIPIO: include-actions and actions
        processIncludeActions(menuElement, null, null, actions, menuLocation, true, currentMenuDefBuildArgs, genBuildArgs);

        actions.trimToSize();
        this.actions = Collections.unmodifiableList(actions);

        // SCIPIO: include-menu-items and menu-item
        processIncludeMenuItems(menuElement, null, null, menuItemList, menuItemMap, menuItemAliasMap, true,
                menuLocation, true, null, null, this.forceAllSubMenuModelScope, null,
                currentMenuDefBuildArgs, genBuildArgs);

        menuItemList.trimToSize();
        this.menuItemList = Collections.unmodifiableList(menuItemList);
        this.menuItemMap = Collections.unmodifiableMap(menuItemMap);
        // SCIPIO: This MUST be set early so the menu item constructor can get the location!
        //this.menuLocation = menuLocation;

        // SCIPIO: at the end, build the map/index of sub-menus
        Map<String, ModelSubMenu> subMenuMap = new HashMap<>();
        addAllSubMenus(subMenuMap, menuItemList);
        this.subMenuMap = Collections.unmodifiableMap(subMenuMap);

        this.menuItemAliasMap = Collections.unmodifiableMap(menuItemAliasMap);
        this.menuItemNameAliasMap = makeMenuItemNameAliasMap(menuItemAliasMap);

        // SCIPIO: cache refs to all the manually-flagged items so don't have to at runtime
        ArrayList<ModelMenuNode> manualSelectedItems = new ArrayList<>();
        ArrayList<ModelMenuNode> manualExpandedItems = new ArrayList<>();
        findManualStateFlaggedItems(menuItemList, manualSelectedItems, manualExpandedItems);
        manualSelectedItems.trimToSize();
        manualExpandedItems.trimToSize();
        this.manualSelectedNodes = Collections.unmodifiableList(manualSelectedItems);
        this.manualExpandedNodes = Collections.unmodifiableList(manualExpandedItems);
    }

    private static FlexibleStringExpander getExpander(Element element, String name, FlexibleStringExpander existing) { // SCIPIO: helper
        if (!element.getAttribute(name).isEmpty())
            return FlexibleStringExpander.getInstance(element.getAttribute(name));
        return existing;
    }

    /**
     * SCIPIO: Menu loading factored out of main constructor and modified for reuse.
     * <p>
     * NOTE: 2016-09-30: the menuLocation argument was removed and replaced with more physical check
     * using the anyMenuElement (via WidgetDocumentInfo).
     */
    static ModelMenu getMenuDefinition(String resource, String name, Element anyMenuElement, GeneralBuildArgs genBuildArgs) {
        ModelMenu modelMenu = null;

        // SCIPIO: 2016-09-30: OVERRIDE this completely now
        String menuLocation = WidgetDocumentInfo.retrieveAlways(anyMenuElement).getResourceLocation();
        if (UtilValidate.isEmpty(menuLocation)) {
            // important to know when this fails now
            throw new IllegalStateException("Unable to get menu widget file original location. Error in code somewhere...");
        }

        final String fullLoc;
        if (resource != null && !resource.isEmpty()) {
            fullLoc = resource + "#" + name;
        } else {
            fullLoc = menuLocation + "#" + name;
        }

        // SCIPIO: added a local cache (on top of the external cache from getMenuFromLocation)
        // because we now very heavily reload local menus
        modelMenu = genBuildArgs.localModelMenuCache.get(fullLoc);
        if (modelMenu == null) {
            // SCIPIO: Added a superficial check for same-location to prevent some endless loops.
            // WARN: this is only a superficial check to prevent endless loops while refactoring menus.
            // it does not resolve to the actual file, but since almost everything is a component://
            // we're probably fine.
            if (resource != null && !resource.isEmpty() && !(menuLocation.equals(resource))) {
                try {
                    modelMenu = MenuFactory.getMenuFromLocation(resource, name);
                } catch (Exception e) {
                    Debug.logError(e, "Failed to load menu definition '" + name + "' at resource '" + resource
                            + "'", module);
                }
            } else {
                resource = menuLocation;

                // try to find a menu definition in the same file
                Element rootElement = anyMenuElement.getOwnerDocument().getDocumentElement();
                List<? extends Element> menuElements = UtilXml.childElementList(rootElement, "menu");
                for (Element menuElementEntry : menuElements) {
                    if (menuElementEntry.getAttribute("name").equals(name)) {
                        modelMenu = new ModelMenu(menuElementEntry, resource);
                        break;
                    }
                }
                if (modelMenu == null) {
                    Debug.logError("Failed to find menu definition '" + name + "' in same document.", module);
                } else {
                    genBuildArgs.localModelMenuCache.put(fullLoc, modelMenu);
                }
            }
            if (modelMenu != null) {
                genBuildArgs.localModelMenuCache.put(fullLoc, modelMenu);
            }
        }
        return modelMenu;
    }

    /**
     * SCIPIO: implements include-actions and actions reading (moved here).
     * Also does include-elements.
     * <p>
     * FIXME: this method interface and its arguments are a mess.
     */
    void processIncludeActions(Element parentElement, List<? extends Element> preInclElements, List<? extends Element> postInclElements, List<ModelAction> actions,
            String currResource, boolean processIncludes, CurrentMenuDefBuildArgs currentMenuDefBuildArgs, GeneralBuildArgs genBuildArgs) {
        // don't think any problems from local cache for actions
        final boolean useCache = true;
        final boolean cacheConsume = false;

        if (processIncludes) {
            List<Element> actionInclElements = new ArrayList<>();
            if (preInclElements != null) {
                actionInclElements.addAll(preInclElements);
            }
            actionInclElements.addAll(UtilXml.childElementList(parentElement, "include-elements"));
            actionInclElements.addAll(UtilXml.childElementList(parentElement, "include-actions"));
            if (postInclElements != null) {
                actionInclElements.addAll(postInclElements);
            }
            for (Element actionInclElement : getMergedIncludeDirectives(actionInclElements, currResource)) {
                String inclMenuName = actionInclElement.getAttribute("menu-name");
                String inclResource = actionInclElement.getAttribute("resource");
                String inclRecursive = actionInclElement.getAttribute("recursive");
                if (inclRecursive.isEmpty()) {
                    inclRecursive = "full";
                }
                String nextResource = UtilValidate.isNotEmpty(inclResource) ? inclResource : currResource;

                if ("no".equals(inclRecursive) || "includes-only".equals(inclRecursive) ||
                    "extends-only".equals(inclRecursive) || "full".equals(inclRecursive)) {
                    Element includedMenuElem = loadIncludedMenu(inclMenuName, inclResource,
                            parentElement, currResource, genBuildArgs.menuElemCache, useCache, cacheConsume);

                    // WARN: we're forced to load this menu model even though we were support to avoid it
                    // because we need some resolved attributes off it
                    ModelMenu includedMenuModel = getMenuDefinition(inclResource, inclMenuName, parentElement, genBuildArgs); // currResource
                    CurrentMenuDefBuildArgs includedNextCurrentMenuDefBuildArgs = new CurrentMenuDefBuildArgs(includedMenuModel != null ? includedMenuModel : this);

                    if (includedMenuElem != null) {
                        if ("extends-only".equals(inclRecursive) || "full".equals(inclRecursive)) {
                            String extendedResource = includedMenuElem.getAttribute("extends-resource");
                            String extendedMenuName = includedMenuElem.getAttribute("extends");
                            String extendedNextResource = UtilValidate.isNotEmpty(extendedResource) ? extendedResource : nextResource;

                            if (UtilValidate.isNotEmpty(extendedMenuName)) {
                                Element extendedMenuElem = loadIncludedMenu(extendedMenuName, extendedResource,
                                        includedMenuElem, nextResource, genBuildArgs.menuElemCache, useCache, cacheConsume);
                                if (extendedMenuElem != null) {

                                    ModelMenu extendedMenuModel = getMenuDefinition(extendedResource, extendedMenuName, includedMenuElem, genBuildArgs); // nextResource
                                    CurrentMenuDefBuildArgs extendedNextCurrentMenuDefBuildArgs = new CurrentMenuDefBuildArgs(extendedMenuModel != null ? extendedMenuModel : this);

                                    processIncludeActions(extendedMenuElem, null, null, actions,
                                            extendedNextResource, true,
                                            extendedNextCurrentMenuDefBuildArgs, genBuildArgs);
                                } else {
                                    Debug.logError("Failed to find (via include-actions or include-elements) parent menu definition '" +
                                            extendedMenuName + "' in resource '" + extendedNextResource + "'", module);
                                }
                            }
                        }

                        if ("includes-only".equals(inclRecursive) || "full".equals(inclRecursive)) {
                            processIncludeActions(includedMenuElem, null, null, actions,
                                    nextResource, true,
                                    includedNextCurrentMenuDefBuildArgs, genBuildArgs);
                        } else {
                            processIncludeActions(includedMenuElem, null, null, actions,
                                    nextResource, false,
                                    includedNextCurrentMenuDefBuildArgs, genBuildArgs);
                        }
                    } else {
                        Debug.logError("Failed to find include-actions or include-elements menu definition '" +
                                inclMenuName + "' in resource '" + nextResource + "'", module);
                    }
                } else {
                    Debug.logError("Unrecognized include-actions or include-elements recursive mode: " + inclRecursive, module);
                }
            }
        }

        // read all actions under the "actions" element
        Element actionsElement = UtilXml.firstChildElement(parentElement, "actions");
        if (actionsElement != null) {
            actions.addAll(ModelMenuAction.readSubActions(this, actionsElement));
        }
    }

    /**
     * SCIPIO: implements include-menu-items and menu-item reading (moved here).
     * Also does include-elements.
     * <p>
     * FIXME: this method interface and its arguments are a mess.
     */
    void processIncludeMenuItems(Element parentElement, List<? extends Element> preInclElements, List<? extends Element> postInclElements, List<ModelMenuItem> menuItemList,
            Map<String, ModelMenuItem> menuItemMap, Map<String, ModelMenuItemAlias> menuItemAliasMap, boolean includeMenuItemAliases, String currResource,
            boolean processIncludes, Set<String> excludeItems, String subMenusFilter, String forceSubMenuModelScope,
            ModelSubMenu parentSubMenu, CurrentMenuDefBuildArgs currentMenuDefBuildArgs, GeneralBuildArgs genBuildArgs) {
        // WARN: even local cache not fully used (cacheConsume=true so only uses cached from prev actions includes)
        // to be safe because known that menu-item Elements get written to in some places and
        // reuse _might_ affect results in complex includes (?).
        // final menus are cached anyway.
        final boolean useCache = true;
        final boolean cacheConsume = true;

        if (excludeItems == null) {
            excludeItems = new HashSet<>();
        }

        if (processIncludes) {
            List<Element> itemInclElements = new ArrayList<>();
            if (preInclElements != null) {
                itemInclElements.addAll(preInclElements);
            }
            itemInclElements.addAll(UtilXml.childElementList(parentElement, "include-elements"));
            itemInclElements.addAll(UtilXml.childElementList(parentElement, "include-menu-items"));
            if (postInclElements != null) {
                itemInclElements.addAll(postInclElements);
            }
            for (Element itemInclElement : getMergedIncludeDirectives(itemInclElements, currResource)) {
                String inclMenuName = itemInclElement.getAttribute("menu-name");
                String inclResource = itemInclElement.getAttribute("resource");
                String inclRecursive = itemInclElement.getAttribute("recursive");
                String inclForceSubMenuModelScope = itemInclElement.getAttribute("force-sub-menu-model-scope");
                if (inclRecursive.isEmpty()) {
                    inclRecursive = "full";
                }
                String inclSubMenus = itemInclElement.getAttribute("sub-menus");
                String nextSubMenusFilter;
                if ("none".equals(subMenusFilter) || "none".equals(inclSubMenus)) {
                    // "none" overrides all
                    nextSubMenusFilter = "none";
                } else {
                    nextSubMenusFilter = inclSubMenus;
                }
                boolean nextIncludeMenuItemAliases = UtilMisc.booleanValue(itemInclElement.getAttribute("include-menu-item-aliases"), true);

                // NOTE: this method implements the force-xxx-sub-menu-model-scope
                // propagation logic in general
                if (forceSubMenuModelScope == null || forceSubMenuModelScope.isEmpty()) {
                    forceSubMenuModelScope = inclForceSubMenuModelScope;
                }

                Set<String> inclExcludeItems = new HashSet<>();
                List<? extends Element> skipItemElems = UtilXml.childElementList(itemInclElement, "exclude-item");
                for (Element skipItemElem : skipItemElems) {
                    String itemName = skipItemElem.getAttribute("name");
                    if (UtilValidate.isNotEmpty(itemName)) {
                        inclExcludeItems.add(itemName);
                    }
                }

                String nextResource = UtilValidate.isNotEmpty(inclResource) ? inclResource : currResource;

                if ("no".equals(inclRecursive) || "includes-only".equals(inclRecursive) ||
                    "extends-only".equals(inclRecursive) || "full".equals(inclRecursive)) {
                    Element includedMenuElem = loadIncludedMenu(inclMenuName, inclResource,
                            parentElement, currResource, genBuildArgs.menuElemCache, useCache, cacheConsume);

                    if (includedMenuElem != null) {
                        inclExcludeItems.addAll(excludeItems);

                        // WARN: we're forced to load this menu model even though we were support to avoid it
                        // because we need some resolved attributes off it
                        // NOTE: this is not meant to be used for any backreferences; we want them all to point to 'this' menu
                        ModelMenu includedMenuModel = getMenuDefinition(inclResource, inclMenuName, parentElement, genBuildArgs); // currResource
                        CurrentMenuDefBuildArgs includedNextCurrentMenuDefBuildArgs = new CurrentMenuDefBuildArgs(includedMenuModel != null ? includedMenuModel : this);

                        String includedForceSubMenuModelScope = forceSubMenuModelScope;
                        if (UtilValidate.isEmpty(includedForceSubMenuModelScope)) {
                            includedForceSubMenuModelScope = includedMenuModel.forceAllSubMenuModelScope;
                        }

                        if ("extends-only".equals(inclRecursive) || "full".equals(inclRecursive)) {
                            String extendedResource = includedMenuElem.getAttribute("extends-resource");
                            String extendedMenuName = includedMenuElem.getAttribute("extends");
                            String extendedNextResource = UtilValidate.isNotEmpty(extendedResource) ? extendedResource : nextResource;
                            if (UtilValidate.isNotEmpty(extendedMenuName)) {

                                Element extendedMenuElem = loadIncludedMenu(extendedMenuName, extendedResource,
                                        includedMenuElem, nextResource, genBuildArgs.menuElemCache, useCache, cacheConsume);

                                ModelMenu extendedMenuModel = getMenuDefinition(extendedResource, extendedMenuName, includedMenuElem, genBuildArgs); // nextResource
                                CurrentMenuDefBuildArgs extendedNextCurrentMenuDefBuildArgs = new CurrentMenuDefBuildArgs(extendedMenuModel != null ? extendedMenuModel : this);

                                String extendedForceSubMenuModelScope = includedForceSubMenuModelScope;
                                if (UtilValidate.isEmpty(extendedForceSubMenuModelScope)) {
                                    extendedForceSubMenuModelScope = includedMenuModel.forceExtendsSubMenuModelScope;
                                    if (UtilValidate.isEmpty(extendedForceSubMenuModelScope)) {
                                        extendedForceSubMenuModelScope = extendedMenuModel.forceAllSubMenuModelScope;
                                    }
                                }

                                if (extendedMenuElem != null) {
                                    processIncludeMenuItems(extendedMenuElem, null, null, menuItemList, menuItemMap, menuItemAliasMap, nextIncludeMenuItemAliases,
                                            extendedNextResource, true, inclExcludeItems, nextSubMenusFilter, extendedForceSubMenuModelScope, parentSubMenu,
                                            extendedNextCurrentMenuDefBuildArgs, genBuildArgs);
                                } else {
                                    Debug.logError("Failed to find (via include-menu-items or include-elements) parent menu definition '" +
                                            extendedMenuName + "' in resource '" + extendedNextResource + "'", module);
                                }
                            }
                        }


                        if ("includes-only".equals(inclRecursive) || "full".equals(inclRecursive)) {
                            processIncludeMenuItems(includedMenuElem, null, null, menuItemList, menuItemMap, menuItemAliasMap, nextIncludeMenuItemAliases,
                                    nextResource, true, inclExcludeItems, nextSubMenusFilter, includedForceSubMenuModelScope, parentSubMenu,
                                    includedNextCurrentMenuDefBuildArgs, genBuildArgs);
                        } else {
                            processIncludeMenuItems(includedMenuElem, null, null, menuItemList, menuItemMap, menuItemAliasMap, nextIncludeMenuItemAliases,
                                    nextResource, false, inclExcludeItems, nextSubMenusFilter, includedForceSubMenuModelScope, parentSubMenu,
                                    includedNextCurrentMenuDefBuildArgs, genBuildArgs);
                        }
                    } else {
                        Debug.logError("Failed to find include-menu-items or include-elements menu definition '" + inclMenuName + "' in resource '" + nextResource + "'", module);
                    }
                } else {
                    Debug.logError("Unrecognized include-menu-items or include-elements recursive mode: " + inclRecursive, module);
                }
            }
        }

        // SCIPIO: NOTE: the first (non-recursive) call to this method actually sets omitSubMenus=false for itemBuildArgs.
        // that's why we can set overrideItemBuildArgs = itemBuildArgs.
        // addUpdateMenuItem is called with omitSubMenus=false, but there will be no submenus on existingMenuItem anyway
        // because the previous recursive calls already built existingMenuItem with omitSubMenus=true, and everything
        // else below with omitSubMenus=true as well. so it automagically works out.
        ModelMenuItem.BuildArgs itemBuildArgs = new ModelMenuItem.BuildArgs(genBuildArgs, currentMenuDefBuildArgs, currResource, forceSubMenuModelScope);
        itemBuildArgs.omitSubMenus = ("none".equals(subMenusFilter));
        ModelMenuItem.BuildArgs overrideItemBuildArgs = itemBuildArgs;

        if (includeMenuItemAliases) {
            List<? extends Element> aliasElements = UtilXml.childElementList(parentElement, "menu-item-alias");
            for(Element aliasElement : aliasElements) {
                // TODO: review: can't remember if this should be itemBuildArgs or something else... makes no difference yet
                ModelMenuItemAlias aliasModel = new ModelMenuItemAlias(aliasElement, this, parentSubMenu, itemBuildArgs);
                menuItemAliasMap.put(aliasModel.getName(), aliasModel);
            }
        }

        List<? extends Element> itemElements = UtilXml.childElementList(parentElement, "menu-item");
        for (Element itemElement : itemElements) {
            String itemName = itemElement.getAttribute("name");
            if (!excludeItems.contains(itemName)) {
                ModelMenuItem modelMenuItem;
                if (parentSubMenu != null) {
                    modelMenuItem = new ModelMenuItem(itemElement, parentSubMenu, overrideItemBuildArgs);
                } else {
                    modelMenuItem = new ModelMenuItem(itemElement, this, overrideItemBuildArgs);
                }
                addUpdateMenuItem(modelMenuItem, menuItemList, menuItemMap, itemBuildArgs);
            }
        }
    }

    String getAutoSubMenuNames(Element menuElement) {
        return menuElement.getAttribute("auto-sub-menu-names");
    }

    private Collection<Element> getMergedIncludeDirectives(Collection<Element> includeElems, String menuLocation) {
        if (includeElems.size() <= 0) {
            return includeElems;
        }

        // must preserve order
        Map<String, Element> dirMap = new LinkedHashMap<>();
        for(Element inclElem : includeElems) {
            String elemKey;

            String inclRef = inclElem.getAttribute("menu-ref");
            String inclMenuName = inclElem.getAttribute("menu-name");
            String inclResource = inclElem.getAttribute("resource");

            if (!inclRef.isEmpty()) {
                if ("sub-menu-model".equals(inclRef)) {
                    elemKey = "#sub-menu-model#";
                } else {
                    Debug.logError("Invalid ref value on include directive (" + menuLocation + "#" + getName() + ")", module);
                    continue;
                }
            } else {

                if (UtilValidate.isEmpty(inclResource) && !inclMenuName.startsWith("#")) {
                    inclResource = menuLocation;
                }
                elemKey = inclResource + "#" + inclMenuName;

                if (inclMenuName.isEmpty()) {
                    Debug.logError("Missing menu-name or ref attribute on include directive (" + menuLocation + "#" + getName() + ")", module);
                    continue;
                }
            }

            // here, we only want to keep the LAST include directive, so later ones override previous,
            // so must remove first
            if (dirMap.containsKey(elemKey)) {
                dirMap.remove(elemKey);
            }
            dirMap.put(elemKey, inclElem);
        }

        // 2016-08-25: SPECIAL CASE: if there's an entry with menu-name "sub-menu-model",
        // it's a reference to another entry. we must remove that entry, re-insert it where
        // we are and modify it.
        Element subMenuRefElem = dirMap.get("#sub-menu-model#");
        if (subMenuRefElem != null) {
            String subMenuModelKey = null;
            Element subMenuModelElem = null;
            for(Map.Entry<String, Element> entry : dirMap.entrySet()) {
                Element elem = entry.getValue();
                if ("true".equals(elem.getAttribute("is-sub-menu-model-entry"))) {
                    subMenuModelKey = entry.getKey();
                    subMenuModelElem = elem;
                }
            }
            if (subMenuModelElem == null) {
                Debug.logError("include directive has menu-ref 'sub-menu-model' to reference "
                        + "a parent element sub-menu-model, but no such model was defined on parent (" + menuLocation + "#" + getName() + ")",
                        module);
                dirMap.remove("#sub-menu-model#"); // can't substitute, so kill it
            } else {
                // remove the original entry
                dirMap.remove(subMenuModelKey);

                // Clone the ref entry so we can modify it
                Element newElem = (Element) subMenuRefElem.cloneNode(true);
                // transfer the attribs not overridden
                copyAllElemAttribsNotSet(subMenuModelElem, newElem, UtilMisc.toSet("menu-ref"));
                dirMap.put("#sub-menu-model#", newElem);
            }
        }

        return dirMap.values();
    }

    static void copyAllElemAttribsNotSet(Element srcElem, Element destElem, Collection<String> excludeAttribs) {
        NamedNodeMap attribs = srcElem.getAttributes();
        for(int i = 0; i < attribs.getLength(); i++) {
            String attrName = attribs.item(i).getNodeName();
            if (!excludeAttribs.contains(attrName)) {
                if (destElem.getAttribute(attrName).isEmpty() && !srcElem.getAttribute(attrName).isEmpty()) {
                    destElem.setAttribute(attrName, srcElem.getAttribute(attrName));
                }
            }
        }
    }


    // SCIPIO: made accessible
    Element loadIncludedMenu(String menuName, String resource,
            Element currMenuElem, String currResource,
            Map<String, Element> menuElemCache, boolean useCache, boolean cacheConsume) {
        Element inclMenuElem = null;
        Element inclRootElem = null;
        String targetResource;
        if (UtilValidate.isNotEmpty(resource)) {
            targetResource = resource;
        }
        else {
            targetResource = currResource;
        }

        String fullLocation = targetResource + "#" + menuName;
        if (useCache && menuElemCache.containsKey(fullLocation)) {
            inclMenuElem = menuElemCache.get(fullLocation);
            if (cacheConsume) {
                menuElemCache.remove(fullLocation);
            }
        }
        else {
            if (true) { // UtilValidate.isNotEmpty(resource)
                try {
                    URL menuFileUrl = FlexibleLocation.resolveLocation(targetResource);
                    Document menuFileDoc = UtilXml.readXmlDocument(menuFileUrl, true, true);
                    // SCIPIO: New: Save original location as user data in Document
                    if (menuFileDoc != null) {
                        WidgetDocumentInfo.retrieveAlways(menuFileDoc).setResourceLocation(targetResource);
                    }
                    inclRootElem = menuFileDoc.getDocumentElement();
                } catch (Exception e) {
                    Debug.logError(e, "Failed to load include-menu-items resource: " + resource, module);
                }
            }
            //else {
                // SCIPIO: No! we must reload the orig doc always because the Elements get written to!
                // must have fresh versions.
                // try to find a menu definition in the same file
                //inclRootElem = currMenuElem.getOwnerDocument().getDocumentElement();
            //}

            if (inclRootElem != null) {
                List<? extends Element> menuElements = UtilXml.childElementList(inclRootElem, "menu");
                for (Element menuElementEntry : menuElements) {
                    if (menuElementEntry.getAttribute("name").equals(menuName)) {
                        inclMenuElem = menuElementEntry;
                        break;
                    }
                }
            }
            if (useCache && !cacheConsume) {
                menuElemCache.put(fullLocation, inclMenuElem);
            }
        }
        return inclMenuElem;
    }

    @Override
    public void accept(ModelWidgetVisitor visitor) throws Exception {
        visitor.visit(this);
    }

    /**
     * add/override modelMenuItem using the menuItemList and menuItemMap
     * <p>
     * SCIPIO: made this static and accessible by ModelMenuItem.
     * <p>
     * NOTE: we assume the overriding modelMenuItem was initialized with the
     * proper backreferences already.
     */
    void addUpdateMenuItem(ModelMenuItem modelMenuItem, List<ModelMenuItem> menuItemList,
            Map<String, ModelMenuItem> menuItemMap, ModelMenuItem.BuildArgs buildArgs) {
        ModelMenuItem existingMenuItem = menuItemMap.get(modelMenuItem.getName());
        if (existingMenuItem != null) {
            // SCIPIO: support a replace mode as well
            ModelMenuItem mergedMenuItem;
            if ("replace".equals(modelMenuItem.getOverrideMode())) {
                mergedMenuItem = modelMenuItem;
                int existingItemIndex = menuItemList.indexOf(existingMenuItem);
                menuItemList.set(existingItemIndex, mergedMenuItem);
            } else if ("remove-replace".equals(modelMenuItem.getOverrideMode())) {
                menuItemList.remove(existingMenuItem);
                menuItemMap.remove(modelMenuItem.getName());
                mergedMenuItem = modelMenuItem;
                menuItemList.add(modelMenuItem);
            } else {
                // does exist, update the item by doing a merge/override
                mergedMenuItem = existingMenuItem.mergeOverrideModelMenuItem(modelMenuItem, buildArgs);
                int existingItemIndex = menuItemList.indexOf(existingMenuItem);
                menuItemList.set(existingItemIndex, mergedMenuItem);
            }
            menuItemMap.put(modelMenuItem.getName(), mergedMenuItem);
        } else {
            // does not exist, add to Map
            menuItemList.add(modelMenuItem);
            menuItemMap.put(modelMenuItem.getName(), modelMenuItem);
        }
    }

    // SCIPIO: find all sub-menus
    void addAllSubMenus(Map<String, ModelSubMenu> subMenuMap, List<ModelMenuItem> menuItemList) {
        for(ModelMenuItem menuItem : menuItemList) {
            menuItem.addAllSubMenus(subMenuMap);
        }
        if (subMenuMap.containsKey(this.getName())) {
            Debug.logError("Menu " + this.getName() + " contains a sub-menu with same name as the top-level menu; "
                    + "invalid and will be ignored in unique sub-menu lookups", module);
            subMenuMap.remove(this.getName());
        }
        if (subMenuMap.containsKey(ModelMenu.TOP_MENU_NAME)) {
            Debug.logError("Menu " + this.getName() + " contains a sub-menu having the special reserved value '" +
                    ModelMenu.TOP_MENU_NAME + "' as name; invalid and will be ignored in unique sub-menu lookups", module);
            subMenuMap.remove(TOP_MENU_NAME);
        }
        if (subMenuMap.containsKey(null)) {
            Debug.logError("Menu " + this.getName() + " contains a null key; should not happen", module);
            subMenuMap.remove(null);
        }
        if (subMenuMap.containsKey("")) {
            Debug.logError("Menu " + this.getName() + " contains an empty key; should not happen", module);
            subMenuMap.remove("");
        }
    }

    /**
     * SCIPIO: Cache refs to all the manually-flagged items so don't have to at runtime.
     */
    void findManualStateFlaggedItems(List<? extends ModelMenuNode> nodeList, List<ModelMenuNode> manualSelectedNodes, List<ModelMenuNode> manualExpandedNodes) {
        if (nodeList == null) {
            return;
        }
        for(ModelMenuNode node : nodeList) {
            // do in-depth first; lower levels last
            findManualStateFlaggedItems(node.getChildrenNodes(), manualSelectedNodes, manualExpandedNodes);
            // do us
            if (node.getSelected() != null && !node.getSelected().getOriginal().isEmpty()) {
                manualSelectedNodes.add(node);
            }
            if (node.getExpanded() != null && !node.getExpanded().getOriginal().isEmpty()) {
                manualExpandedNodes.add(node);
            }
        }
    }

    public List<ModelAction> getActions() {
        return actions;
    }

    @Override
    public String getBoundaryCommentName() {
        return menuLocation + "#" + getName();
    }

    public String getCurrentMenuName(Map<String, Object> context) {
        return getName();
    }

    public String getDefaultAlign() {
        return this.defaultAlign;
    }

    public String getDefaultAlignStyle() {
        return this.defaultAlignStyle;
    }

    public FlexibleStringExpander getDefaultAssociatedContentId() {
        return defaultAssociatedContentId;
    }

    public String getDefaultAssociatedContentId(Map<String, Object> context) {
        return defaultAssociatedContentId.expandString(context);
    }

    public String getDefaultCellWidth() {
        return this.defaultCellWidth;
    }

    public String getDefaultDisabledTitleStyle() {
        return this.defaultDisabledTitleStyle;
    }

    public String getDefaultEntityName() {
        return this.defaultEntityName;
    }

    public Boolean getDefaultHideIfSelected() {
        return this.defaultHideIfSelected;
    }

    public String getDefaultMenuItemName() {
        return this.defaultMenuItemName;
    }

    /**
     * SCIPIO: Returns default item name from submenu or from this menu if submenu is null,
     * or null if not set anywhere.
     */
    public String getDefaultMenuItemName(ModelSubMenu subMenu) {
        if (UtilValidate.isNotEmpty(subMenu.getDefaultMenuItemName())) {
            return subMenu.getDefaultMenuItemName();
        }
        return this.getDefaultMenuItemName();
    }

    public String getDefaultPermissionEntityAction() {
        return this.defaultPermissionEntityAction;
    }

    public String getDefaultPermissionOperation() {
        return this.defaultPermissionOperation;
    }

    public String getDefaultSelectedStyle() {
        return this.defaultSelectedStyle;
    }

    public String getDefaultSelectedAncestorStyle() { // SCIPIO
        return this.defaultSelectedAncestorStyle;
    }

    public String getDefaultTitleStyle() {
        return this.defaultTitleStyle;
    }

    public String getDefaultTooltipStyle() {
        return this.defaultTooltipStyle;
    }

    public String getDefaultWidgetStyle() {
        return this.defaultWidgetStyle;
    }

    public String getDefaultLinkStyle() { // SCIPIO
        return this.defaultLinkStyle;
    }

    public FlexibleStringExpander getExtraIndex() {
        return extraIndex;
    }

    public String getExtraIndex(Map<String, Object> context) {
        try {
            return extraIndex.expandString(context);
        } catch (Exception ex) {
            return "";
        }
    }

    public String getFillStyle() {
        return this.fillStyle;
    }

    public String getId() {
        return this.id;
    }

    public String getMenuContainerStyle(Map<String, Object> context) {
        return menuContainerStyleExdr.expandString(context);
    }

    public String getMenuContainerStyle() { // SCIPIO
        return menuContainerStyleExdr.getOriginal();
    }

    public FlexibleStringExpander getMenuContainerStyleExdr() {
        return menuContainerStyleExdr;
    }

    /**
     * SCIPIO: Builds a style string from current, parent, and default, based on "+"/"="
     * combination logic.
     * <p>
     * NOTE: subtle difference between null and empty string.
     * <p>
     * FIXME?: there is a loss of information here, because the +/= styles are always
     * removed in the final string. should preserve so macros can use?
     * <p>
     * FIXME: this is inefficient in cases where parent style does not need to be visited,
     * but can't change easily in java.
     */
    static String buildStyle(String style, String parentStyle, String defaultStyle) {
        String res;
        if (!style.isEmpty()) {
            // SCIPIO: support extending styles
            if (style.startsWith("+")) {
                String addStyles = style.substring(1);
                String inheritedStyles;
                if (parentStyle != null) {
                    inheritedStyles = parentStyle;
                } else {
                    inheritedStyles = defaultStyle;
                }
                if (inheritedStyles != null && !inheritedStyles.isEmpty()) {
                    if (!addStyles.isEmpty()) {
                        res = inheritedStyles + (addStyles.startsWith(" ") ? "" : " ") + addStyles;
                    } else {
                        res = inheritedStyles;
                    }
                } else {
                    // PRESERVE the original "+" so it may tricle down to FTL macros...
                    //res = addStyles;
                    res = style;
                }
            }
            else {
                // DON'T remove this anymore... let it trickle down to FTL macro for further interpretation
                //if (style.startsWith("=")) {
                //    style = style.substring(1);
                //}
                res = style;
            }
        } else if (parentStyle != null) {
            res = parentStyle;
        } else {
            res = defaultStyle;
        }
        if (res != null) {
            res = res.trim();
        }
        return res;
    }

    /**
     * SCIPIO: Combines an extra style (like selected-style) to a main style
     * string (like widget-style).
     * <p>
     * NOTE: currently, the extra style is always added as an extra, and
     * never replaces. The extra's prefix (+/=) is stripped.
     * <p>
     * ALWAYS USE THIS METHOD TO CONCATENATE EXTRA STYLES.
     */
    public static String combineExtraStyle(String style, String extraStyle) {
        String res;
        if (style == null) {
            style = "";
        } else {
            style = style.trim();
        }
        if (extraStyle == null) {
            extraStyle = "";
        } else {
            extraStyle = extraStyle.trim();
        }

        if (style.isEmpty()) {
            // In this case, prefix the result with "+" to be sure we don't
            // turn the string into a "replacing" string
            if (extraStyle.startsWith("=") || extraStyle.startsWith("+")) {
                res = "+" + extraStyle.substring(1);
            } else {
                res = "+" + extraStyle;
            }
        } else {
            // Here, resulting string prefix is left to the input style
            if (extraStyle.startsWith("=") || extraStyle.startsWith("+")) {
                res = style + " " + extraStyle.substring(1);
            } else {
                res = style + " " + extraStyle;
            }
        }

        return res;
    }

    public List<ModelMenuItem> getMenuItemList() {
        return menuItemList;
    }

    public List<ModelMenuItem> getOrderedMenuItemList(final Map<String, Object> context) {
        return getOrderedMenuItemList(context, getItemsSortMode(), getMenuItemList());
    }

    protected static List<ModelMenuItem> getOrderedMenuItemList(final Map<String, Object> context,
            String itemsSortMode, List<ModelMenuItem> menuItemList) {
        if (itemsSortMode != null && !"off".equals(itemsSortMode)) {
            boolean ignoreCase = itemsSortMode.endsWith("-ignorecase");
            if (ignoreCase) {
                itemsSortMode = itemsSortMode.substring(0, itemsSortMode.length() - "-ignorecase".length());
            }

            // remove non-sortables
            List<ModelMenuItem> sorted = new ArrayList<>(menuItemList.size());
            for(ModelMenuItem item : menuItemList) {
                if (!"off".equals(item.getSortMode())) {
                    sorted.add(item);
                }
            }

            Comparator<ModelMenuItem> cmp = null;
            if ("name".equals(itemsSortMode)) {
                if (ignoreCase) {
                    cmp = new Comparator<ModelMenuItem>() {
                        @Override
                        public int compare(ModelMenuItem o1, ModelMenuItem o2) {
                            return o1.getName().compareToIgnoreCase(o2.getName());
                        }
                    };
                }
                else {
                    cmp = new Comparator<ModelMenuItem>() {
                        @Override
                        public int compare(ModelMenuItem o1, ModelMenuItem o2) {
                            return o1.getName().compareTo(o2.getName());
                        }
                    };
                }
            } else if ("title".equals(itemsSortMode)) {
                if (ignoreCase) {
                    cmp = new Comparator<ModelMenuItem>() {
                        @Override
                        public int compare(ModelMenuItem o1, ModelMenuItem o2) {
                            return o1.getTitle(context).compareToIgnoreCase(o2.getTitle(context));
                        }
                    };
                } else {
                    cmp = new Comparator<ModelMenuItem>() {
                        @Override
                        public int compare(ModelMenuItem o1, ModelMenuItem o2) {
                            return o1.getTitle(context).compareTo(o2.getTitle(context));
                        }
                    };
                }
            } else if ("linktext".equals(itemsSortMode)) {
                if (ignoreCase) {
                    cmp = new Comparator<ModelMenuItem>() {
                        @Override
                        public int compare(ModelMenuItem o1, ModelMenuItem o2) {
                            MenuLink l1 = o1.getLink();
                            MenuLink l2 = o2.getLink();
                            if (l1 != null) {
                                if (l2 != null) {
                                    return l1.getTextExdr().expandString(context).compareToIgnoreCase(l2.getTextExdr().expandString(context));
                                }
                                else {
                                    return 1;
                                }
                            } else {
                                if (l2 != null) {
                                    return -1;
                                } else {
                                    return 0;
                                }
                            }
                        }
                    };
                } else {
                    cmp = new Comparator<ModelMenuItem>() {
                        @Override
                        public int compare(ModelMenuItem o1, ModelMenuItem o2) {
                            MenuLink l1 = o1.getLink();
                            MenuLink l2 = o2.getLink();
                            if (l1 != null) {
                                if (l2 != null) {
                                    return l1.getTextExdr().expandString(context).compareTo(l2.getTextExdr().expandString(context));
                                } else {
                                    return 1;
                                }
                            } else {
                                if (l2 != null) {
                                    return -1;
                                } else {
                                    return 0;
                                }
                            }
                        }
                    };
                }
            } else if ("displaytext".equals(itemsSortMode)) {
                if (ignoreCase) {
                    cmp = new Comparator<ModelMenuItem>() {
                        @Override
                        public int compare(ModelMenuItem o1, ModelMenuItem o2) {
                            return o1.getDisplayText(context).compareToIgnoreCase(o2.getDisplayText(context));
                        }
                    };
                } else {
                    cmp = new Comparator<ModelMenuItem>() {
                        @Override
                        public int compare(ModelMenuItem o1, ModelMenuItem o2) {
                            return o1.getDisplayText(context).compareTo(o2.getDisplayText(context));
                        }
                    };
                }
            }
            if (cmp != null) {
                Collections.sort(sorted, cmp);
            }

            // reintegrate with the items that weren't supposed to be sorted; preserve their positions
            // and insert the sorted ones around them
            List<ModelMenuItem> finalList = new ArrayList<>(menuItemList.size());
            Iterator<ModelMenuItem> sortedIt = sorted.iterator();
            for(ModelMenuItem origItem : menuItemList) {
                if ("off".equals(origItem.getSortMode())) {
                    finalList.add(origItem);
                } else {
                    finalList.add(sortedIt.next());
                }
            }

            return finalList;
        }
        return menuItemList;
    }

    public Map<String, ModelMenuItem> getMenuItemMap() {
        return menuItemMap;
    }

    public String getMenuLocation() {
        return menuLocation;
    }

    public String getMenuWidth() {
        return this.menuWidth;
    }

    /**
     * Gets menu item by name.
     * @deprecated SCIPIO: use {@link #getMenuItemByName}
     */
    @Deprecated
    public ModelMenuItem getModelMenuItemByName(String name) {
        return getMenuItemByName(name);
    }

    /**
     * Gets menu item by name.
     * <p>
     * SCIPIO: NOTE: does NOT translate menu items. you might have to call
     * {@link #getMappedMenuItemName} or {@link #getMenuItemByNameMapped}.
     */
    public ModelMenuItem getMenuItemByName(String name) {
        return this.menuItemMap.get(name);
    }

    public ModelMenuItem getMenuItemByNameMapped(String name) {
        return this.menuItemMap.get(getMappedMenuItemName(name));
    }

    /**
     * SCIPIO: Gets a menu item by name, with extended lookup support.
     * @deprecated INCOMPLETE, do not use until reviewed in future, misses mapping logic
     * <p>
     * SCIPIO: 2016-08-30: this method is enhanced so that it can lookup menu items
     * in sub-menus, using a dot syntax:
     * topMenuItemName.subMenuItemName.subSubMenuItemName
     * In case a menu item has multiple sub-menus, they can be disambiguated by adding an
     * intermediate sub-menu name prefixed with ":":
     * topMenuItemName.subMenu1:subMenuItemName.subSubMenu2:subSubMenuItemName
     */
    @Deprecated
    public MenuAndItemLookup resolveMenuAndItemByTrail(String nameExpr) {
        return resolveMenuAndItemByTrail(splitMenuItemTrailExpr(nameExpr));
    }

    public static String[] splitMenuItemTrailExpr(String nameExpr) {
        return nameExpr.split("\\.");
    }

    @Deprecated
    public MenuAndItemLookup resolveMenuAndItemByTrail(String[] nameList) {
        return resolveMenuAndItemByTrail(nameList[0], nameList, 1);
    }

    @Deprecated
    public MenuAndItemLookup resolveMenuAndItemByTrail(String name, String[] nameList, int nextNameIndex) {
        ModelMenuItem currLevelItem = this.menuItemMap.get(name);
        if (nextNameIndex >= nameList.length) {
            return new MenuAndItemLookup(currLevelItem);
        } else if (currLevelItem != null) {
            return currLevelItem.resolveMenuAndItemByTrail(nameList[nextNameIndex], nameList, nextNameIndex + 1);
        } else {
            return null;
        }
    }

    /**
     * SCIPIO: Finds nested menu item using format:
     * subMenuName:menuItemName.
     */
    public MenuAndItemLookup resolveMenuAndItem(String nameExpr) {
        String[] parts = nameExpr.split(":");
        if (parts.length >= 2) {
            return resolveMenuAndItem(parts[1], parts[0]);
        } else {
            return resolveMenuAndItem(parts[0], (String) null);
        }
    }

    public boolean isMenuNameTopMenu(String menuName) {
        return (menuName == null || menuName.isEmpty() || TOP_MENU_NAME.equals(menuName) || menuName.equals(getName()));
    }

    public boolean isMenuNameSubMenu(String menuName) {
        return getSubMenuByName(menuName) != null;
    }

    public boolean isMenuNameWithinMenu(String menuName) {
        return isMenuNameTopMenu(menuName) || isMenuNameSubMenu(menuName);
    }

    /**
     * Gets menu item for sub-menu (or its parent if PARENT applies), applying all the name translations as needed.
     * <p>
     * @see #resolveMenuItem(String, ModelSubMenu, boolean, boolean)
     */
    public MenuAndItemLookup resolveMenuAndItem(String menuItemName, String subMenuName, boolean mapNames) {
        if (isMenuNameTopMenu(subMenuName)) {
            return resolveMenuAndItem(menuItemName, subMenuName, (ModelSubMenu) null, mapNames);
        } else {
            ModelSubMenu subMenu = getSubMenuByName(subMenuName);
            return (subMenu != null) ? resolveMenuAndItem(menuItemName, subMenuName, subMenu, mapNames) : MenuAndItemLookup.EMPTY;
        }
    }
    public MenuAndItemLookup resolveMenuAndItem(String menuItemName, String subMenuName) {
        return resolveMenuAndItem(menuItemName, subMenuName, true);
    }

    public MenuAndItemLookup resolveMenuAndItem(String menuItemName, ModelSubMenu subMenu, boolean mapNames) {
        return resolveMenuAndItem(menuItemName, subMenu != null ? subMenu.getName() : null, subMenu, mapNames);
    }

    /**
     * SCIPIO: Gets menu item for sub-menu (or its parent if PARENT applies).
     * If subMenu is null, assumes the item is meant for lookup in the top menu (this method should only
     * be called after the sub menu name was validated).
     * <p>
     * NOTE: This handles the special PARENT/PARENT-WITHSUB/PARENT-NOSUB values, and NONE handling.
     * <p>
     * NOTE: this overload assumes subMenu was already looked up and merely stores subMenuName in the lookup result.
     */
    public MenuAndItemLookup resolveMenuAndItem(String menuItemName, String subMenuName, ModelSubMenu subMenu, boolean mapNames) {
        if (subMenu == null) { // top menu
            String mappedMenuItemName = mapNames ? this.getMappedMenuItemName(menuItemName) : menuItemName;

            ModelMenuItem menuItem = getMenuItemByName(mappedMenuItemName);
            if (menuItem == null && UtilValidate.isNotEmpty(getDefaultMenuItemName())) {
                mappedMenuItemName = getDefaultMenuItemName();
                menuItem = getMenuItemByName(mappedMenuItemName);
            }

            return new MenuAndItemLookup(null, menuItem, subMenuName, mappedMenuItemName);
        } else {
            String mappedMenuItemName = mapNames ? subMenu.getMappedMenuItemName(menuItemName) : menuItemName;
            if (ModelMenuItem.parentMenuItemNames.contains(mappedMenuItemName)) {
                if (ModelMenuItem.PARENT_NOSUB_MENU_ITEM_NAME.equals(mappedMenuItemName)) {
                    // here we ditch our sub-menu
                    return new MenuAndItemLookup(subMenu.getParentMenuItem().getParentSubMenu(), subMenu.getParentMenuItem(), subMenuName, mappedMenuItemName);
                } else {
                    return new MenuAndItemLookup(subMenu, subMenu.getParentMenuItem(), subMenuName, mappedMenuItemName);
                }
            } else {
                ModelMenuItem menuItem = subMenu.getMenuItemByName(mappedMenuItemName);

                if (menuItem == null && UtilValidate.isNotEmpty(subMenu.getDefaultMenuItemName())) {
                    mappedMenuItemName = subMenu.getDefaultMenuItemName();
                    if (ModelMenuItem.parentMenuItemNames.contains(mappedMenuItemName)) {
                        if (ModelMenuItem.PARENT_NOSUB_MENU_ITEM_NAME.equals(mappedMenuItemName)) {
                            // here we ditch our sub-menu
                            return new MenuAndItemLookup(subMenu.getParentMenuItem().getParentSubMenu(), subMenu.getParentMenuItem(), subMenuName, mappedMenuItemName);
                        } else {
                            return new MenuAndItemLookup(subMenu, subMenu.getParentMenuItem(), subMenuName, mappedMenuItemName);
                        }
                    } else {
                        menuItem = subMenu.getMenuItemByName(mappedMenuItemName);
                    }
                }
                return new MenuAndItemLookup(subMenu, menuItem, subMenuName, mappedMenuItemName);
            }
        }
    }

    public MenuAndItemLookup resolveMenuAndItem(String menuItemName, ModelSubMenu subMenu) {
        return resolveMenuAndItem(menuItemName, subMenu, true);
    }

    /**
     * SCIPIO: If menuItemName is NONE or equivalent and subMenu is non-null, returns the parent item of the sub-menu,
     * as default selection handling for null; else returns the passed defaultMenuItem.
     * <p>
     * NOTE: Performs no name translations on its own.
     * <code></code>
     */
    public ModelMenuItem getMenuItemIfNone(String menuItemName, ModelSubMenu subMenu, ModelMenuItem defaultMenuItem) {
        if (ModelMenuItem.isNoneMenuItemName(menuItemName) && subMenu != null) {
            return subMenu.getParentMenuItem();
        } else {
            return defaultMenuItem;
        }
    }

    /**
     * SCIPIO: get the sub-menu by unique name.
     * <p>
     * DEV NOTE: this method is only valid for use post-construction of ModelMenu.
     */
    public ModelSubMenu getSubMenuByName(String name) {
        // OPTIMIZATION: check not needed because "TOP", null and empty return null here
        //if (isMenuNameTopMenu(name)) {
        //    return null;
        //}
        return this.subMenuMap.get(name);
    }

    public String getOrientation() {
        return this.orientation;
    }

    public ModelMenu getParentMenu() {
        return parentMenu;
    }

    // SCIPIO: NOTE: all the stock getSelectedMenu* methods have been modified.
    // NOTE: 2018-09-04: The context-less methods have now been modified so they return the FlexibleMapAccessor<String>
    // instead of the original string - this could break compatibility in some rare cases, but virtually
    // no code except for internal Scipio code would have been using these overloads with no context,
    // unless somebody decided to copy-paste the PrepareComplexMenu.groovy script (unlikely).

    public String getSelectedMenuItemContextFieldNameExprStr() { // SCIPIO
        return selectedMenuItemContextFieldNameStr;
    }

    public FlexibleMapAccessor<String> getSelectedMenuItemContextFieldNameFirst() { // SCIPIO
        if (this.selectedMenuItemContextFieldName != null && !this.selectedMenuItemContextFieldName.isEmpty()) {
            return this.selectedMenuItemContextFieldName.get(0);
        }
        return null;
    }

    public List<FlexibleMapAccessor<String>> getSelectedMenuItemContextFieldNames() { // SCIPIO
        return this.selectedMenuItemContextFieldName;
    }

    /**
     * @deprecated SCIPIO: 2018-09-04: use {@link #getSelectedMenuItemContextFieldNames()}, which
     * now returns a list of FlexibleMapAccessor.
     */
    @Deprecated
    public List<FlexibleMapAccessor<String>> getSelectedMenuItemContextFieldNamesExpr() { // SCIPIO
        return this.selectedMenuItemContextFieldName;
    }

    /**
     * Returns selected menu item context field name.
     * @deprecated SCIPIO: because this now supports multiple values and extended syntax,
     *   must use {@link #getSelectedMenuItemContextFieldNameExpr} or
     *   {@link #getSelectedMenuItemContextFieldNameFirst} to disambiguate.
     */
    @Deprecated
    public FlexibleMapAccessor<String> getSelectedMenuItemContextFieldName() {
        return getSelectedMenuItemContextFieldNameFirst();
    }

    /**
     * getSelectedMenuItemContextFieldName.
     * <p>
     * SCIPIO: WARN: This method has been modified; it is too limited so it no longer
     * handles the default-menu-item-name.
     * Use getSelectedMenuItem instead.
     */
    public String getSelectedMenuItemContextFieldName(Map<String, Object> context) {
        // SCIPIO: we support multiple lookups.
        //String menuItemName = this.selectedMenuItemContextFieldName.get(context);
        // SCIPIO: New code start...
        String menuItemName = null;
        String firstMenuItemName = null;
        for (FlexibleMapAccessor<String> fieldNameExpr : this.selectedMenuItemContextFieldName) {
            menuItemName = fieldNameExpr.get(context);
            // The menu item must not be empty AND it must exist in this menu to be accepted
            // But record firstMenuItemName to be able to handle defaultMenuItemName in legacy fashion.
            if (UtilValidate.isNotEmpty(menuItemName)) {
                if (firstMenuItemName == null) {
                    firstMenuItemName = menuItemName;
                }
                if (this.menuItemMap.containsKey(menuItemName)) {
                    break;
                }
                else {
                    menuItemName = null;
                }
            }
        }
        // firstMenuItemName is a hack to preserve legacy ofbiz behavior:
        // If we did find a non-empty entry but it didn't match any item in our menu,
        // don't fall back to defaultMenuItemName - just return the first non-matching item.
        // Only use the "is in menu" check for the fields fallback logic, but not for defaultMenuItemName.
        // defaultMenuItemName is only used if all fields in list were empty.
        if (UtilValidate.isEmpty(menuItemName) && UtilValidate.isNotEmpty(firstMenuItemName)) {
            menuItemName = firstMenuItemName;
        }
        // SCIPIO: ... new code end
        // SCIPIO: 2016-08-30: cannot do this here anymore
        //if (UtilValidate.isEmpty(menuItemName)) {
        //    return this.defaultMenuItemName;
        //}
        return menuItemName;
    }

    public String getSelectedMenuContextFieldName(Map<String, Object> context) {
        return this.selectedMenuContextFieldName.get(context);
    }

    public FlexibleMapAccessor<String> getSelectedMenuContextFieldName() {
        return this.selectedMenuContextFieldName;
    }

    /**
     * SCIPIO: Returns selected menu and item.
     */
    public MenuAndItem getSelectedMenuAndItem(Map<String, Object> context, boolean logWarnings) {
        String fullSelItemName = getSelectedMenuItemContextFieldName(context);

        String selItemName;
        String selMenuName;
        if (UtilValidate.isNotEmpty(fullSelItemName)) {
            String[] parts = fullSelItemName.split(":");
            if (parts.length >= 2) {
                selItemName = parts[1];
                selMenuName = parts[0];
            } else {
                selItemName = parts[0];
                selMenuName = getSelectedMenuContextFieldName(context);
            }
        } else {
            selMenuName = getSelectedMenuContextFieldName(context);
            selItemName = null;
        }
        return getSelectedMenuAndItem(selItemName, selMenuName, logWarnings);
    }

    public MenuAndItem getSelectedMenuAndItem(Map<String, Object> context) {
        return getSelectedMenuAndItem(context, true);
    }

    /**
     * SCIPIO: Returns selected menu and item. The item will be either a child of
     * the (sub-)menu, the parent item of the sub-menu, or null, but the calling code should guard
     * against strange cases.
     * <p>
     * The (sub-)menu name supports special value "TOP". The item name supports
     * special values "NONE" (same as null except prevent default-menu-item-name fallback),
     * "PARENT-WITHSUB"/"PARENT", and "PARENT-NOSUB".
     * <p>
     * Note the default menu item fallback (default-menu-item-name) is ONLY used if the queried selItemName
     * is empty, but NOT if it's the value NONE nor if the lookup fails. NONE bypasses it, while
     * failed lookups are handled by displaying the menu or sub-menu as selected but without any item selected,
     * as visual debugging help.
     */
    public MenuAndItemLookup getSelectedMenuAndItem(String selItemName, String selMenuName, boolean logWarnings) {
        boolean menuNameTopMenu = isMenuNameTopMenu(selMenuName);
        // perform sub-menu lookup
        ModelSubMenu subMenu = getSubMenuByName(selMenuName);
        if (subMenu != null || menuNameTopMenu) {
            // menu item lookup. performs all the mappings but NOT the fallbacks, which handle below.
            MenuAndItemLookup lookup = resolveMenuAndItem(selItemName, selMenuName, subMenu, true);
            if (lookup.hasMenuItem()) {
                return lookup;
            } else {
                // NOTE: here, NONE is not a failure.
                // TODO: REVIEW: we will not print warnings for now if the menu item name was simply empty
                // and it's the top level of the menu; otherwise there will simply be too many warnings all the time,
                // coming from non-complex menus like tab bars
                if (UtilValidate.isNotEmpty(selItemName) || subMenu != null) {
                    if (!ModelMenuItem.NONE_MENU_ITEM_NAME.equals(lookup.getLookupMenuItemName()) && logWarnings) {
                        if (menuNameTopMenu) {
                            Debug.logWarning("Menu-item name [" + lookup.getLookupMenuItemName() + "] (mapped from [" + selItemName +
                                    "]) was not found under top level of complex menu [" + this.getFullLocationAndName() + "]", module);
                        } else {
                            Debug.logWarning("Menu-item name [" + lookup.getLookupMenuItemName() + "] (mapped from [" + selItemName +
                                    "]) was not found within sub-menu [" + selMenuName + "] under complex menu [" + this.getFullLocationAndName() + "]", module);
                        }
                    }
                }
                ModelMenuItem menuItem = null;
                if (subMenu != null) {
                    // 2016-09-28: EXTRA FALLBACK: here we will return the parent item of the sub-menu as target (same as "PARENT")
                    // this is the correct behavior for NONE and helps debug visually the other error cases.
                    menuItem = subMenu.getParentMenuItem();
                }
                return new MenuAndItemLookup(subMenu, menuItem);
            }
        } else {
            if (logWarnings) {
                Debug.logWarning("Sub-menu name [" + selMenuName + "] was not found under top complex menu [" + this.getFullLocationAndName() + "]", module);
            }
            return MenuAndItemLookup.EMPTY;
        }
    }

    public MenuAndItemLookup getSelectedMenuAndItem(String selItemName, String selMenuName) {
        return getSelectedMenuAndItem(selItemName, selMenuName, true);
    }

    public Map<String, ModelMenuItemAlias> getMenuItemAliasMap() { // SCIPIO: new
        return menuItemAliasMap;
    }

    public static List<String> readMenuItemNamesList(String namesStr) {
        if (UtilValidate.isEmpty(namesStr)) {
            return new ArrayList<>();
        }
        String[] namesArr = namesStr.split(",");
        ArrayList<String> namesList = new ArrayList<>(namesArr.length);
        for(String name : namesArr) {
            namesList.add(name.trim());
        }
        return namesList;
    }

    public static Set<String> readMenuItemNamesSet(String namesStr) {
        return new HashSet<>(readMenuItemNamesList(namesStr));
    }

    /**
     * Translates menu item for top menu.
     * NOTE: for top menu there shouldn't be any PARENT or PARENT-NOSUB entries, so we simply
     * return the original for those (which will result in a further warning in most, but that's better).
     * For null/empty/NONE, returns NONE.
     */
    public String getMappedMenuItemName(String menuItemName) {
        // translate null to NONE as first thing so it can be recognized in mappings (in theory; might not be used/usable or redundant)
        menuItemName = ModelMenuItem.getNoneMenuItemNameAsConstant(menuItemName);
        String res = this.menuItemNameAliasMap.get(menuItemName);
        if (UtilValidate.isNotEmpty(res)) {
            if (ModelMenuItem.parentMenuItemNames.contains(res)) { // don't support PARENT-XX in top menu
                return menuItemName;
            } else {
                return res;
            }
        } else {
            return menuItemName;
        }
    }

    /**
     * Translates for sub-menu or, if null, this instance.
     */
    public String getMappedMenuItemName(String menuItemName, ModelSubMenu subMenu) {
        return subMenu != null ? subMenu.getMappedMenuItemName(menuItemName) : this.getMappedMenuItemName(menuItemName);
    }

    protected static Map<String, String> makeMenuItemNameAliasMap(Map<String, ModelMenuItemAlias> aliasMap) {
        Map<String, String> nameMap = new HashMap<>();
        for(ModelMenuItemAlias aliasModel : aliasMap.values()) {
            nameMap.put(aliasModel.getName(), aliasModel.getForName());
        }
        return nameMap;
    }

    public boolean isParentOf(ModelMenuItem menuItem) { // SCIPIO: new
        if (menuItem == null) {
            return false;
        }
        return menuItem.isSame(menuItemMap.get(menuItem.getName()));
    }

    public String getTarget() {
        return target;
    }

    public FlexibleStringExpander getTitle() {
        return title;
    }

    public String getTitle(Map<String, Object> context) {
        return title.expandString(context);
    }

    /**
     * SCIPIO: Special title style for some kinds of menus (has versatile/generic meaning).
     */
    public FlexibleStringExpander getTitleStyle() {
        return titleStyle;
    }

    /**
     * SCIPIO: Special title style for some kinds of menus (has versatile/generic meaning).
     */
    public String getTitleStyle(Map<String, Object> context) {
        return titleStyle.expandString(context);
    }

    public String getTooltip() {
        return this.tooltip;
    }

    public String getType() {
        return this.type;
    }

    public String getItemsSortMode() { // SCIPIO
        return this.itemsSortMode;
    }

    /**
     * Returns rendered menu item count.
     */
    public int renderedMenuItemCount(Map<String, Object> context) {
        int count = 0;
        for (ModelMenuItem item : this.menuItemList) {
            // SCIPIO: every item needs context prepare for it now, otherwise conditions and count may be wrong
            MenuRenderState renderState = MenuRenderState.retrieve(context);
            Object prevItemContext = item.prepareItemContext(context, renderState);

            if (item.shouldBeRendered(context)) {
                count++;
            }

            item.restoreItemContext(context, prevItemContext, renderState);
        }
        return count;
    }

    public String getAutoSubMenuNames() { // SCIPIO
        return autoSubMenuNames;
    }

    public String getDefaultSubMenuModelScope() { // SCIPIO
        return defaultSubMenuModelScope;
    }

    public String getDefaultSubMenuInstanceScope() { // SCIPIO
        return defaultSubMenuInstanceScope;
    }

    public String getForceExtendsSubMenuModelScope() { // SCIPIO
        return forceExtendsSubMenuModelScope;
    }

    public String getForceAllSubMenuModelScope() { // SCIPIO
        return forceAllSubMenuModelScope;
    }

    public boolean isAlwaysExpandSelectedOrAncestor() { // SCIPIO
        return this.alwaysExpandSelectedOrAncestor;
    }

    public List<ModelMenuNode> getManualSelectedNodes() { // SCIPIO
        return manualSelectedNodes;
    }

    public List<ModelMenuNode> getManualExpandedNodes() { // SCIPIO
        return manualExpandedNodes;
    }

    // SCIPIO: DEV NOTE: WARN: DO NOT CALL THESE separate menu accessors on the fly; use MenuRenderState instead
    private String getSeparateMenuType(Map<String, Object> context) { // SCIPIO
        return separateMenuType.expandString(context);
    }

    private String getSeparateMenuTargetStyle(Map<String, Object> context) { // SCIPIO
        return separateMenuTargetStyle.expandString(context);
    }

    private String getSeparateMenuTargetPreference(Map<String, Object> context) { // SCIPIO
        return separateMenuTargetPreference.expandString(context);
    }

    private String getSeparateMenuTargetOriginalAction(Map<String, Object> context) { // SCIPIO
        return separateMenuTargetOriginalAction.expandString(context);
    }

    public SeparateMenuConfig getSeparateMenuConfig(Map<String, Object> context) { // SCIPIO
        return new SeparateMenuConfig(this, context);
    }

    public static class SeparateMenuConfig { // SCIPIO
        private final String type;
        private final String targetStyle;
        private final String targetPreference;
        private final String targetOriginalAction;
        private final boolean enabled;

        private SeparateMenuConfig(ModelMenu modelMenu, Map<String, Object> context) {
            this.type = modelMenu.getSeparateMenuType(context);
            this.targetStyle = modelMenu.getSeparateMenuTargetStyle(context);
            this.targetPreference = modelMenu.getSeparateMenuTargetPreference(context);
            this.targetOriginalAction = modelMenu.getSeparateMenuTargetOriginalAction(context);
            this.enabled = !type.isEmpty() && !targetStyle.isEmpty();
        }

        public String getType() {
            return type;
        }

        public String getTargetStyle() {
            return targetStyle;
        }

        public String getTargetPreference() {
            return targetPreference;
        }

        public String getTargetOriginalAction() {
            return targetOriginalAction;
        }

        public boolean isEnabled() {
            return enabled;
        }
    }

    /**
     * Renders this menu to a String, i.e. in a text format, as defined with the
     * MenuStringRenderer implementation.
     *
     * @param writer The Writer that the menu text will be written to
     * @param context Map containing the menu context; the following are
     *   reserved words in this context: parameters (Map), isError (Boolean),
     *   itemIndex (Integer, for lists only, otherwise null), menuName
     *   (String, optional alternate name for menu, defaults to the
     *   value of the name attribute)
     * @param menuStringRenderer An implementation of the MenuStringRenderer
     *   interface that is responsible for the actual text generation for
     *   different menu elements; implementing you own makes it possible to
     *   use the same menu definitions for many types of menu UIs
     */
    public void renderMenuString(Appendable writer, Map<String, Object> context, MenuStringRenderer menuStringRenderer)
            throws IOException {
        AbstractModelAction.runSubActions(this.actions, context);
        // SCIPIO: do this now so nothing can mess with the selection
        MenuRenderState.retrieve(context).updateSelectedMenuAndItem(context);
        if ("simple".equals(this.type)) {
            this.renderSimpleMenuString(writer, context, menuStringRenderer);
        } else {
            throw new IllegalArgumentException("The type " + this.getType() + " is not supported for menu with name "
                    + this.getName());
        }
    }

    public void renderSimpleMenuString(Appendable writer, Map<String, Object> context, MenuStringRenderer menuStringRenderer)
            throws IOException {
        // render menu open
        menuStringRenderer.renderMenuOpen(writer, context, this);

        // render formatting wrapper open
        menuStringRenderer.renderFormatSimpleWrapperOpen(writer, context, this);

        // render each menuItem row, except hidden & ignored rows
        for (ModelMenuItem item : this.getOrderedMenuItemList(context)) { // SCIPIO: switched this.menuItemList to getOrderedMenuItemList
            item.renderMenuItemString(writer, context, menuStringRenderer);
        }
        // render formatting wrapper close
        menuStringRenderer.renderFormatSimpleWrapperClose(writer, context, this);

        // render menu close
        menuStringRenderer.renderMenuClose(writer, context, this);
    }

    public void runActions(Map<String, Object> context) {
        AbstractModelAction.runSubActions(this.actions, context);
    }

    /**
     * SCIPIO: make list of flexible accessors from a semicolon-separated string.
     * TODO: support escaping semicolons
     */
    private static List<FlexibleMapAccessor<String>> makeAccessorList(String accessorsStr) {
        String[] parts = accessorsStr.split(";");
        List<FlexibleMapAccessor<String>> list = new ArrayList<>(parts.length);
        for(String part : parts) {
            list.add(FlexibleMapAccessor.<String>getInstance(part));
        }
        return list;
    }


    /**
     * SCIPIO: Passed across the whole top menu render including all its included externals.
     */
    public static class GeneralBuildArgs {
        /**
         * WARN: this is not an exact figure and is mostly for generating names.
         */
        public int totalSubMenuCount = 0;
        /**
         * WARN: this is not an exact figure and is mostly for generating names.
         */
        public int totalMenuItemCount = 0;
        public Map<String, ModelMenu> localModelMenuCache = new HashMap<>();

        public final Map<String, Element> menuElemCache = new HashMap<>();
    }

    /**
     * SCIPIO: passed across the render of only those elements whose XML falls within
     * the current top-level menu.
     */
    public static class CurrentMenuDefBuildArgs {
        public MenuDefCodeBehavior codeBehavior;

        public CurrentMenuDefBuildArgs(ModelMenu modelMenu) {
            this.codeBehavior = new MenuDefCodeBehavior(modelMenu);
        }

        public CurrentMenuDefBuildArgs(MenuDefCodeBehavior codeBehavior) {
            this.codeBehavior = codeBehavior;
        }
    }

    public static class MenuDefCodeBehavior implements Serializable {
        public String autoSubMenuNames;
        public String defaultSubMenuModelScope;
        public String defaultSubMenuInstanceScope;

        public MenuDefCodeBehavior() {
            this.autoSubMenuNames = null;
            this.defaultSubMenuModelScope = null;
            this.defaultSubMenuInstanceScope = null;
        }

        public MenuDefCodeBehavior(ModelMenu modelMenu) {
            this.autoSubMenuNames = modelMenu.autoSubMenuNames;
            this.defaultSubMenuModelScope = modelMenu.defaultSubMenuModelScope;
            this.defaultSubMenuInstanceScope = modelMenu.defaultSubMenuInstanceScope;
        }
    }

    /**
     * SCIPIO: Simple sub-menu and menu-item pair. One or both may be null.
     * Usually when subMenu is null (while menuItem is non-null) it means the item is top level.
     */
    public static class MenuAndItem implements Serializable {
        public static final MenuAndItemLookup EMPTY = new MenuAndItemLookup();

        protected final ModelSubMenu subMenu;
        protected final ModelMenuItem menuItem;

        public MenuAndItem(ModelSubMenu subMenu, ModelMenuItem menuItem) {
            this.subMenu = subMenu;
            this.menuItem = menuItem;
        }

        /**
         * Makes pair from the passed menuItem and its parent sub-menu (if any).
         */
        public MenuAndItem(ModelMenuItem menuItem) {
            this.subMenu = (menuItem != null) ? menuItem.getParentSubMenu() : null;
            this.menuItem = menuItem;
        }

        public MenuAndItem() {
            this.subMenu = null;
            this.menuItem = null;
        }

        public boolean isEmpty() {
            return this.subMenu == null && this.menuItem == null;
        }

        public static boolean isEmpty(MenuAndItem menuAndItem) {
            return menuAndItem == null || menuAndItem.isEmpty();
        }

        public ModelSubMenu getSubMenu() {
            return subMenu;
        }

        public ModelMenuItem getMenuItem() {
            return menuItem;
        }

        public boolean isItemTopLevel() {
            return menuItem != null && subMenu == null;
        }

        public boolean isSubMenu() {
            return subMenu != null;
        }

        public boolean hasMenuItem() {
            return menuItem != null;
        }
    }

    /**
     * (Sub)Menu and item lookup result, including the lookup names used to query them.
     * The lookup names may differ from the effective names of the instances.
     */
    public static class MenuAndItemLookup extends MenuAndItem {
        public static final MenuAndItemLookup EMPTY = new MenuAndItemLookup();

        protected final String lookupSubMenuName;
        protected final String lookupMenuItemName;

        public MenuAndItemLookup(ModelSubMenu subMenu, ModelMenuItem menuItem, String lookupSubMenuName, String lookupMenuItemName) {
            super(subMenu, menuItem);
            this.lookupSubMenuName = lookupSubMenuName;
            this.lookupMenuItemName = lookupMenuItemName;
        }

        public MenuAndItemLookup(ModelSubMenu subMenu, ModelMenuItem menuItem) {
            super(subMenu, menuItem);
            this.lookupSubMenuName = null;
            this.lookupMenuItemName = null;
        }

        public MenuAndItemLookup(ModelMenuItem menuItem) {
            super(menuItem);
            this.lookupSubMenuName = null;
            this.lookupMenuItemName = null;
        }

        public MenuAndItemLookup() {
            super();
            this.lookupSubMenuName = null;
            this.lookupMenuItemName = null;
        }

        public MenuAndItem toMenuAndItem() {
            return new MenuAndItem(this.subMenu, this.menuItem);
        }

        public String getLookupSubMenuName() {
            return lookupSubMenuName;
        }

        public String getLookupMenuItemName() {
            return lookupMenuItemName;
        }
    }

    // SCIPIO: ModelMenuNode methods (new)

    @Override
    public ModelMenuItemNode getParentNode() {
        return null;
    }

    @Override
    public List<ModelMenuItem> getChildrenNodes() {
        return menuItemList;
    }

    @Override
    public FlexibleStringExpander getSelected() {
        return null;
    }

    @Override
    public FlexibleStringExpander getDisabled() {
        return null;
    }

    @Override
    public FlexibleStringExpander getExpanded() {
        return null;
    }

    public static class FlaggedMenuNodes {
        private final Set<ModelMenuNode> selectedTargets;
        private final Set<ModelMenuNode> selectedAncestors;
        private final Set<ModelMenuNode> expanded;

        public FlaggedMenuNodes(Set<ModelMenuNode> selectedTargets, Set<ModelMenuNode> selectedAncestors,
                Set<ModelMenuNode> expanded) {
            super();
            this.selectedTargets = selectedTargets;
            this.selectedAncestors = selectedAncestors;
            this.expanded = expanded;
        }

        public static FlaggedMenuNodes resolve(Map<String, Object> context, List<ModelMenuNode> selectedNodeCandidates,
                List<ModelMenuNode> expandedNodeCandidates, MenuAndItem mainSelectedMenuAndItem) {
            Set<ModelMenuNode> selectedTargets = new HashSet<>();
            Set<ModelMenuNode> selectedAncestors = new HashSet<>();
            Set<ModelMenuNode> expanded = new HashSet<>();

            // note: mainSelectedMenuAndItem counts as both selected(On) AND expanded(On)

            // SELECTED
            Set<ModelMenuNode> selectedOn = new HashSet<>();
            Set<ModelMenuNode> selectedOff = new HashSet<>();
            for(ModelMenuNode node : selectedNodeCandidates) {
                Boolean selectedBool = UtilMisc.booleanValue(node.getSelected().expandString(context));
                if (Boolean.TRUE.equals(selectedBool)) {
                    selectedOn.add(node);
                } else if (Boolean.TRUE.equals(selectedBool)) {
                    selectedOff.add(node);
                }
            }
            if (mainSelectedMenuAndItem.getSubMenu() != null) {
                selectedOn.add(mainSelectedMenuAndItem.getSubMenu());
            }
            if (mainSelectedMenuAndItem.getMenuItem() != null) {
                selectedOn.add(mainSelectedMenuAndItem.getMenuItem());
            }

            // TODO

            // EXPANDED
            Set<ModelMenuNode> expandedOn = new HashSet<>();
            Set<ModelMenuNode> expandedOff = new HashSet<>();
            for(ModelMenuNode node : expandedNodeCandidates) {
                Boolean expandedBool = UtilMisc.booleanValue(node.getExpanded().expandString(context));
                if (Boolean.TRUE.equals(expandedBool)) {
                    expandedOn.add(node);
                } else if (Boolean.TRUE.equals(expandedBool)) {
                    expandedOff.add(node);
                }
            }
            if (mainSelectedMenuAndItem.getSubMenu() != null) {
                expandedOn.add(mainSelectedMenuAndItem.getSubMenu());
            }
            if (mainSelectedMenuAndItem.getMenuItem() != null) {
                expandedOn.add(mainSelectedMenuAndItem.getMenuItem());
            }

            // TODO

            return new FlaggedMenuNodes(selectedTargets, selectedAncestors, expanded);
        }

        public boolean isSelectedTarget(ModelMenuNode node) {
            return selectedTargets.contains(node);
        }

        public boolean isSelectedAncestor(ModelMenuNode node) {
            return selectedAncestors.contains(node);
        }

        public boolean isExpanded(ModelMenuNode node) {
            return expanded.contains(node);
        }

        public Set<ModelMenuNode> getSelectedTargets() {
            return selectedTargets;
        }

        public Set<ModelMenuNode> getSelectedAncestors() {
            return selectedAncestors;
        }

        public Set<ModelMenuNode> getExpanded() {
            return expanded;
        }
    }

    @Override
    public String getContainerLocation() { // SCIPIO: new
        return menuLocation;
    }

    @Override
    public String getWidgetType() { // SCIPIO: new
        return "menu";
    }

}
