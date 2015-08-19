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
import java.net.MalformedURLException;
import java.net.URL;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import javax.xml.parsers.ParserConfigurationException;

import org.ofbiz.base.location.FlexibleLocation;
import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.base.util.UtilXml;
import org.ofbiz.base.util.collections.FlexibleMapAccessor;
import org.ofbiz.base.util.string.FlexibleStringExpander;
import org.ofbiz.widget.renderer.MenuStringRenderer;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.xml.sax.SAXException;

/**
 * Models the &lt;menu&gt; element.
 * 
 * @see <code>widget-menu.xsd</code>
 */
@SuppressWarnings("serial")
public class ModelMenu extends ModelWidget {

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

    public static final String module = ModelMenu.class.getName();

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
    private final String defaultTitleStyle;
    private final String defaultTooltipStyle;
    private final String defaultWidgetStyle;
    private final String defaultLinkStyle;
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
    private final FlexibleMapAccessor<String> selectedMenuItemContextFieldName;
    private final String target;
    private final FlexibleStringExpander title;
    private final String tooltip;
    private final String type;

    /** XML Constructor */
    public ModelMenu(Element menuElement, String menuLocation) {
        super(menuElement);
        ArrayList<ModelAction> actions = new ArrayList<ModelAction>();
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
        String defaultTitleStyle = "";
        String defaultTooltipStyle = "";
        String defaultWidgetStyle = "";
        String defaultLinkStyle = "";
        FlexibleStringExpander extraIndex = FlexibleStringExpander.getInstance("");
        String fillStyle = "";
        String id = "";
        FlexibleStringExpander menuContainerStyleExdr = FlexibleStringExpander.getInstance("");
        ArrayList<ModelMenuItem> menuItemList = new ArrayList<ModelMenuItem>();
        Map<String, ModelMenuItem> menuItemMap = new HashMap<String, ModelMenuItem>();
        String menuWidth = "";
        String orientation = "horizontal";
        FlexibleMapAccessor<String> selectedMenuItemContextFieldName = FlexibleMapAccessor.getInstance("");
        String target = "";
        FlexibleStringExpander title = FlexibleStringExpander.getInstance("");
        String tooltip = "";
        String type = "";
        // check if there is a parent menu to inherit from
        ModelMenu parent = null;
        String parentResource = menuElement.getAttribute("extends-resource");
        String parentMenu = menuElement.getAttribute("extends");
        if (!parentMenu.isEmpty()) {
            if (!parentResource.isEmpty()) {
                try {
                    parent = MenuFactory.getMenuFromLocation(parentResource, parentMenu);
                } catch (Exception e) {
                    Debug.logError(e, "Failed to load parent menu definition '" + parentMenu + "' at resource '" + parentResource
                            + "'", module);
                }
            } else {
                parentResource = menuLocation;
                // try to find a menu definition in the same file
                Element rootElement = menuElement.getOwnerDocument().getDocumentElement();
                List<? extends Element> menuElements = UtilXml.childElementList(rootElement, "menu");
                for (Element menuElementEntry : menuElements) {
                    if (menuElementEntry.getAttribute("name").equals(parentMenu)) {
                        parent = new ModelMenu(menuElementEntry, parentResource);
                        break;
                    }
                }
                if (parent == null) {
                    Debug.logError("Failed to find parent menu definition '" + parentMenu + "' in same document.", module);
                }
            }
            if (parent != null) {
                type = parent.type;
                target = parent.target;
                id = parent.id;
                title = parent.title;
                tooltip = parent.tooltip;
                defaultEntityName = parent.defaultEntityName;
                defaultTitleStyle = parent.defaultTitleStyle;
                defaultSelectedStyle = parent.defaultSelectedStyle;
                defaultWidgetStyle = parent.defaultWidgetStyle;
                defaultLinkStyle = parent.defaultLinkStyle;
                defaultTooltipStyle = parent.defaultTooltipStyle;
                defaultMenuItemName = parent.defaultMenuItemName;
                menuItemList.addAll(parent.menuItemList);
                menuItemMap.putAll(parent.menuItemMap);
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
                selectedMenuItemContextFieldName = parent.selectedMenuItemContextFieldName;
                menuContainerStyleExdr = parent.menuContainerStyleExdr;
                if (parent.actions != null) {
                    actions.addAll(parent.actions);
                }
            }
        }
        if (!menuElement.getAttribute("type").isEmpty())
            type = menuElement.getAttribute("type");
        if (!menuElement.getAttribute("target").isEmpty())
            target = menuElement.getAttribute("target");
        if (!menuElement.getAttribute("id").isEmpty())
            id = menuElement.getAttribute("id");
        if (!menuElement.getAttribute("title").isEmpty())
            title = FlexibleStringExpander.getInstance(menuElement.getAttribute("title"));
        if (!menuElement.getAttribute("tooltip").isEmpty())
            tooltip = menuElement.getAttribute("tooltip");
        if (!menuElement.getAttribute("default-entity-name").isEmpty())
            defaultEntityName = menuElement.getAttribute("default-entity-name");
        if (!menuElement.getAttribute("default-title-style").isEmpty())
            defaultTitleStyle = menuElement.getAttribute("default-title-style");
        if (!menuElement.getAttribute("default-selected-style").isEmpty())
            defaultSelectedStyle = menuElement.getAttribute("default-selected-style");
        if (!menuElement.getAttribute("default-widget-style").isEmpty())
            defaultWidgetStyle = menuElement.getAttribute("default-widget-style");
        if (!menuElement.getAttribute("default-link-style").isEmpty())
            defaultLinkStyle = menuElement.getAttribute("default-link-style");
        if (!menuElement.getAttribute("default-tooltip-style").isEmpty())
            defaultTooltipStyle = menuElement.getAttribute("default-tooltip-style");
        if (!menuElement.getAttribute("default-menu-item-name").isEmpty())
            defaultMenuItemName = menuElement.getAttribute("default-menu-item-name");
        if (!menuElement.getAttribute("default-permission-operation").isEmpty())
            defaultPermissionOperation = menuElement.getAttribute("default-permission-operation");
        if (!menuElement.getAttribute("default-permission-entity-action").isEmpty())
            defaultPermissionEntityAction = menuElement.getAttribute("default-permission-entity-action");
        if (!menuElement.getAttribute("default-associated-content-id").isEmpty())
            defaultAssociatedContentId = FlexibleStringExpander.getInstance(menuElement
                    .getAttribute("default-associated-content-id"));
        if (!menuElement.getAttribute("orientation").isEmpty())
            orientation = menuElement.getAttribute("orientation");
        if (!menuElement.getAttribute("menu-width").isEmpty())
            menuWidth = menuElement.getAttribute("menu-width");
        if (!menuElement.getAttribute("default-cell-width").isEmpty())
            defaultCellWidth = menuElement.getAttribute("default-cell-width");
        if (!menuElement.getAttribute("default-hide-if-selected").isEmpty())
            defaultHideIfSelected = "true".equals(menuElement.getAttribute("default-hide-if-selected").isEmpty());
        if (!menuElement.getAttribute("default-disabled-title-style").isEmpty())
            defaultDisabledTitleStyle = menuElement.getAttribute("default-disabled-title-style");
        if (!menuElement.getAttribute("selected-menuitem-context-field-name").isEmpty())
            selectedMenuItemContextFieldName = FlexibleMapAccessor.getInstance(menuElement
                    .getAttribute("selected-menuitem-context-field-name"));
        if (!menuElement.getAttribute("menu-container-style").isEmpty())
            menuContainerStyleExdr = FlexibleStringExpander.getInstance(menuElement.getAttribute("menu-container-style"));
        if (!menuElement.getAttribute("default-align").isEmpty())
            defaultAlign = menuElement.getAttribute("default-align");
        if (!menuElement.getAttribute("default-align-style").isEmpty())
            defaultAlignStyle = menuElement.getAttribute("default-align-style");
        if (!menuElement.getAttribute("fill-style").isEmpty())
            fillStyle = menuElement.getAttribute("fill-style");
        if (!menuElement.getAttribute("extra-index").isEmpty())
            extraIndex = FlexibleStringExpander.getInstance(menuElement.getAttribute("extra-index"));
        
        // Cato: include-actions and actions
        Map<String, Element> menuElemCache = new HashMap<String, Element>();
        processIncludeActions(menuElement, actions, menuLocation, true, menuElemCache);
        
        actions.trimToSize();
        this.actions = Collections.unmodifiableList(actions);
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
        this.defaultTitleStyle = defaultTitleStyle;
        this.defaultTooltipStyle = defaultTooltipStyle;
        this.defaultWidgetStyle = defaultWidgetStyle;
        this.defaultLinkStyle = defaultLinkStyle;
        this.extraIndex = extraIndex;
        this.fillStyle = fillStyle;
        this.id = id;
        this.menuContainerStyleExdr = menuContainerStyleExdr;
        
        // Cato: include-menu-items and menu-item
        processIncludeMenuItems(menuElement, menuItemList, menuItemMap, 
                menuLocation, true, null, menuElemCache);
        
        menuItemList.trimToSize();
        this.menuItemList = Collections.unmodifiableList(menuItemList);
        this.menuItemMap = Collections.unmodifiableMap(menuItemMap);
        this.menuLocation = menuLocation;
        this.menuWidth = menuWidth;
        this.orientation = orientation;
        this.parentMenu = parent;
        this.selectedMenuItemContextFieldName = selectedMenuItemContextFieldName;
        this.target = target;
        this.title = title;
        this.tooltip = tooltip;
        this.type = type;
    }

    /**
     * Cato: implements include-actions and actions reading (moved here).
     * Also does include-elements.
     */
    private void processIncludeActions(Element menuElement, List<ModelAction> actions, 
            String currResource, boolean processIncludes, Map<String, Element> menuElemCache) {
        // don't think any problems from local cache for actions
        final boolean useCache = true;  
        final boolean cacheConsume = false;

        if (processIncludes) {
            List<Element> actionInclElements = new ArrayList<Element>();
            actionInclElements.addAll(UtilXml.childElementList(menuElement, "include-elements"));
            actionInclElements.addAll(UtilXml.childElementList(menuElement, "include-actions"));
            for (Element actionInclElement : getMergedIncludeDirectives(actionInclElements, menuLocation)) {
                String inclMenuName = actionInclElement.getAttribute("menu-name");
                String inclResource = actionInclElement.getAttribute("resource");
                String inclRecursive = actionInclElement.getAttribute("recursive");
                
                if ("no".equals(inclRecursive) || "includes-only".equals(inclRecursive) ||
                    "extends-only".equals(inclRecursive) || "full".equals(inclRecursive)) {
                    Element inclMenuElem = loadIncludedMenu(inclMenuName, inclResource, 
                            menuElement, currResource, menuElemCache, useCache, cacheConsume);
                    
                    if (inclMenuElem != null) {
                        String nextResource;
                        if (UtilValidate.isNotEmpty(inclResource)) {
                            nextResource = inclResource;
                        }
                        else {
                            nextResource = currResource;
                        }
                        
                        if ("extends-only".equals(inclRecursive) || "full".equals(inclRecursive)) {
                            String parentResource = inclMenuElem.getAttribute("extends-resource");
                            String parentMenu = inclMenuElem.getAttribute("extends");
                            if (UtilValidate.isNotEmpty(parentMenu)) {
                                Element parentMenuElem = loadIncludedMenu(parentMenu, parentResource, 
                                        inclMenuElem, currResource, menuElemCache, useCache, cacheConsume);
                                if (parentMenuElem != null) {
                                    processIncludeActions(parentMenuElem, actions, 
                                            nextResource, true, menuElemCache);
                                }
                                else {
                                    Debug.logError("Failed to find (via include-actions or include-elements) parent menu definition '" + parentMenu + "' in resource '" + parentResource + "'", module);
                                }
                            }
                        }
                        
                        if ("includes-only".equals(inclRecursive) || "full".equals(inclRecursive)) {
                            processIncludeActions(inclMenuElem, actions, 
                                    nextResource, true, menuElemCache);
                        }
                        else {
                            processIncludeActions(inclMenuElem, actions, 
                                    nextResource, false, menuElemCache);
                        }
                    }
                    else {
                        Debug.logError("Failed to find include-actions or include-elements menu definition '" + inclMenuName + "' in resource '" + inclResource + "'", module);
                    }
                }
                else {
                    Debug.logError("Unrecognized include-actions or include-elements recursive mode: " + inclRecursive, module);
                }
            } 
        }
        
        // read all actions under the "actions" element
        Element actionsElement = UtilXml.firstChildElement(menuElement, "actions");
        if (actionsElement != null) {
            actions.addAll(ModelMenuAction.readSubActions(this, actionsElement));
        }
    }
    
    /**
     * Cato: implements include-menu-items and menu-item reading (moved here).
     * Also does include-elements.
     */
    private void processIncludeMenuItems(Element menuElement, List<ModelMenuItem> menuItemList,
            Map<String, ModelMenuItem> menuItemMap, String currResource, 
            boolean processIncludes, Set<String> excludeItems, Map<String, Element> menuElemCache) {
        // WARN: even local cache not fully used (cacheConsume=true so only uses cached from prev actions includes) 
        // to be safe because known that menu-item Elements get written to in some places and 
        // reuse _might_ affect results in complex includes (?).
        // final menus are cached anyway.
        final boolean useCache = true;  
        final boolean cacheConsume = true;
        
        if (excludeItems == null) {
            excludeItems = new HashSet<String>();
        }
        
        if (processIncludes) {
            List<Element> itemInclElements = new ArrayList<Element>();
            itemInclElements.addAll(UtilXml.childElementList(menuElement, "include-elements"));
            itemInclElements.addAll(UtilXml.childElementList(menuElement, "include-menu-items"));
            for (Element itemInclElement : getMergedIncludeDirectives(itemInclElements, menuLocation)) {
                String inclMenuName = itemInclElement.getAttribute("menu-name");
                String inclResource = itemInclElement.getAttribute("resource");
                String inclRecursive = itemInclElement.getAttribute("recursive");
                
                Set<String> inclExcludeItems = new HashSet<String>();
                List<? extends Element> skipItemElems = UtilXml.childElementList(itemInclElement, "exclude-item");
                for (Element skipItemElem : skipItemElems) {
                    String itemName = skipItemElem.getAttribute("name");
                    if (UtilValidate.isNotEmpty(itemName)) {
                        inclExcludeItems.add(itemName);
                    }
                } 
                
                if ("no".equals(inclRecursive) || "includes-only".equals(inclRecursive) ||
                    "extends-only".equals(inclRecursive) || "full".equals(inclRecursive)) {
                    Element inclMenuElem = loadIncludedMenu(inclMenuName, inclResource, 
                            menuElement, currResource, menuElemCache, useCache, cacheConsume);
                    
                    if (inclMenuElem != null) {
                        inclExcludeItems.addAll(excludeItems);
                        String nextResource;
                        if (UtilValidate.isNotEmpty(inclResource)) {
                            nextResource = inclResource;
                        }
                        else {
                            nextResource = currResource;
                        }
                        
                        if ("extends-only".equals(inclRecursive) || "full".equals(inclRecursive)) {
                            String parentResource = inclMenuElem.getAttribute("extends-resource");
                            String parentMenu = inclMenuElem.getAttribute("extends");
                            if (UtilValidate.isNotEmpty(parentMenu)) {
                                Element parentMenuElem = loadIncludedMenu(parentMenu, parentResource, 
                                        inclMenuElem, currResource, menuElemCache, useCache, cacheConsume);
                                if (parentMenuElem != null) {
                                    processIncludeMenuItems(parentMenuElem, menuItemList, menuItemMap, 
                                            nextResource, true, inclExcludeItems, menuElemCache);
                                }
                                else {
                                    Debug.logError("Failed to find (via include-menu-items or include-elements) parent menu definition '" + parentMenu + "' in resource '" + parentResource + "'", module);
                                }
                            }
                        }
                        
                        if ("includes-only".equals(inclRecursive) || "full".equals(inclRecursive)) {
                            processIncludeMenuItems(inclMenuElem, menuItemList, menuItemMap, 
                                    nextResource, true, inclExcludeItems, menuElemCache);
                        }
                        else {
                            processIncludeMenuItems(inclMenuElem, menuItemList, menuItemMap, 
                                    nextResource, false, inclExcludeItems, menuElemCache);
                        }
                    }
                    else {
                        Debug.logError("Failed to find include-menu-items or include-elements menu definition '" + inclMenuName + "' in resource '" + inclResource + "'", module);
                    }
                }
                else {
                    Debug.logError("Unrecognized include-menu-items or include-elements recursive mode: " + inclRecursive, module);
                }
            } 
        }
        
        List<? extends Element> itemElements = UtilXml.childElementList(menuElement, "menu-item");
        for (Element itemElement : itemElements) {
            String itemName = itemElement.getAttribute("name");
            if (!excludeItems.contains(itemName)) {
                ModelMenuItem modelMenuItem = new ModelMenuItem(itemElement, this);
                addUpdateMenuItem(modelMenuItem, menuItemList, menuItemMap);
            }
        }
    }
    
    private Collection<Element> getMergedIncludeDirectives(Collection<Element> includeElems, String menuLocation) {
        if (includeElems.size() <= 0) {
            return includeElems;
        }
        
        // must preserve order
        Map<String, Element> dirMap = new LinkedHashMap<String, Element>();
        for(Element inclElem : includeElems) {
            String inclMenuName = inclElem.getAttribute("menu-name");
            String inclResource = inclElem.getAttribute("resource");
            if (UtilValidate.isEmpty(inclResource)) {
                inclResource = menuLocation;
            }
            String fullLocation = inclResource + "#" + inclMenuName;
            
            // here, we only want to keep the LAST include directive, so later ones override previous,
            // so must remove first
            if (dirMap.containsKey(fullLocation)) {
                dirMap.remove(fullLocation);
            }
            dirMap.put(fullLocation, inclElem);
        }

        return dirMap.values();
    }
    
    private Element loadIncludedMenu(String menuName, String resource, 
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
                    inclRootElem = menuFileDoc.getDocumentElement();
                } catch (Exception e) {
                    Debug.logError(e, "Failed to load include-menu-items resource: " + resource, module);
                }
            }
            //else {
                // Cato: No! we must reload the orig doc always because the Elements get written to!
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
     *
     */
    private void addUpdateMenuItem(ModelMenuItem modelMenuItem, List<ModelMenuItem> menuItemList,
            Map<String, ModelMenuItem> menuItemMap) {
        ModelMenuItem existingMenuItem = menuItemMap.get(modelMenuItem.getName());
        if (existingMenuItem != null) {
            // Cato: support a replace mode as well
            ModelMenuItem mergedMenuItem;
            if ("replace".equals(modelMenuItem.getOverrideMode())) {
                mergedMenuItem = modelMenuItem;
            }
            else {
                // does exist, update the item by doing a merge/override
                mergedMenuItem = existingMenuItem.mergeOverrideModelMenuItem(modelMenuItem);
            }
            int existingItemIndex = menuItemList.indexOf(existingMenuItem);
            menuItemList.set(existingItemIndex, mergedMenuItem);
            menuItemMap.put(modelMenuItem.getName(), mergedMenuItem);
        } else {
            // does not exist, add to Map
            menuItemList.add(modelMenuItem);
            menuItemMap.put(modelMenuItem.getName(), modelMenuItem);
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

    public String getDefaultPermissionEntityAction() {
        return this.defaultPermissionEntityAction;
    }

    public String getDefaultPermissionOperation() {
        return this.defaultPermissionOperation;
    }

    public String getDefaultSelectedStyle() {
        return this.defaultSelectedStyle;
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
    
    public String getDefaultLinkStyle() {
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

    public FlexibleStringExpander getMenuContainerStyleExdr() {
        return menuContainerStyleExdr;
    }

    public List<ModelMenuItem> getMenuItemList() {
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

    public ModelMenuItem getModelMenuItemByName(String name) {
        return this.menuItemMap.get(name);
    }

    public String getOrientation() {
        return this.orientation;
    }

    public ModelMenu getParentMenu() {
        return parentMenu;
    }

    public FlexibleMapAccessor<String> getSelectedMenuItemContextFieldName() {
        return selectedMenuItemContextFieldName;
    }

    public String getSelectedMenuItemContextFieldName(Map<String, Object> context) {
        String menuItemName = this.selectedMenuItemContextFieldName.get(context);
        if (UtilValidate.isEmpty(menuItemName)) {
            return this.defaultMenuItemName;
        }
        return menuItemName;
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

    public String getTooltip() {
        return this.tooltip;
    }

    public String getType() {
        return this.type;
    }

    public int renderedMenuItemCount(Map<String, Object> context) {
        int count = 0;
        for (ModelMenuItem item : this.menuItemList) {
            if (item.shouldBeRendered(context))
                count++;
        }
        return count;
    }

    /**
     * Renders this menu to a String, i.e. in a text format, as defined with the
     * MenuStringRenderer implementation.
     *
     * @param writer The Writer that the menu text will be written to
     * @param context Map containing the menu context; the following are
     *   reserved words in this context: parameters (Map), isError (Boolean),
     *   itemIndex (Integer, for lists only, otherwise null), bshInterpreter,
     *   menuName (String, optional alternate name for menu, defaults to the
     *   value of the name attribute)
     * @param menuStringRenderer An implementation of the MenuStringRenderer
     *   interface that is responsible for the actual text generation for
     *   different menu elements; implementing you own makes it possible to
     *   use the same menu definitions for many types of menu UIs
     */
    public void renderMenuString(Appendable writer, Map<String, Object> context, MenuStringRenderer menuStringRenderer)
            throws IOException {
        AbstractModelAction.runSubActions(this.actions, context);
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
        for (ModelMenuItem item : this.menuItemList) {
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
}
