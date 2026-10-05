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
package com.ilscipio.scipio.widget.def.menu;

import com.ilscipio.scipio.ce.base.component.ComponentReflectInfo;
import com.ilscipio.scipio.widget.def.condition.Condition;
import com.ilscipio.scipio.widget.def.condition.ConditionNode;
import com.ilscipio.scipio.widget.def.condition.NestedCondition;
import com.ilscipio.scipio.widget.def.condition.NestedCondition2;
import com.ilscipio.scipio.widget.def.condition.WidgetCondition;
import com.ilscipio.scipio.widget.def.condition.impl.*;
import com.ilscipio.scipio.widget.def.screen.ConditionExpr;
import com.ilscipio.scipio.widget.def.screen.EntityConditionAction;
import com.ilscipio.scipio.widget.def.screen.EntityOneAction;
import com.ilscipio.scipio.widget.def.screen.FieldMap;
import com.ilscipio.scipio.widget.def.screen.PropertyToFieldAction;
import com.ilscipio.scipio.widget.def.screen.ScriptAction;
import com.ilscipio.scipio.widget.def.screen.ServiceAction;
import com.ilscipio.scipio.widget.def.screen.SetAction;
import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilTimer;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.widget.model.MenuFactory;
import org.ofbiz.widget.model.ModelMenu;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

/**
 * Menu annotation reader - creates ModelMenu objects from @Menu annotations.
 *
 * <p>This reader scans classes annotated with @Menu and builds corresponding
 * ModelMenu objects that can be used by the widget framework.</p>
 *
 * <p>The reader generates synthetic XML elements from annotations, which are then
 * passed to the existing ModelMenu/MenuFactory constructors. This approach
 * ensures compatibility with the existing widget infrastructure.</p>
 *
 * <p>SCIPIO: 4.0.0: Added for menu annotations support.</p>
 */
@SuppressWarnings("serial")
public class MenuAnnotationReader implements Serializable {

    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    // SCIPIO: 4.0.0: Reused per-thread DocumentBuilder. Previously buildSingleMenuDocument() called
    // DocumentBuilderFactory.newInstance().newDocumentBuilder() PER MENU, and each instantiation
    // re-scans the Xerces classpath resources. Mirrors the identical fix in FormAnnotationReader.
    private static final ThreadLocal<DocumentBuilder> THREAD_LOCAL_DOCUMENT_BUILDER = ThreadLocal.withInitial(() -> {
        try {
            return DocumentBuilderFactory.newInstance().newDocumentBuilder();
        } catch (ParserConfigurationException e) {
            throw new IllegalStateException("Error creating shared DocumentBuilder for menu annotation reading", e);
        }
    });

    protected final ComponentReflectInfo reflectInfo;

    public MenuAnnotationReader(ComponentReflectInfo reflectInfo) {
        this.reflectInfo = reflectInfo;
    }

    /**
     * Reads all @Menu annotated classes/interfaces and returns a map of menu names to ModelMenu objects.
     */
    public Map<String, ModelMenu> getModelMenus() {
        return getModelMenus(null);
    }

    /**
     * Reads all @Menu annotated classes/interfaces and returns a map of menu names to ModelMenu objects.
     * Also populates the locationAliases map if provided.
     *
     * @param locationAliases Optional map to populate with location aliases (location -> name -> menu)
     */
    public Map<String, ModelMenu> getModelMenus(Map<String, Map<String, ModelMenu>> locationAliases) {
        UtilTimer utilTimer = new UtilTimer();
        utilTimer.timerString("Before start of menu loop in menu annotations for component [" +
                reflectInfo.getComponent().getGlobalName() + "]");

        Map<String, ModelMenu> modelMenus = new LinkedHashMap<>();
        int menuCount = 0;

        for (Class<?> menuClass : reflectInfo.getReflectQuery().getAnnotatedClasses(Menu.class)) {
            try {
                List<ModelMenu> menus = readMenusFromClass(menuClass);
                for (ModelMenu menu : menus) {
                    if (modelMenus.containsKey(menu.getName())) {
                        Debug.logWarning("Menu " + menu.getName() + " is defined more than once, " +
                                "most recent will over-write previous definition(s)", module);
                    }
                    modelMenus.put(menu.getName(), menu);
                    menuCount++;

                    // Register location aliases if map is provided
                    if (locationAliases != null) {
                        registerLocationAliases(locationAliases, menu, menuClass);
                    }
                }
            } catch (Exception e) {
                Debug.logError(e, "Error creating menus from annotations in class " + menuClass.getName(), module);
            }
        }

        utilTimer.timerString("Finished menu annotations for component [" +
                reflectInfo.getComponent().getGlobalName() + "] - Total Menus: " + menuCount + " FINISHED");
        Debug.logInfo("Loaded [" + menuCount + "] Menus from annotations for component [" +
                reflectInfo.getComponent().getGlobalName() + "]", module);

        return modelMenus;
    }

    /**
     * SCIPIO: 4.0.0: Reads all @Menu annotated classes/interfaces and returns a map of menu names to
     * MenuDocumentInfo objects (containing Documents, not ModelMenus).
     *
     * <p>This method is used for two-pass loading to handle circular references between menus.
     * The Documents are collected first, then ModelMenus are created in a separate pass.</p>
     *
     * @return Map of menu names to MenuDocumentInfo objects
     */
    public Map<String, MenuFactory.MenuDocumentInfo> getMenuDocuments() {
        UtilTimer utilTimer = new UtilTimer();
        utilTimer.timerString("Before start of menu document collection for component [" +
                reflectInfo.getComponent().getGlobalName() + "]");

        Map<String, MenuFactory.MenuDocumentInfo> menuDocs = new LinkedHashMap<>();
        int menuCount = 0;

        for (Class<?> menuClass : reflectInfo.getReflectQuery().getAnnotatedClasses(Menu.class)) {
            try {
                String sourceLocation = "class://" + menuClass.getName();

                // Check for @MenuList (container for multiple @Menu)
                MenuList menuList = menuClass.getAnnotation(MenuList.class);
                if (menuList != null) {
                    for (Menu menuDef : menuList.value()) {
                        String menuName = menuDef.name();
                        if (UtilValidate.isEmpty(menuName)) {
                            Debug.logWarning("Menu annotation in class " + menuClass.getName() +
                                    " has no name, skipping", module);
                            continue;
                        }
                        Document doc = buildMenuDocument(menuDef, menuClass, sourceLocation);
                        if (doc != null) {
                            if (menuDocs.containsKey(menuName)) {
                                Debug.logWarning("Menu " + menuName + " is defined more than once, " +
                                        "most recent will over-write previous definition(s)", module);
                            }
                            menuDocs.put(sourceLocation + "#" + menuName, new MenuFactory.MenuDocumentInfo(doc, sourceLocation, menuDef)); // SCIPIO: 4.0.0: unique key (same-named menus in different files must not overwrite each other)
                            menuCount++;
                        }
                    }
                }

                // Check for single @Menu annotation
                Menu menuDef = menuClass.getAnnotation(Menu.class);
                if (menuDef != null) {
                    String menuName = menuDef.name();
                    if (UtilValidate.isEmpty(menuName)) {
                        Debug.logWarning("Menu annotation in class " + menuClass.getName() +
                                " has no name, skipping", module);
                    } else {
                        Document doc = buildMenuDocument(menuDef, menuClass, sourceLocation);
                        if (doc != null) {
                            if (menuDocs.containsKey(menuName)) {
                                Debug.logWarning("Menu " + menuName + " is defined more than once, " +
                                        "most recent will over-write previous definition(s)", module);
                            }
                            menuDocs.put(sourceLocation + "#" + menuName, new MenuFactory.MenuDocumentInfo(doc, sourceLocation, menuDef)); // SCIPIO: 4.0.0: unique key (same-named menus in different files must not overwrite each other)
                            menuCount++;
                        }
                    }
                }
            } catch (Exception e) {
                Debug.logError(e, "Error collecting menu documents from annotations in class " + menuClass.getName(), module);
            }
        }

        utilTimer.timerString("Finished menu document collection for component [" +
                reflectInfo.getComponent().getGlobalName() + "] - Total Menus: " + menuCount + " FINISHED");
        Debug.logInfo("Collected [" + menuCount + "] menu documents from annotations for component [" +
                reflectInfo.getComponent().getGlobalName() + "]", module);

        return menuDocs;
    }

    /**
     * Registers location aliases for a menu based on the @Menu annotation's location/locations attributes.
     */
    protected void registerLocationAliases(Map<String, Map<String, ModelMenu>> locationAliases,
                                           ModelMenu menu, Class<?> menuClass) {
        Menu menuDef = menuClass.getAnnotation(Menu.class);
        if (menuDef == null) {
            MenuList menuList = menuClass.getAnnotation(MenuList.class);
            if (menuList != null && menuList.value().length > 0) {
                for (Menu m : menuList.value()) {
                    if (m.name().equals(menu.getName())) {
                        menuDef = m;
                        break;
                    }
                }
            }
        }

        if (menuDef == null) {
            return;
        }

        // Single location alias
        if (UtilValidate.isNotEmpty(menuDef.location())) {
            locationAliases
                    .computeIfAbsent(menuDef.location(), k -> new LinkedHashMap<>())
                    .put(menu.getName(), menu);
        }

        // Multiple location aliases
        if (menuDef.locations() != null && menuDef.locations().length > 0) {
            for (String loc : menuDef.locations()) {
                if (UtilValidate.isNotEmpty(loc)) {
                    locationAliases
                            .computeIfAbsent(loc, k -> new LinkedHashMap<>())
                            .put(menu.getName(), menu);
                }
            }
        }
    }

    /**
     * Reads @Menu annotations from a class (may have multiple via @MenuList).
     */
    protected List<ModelMenu> readMenusFromClass(Class<?> menuClass) throws ParserConfigurationException {
        List<ModelMenu> menus = new ArrayList<>();
        String sourceLocation = "class://" + menuClass.getName();

        // Check for @MenuList (container for multiple @Menu)
        MenuList menuList = menuClass.getAnnotation(MenuList.class);
        if (menuList != null) {
            for (Menu menuDef : menuList.value()) {
                ModelMenu modelMenu = createModelMenu(menuDef, menuClass, sourceLocation);
                if (modelMenu != null) {
                    menus.add(modelMenu);
                }
            }
        }

        // Check for single @Menu annotation
        Menu menuDef = menuClass.getAnnotation(Menu.class);
        if (menuDef != null) {
            ModelMenu modelMenu = createModelMenu(menuDef, menuClass, sourceLocation);
            if (modelMenu != null) {
                menus.add(modelMenu);
            }
        }

        return menus;
    }

    /**
     * Creates a ModelMenu from a @Menu annotation.
     */
    protected ModelMenu createModelMenu(Menu menuDef, Class<?> menuClass, String sourceLocation)
            throws ParserConfigurationException {
        String menuName = menuDef.name();
        if (UtilValidate.isEmpty(menuName)) {
            Debug.logWarning("Menu annotation in class " + menuClass.getName() +
                    " has no name, skipping", module);
            return null;
        }

        // Build synthetic XML document
        Document doc = buildMenuDocument(menuDef, menuClass, sourceLocation);
        if (doc == null) {
            return null;
        }

        // Create ModelMenu from the document using MenuFactory
        return MenuFactory.createModelMenu(doc, sourceLocation, menuName);
    }

    /**
     * Builds a synthetic XML document for a menu, INCLUDING any same-document menus it depends on
     * (transitively): {@code extends} without {@code extends-resource} and {@code include-elements}
     * without {@code resource}. Annotation-based menus are otherwise built one-per-document, which
     * broke ModelMenu's same-document resolution (mirrors the FormAnnotationReader extends fix).
     *
     * <p>SCIPIO: 4.0.0: Made public to support on-demand menu loading from MenuFactory.</p>
     */
    protected String currentMenuLocation; // SCIPIO: 4.0.0

    public Document buildMenuDocument(Menu menuDef, Class<?> menuClass, String sourceLocation)
            throws ParserConfigurationException {
        this.currentMenuLocation = menuDef.location(); // SCIPIO: 4.0.0: location alias of the menu being built
        Document doc = buildSingleMenuDocument(menuDef, menuClass, sourceLocation);
        if (doc == null) {
            return null;
        }
        Element menusElement = doc.getDocumentElement();
        java.util.Set<String> present = new java.util.HashSet<>();
        present.add(menuDef.name());
        java.util.Deque<Menu> queue = new java.util.ArrayDeque<>();
        queue.add(menuDef);
        while (!queue.isEmpty()) {
            Menu cur = queue.poll();
            for (String depName : getSameDocumentMenuDeps(cur)) {
                if (present.contains(depName)) {
                    continue;
                }
                present.add(depName);
                Menu dep = findMenuInClass(menuClass, depName);
                if (dep == null) {
                    // Defined elsewhere; ModelMenu logs if truly unresolvable.
                    continue;
                }
                Document depDoc = buildSingleMenuDocument(dep, menuClass, sourceLocation);
                if (depDoc != null) {
                    org.w3c.dom.NodeList depMenus = depDoc.getDocumentElement().getElementsByTagName("menu");
                    if (depMenus.getLength() > 0) {
                        menusElement.appendChild(doc.importNode(depMenus.item(0), true));
                    }
                }
                queue.add(dep);
            }
        }
        return doc;
    }

    /**
     * Returns the names of menus this menu references that must resolve within the same document:
     * same-document {@code extends} (no extends-resource) and {@code include-elements} entries
     * with no {@code resource}.
     */
    protected java.util.List<String> getSameDocumentMenuDeps(Menu menuDef) {
        java.util.List<String> deps = new java.util.ArrayList<>();
        if (UtilValidate.isNotEmpty(menuDef.extendsMenu()) && UtilValidate.isEmpty(menuDef.extendsResource())
                && !menuDef.extendsMenu().contains("#")) {
            deps.add(menuDef.extendsMenu());
        }
        for (IncludeElements includeElements : menuDef.includeElements()) {
            if (UtilValidate.isNotEmpty(includeElements.menuName()) && UtilValidate.isEmpty(includeElements.resource())) {
                deps.add(includeElements.menuName());
            }
        }
        return deps;
    }

    /**
     * Finds a sibling {@code @Menu} annotation by name. Menus from one source XML are generated as
     * separate nested interfaces inside a single container class, so a same-document dependency
     * lives on a sibling nested interface. Search the enclosing container class and ALL its nested
     * {@code @Menu} types (also handles {@code @MenuList}).
     */
    protected Menu findMenuInClass(Class<?> menuClass, String name) {
        Class<?> container = menuClass.getEnclosingClass();
        if (container == null) {
            container = menuClass;
        }
        Menu m = matchMenuByName(container, name);
        if (m != null) {
            return m;
        }
        for (Class<?> nested : container.getDeclaredClasses()) {
            m = matchMenuByName(nested, name);
            if (m != null) {
                return m;
            }
        }
        return null;
    }

    private Menu matchMenuByName(Class<?> c, String name) {
        MenuList menuList = c.getAnnotation(MenuList.class);
        if (menuList != null) {
            for (Menu m : menuList.value()) {
                if (name.equals(m.name())) {
                    return m;
                }
            }
        }
        Menu single = c.getAnnotation(Menu.class);
        if (single != null && name.equals(single.name())) {
            return single;
        }
        return null;
    }

    /**
     * Builds a synthetic XML document representing a single menu definition (no dependency inlining).
     */
    protected Document buildSingleMenuDocument(Menu menuDef, Class<?> menuClass, String sourceLocation)
            throws ParserConfigurationException {
        // SCIPIO: 4.0.0: Reuse the per-thread DocumentBuilder instead of creating a new
        // DocumentBuilderFactory/DocumentBuilder for every single menu (see field javadoc above).
        DocumentBuilder builder = THREAD_LOCAL_DOCUMENT_BUILDER.get();
        Document doc = builder.newDocument();

        // Root <menus> element
        Element menusElement = doc.createElement("menus");
        doc.appendChild(menusElement);

        // <menu> element
        Element menuElement = doc.createElement("menu");
        menuElement.setAttribute("name", menuDef.name());

        // Type attribute
        // SCIPIO: 4.0.0: always emit (the XSD default "simple" does not apply to the synthetic DOM; an empty type
        // fails at render time once the extended parent is annotation-based too)
        menuElement.setAttribute("type", menuDef.type().getXmlValue());

        // Basic attributes
        setAttrIfNotEmpty(menuElement, "id", menuDef.id());
        setAttrIfNotEmpty(menuElement, "title", menuDef.title());
        setAttrIfNotEmpty(menuElement, "title-style", menuDef.titleStyle());
        setAttrIfNotEmpty(menuElement, "tooltip", menuDef.tooltip());
        setAttrIfNotEmpty(menuElement, "default-entity-name", menuDef.defaultEntityName());

        // Default styles
        setAttrIfNotEmpty(menuElement, "default-title-style", menuDef.defaultTitleStyle());
        setAttrIfNotEmpty(menuElement, "default-widget-style", menuDef.defaultWidgetStyle());
        setAttrIfNotEmpty(menuElement, "default-link-style", menuDef.defaultLinkStyle());
        setAttrIfNotEmpty(menuElement, "default-tooltip-style", menuDef.defaultTooltipStyle());
        setAttrIfNotEmpty(menuElement, "default-selected-style", menuDef.defaultSelectedStyle());
        setAttrIfNotEmpty(menuElement, "default-selected-ancestor-style", menuDef.defaultSelectedAncestorStyle());
        setAttrIfNotEmpty(menuElement, "default-align-style", menuDef.defaultAlignStyle());
        setAttrIfNotEmpty(menuElement, "default-disabled-title-style", menuDef.defaultDisabledTitleStyle());

        // Layout
        if (menuDef.orientation() != Orientation.HORIZONTAL) {
            menuElement.setAttribute("orientation", menuDef.orientation().getXmlValue());
        }
        if (menuDef.defaultAlign() != Align.LEFT) {
            menuElement.setAttribute("default-align", menuDef.defaultAlign().getXmlValue());
        }
        setAttrIfNotEmpty(menuElement, "menu-width", menuDef.menuWidth());
        setAttrIfNotEmpty(menuElement, "default-cell-width", menuDef.defaultCellWidth());
        setAttrIfNotEmpty(menuElement, "menu-container-style", menuDef.menuContainerStyle());
        setAttrIfNotEmpty(menuElement, "fill-style", menuDef.fillStyle());
        setAttrIfNotEmpty(menuElement, "extra-index", menuDef.extraIndex());

        // Extension
        setAttrIfNotEmpty(menuElement, "extends", menuDef.extendsMenu());
        setAttrIfNotEmpty(menuElement, "extends-resource", UtilValidate.isNotEmpty(menuDef.extendsResource()) ? menuDef.extendsResource() : (UtilValidate.isNotEmpty(menuDef.extendsMenu()) ? currentMenuLocation : "")); // SCIPIO: 4.0.0

        // Selection
        setAttrIfNotEmpty(menuElement, "default-menu-item-name", menuDef.defaultMenuItemName());
        setAttrIfNotEmpty(menuElement, "default-associated-content-id", menuDef.defaultAssociatedContentId());
        if (menuDef.defaultHideIfSelected()) {
            menuElement.setAttribute("default-hide-if-selected", "true");
        }
        setAttrIfNotEmpty(menuElement, "selected-menuitem-context-field-name", menuDef.selectedMenuItemContextFieldName());
        setAttrIfNotEmpty(menuElement, "selected-menu-context-field-name", menuDef.selectedMenuContextFieldName());

        // Permissions
        setAttrIfNotEmpty(menuElement, "default-permission-operation", menuDef.defaultPermissionOperation());
        setAttrIfNotEmpty(menuElement, "default-permission-entity-action", menuDef.defaultPermissionEntityAction());

        // SCIPIO-specific
        setAttrIfNotEmpty(menuElement, "items-sort-mode", menuDef.itemsSortMode());
        setAttrIfNotEmpty(menuElement, "auto-sub-menu-names", menuDef.autoSubMenuNames());
        setAttrIfNotEmpty(menuElement, "default-sub-menu-model-scope", menuDef.defaultSubMenuModelScope());
        setAttrIfNotEmpty(menuElement, "default-sub-menu-include-scope", menuDef.defaultSubMenuIncludeScope());
        setAttrIfNotEmpty(menuElement, "always-expand-selected-or-ancestor", menuDef.alwaysExpandSelectedOrAncestor());
        setAttrIfNotEmpty(menuElement, "separate-menu-type", menuDef.separateMenuType());
        setAttrIfNotEmpty(menuElement, "separate-menu-target-style", menuDef.separateMenuTargetStyle());
        setAttrIfNotEmpty(menuElement, "item-condition-mode", menuDef.itemConditionMode());
        setAttrIfNotEmpty(menuElement, "force-extends-sub-menu-model-scope", menuDef.forceExtendsSubMenuModelScope());
        setAttrIfNotEmpty(menuElement, "force-all-sub-menu-model-scope", menuDef.forceAllSubMenuModelScope());
        setAttrIfNotEmpty(menuElement, "separate-menu-target-preference", menuDef.separateMenuTargetPreference());
        setAttrIfNotEmpty(menuElement, "separate-menu-target-original-action", menuDef.separateMenuTargetOriginalAction());

        // Add include-elements
        for (IncludeElements includeElements : menuDef.includeElements()) {
            addIncludeElementsElement(doc, menuElement, includeElements);
        }

        // Add actions
        if (!menuDef.actions().UNSET()) {
            Element actionsElement = doc.createElement("actions");
            addActionsContent(doc, actionsElement, menuDef.actions());
            if (actionsElement.hasChildNodes()) {
                menuElement.appendChild(actionsElement);
            }
        }

        // Add menu items
        for (MenuItem itemDef : menuDef.items()) {
            addMenuItemElement(doc, menuElement, itemDef);
        }

        menusElement.appendChild(menuElement);
        return doc;
    }

    /**
     * Adds actions content to an actions element.
     */
    protected void addActionsContent(Document doc, Element actionsElement, MenuActions actions) {
        // Set actions
        for (SetAction setAction : actions.set()) {
            addSetActionElement(doc, actionsElement, setAction);
        }

        // Entity-one actions
        for (EntityOneAction entityOne : actions.entityOne()) {
            addEntityOneActionElement(doc, actionsElement, entityOne);
        }

        // Entity-condition actions
        for (EntityConditionAction entityCondition : actions.entityCondition()) {
            addEntityConditionActionElement(doc, actionsElement, entityCondition);
        }

        // Service actions
        for (ServiceAction service : actions.service()) {
            addServiceActionElement(doc, actionsElement, service);
        }

        // Script actions
        for (ScriptAction script : actions.script()) {
            addScriptActionElement(doc, actionsElement, script);
        }

        // Property-to-field actions
        for (PropertyToFieldAction prop : actions.propertyToField()) {
            addPropertyToFieldActionElement(doc, actionsElement, prop);
        }
    }

    /**
     * Adds a menu-item element to a parent element.
     */
    protected void addMenuItemElement(Document doc, Element parentElement, MenuItem itemDef) {
        Element itemElement = doc.createElement("menu-item");
        itemElement.setAttribute("name", itemDef.name());

        setAttrIfNotEmpty(itemElement, "title", itemDef.title());
        setAttrIfNotEmpty(itemElement, "tooltip", itemDef.tooltip());

        // Styles
        setAttrIfNotEmpty(itemElement, "title-style", itemDef.titleStyle());
        setAttrIfNotEmpty(itemElement, "widget-style", itemDef.widgetStyle());
        setAttrIfNotEmpty(itemElement, "link-style", itemDef.linkStyle());
        setAttrIfNotEmpty(itemElement, "align-style", itemDef.alignStyle());
        setAttrIfNotEmpty(itemElement, "tooltip-style", itemDef.tooltipStyle());
        setAttrIfNotEmpty(itemElement, "selected-style", itemDef.selectedStyle());
        setAttrIfNotEmpty(itemElement, "selected-ancestor-style", itemDef.selectedAncestorStyle());
        setAttrIfNotEmpty(itemElement, "disabled-title-style", itemDef.disabledTitleStyle());

        // Position and layout
        if (!"1".equals(itemDef.position())) {
            itemElement.setAttribute("position", itemDef.position());
        }
        if (itemDef.align() != Align.LEFT) {
            itemElement.setAttribute("align", itemDef.align().getXmlValue());
        }
        setAttrIfNotEmpty(itemElement, "cell-width", itemDef.cellWidth());
        setAttrIfNotEmpty(itemElement, "associated-content-id", itemDef.associatedContentId());
        setAttrIfNotEmpty(itemElement, "hide-if-selected", itemDef.hideIfSelected());
        setAttrIfNotEmpty(itemElement, "target-window", itemDef.targetWindow());

        // Behavior
        setAttrIfNotEmpty(itemElement, "disabled", itemDef.disabled());
        setAttrIfNotEmpty(itemElement, "disable-if-empty", itemDef.disableIfEmpty());
        setAttrIfNotEmpty(itemElement, "override-mode", itemDef.overrideMode());
        setAttrIfNotEmpty(itemElement, "sort-mode", itemDef.sortMode());
        setAttrIfNotEmpty(itemElement, "always-expand-selected-or-ancestor", itemDef.alwaysExpandSelectedOrAncestor());

        // Add condition
        if (!itemDef.condition().UNSET()) {
            addMenuItemConditionElement(doc, itemElement, itemDef.condition());
        }

        // Add actions
        if (!itemDef.itemActions().UNSET()) {
            Element actionsElement = doc.createElement("actions");
            addActionsContent(doc, actionsElement, itemDef.itemActions());
            if (actionsElement.hasChildNodes()) {
                itemElement.appendChild(actionsElement);
            }
        }

        // Add link
        if (!itemDef.link().UNSET()) {
            addMenuLinkElement(doc, itemElement, itemDef.link());
        }

        // Add sub-menus
        for (SubMenu subMenuDef : itemDef.subMenus()) {
            addSubMenuElement(doc, itemElement, subMenuDef);
        }

        parentElement.appendChild(itemElement);
    }

    /**
     * Adds a condition element to a menu-item.
     *
     * <p>Supports simplified condition attributes:</p>
     * <ul>
     *   <li>conditions() - functional interface conditions (takes precedence)</li>
     *   <li>permission + permissionAction - if-has-permission</li>
     *   <li>ifEmpty - if-empty</li>
     *   <li>ifNotEmpty - if-not-empty (negated if-empty)</li>
     *   <li>conditionExpr - flexible expression condition</li>
     * </ul>
     */
    /**
     * SCIPIO: 4.0.0: Appends the simple conditions of an OrCondition/XorCondition group to parent.
     */
    protected void addSimpleConditionGroup(Document doc, Element parent, String[] ifEmpty, String[] ifNotEmpty, String[] ifTrue, String[] ifFalse,
            com.ilscipio.scipio.widget.def.screen.IfCompare[] ifCompare, com.ilscipio.scipio.widget.def.screen.IfHasPermission[] ifHasPermission) {
        for (String f : ifEmpty) { Element e = doc.createElement("if-empty"); e.setAttribute("field", f); parent.appendChild(e); }
        for (String f : ifNotEmpty) { Element n = doc.createElement("not"); Element e = doc.createElement("if-empty"); e.setAttribute("field", f); n.appendChild(e); parent.appendChild(n); }
        for (String f : ifTrue) { Element e = doc.createElement("if-true"); e.setAttribute("field", f); parent.appendChild(e); }
        for (String f : ifFalse) { Element e = doc.createElement("if-false"); e.setAttribute("field", f); parent.appendChild(e); }
        for (com.ilscipio.scipio.widget.def.screen.IfCompare c : ifCompare) {
            Element e = doc.createElement("if-compare");
            e.setAttribute("field", c.field()); e.setAttribute("operator", c.operator()); e.setAttribute("value", c.value());
            setAttrIfNotEmpty(e, "type", c.type()); setAttrIfNotEmpty(e, "format", c.format());
            parent.appendChild(e);
        }
        for (com.ilscipio.scipio.widget.def.screen.IfHasPermission hp : ifHasPermission) {
            Element e = doc.createElement("if-has-permission");
            e.setAttribute("permission", hp.permission()); setAttrIfNotEmpty(e, "action", hp.action());
            parent.appendChild(e);
        }
    }

    protected void addMenuItemConditionElement(Document doc, Element itemElement, MenuItemCondition condition) {
        Element conditionElement = doc.createElement("condition");

        // SCIPIO: Condition mode and styles
        setAttrIfNotEmpty(conditionElement, "mode", condition.mode());
        setAttrIfNotEmpty(conditionElement, "pass-style", condition.passStyle());
        setAttrIfNotEmpty(conditionElement, "disabled-style", condition.disabledStyle());

        // Add the actual condition (exactly one should be set)
        boolean hasCondition = false;

        // NEW: Functional conditions take precedence
        if (!hasCondition && condition.conditions() != null && condition.conditions().length > 0) {
            if (condition.conditions().length == 1) {
                // Single condition - add directly
                addFunctionalConditionElement(doc, conditionElement, condition.conditions()[0]);
            } else {
                // Multiple conditions - wrap in AND
                Element andElement = doc.createElement("and");
                for (Condition funcCondition : condition.conditions()) {
                    addFunctionalConditionElement(doc, andElement, funcCondition);
                }
                conditionElement.appendChild(andElement);
            }
            hasCondition = true;
        }

        // Permission condition
        if (!hasCondition && UtilValidate.isNotEmpty(condition.permission())) {
            Element elem = doc.createElement("if-has-permission");
            elem.setAttribute("permission", condition.permission());
            setAttrIfNotEmpty(elem, "action", condition.permissionAction());
            conditionElement.appendChild(elem);
            hasCondition = true;
        }

        // If-empty condition
        if (!hasCondition && UtilValidate.isNotEmpty(condition.ifEmpty())) {
            Element elem = doc.createElement("if-empty");
            elem.setAttribute("field", condition.ifEmpty());
            conditionElement.appendChild(elem);
            hasCondition = true;
        }

        // If-not-empty condition (negated if-empty)
        if (!hasCondition && UtilValidate.isNotEmpty(condition.ifNotEmpty())) {
            Element notElement = doc.createElement("not");
            Element elem = doc.createElement("if-empty");
            elem.setAttribute("field", condition.ifNotEmpty());
            notElement.appendChild(elem);
            conditionElement.appendChild(notElement);
            hasCondition = true;
        }

        // Flexible condition expression
        if (!hasCondition && UtilValidate.isNotEmpty(condition.conditionExpr())) {
            Element elem = doc.createElement("if-compare");
            elem.setAttribute("field", condition.conditionExpr());
            elem.setAttribute("operator", "equals");
            elem.setAttribute("value", "true");
            elem.setAttribute("type", "Boolean");
            conditionElement.appendChild(elem);
            hasCondition = true;
        }

        // SCIPIO: 4.0.0: or/xor groups (AND-ed with the entries above) and the not wrapper
        for (com.ilscipio.scipio.widget.def.screen.OrCondition orCond : condition.or()) {
            Element orElement = doc.createElement("or");
            addSimpleConditionGroup(doc, orElement, orCond.ifEmpty(), orCond.ifNotEmpty(), orCond.ifTrue(), orCond.ifFalse(), orCond.ifCompare(), orCond.ifHasPermission());
            if (orElement.hasChildNodes()) conditionElement.appendChild(orElement);
        }
        for (com.ilscipio.scipio.widget.def.screen.XorCondition xorCond : condition.xor()) {
            Element xorElement = doc.createElement("xor");
            addSimpleConditionGroup(doc, xorElement, xorCond.ifEmpty(), xorCond.ifNotEmpty(), xorCond.ifTrue(), xorCond.ifFalse(), xorCond.ifCompare(), xorCond.ifHasPermission());
            if (xorElement.hasChildNodes()) conditionElement.appendChild(xorElement);
        }
        if (condition.not() && conditionElement.hasChildNodes()) {
            Element inner;
            if (conditionElement.getChildNodes().getLength() == 1) {
                inner = (Element) conditionElement.removeChild(conditionElement.getFirstChild());
            } else {
                inner = doc.createElement("and");
                while (conditionElement.hasChildNodes()) inner.appendChild(conditionElement.removeChild(conditionElement.getFirstChild()));
            }
            Element notElement = doc.createElement("not");
            notElement.appendChild(inner);
            conditionElement.appendChild(notElement);
        }
        // SCIPIO: 4.0.0: a <condition> without an inner condition element (e.g. an Always fallback from the
        // converter, with mode/style attributes) makes ModelMenuCondition throw and silently drops the whole
        // menu/sub-menu build; treat it as unconditional instead.
        if (conditionElement.hasChildNodes()) {
            itemElement.appendChild(conditionElement);
        } else if (conditionElement.hasAttributes()) {
            Debug.logWarning("Menu item condition has no inner condition (unrepresentable/Always); treating as unconditional", module);
        }
    }

    /**
     * Adds a functional condition element to a parent element.
     * Converts @Condition annotations to their XML equivalents.
     *
     * <p>Note: Composite condition types (And, Or, Xor, Not) are not supported at the
     * individual Condition annotation level due to Java's cyclic annotation limitations.
     * Multiple conditions in MenuItemCondition.conditions() are automatically AND-ed together.</p>
     */
    protected void addFunctionalConditionElement(Document doc, Element parentElement, Condition condition) {
        Class<? extends WidgetCondition> type = condition.type();

        // SCIPIO: 4.0.0: composite conditions were skipped here, so or/xor/not silently became always-true
        if (isCompositeConditionType(type) && condition.tree().length > 0) {
            Element treeElem = buildConditionTreeElement(doc, type.getSimpleName(), condition.tree());
            if (treeElem != null) {
                parentElement.appendChild(treeElem);
            }
            return;
        }
        if (type == And.class || type == Or.class || type == Xor.class) {
            Element compositeElem = doc.createElement(type.getSimpleName().toLowerCase());
            for (NestedCondition nested : condition.nested()) {
                addNestedConditionElement(doc, compositeElem, nested);
            }
            if (compositeElem.hasChildNodes()) {
                parentElement.appendChild(compositeElem);
            }
            return;
        }
        if (type == Not.class) {
            NestedCondition[] nestedConds = condition.nested();
            if (nestedConds.length == 1) {
                Element notElem = doc.createElement("not");
                addNestedConditionElement(doc, notElem, nestedConds[0]);
                if (notElem.hasChildNodes()) {
                    parentElement.appendChild(notElem);
                }
            }
            return;
        }
        addSimpleConditionElement(doc, parentElement, type, condition.params());
    }

    /**
     * SCIPIO: 4.0.0: Adds the XML element of a nested (one level deep) condition, negated when requested.
     */
    /**
     * SCIPIO: 4.0.0: Builds a composite condition element from the flat Condition.tree() form,
     * which carries any depth (the NestedCondition chain stopped at a fixed one).
     */
    protected Element buildConditionTreeElement(Document doc, String typeName, ConditionNode[] nodes) {
        Element root = doc.createElement(typeName.toLowerCase());
        appendConditionTreeChildren(doc, root, nodes, -1);
        if (!root.hasChildNodes()) {
            return null;
        }
        if ("not".equals(root.getNodeName()) && root.getChildNodes().getLength() != 1) {
            return null;
        }
        return root;
    }

    /** SCIPIO: 4.0.0: Appends the tree nodes whose parent is parentIndex, depth first. */
    protected void appendConditionTreeChildren(Document doc, Element parent, ConditionNode[] nodes, int parentIndex) {
        for (int i = 0; i < nodes.length; i++) {
            ConditionNode node = nodes[i];
            if (node.parent() != parentIndex) {
                continue;
            }
            Element target = parent;
            Element notElem = null;
            if (node.not()) {
                notElem = doc.createElement("not");
                target = notElem;
            }
            String nodeType = node.type().getSimpleName();
            if (isCompositeConditionType(node.type())) {
                Element composite = doc.createElement(nodeType.toLowerCase());
                appendConditionTreeChildren(doc, composite, nodes, i);
                if (composite.hasChildNodes()) {
                    target.appendChild(composite);
                }
            } else {
                addSimpleConditionElement(doc, target, node.type(), node.params());
            }
            if (notElem != null && notElem.hasChildNodes()) {
                parent.appendChild(notElem);
            }
        }
    }

    /** SCIPIO: 4.0.0: True for the condition types that hold members rather than parameters. */
    protected boolean isCompositeConditionType(Class<? extends WidgetCondition> type) {
        return type == And.class || type == Or.class || type == Xor.class || type == Not.class;
    }

    protected void addNestedConditionElement(Document doc, Element parentElement, NestedCondition nested) {
        Element target = parentElement;
        Element notElem = null;
        if (nested.not()) {
            notElem = doc.createElement("not");
            target = notElem;
        }
        Class<? extends WidgetCondition> type = nested.type();
        if (type == And.class || type == Or.class || type == Xor.class) {
            // SCIPIO: 4.0.0: a composite inside a composite fell back to an always-true condition
            Element compositeElem = doc.createElement(type.getSimpleName().toLowerCase());
            for (NestedCondition2 member : nested.nested()) {
                addNested2ConditionElement(doc, compositeElem, member);
            }
            if (compositeElem.hasChildNodes()) {
                target.appendChild(compositeElem);
            }
        } else if (type == Not.class) {
            NestedCondition2[] members = nested.nested();
            if (members.length == 1) {
                Element innerNot = doc.createElement("not");
                addNested2ConditionElement(doc, innerNot, members[0]);
                if (innerNot.hasChildNodes()) {
                    target.appendChild(innerNot);
                }
            }
        } else {
            addSimpleConditionElement(doc, target, type, nested.params());
        }
        if (notElem != null && notElem.hasChildNodes()) {
            parentElement.appendChild(notElem);
        }
    }

    /**
     * SCIPIO: 4.0.0: Adds the XML element of a second-level nested condition, negated when requested.
     */
    protected void addNested2ConditionElement(Document doc, Element parentElement, NestedCondition2 nested) {
        if (nested.not()) {
            Element notElem = doc.createElement("not");
            addSimpleConditionElement(doc, notElem, nested.type(), nested.params());
            if (notElem.hasChildNodes()) {
                parentElement.appendChild(notElem);
            }
        } else {
            addSimpleConditionElement(doc, parentElement, nested.type(), nested.params());
        }
    }

    /**
     * SCIPIO: 4.0.0: Adds the XML element of a non-composite condition type.
     */
    protected void addSimpleConditionElement(Document doc, Element parentElement,
            Class<? extends WidgetCondition> type, String[] params) {

        // Permission conditions
        if (type == HasPermission.class) {
            Element elem = doc.createElement("if-has-permission");
            if (params.length > 0) elem.setAttribute("permission", params[0]);
            if (params.length > 1) elem.setAttribute("action", params[1]);
            parentElement.appendChild(elem);
        } else if (type == ServicePermission.class) {
            Element elem = doc.createElement("if-service-permission");
            if (params.length > 0) elem.setAttribute("service-name", params[0]);
            if (params.length > 1) elem.setAttribute("main-action", params[1]);
            if (params.length > 2) elem.setAttribute("context-map", params[2]);
            if (params.length > 3) elem.setAttribute("resource-description", params[3]);
            parentElement.appendChild(elem);
        } else if (type == EntityPermission.class) {
            Element elem = doc.createElement("if-entity-permission");
            if (params.length > 0) elem.setAttribute("entity-name", params[0]);
            if (params.length > 1) elem.setAttribute("entity-id", params[1]);
            if (params.length > 2) elem.setAttribute("target-operation", params[2]);
            parentElement.appendChild(elem);
        }
        // Comparison conditions
        else if (type == Empty.class) {
            Element elem = doc.createElement("if-empty");
            if (params.length > 0) elem.setAttribute("field", params[0]);
            parentElement.appendChild(elem);
        } else if (type == NotEmpty.class) {
            Element notElem = doc.createElement("not");
            Element elem = doc.createElement("if-empty");
            if (params.length > 0) elem.setAttribute("field", params[0]);
            notElem.appendChild(elem);
            parentElement.appendChild(notElem);
        } else if (type == Compare.class) {
            Element elem = doc.createElement("if-compare");
            if (params.length > 0) elem.setAttribute("field", params[0]);
            if (params.length > 1) elem.setAttribute("operator", params[1]);
            if (params.length > 2) elem.setAttribute("value", params[2]);
            // SCIPIO: Always set type - default to String if not provided
            if (params.length > 3 && UtilValidate.isNotEmpty(params[3])) {
                elem.setAttribute("type", params[3]);
            } else {
                elem.setAttribute("type", "String");
            }
            if (params.length > 4) elem.setAttribute("format", params[4]);
            parentElement.appendChild(elem);
        } else if (type == CompareField.class) {
            Element elem = doc.createElement("if-compare-field");
            if (params.length > 0) elem.setAttribute("field", params[0]);
            if (params.length > 1) elem.setAttribute("operator", params[1]);
            if (params.length > 2) elem.setAttribute("to-field", params[2]);
            // SCIPIO: Always set type - default to String if not provided
            if (params.length > 3 && UtilValidate.isNotEmpty(params[3])) {
                elem.setAttribute("type", params[3]);
            } else {
                elem.setAttribute("type", "String");
            }
            if (params.length > 4) elem.setAttribute("format", params[4]);
            parentElement.appendChild(elem);
        } else if (type == Regexp.class) {
            Element elem = doc.createElement("if-regexp");
            if (params.length > 0) elem.setAttribute("field", params[0]);
            if (params.length > 1) elem.setAttribute("expr", params[1]);
            parentElement.appendChild(elem);
        }
        // Validation conditions
        else if (type == ValidateMethod.class) {
            Element elem = doc.createElement("if-validate-method");
            if (params.length > 0) elem.setAttribute("field", params[0]);
            if (params.length > 1) elem.setAttribute("method", params[1]);
            if (params.length > 2) elem.setAttribute("class", params[2]);
            parentElement.appendChild(elem);
        } else if (type == Always.class) {
            // SCIPIO: 4.0.0: Always condition - no element needed (always passes)
            // This is used as a fallback when complex conditions can't be converted
        } else if (type == True.class) {
            Element elem = doc.createElement("if-true");
            if (params.length > 0) elem.setAttribute("field", params[0]);
            parentElement.appendChild(elem);
        } else if (type == False.class) {
            Element elem = doc.createElement("if-false");
            if (params.length > 0) elem.setAttribute("field", params[0]);
            parentElement.appendChild(elem);
        }
        // System conditions
        else if (type == WidgetDefined.class) {
            Element elem = doc.createElement("if-widget");
            if (params.length > 0) elem.setAttribute("name", params[0]);
            if (params.length > 1) elem.setAttribute("location", params[1]);
            if (params.length > 2) elem.setAttribute("type", params[2]);
            parentElement.appendChild(elem);
        } else if (type == ComponentEnabled.class) {
            Element elem = doc.createElement("if-component");
            if (params.length > 0) elem.setAttribute("component-name", params[0]);
            parentElement.appendChild(elem);
        } else if (type == EntityDefined.class) {
            Element elem = doc.createElement("if-entity");
            if (params.length > 0) elem.setAttribute("entity-name", params[0]);
            parentElement.appendChild(elem);
        } else if (type == ServiceDefined.class) {
            Element elem = doc.createElement("if-service");
            if (params.length > 0) elem.setAttribute("service-name", params[0]);
            parentElement.appendChild(elem);
        } else if (type == EmptySection.class) {
            Element elem = doc.createElement("if-empty-section");
            if (params.length > 0) elem.setAttribute("section-name", params[0]);
            parentElement.appendChild(elem);
        } else {
            Debug.logWarning("Unknown functional condition type: " + type.getName(), module);
        }
    }

    /**
     * Adds a link element to a menu-item.
     */
    protected void addMenuLinkElement(Document doc, Element itemElement, MenuLink link) {
        Element linkElement = doc.createElement("link");

        setAttrIfNotEmpty(linkElement, "text", link.text());
        setAttrIfNotEmpty(linkElement, "id", link.id());
        setAttrIfNotEmpty(linkElement, "style", link.style());
        setAttrIfNotEmpty(linkElement, "name", link.name());
        setAttrIfNotEmpty(linkElement, "title", link.title());
        if (link.size() > 0) {
            linkElement.setAttribute("size", String.valueOf(link.size()));
        }
        setAttrIfNotEmpty(linkElement, "target", link.target());
        setAttrIfNotEmpty(linkElement, "target-window", link.targetWindow());
        setAttrIfNotEmpty(linkElement, "prefix", link.prefix());
        setAttrIfNotEmpty(linkElement, "width", link.width());
        setAttrIfNotEmpty(linkElement, "height", link.height());

        if (link.linkType() != LinkType.AUTO) {
            linkElement.setAttribute("link-type", link.linkType().getXmlValue());
        }
        // SCIPIO: 4.0.0: always emit (XSD default "intra-app" does not apply to the synthetic DOM; an empty
        // url-mode renders plain relative hrefs)
        linkElement.setAttribute("url-mode", link.urlMode().getXmlValue());

        setAttrIfNotEmpty(linkElement, "full-path", link.fullPath());
        setAttrIfNotEmpty(linkElement, "secure", link.secure());
        setAttrIfNotEmpty(linkElement, "encode", link.encode());

        if (link.requestConfirmation()) {
            linkElement.setAttribute("request-confirmation", "true");
        }
        setAttrIfNotEmpty(linkElement, "confirmation-message", link.confirmationMessage());
        setAttrIfNotEmpty(linkElement, "use-when", link.useWhen());

        // Add parameters
        for (MenuParameter param : link.parameters()) {
            addParameterElement(doc, linkElement, param);
        }

        // Add auto-parameters-service
        if (!link.autoParametersService().UNSET()) {
            addAutoParametersServiceElement(doc, linkElement, link.autoParametersService());
        }

        // Add auto-parameters-entity
        if (!link.autoParametersEntity().UNSET()) {
            addAutoParametersEntityElement(doc, linkElement, link.autoParametersEntity());
        }

        // Add image
        if (!link.image().UNSET()) {
            addImageElement(doc, linkElement, link.image());
        }

        itemElement.appendChild(linkElement);
    }

    /**
     * Adds a sub-menu element to a menu-item.
     */
    /**
     * SCIPIO: 4.0.0: A class:// menu address without "#menuName" cannot be split into resource + name by
     * ModelLocation, so sub-menu include/model references silently resolved to nothing (complex side bars lost
     * every sub-menu, e.g. "Could not find (active) sub menu 'Templates'"). Appends "#name" from the @Menu
     * annotation of the referenced class (or the inner class simple name as a last resort).
     */
    protected String normalizeClassMenuAddress(String address) {
        if (UtilValidate.isEmpty(address) || !address.startsWith("class://") || address.contains("#")) {
            return address;
        }
        String className = address.substring("class://".length());
        try {
            Class<?> cls = Class.forName(className);
            Menu menuDef = cls.getAnnotation(Menu.class);
            if (menuDef != null && UtilValidate.isNotEmpty(menuDef.name())) {
                return address + "#" + menuDef.name();
            }
            MenuList menuList = cls.getAnnotation(MenuList.class);
            if (menuList != null && menuList.value().length == 1 && UtilValidate.isNotEmpty(menuList.value()[0].name())) {
                return address + "#" + menuList.value()[0].name();
            }
        } catch (ClassNotFoundException e) {
            // leave unchanged; ModelMenu reports the unresolved include
        }
        int dollar = className.lastIndexOf('$');
        return (dollar >= 0) ? address + "#" + className.substring(dollar + 1) : address;
    }

    protected void addSubMenuElement(Document doc, Element itemElement, SubMenu subMenu) {
        Element subMenuElement = doc.createElement("sub-menu");

        setAttrIfNotEmpty(subMenuElement, "name", subMenu.name());
        setAttrIfNotEmpty(subMenuElement, "id", subMenu.id());
        setAttrIfNotEmpty(subMenuElement, "style", subMenu.style());
        setAttrIfNotEmpty(subMenuElement, "title", subMenu.title());
        setAttrIfNotEmpty(subMenuElement, "model", normalizeClassMenuAddress(subMenu.model()));
        setAttrIfNotEmpty(subMenuElement, "model-scope", subMenu.modelScope());
        setAttrIfNotEmpty(subMenuElement, "include", normalizeClassMenuAddress(subMenu.include()));
        setAttrIfNotEmpty(subMenuElement, "items-sort-mode", subMenu.itemsSortMode());
        setAttrIfNotEmpty(subMenuElement, "share-scope", subMenu.shareScope());
        setAttrIfNotEmpty(subMenuElement, "expanded", subMenu.expanded());

        // Add condition
        if (!subMenu.condition().UNSET()) {
            addMenuItemConditionElement(doc, subMenuElement, subMenu.condition());
        }

        // Add actions
        if (!subMenu.actions().UNSET()) {
            Element actionsElement = doc.createElement("actions");
            addActionsContent(doc, actionsElement, subMenu.actions());
            if (actionsElement.hasChildNodes()) {
                subMenuElement.appendChild(actionsElement);
            }
        }

        // Add menu items (SubMenuItem to avoid cyclic annotations)
        for (SubMenuItem nestedItem : subMenu.items()) {
            addSubMenuItemElement(doc, subMenuElement, nestedItem);
        }

        itemElement.appendChild(subMenuElement);
    }

    /**
     * Adds a sub-menu-item element (simplified MenuItem without subMenus).
     */
    protected void addSubMenuItemElement(Document doc, Element parentElement, SubMenuItem itemDef) {
        Element itemElement = doc.createElement("menu-item");
        itemElement.setAttribute("name", itemDef.name());

        setAttrIfNotEmpty(itemElement, "title", itemDef.title());
        setAttrIfNotEmpty(itemElement, "tooltip", itemDef.tooltip());

        // Styles
        setAttrIfNotEmpty(itemElement, "title-style", itemDef.titleStyle());
        setAttrIfNotEmpty(itemElement, "widget-style", itemDef.widgetStyle());
        setAttrIfNotEmpty(itemElement, "link-style", itemDef.linkStyle());
        setAttrIfNotEmpty(itemElement, "align-style", itemDef.alignStyle());
        setAttrIfNotEmpty(itemElement, "tooltip-style", itemDef.tooltipStyle());
        setAttrIfNotEmpty(itemElement, "selected-style", itemDef.selectedStyle());
        setAttrIfNotEmpty(itemElement, "selected-ancestor-style", itemDef.selectedAncestorStyle());
        setAttrIfNotEmpty(itemElement, "disabled-title-style", itemDef.disabledTitleStyle());

        // Position and layout
        if (!"1".equals(itemDef.position())) {
            itemElement.setAttribute("position", itemDef.position());
        }
        if (itemDef.align() != Align.LEFT) {
            itemElement.setAttribute("align", itemDef.align().getXmlValue());
        }
        setAttrIfNotEmpty(itemElement, "cell-width", itemDef.cellWidth());
        setAttrIfNotEmpty(itemElement, "associated-content-id", itemDef.associatedContentId());
        setAttrIfNotEmpty(itemElement, "hide-if-selected", itemDef.hideIfSelected());
        setAttrIfNotEmpty(itemElement, "target-window", itemDef.targetWindow());

        // Behavior
        setAttrIfNotEmpty(itemElement, "disabled", itemDef.disabled());
        setAttrIfNotEmpty(itemElement, "disable-if-empty", itemDef.disableIfEmpty());
        setAttrIfNotEmpty(itemElement, "override-mode", itemDef.overrideMode());
        setAttrIfNotEmpty(itemElement, "sort-mode", itemDef.sortMode());
        setAttrIfNotEmpty(itemElement, "always-expand-selected-or-ancestor", itemDef.alwaysExpandSelectedOrAncestor());

        // Add condition
        if (!itemDef.condition().UNSET()) {
            addMenuItemConditionElement(doc, itemElement, itemDef.condition());
        }

        // Add actions
        if (!itemDef.itemActions().UNSET()) {
            Element actionsElement = doc.createElement("actions");
            addActionsContent(doc, actionsElement, itemDef.itemActions());
            if (actionsElement.hasChildNodes()) {
                itemElement.appendChild(actionsElement);
            }
        }

        // Add link
        if (!itemDef.link().UNSET()) {
            addMenuLinkElement(doc, itemElement, itemDef.link());
        }

        // Note: SubMenuItem does not have subMenus to avoid cyclic references

        parentElement.appendChild(itemElement);
    }

    /**
     * Adds a parameter element.
     */
    protected void addParameterElement(Document doc, Element parentElement, MenuParameter param) {
        Element paramElement = doc.createElement("parameter");
        paramElement.setAttribute("param-name", param.paramName());
        setAttrIfNotEmpty(paramElement, "value", param.value());
        setAttrIfNotEmpty(paramElement, "from-field", param.fromField());
        parentElement.appendChild(paramElement);
    }

    /**
     * Adds auto-parameters-service element.
     */
    protected void addAutoParametersServiceElement(Document doc, Element parentElement, AutoParametersService autoParams) {
        Element autoParamsElement = doc.createElement("auto-parameters-service");
        setAttrIfNotEmpty(autoParamsElement, "service-name", autoParams.serviceName());
        if (!autoParams.sendIfEmpty()) {
            autoParamsElement.setAttribute("send-if-empty", "false");
        }
        for (String exclude : autoParams.exclude()) {
            Element excludeElement = doc.createElement("exclude");
            excludeElement.setAttribute("field-name", exclude);
            autoParamsElement.appendChild(excludeElement);
        }
        parentElement.appendChild(autoParamsElement);
    }

    /**
     * Adds auto-parameters-entity element.
     */
    protected void addAutoParametersEntityElement(Document doc, Element parentElement, AutoParametersEntity autoParams) {
        Element autoParamsElement = doc.createElement("auto-parameters-entity");
        setAttrIfNotEmpty(autoParamsElement, "entity-name", autoParams.entityName());
        setAttrIfNotEmpty(autoParamsElement, "include", autoParams.include());
        if (!autoParams.sendIfEmpty()) {
            autoParamsElement.setAttribute("send-if-empty", "false");
        }
        for (String exclude : autoParams.exclude()) {
            Element excludeElement = doc.createElement("exclude");
            excludeElement.setAttribute("field-name", exclude);
            autoParamsElement.appendChild(excludeElement);
        }
        parentElement.appendChild(autoParamsElement);
    }

    /**
     * Adds an image element.
     */
    protected void addImageElement(Document doc, Element parentElement, MenuImage image) {
        Element imageElement = doc.createElement("image");
        setAttrIfNotEmpty(imageElement, "src", image.src());
        setAttrIfNotEmpty(imageElement, "id", image.id());
        setAttrIfNotEmpty(imageElement, "style", image.style());
        setAttrIfNotEmpty(imageElement, "width", image.width());
        setAttrIfNotEmpty(imageElement, "height", image.height());
        setAttrIfNotEmpty(imageElement, "border", image.border());
        setAttrIfNotEmpty(imageElement, "alt", image.alt());
        setAttrIfNotEmpty(imageElement, "title", image.title());
        if (image.urlMode() != UrlMode.CONTENT) {
            imageElement.setAttribute("url-mode", image.urlMode().getXmlValue());
        }
        parentElement.appendChild(imageElement);
    }

    // Action element methods (reused from form/screen pattern)

    protected void addSetActionElement(Document doc, Element actionsElement, SetAction setAction) {
        if (UtilValidate.isEmpty(setAction.field())) {
            return;
        }
        Element setElement = doc.createElement("set");
        setElement.setAttribute("field", setAction.field());
        setAttrIfNotEmpty(setElement, "value", setAction.value());
        setAttrIfNotEmpty(setElement, "from-field", setAction.fromField());
        setAttrIfNotEmpty(setElement, "default-value", setAction.defaultValue());
        setAttrIfNotEmpty(setElement, "type", setAction.type());
        if (setAction.global()) {
            setElement.setAttribute("global", "true");
        }
        if (!setAction.setIfEmpty()) {
            setElement.setAttribute("set-if-empty", "false");
        }
        if (!setAction.setIfNull()) {
            setElement.setAttribute("set-if-null", "false");
        }
        actionsElement.appendChild(setElement);
    }

    protected void addServiceActionElement(Document doc, Element actionsElement, ServiceAction serviceAction) {
        if (UtilValidate.isEmpty(serviceAction.serviceName())) {
            return;
        }
        Element serviceElement = doc.createElement("service");
        serviceElement.setAttribute("service-name", serviceAction.serviceName());
        setAttrIfNotEmpty(serviceElement, "result-map", serviceAction.resultMapName());
        serviceElement.setAttribute("auto-field-map", serviceAction.autoFieldMap() ? "true" : "false"); // SCIPIO: 4.0.0: always emit; synthetic DOM has no XSD default and empty means no auto-field-map at runtime
        setAttrIfNotEmpty(serviceElement, "result-map-field", serviceAction.resultMapField());
        for (FieldMap fieldMap : serviceAction.fieldMaps()) {
            Element fieldMapElement = doc.createElement("field-map");
            fieldMapElement.setAttribute("field-name", fieldMap.fieldName());
            setAttrIfNotEmpty(fieldMapElement, "from-field", fieldMap.fromField());
            setAttrIfNotEmpty(fieldMapElement, "value", fieldMap.value());
            serviceElement.appendChild(fieldMapElement);
        }
        actionsElement.appendChild(serviceElement);
    }

    protected void addEntityOneActionElement(Document doc, Element actionsElement, EntityOneAction entityOne) {
        if (UtilValidate.isEmpty(entityOne.entityName()) || UtilValidate.isEmpty(entityOne.valueField())) {
            return;
        }
        Element entityOneElement = doc.createElement("entity-one");
        entityOneElement.setAttribute("entity-name", entityOne.entityName());
        entityOneElement.setAttribute("value-field", entityOne.valueField());
        entityOneElement.setAttribute("auto-field-map", entityOne.autoFieldMap() ? "true" : "false"); // SCIPIO: 4.0.0: always emit; synthetic DOM has no XSD default and empty means no auto-field-map at runtime
        if (entityOne.useCache()) {
            entityOneElement.setAttribute("use-cache", "true");
        }
        for (FieldMap fieldMap : entityOne.fieldMaps()) {
            Element fieldMapElement = doc.createElement("field-map");
            fieldMapElement.setAttribute("field-name", fieldMap.fieldName());
            setAttrIfNotEmpty(fieldMapElement, "from-field", fieldMap.fromField());
            setAttrIfNotEmpty(fieldMapElement, "value", fieldMap.value());
            entityOneElement.appendChild(fieldMapElement);
        }
        actionsElement.appendChild(entityOneElement);
    }

    protected void addEntityConditionActionElement(Document doc, Element actionsElement, EntityConditionAction entityCondition) {
        if (UtilValidate.isEmpty(entityCondition.entityName()) || UtilValidate.isEmpty(entityCondition.list())) {
            return;
        }
        Element entityConditionElement = doc.createElement("entity-condition");
        entityConditionElement.setAttribute("entity-name", entityCondition.entityName());
        entityConditionElement.setAttribute("list", entityCondition.list());
        if (entityCondition.useCache()) {
            entityConditionElement.setAttribute("use-cache", "true");
        }
        if (entityCondition.filterByDate()) {
            entityConditionElement.setAttribute("filter-by-date", "true");
        }
        if (entityCondition.distinct()) {
            entityConditionElement.setAttribute("distinct", "true");
        }
        setAttrIfNotEmpty(entityConditionElement, "delegator-name", entityCondition.delegatorName());

        for (ConditionExpr condExpr : entityCondition.conditions()) {
            Element condExprElement = doc.createElement("condition-expr");
            condExprElement.setAttribute("field-name", condExpr.fieldName());
            condExprElement.setAttribute("operator", condExpr.operator());
            setAttrIfNotEmpty(condExprElement, "value", condExpr.value());
            setAttrIfNotEmpty(condExprElement, "from-field", condExpr.fromField());
            setAttrIfNotEmpty(condExprElement, "env-name", condExpr.envName());
            if (condExpr.ignoreCase()) {
                condExprElement.setAttribute("ignore-case", "true");
            }
            if (condExpr.ignoreIfEmpty()) {
                condExprElement.setAttribute("ignore-if-empty", "true");
            }
            if (condExpr.ignoreIfNull()) {
                condExprElement.setAttribute("ignore-if-null", "true");
            }
            entityConditionElement.appendChild(condExprElement);
        }

        for (String selectField : entityCondition.selectFields()) {
            Element selectFieldElement = doc.createElement("select-field");
            selectFieldElement.setAttribute("field-name", selectField);
            entityConditionElement.appendChild(selectFieldElement);
        }

        for (String orderBy : entityCondition.orderBy()) {
            Element orderByElement = doc.createElement("order-by");
            orderByElement.setAttribute("field-name", orderBy);
            entityConditionElement.appendChild(orderByElement);
        }

        actionsElement.appendChild(entityConditionElement);
    }

    protected void addScriptActionElement(Document doc, Element actionsElement, ScriptAction scriptAction) {
        if (UtilValidate.isEmpty(scriptAction.location()) && UtilValidate.isEmpty(scriptAction.script())) {
            return;
        }
        Element scriptElement = doc.createElement("script");
        if (UtilValidate.isNotEmpty(scriptAction.location())) {
            scriptElement.setAttribute("location", scriptAction.location());
        } else if (UtilValidate.isNotEmpty(scriptAction.script())) {
            scriptElement.setAttribute("lang", scriptAction.lang());
            scriptElement.setTextContent(scriptAction.script());
        }
        actionsElement.appendChild(scriptElement);
    }

    protected void addPropertyToFieldActionElement(Document doc, Element actionsElement, PropertyToFieldAction prop) {
        if (UtilValidate.isEmpty(prop.field()) || UtilValidate.isEmpty(prop.resource()) || UtilValidate.isEmpty(prop.property())) {
            return;
        }
        Element propElement = doc.createElement("property-to-field");
        propElement.setAttribute("field", prop.field());
        propElement.setAttribute("resource", prop.resource());
        propElement.setAttribute("property", prop.property());
        setAttrIfNotEmpty(propElement, "default", prop.defaultValue());
        if (prop.noLocale()) {
            propElement.setAttribute("no-locale", "true");
        }
        setAttrIfNotEmpty(propElement, "arg-list-name", prop.argListName());
        if (prop.global()) {
            propElement.setAttribute("global", "true");
        }
        actionsElement.appendChild(propElement);
    }

    /**
     * Helper method to set an attribute only if the value is not empty.
     */
    protected void setAttrIfNotEmpty(Element element, String attrName, String value) {
        if (UtilValidate.isNotEmpty(value)) {
            element.setAttribute(attrName, value);
        }
    }

    /**
     * Adds an include-elements element to a parent element.
     *
     * <p>SCIPIO: 4.0.0: Added for menu annotations support.</p>
     */
    protected void addIncludeElementsElement(Document doc, Element parentElement, IncludeElements includeElements) {
        Element includeElement = doc.createElement("include-elements");

        // Menu name or menu-ref (one is required)
        setAttrIfNotEmpty(includeElement, "menu-name", includeElements.menuName());
        setAttrIfNotEmpty(includeElement, "resource", UtilValidate.isNotEmpty(includeElements.resource()) ? includeElements.resource() : currentMenuLocation); // SCIPIO: 4.0.0: same-file includes resolve through the location alias
        setAttrIfNotEmpty(includeElement, "menu-ref", includeElements.menuRef());

        // Recursive mode - only set if not default (FULL)
        if (includeElements.recursive() != RecursiveMode.FULL) {
            includeElement.setAttribute("recursive", includeElements.recursive().getXmlValue());
        }

        // Optional attributes
        setAttrIfNotEmpty(includeElement, "sub-menus", includeElements.subMenus());
        setAttrIfNotEmpty(includeElement, "force-sub-menu-model-scope", includeElements.forceSubMenuModelScope());

        // Include menu item aliases - only set if false (true is default)
        if (!includeElements.includeMenuItemAliases()) {
            includeElement.setAttribute("include-menu-item-aliases", "false");
        }

        // Exclude items
        for (String excludeItem : includeElements.excludeItems()) {
            if (UtilValidate.isNotEmpty(excludeItem)) {
                Element excludeElement = doc.createElement("exclude-item");
                excludeElement.setAttribute("name", excludeItem);
                includeElement.appendChild(excludeElement);
            }
        }

        parentElement.appendChild(includeElement);
    }
}
