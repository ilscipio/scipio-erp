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
package org.ofbiz.widget.model;

import java.io.IOException;
import java.net.URL;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;

import javax.servlet.ServletContext;
import javax.servlet.http.HttpServletRequest;
import javax.xml.parsers.ParserConfigurationException;

import com.ilscipio.scipio.ce.base.component.ComponentReflectInfo;
import com.ilscipio.scipio.ce.base.component.ComponentReflectRegistry;
import com.ilscipio.scipio.widget.def.menu.MenuAnnotationReader;
import org.ofbiz.base.location.FlexibleLocation;
import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilHttp;
import org.ofbiz.base.util.UtilXml;
import org.ofbiz.base.util.cache.UtilCache;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.xml.sax.SAXException;


/**
 * Widget Library - Menu factory class
 * <p>
 * SCIPIO: now also as instance
 */
@SuppressWarnings("serial")
public class MenuFactory extends WidgetFactory {

    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    public static final UtilCache<String, Map<String, ModelMenu>> menuWebappCache = UtilCache.createUtilCache("widget.menu.webappResource", 0, 0, false);
    public static final UtilCache<String, Map<String, ModelMenu>> menuLocationCache = UtilCache.createUtilCache("widget.menu.locationResource", 0, 0, false);

    // SCIPIO: 4.0.0: Annotation-based menu cache - key is "class://fully.qualified.ClassName"
    private static final String CLASS_LOCATION_PREFIX = "class://";
    private static volatile Map<String, ModelMenu> annotationMenuCache = null;
    // SCIPIO: 4.0.0: Location alias registry - maps component:// URLs to menu maps
    private static volatile Map<String, Map<String, ModelMenu>> menuLocationAliases = null;
    private static volatile boolean annotationMenusLoaded = false;
    // SCIPIO: 4.0.0: pending annotation menu documents (only during loadAnnotationMenus) + keys already built,
    // so a menu that extends/includes a not-yet-built annotation menu (e.g. common CommonMenus) can build it on demand
    private static volatile Map<String, MenuDocumentInfo> pendingMenuDocuments = null;
    private static final Set<String> builtMenuKeys = java.util.Collections.synchronizedSet(new HashSet<>());
    private static final ThreadLocal<Set<String>> menusBeingBuilt = ThreadLocal.withInitial(HashSet::new);

    /**
     * SCIPIO: 4.0.0: Builds (once) every pending annotation menu registered for the given location alias that is not
     * built yet; returns true if something was built. Cycle-safe via a per-thread in-progress set.
     */
    private static boolean buildPendingMenusForLocation(String location) {
        Map<String, MenuDocumentInfo> pending = pendingMenuDocuments;
        if (pending == null || location == null) {
            return false;
        }
        boolean built = false;
        for (Map.Entry<String, MenuDocumentInfo> e : pending.entrySet()) {
            MenuDocumentInfo docInfo = e.getValue();
            if (docInfo.menuDef == null) continue;
            boolean atLocation = location.equals(docInfo.menuDef.location());
            if (!atLocation) {
                for (String loc : docInfo.menuDef.locations()) { if (location.equals(loc)) { atLocation = true; break; } }
            }
            if (!atLocation) continue;
            if (buildPendingMenu(e.getKey(), docInfo)) built = true;
        }
        return built;
    }

    /**
     * SCIPIO: 4.0.0: Builds one pending annotation menu (re-entrancy safe); returns true if it was built now.
     */
    private static boolean buildPendingMenu(String key, MenuDocumentInfo docInfo) {
        if (key == null || docInfo == null || docInfo.menuDef == null) return false;
        if (builtMenuKeys.contains(key) || menusBeingBuilt.get().contains(key)) return false;
        builtMenuKeys.add(key);
        menusBeingBuilt.get().add(key);
        try {
            String realMenuName = docInfo.menuDef.name().isEmpty() ? key : docInfo.menuDef.name();
            ModelMenu modelMenu = createModelMenu(docInfo.document, docInfo.sourceLocation, realMenuName);
            if (modelMenu != null && annotationMenuCache != null && menuLocationAliases != null) {
                annotationMenuCache.put(realMenuName, modelMenu);
                registerMenuLocationAliases(menuLocationAliases, modelMenu, docInfo.menuDef);
                return true;
            }
        } catch (Throwable t) {
            Debug.logError(t, "Error creating annotation menu [" + key + "] on demand: " + t, module);
        } finally {
            menusBeingBuilt.get().remove(key);
        }
        return false;
    }

    /**
     * SCIPIO: 4.0.0: Builds the pending annotation menu [location#name] on demand (lookups during another menu build).
     */
    private static boolean buildPendingMenu(String location, String name) {
        Map<String, MenuDocumentInfo> pending = pendingMenuDocuments;
        if (pending == null || location == null || name == null) return false;
        String key = findPendingMenuKeyByLocation(pending, location, name);
        return key != null && buildPendingMenu(key, pending.get(key));
    }

    private static boolean buildPendingMenuByName(String name) {
        Map<String, MenuDocumentInfo> pending = pendingMenuDocuments;
        if (pending == null || name == null) return false;
        String key = findPendingMenuKeyByName(pending, name, null);
        return key != null && buildPendingMenu(key, pending.get(key));
    }



    public static MenuFactory getMenuFactory() { // SCIPIO: new
        return menuFactory;
    }

    /**
     * SCIPIO: 4.0.0: Checks if a resource location refers to an annotation-based menu class.
     */
    public static boolean isClassLocation(String resourceName) {
        return resourceName != null && resourceName.startsWith(CLASS_LOCATION_PREFIX);
    }

    /**
     * SCIPIO: 4.0.0: Gets the class name from a class:// location.
     */
    public static String getClassNameFromLocation(String resourceName) {
        if (!isClassLocation(resourceName)) {
            throw new IllegalArgumentException("Not a class location: " + resourceName);
        }
        return resourceName.substring(CLASS_LOCATION_PREFIX.length());
    }

    /**
     * SCIPIO: 4.0.0: Loads all annotation-based menus from all components.
     *
     * <p>Uses two-pass loading to handle circular references between menus:
     * <ol>
     *   <li>First pass: Collect all menu Documents from annotations (no ModelMenu construction)</li>
     *   <li>Second pass: Create ModelMenus from Documents (can now resolve include references)</li>
     * </ol></p>
     */
    private static void loadAnnotationMenus() {
        if (annotationMenusLoaded) {
            return;
        }
        synchronized (MenuFactory.class) {
            if (annotationMenusLoaded) {
                return;
            }
            // SCIPIO: 4.0.0: Initialize empty caches FIRST to prevent infinite recursion
            // when menu constructors try to load extended menus during initial load
            Map<String, ModelMenu> menus = new ConcurrentHashMap<>();
            Map<String, Map<String, ModelMenu>> locationAliases = new ConcurrentHashMap<>();
            annotationMenuCache = menus;
            menuLocationAliases = locationAliases;
            // SCIPIO: 4.0.0: Set BEFORE loading to prevent re-entry (menu constructors may trigger
            // another loadAnnotationMenus() call while resolving extended menus - this is intentional
            // and must NOT be moved into a finally block, unlike FormFactory/ScreenFactory, or it would
            // reintroduce StackOverflowError via infinite recursion).
            annotationMenusLoaded = true;

            try {
                // SCIPIO: 4.0.0: Two-pass loading to handle circular references
                // Pass 1: Collect all Documents without creating ModelMenus
                Map<String, MenuDocumentInfo> pendingDocuments = new LinkedHashMap<>();

                for (ComponentReflectInfo cri : ComponentReflectRegistry.getReflectInfos()) {
                    try {
                        MenuAnnotationReader reader = new MenuAnnotationReader(cri);
                        // Collect documents instead of creating ModelMenus
                        Map<String, MenuDocumentInfo> componentDocs = reader.getMenuDocuments();
                        for (Map.Entry<String, MenuDocumentInfo> entry : componentDocs.entrySet()) {
                            String menuName = entry.getKey();
                            if (pendingDocuments.containsKey(menuName)) {
                                Debug.logWarning("Annotation menu [" + menuName +
                                        "] is defined more than once, most recent will over-write previous definition(s)", module);
                            }
                            pendingDocuments.put(menuName, entry.getValue());
                        }
                    } catch (Throwable t) { // SCIPIO: 4.0.0: was catch (Exception) - an Error must not
                        // silently abort the whole component loop
                        Debug.logError(t, "Error collecting annotation menu documents from component [" +
                                cri.getComponent().getGlobalName() + "]: " + t, module);
                    }
                }

                // Pass 2: Create ModelMenus from collected Documents
                // SCIPIO: 4.0.0: Sort by dependency order so base menus are created before extending menus
                pendingMenuDocuments = pendingDocuments; // SCIPIO: 4.0.0: allows on-demand builds while loading (see getAnnotationMenusByLocation)
                List<String> sortedMenuNames = sortMenusByDependency(pendingDocuments);

                for (String menuName : sortedMenuNames) {
                    MenuDocumentInfo docInfo = pendingDocuments.get(menuName);
                    if (builtMenuKeys.contains(menuName)) {
                        continue; // built on demand by a dependent menu
                    }
                    builtMenuKeys.add(menuName);
                    try {
                        // SCIPIO: 4.0.0: the map key is "sourceLocation#name" (unique); the real menu name comes from the definition
                        String realMenuName = (docInfo.menuDef != null && !docInfo.menuDef.name().isEmpty()) ? docInfo.menuDef.name() : menuName;
                        ModelMenu modelMenu = createModelMenu(docInfo.document, docInfo.sourceLocation, realMenuName);
                        if (modelMenu != null) {
                            menus.put(realMenuName, modelMenu); // name-keyed fallback only (last wins); location aliases hold every definition
                            // Register location aliases
                            registerMenuLocationAliases(locationAliases, modelMenu, docInfo.menuDef);
                        }
                    } catch (Throwable t) { // SCIPIO: 4.0.0: was catch (Exception) - an Error must not
                        // silently abort the whole menu creation loop
                        Debug.logError(t, "Error creating annotation menu [" + menuName + "] from class [" +
                                docInfo.sourceLocation + "]: " + t, module);
                    }
                }

                pendingMenuDocuments = null;
                Debug.logInfo("Loaded [" + menus.size() + "] annotation-based menus with [" +
                        locationAliases.size() + "] location aliases", module);
            } catch (Throwable t) { // SCIPIO: 4.0.0: was catch (Exception) - must catch Throwable so a
                // fatal Error is logged instead of escaping silently. Note annotationMenusLoaded is
                // already true at this point (set above), so unlike Form/Screen this does not cause a
                // rescan storm, but WITHOUT this catch the Error would still propagate silently to the
                // caller and the partial menu cache would never be flagged.
                Debug.logError(t, "FATAL: annotation menu loading aborted - " + t, module);
                Debug.logError("FATAL: annotation menu loading only partially completed - " +
                        "[" + menus.size() + "] menus collected before abort; " +
                        "some components were NOT scanned; see FATAL stack trace above", module);
            }
        }
    }

    /**
     * SCIPIO: 4.0.0: Sorts menus by dependency order using topological sort.
     *
     * <p>Menus that extend other menus (without extends-resource) depend on those menus
     * and must be created after them. This method ensures base menus are created first.</p>
     *
     * @param pendingDocuments Map of menu names to their document info
     * @return List of menu names in dependency order (base menus first)
     */
    /**
     * SCIPIO: 4.0.0: Resolves a bare menu name (extends without resource) to a pending-document key, preferring
     * a menu at the same location alias, then the same class file, then any menu with that name.
     */
    /** SCIPIO: 4.0.0: Resolves (location alias, menu name) to a pending-document key, or null. */
    private static String findPendingMenuKeyByLocation(Map<String, MenuDocumentInfo> pendingDocuments, String location, String name) {
        for (Map.Entry<String, MenuDocumentInfo> e : pendingDocuments.entrySet()) {
            MenuDocumentInfo d = e.getValue();
            if (d.menuDef == null || !name.equals(d.menuDef.name())) continue;
            if (location.equals(d.menuDef.location())) return e.getKey();
            for (String loc : d.menuDef.locations()) { if (location.equals(loc)) return e.getKey(); }
        }
        return null;
    }

    private static String findPendingMenuKeyByName(Map<String, MenuDocumentInfo> pendingDocuments, String name, MenuDocumentInfo from) {
        String fromLoc = (from != null && from.menuDef != null) ? from.menuDef.location() : null;
        String fromClass = (from != null && from.sourceLocation != null && from.sourceLocation.indexOf('$') > 0) ? from.sourceLocation.substring(0, from.sourceLocation.indexOf('$')) : from != null ? from.sourceLocation : null;
        String sameClass = null, any = null;
        for (Map.Entry<String, MenuDocumentInfo> e : pendingDocuments.entrySet()) {
            MenuDocumentInfo d = e.getValue();
            if (d.menuDef == null || !name.equals(d.menuDef.name())) continue;
            if (fromLoc != null && !fromLoc.isEmpty() && fromLoc.equals(d.menuDef.location())) return e.getKey();
            if (sameClass == null && fromClass != null && d.sourceLocation != null && d.sourceLocation.startsWith(fromClass)) sameClass = e.getKey();
            if (any == null) any = e.getKey();
        }
        return (sameClass != null) ? sameClass : any;
    }

    private static List<String> sortMenusByDependency(Map<String, MenuDocumentInfo> pendingDocuments) {
        // Build dependency graph: menu name -> name of menu it extends (if local)
        Map<String, String> dependsOn = new HashMap<>();
        for (Map.Entry<String, MenuDocumentInfo> entry : pendingDocuments.entrySet()) {
            String menuName = entry.getKey();
            MenuDocumentInfo docInfo = entry.getValue();
            if (docInfo.menuDef != null) {
                String extendsMenu = docInfo.menuDef.extendsMenu();
                String extendsResource = docInfo.menuDef.extendsResource();
                // Only track local dependencies (no external resource)
                if (extendsMenu != null && !extendsMenu.isEmpty()) {
                    // SCIPIO: 4.0.0: also order by extends with an explicit resource when that resource is the location
                    // alias of a pending annotation menu (e.g. component://common/widget/CommonMenus.xml); otherwise the
                    // extending menu is built before its parent is registered and fails to load
                    String depKey = (extendsResource == null || extendsResource.isEmpty())
                            ? findPendingMenuKeyByName(pendingDocuments, extendsMenu, docInfo)
                            : findPendingMenuKeyByLocation(pendingDocuments, extendsResource, extendsMenu);
                    if (depKey != null && !depKey.equals(menuName)) {
                        dependsOn.put(menuName, depKey);
                    }
                }
            }
        }

        // Topological sort using Kahn's algorithm
        List<String> result = new ArrayList<>();
        Set<String> processed = new HashSet<>();

        while (result.size() < pendingDocuments.size()) {
            boolean progress = false;
            for (String menuName : pendingDocuments.keySet()) {
                if (processed.contains(menuName)) {
                    continue;
                }
                String dep = dependsOn.get(menuName);
                // Process if no dependency or dependency already processed
                if (dep == null || processed.contains(dep)) {
                    result.add(menuName);
                    processed.add(menuName);
                    progress = true;
                }
            }
            // If no progress was made, we have a cycle - add remaining menus anyway
            if (!progress) {
                Debug.logWarning("Circular menu dependency detected, processing remaining menus in arbitrary order", module);
                for (String menuName : pendingDocuments.keySet()) {
                    if (!processed.contains(menuName)) {
                        result.add(menuName);
                        processed.add(menuName);
                    }
                }
            }
        }

        return result;
    }

    /**
     * SCIPIO: 4.0.0: Registers location aliases for an annotation-based menu.
     */
    private static void registerMenuLocationAliases(Map<String, Map<String, ModelMenu>> locationAliases,
                                                     ModelMenu menu, com.ilscipio.scipio.widget.def.menu.Menu menuDef) {
        if (menuDef == null || menu == null) {
            return;
        }
        // Single location alias
        String location = menuDef.location();
        if (location != null && !location.isEmpty()) {
            locationAliases
                    .computeIfAbsent(location, k -> new ConcurrentHashMap<>())
                    .put(menu.getName(), menu);
            Debug.logInfo("Registered menu [" + menu.getName() + "] as location alias for [" + location + "]", module);
        }
        // Multiple location aliases
        String[] locations = menuDef.locations();
        if (locations != null) {
            for (String loc : locations) {
                if (loc != null && !loc.isEmpty()) {
                    locationAliases
                            .computeIfAbsent(loc, k -> new ConcurrentHashMap<>())
                            .put(menu.getName(), menu);
                }
            }
        }
    }

    /**
     * SCIPIO: 4.0.0: Container for menu document info during two-pass loading.
     */
    public static class MenuDocumentInfo {
        public final Document document;
        public final String sourceLocation;
        public final com.ilscipio.scipio.widget.def.menu.Menu menuDef;

        public MenuDocumentInfo(Document document, String sourceLocation, com.ilscipio.scipio.widget.def.menu.Menu menuDef) {
            this.document = document;
            this.sourceLocation = sourceLocation;
            this.menuDef = menuDef;
        }
    }

    /**
     * SCIPIO: 4.0.0: Gets an annotation-based menu by name.
     */
    public static ModelMenu getAnnotationMenu(String menuName) {
        loadAnnotationMenus();
        ModelMenu menu = annotationMenuCache != null ? annotationMenuCache.get(menuName) : null;
        if (menu == null && buildPendingMenuByName(menuName)) { // SCIPIO: 4.0.0: on-demand build
            menu = annotationMenuCache != null ? annotationMenuCache.get(menuName) : null;
        }
        return menu;
    }

    /**
     * SCIPIO: 4.0.0: Gets an annotation-based menu by class resource and name.
     *
     * <p>This method supports on-demand loading from a specific class, similar to how
     * XML menus are loaded from files. If the menu is not in the cache, it will
     * attempt to load it directly from the specified class.</p>
     *
     * @param classResource The class:// resource (e.g., "class://com.example.Menus$MyMenu")
     * @param menuName The menu name to look for
     * @return The ModelMenu, or null if not found
     */
    public static ModelMenu getAnnotationMenuFromClass(String classResource, String menuName) {
        loadAnnotationMenus();

        // First check cache - verify source matches for class:// requests
        if (annotationMenuCache != null) {
            ModelMenu cached = annotationMenuCache.get(menuName);
            if (cached != null) {
                String cachedLocation = cached.getMenuLocation();
                if (cachedLocation != null && classResource.equals(cachedLocation)) {
                    return cached;
                }
                // Wrong source - fall through to on-demand loading
            }
        }

        // Not in cache - try to load on-demand from the class (like XML does)
        if (!isClassLocation(classResource)) {
            return null;
        }

        try {
            String className = getClassNameFromLocation(classResource);
            Class<?> menuClass = Class.forName(className);
            return loadMenuFromClass(menuClass, menuName);
        } catch (ClassNotFoundException e) {
            Debug.logError(e, "Could not find menu class: " + classResource, module);
            return null;
        } catch (Exception e) {
            Debug.logError(e, "Error loading menu [" + menuName + "] from class [" + classResource + "]", module);
            return null;
        }
    }

    /**
     * SCIPIO: 4.0.0: Loads a menu from a class on-demand.
     */
    private static ModelMenu loadMenuFromClass(Class<?> menuClass, String menuName) throws Exception {
        String sourceLocation = CLASS_LOCATION_PREFIX + menuClass.getName();

        // Check for @MenuList
        com.ilscipio.scipio.widget.def.menu.MenuList menuList =
                menuClass.getAnnotation(com.ilscipio.scipio.widget.def.menu.MenuList.class);
        if (menuList != null) {
            for (com.ilscipio.scipio.widget.def.menu.Menu menuDef : menuList.value()) {
                if (menuName.equals(menuDef.name())) {
                    return createMenuFromAnnotation(menuDef, menuClass, sourceLocation, menuName);
                }
            }
        }

        // Check for single @Menu
        com.ilscipio.scipio.widget.def.menu.Menu menuDef =
                menuClass.getAnnotation(com.ilscipio.scipio.widget.def.menu.Menu.class);
        if (menuDef != null && menuName.equals(menuDef.name())) {
            return createMenuFromAnnotation(menuDef, menuClass, sourceLocation, menuName);
        }

        // Check nested interfaces/classes (for Menus$NestedMenu pattern)
        for (Class<?> nestedClass : menuClass.getDeclaredClasses()) {
            com.ilscipio.scipio.widget.def.menu.Menu nestedMenuDef =
                    nestedClass.getAnnotation(com.ilscipio.scipio.widget.def.menu.Menu.class);
            if (nestedMenuDef != null && menuName.equals(nestedMenuDef.name())) {
                String nestedSourceLocation = CLASS_LOCATION_PREFIX + nestedClass.getName();
                return createMenuFromAnnotation(nestedMenuDef, nestedClass, nestedSourceLocation, menuName);
            }
        }

        // Menus from one source XML become sibling nested classes in a container class, so a
        // same-document reference resolved against one nested class must search the enclosing
        // container (covers include-elements/extends with no resource)
        Class<?> enclosingClass = menuClass.getEnclosingClass();
        if (enclosingClass != null) {
            return loadMenuFromClass(enclosingClass, menuName);
        }

        return null;
    }

    /**
     * SCIPIO: 4.0.0: Creates a menu from an annotation and caches it.
     */
    private static ModelMenu createMenuFromAnnotation(com.ilscipio.scipio.widget.def.menu.Menu menuDef,
                                                       Class<?> menuClass, String sourceLocation, String menuName) throws Exception {
        // Build synthetic XML document using MenuAnnotationReader's document builder
        MenuAnnotationReader reader = new MenuAnnotationReader(null);
        Document doc = reader.buildMenuDocument(menuDef, menuClass, sourceLocation);
        if (doc == null) {
            return null;
        }

        // Create ModelMenu
        ModelMenu modelMenu = createModelMenu(doc, sourceLocation, menuName);
        if (modelMenu != null && annotationMenuCache != null) {
            // Cache for future lookups
            annotationMenuCache.put(menuName, modelMenu);
        }
        return modelMenu;
    }

    /**
     * SCIPIO: 4.0.0: Gets the menu Element from a class:// resource.
     *
     * <p>This method builds a synthetic Document from the annotation and returns the
     * menu Element, which is needed by loadIncludedMenu() for processing includes.</p>
     *
     * @param classResource The class:// resource (e.g., "class://com.example.Menus$MyMenu")
     * @param menuName The menu name to look for
     * @return The menu Element, or null if not found
     */
    public static Element getMenuElementFromClass(String classResource, String menuName) {
        if (!isClassLocation(classResource)) {
            return null;
        }

        try {
            String className = getClassNameFromLocation(classResource);
            Class<?> menuClass = Class.forName(className);
            return getMenuElementFromClass(menuClass, menuName);
        } catch (ClassNotFoundException e) {
            Debug.logError(e, "Could not find menu class: " + classResource, module);
            return null;
        } catch (Exception e) {
            Debug.logError(e, "Error getting menu element [" + menuName + "] from class [" + classResource + "]", module);
            return null;
        }
    }

    /**
     * SCIPIO: 4.0.0: Gets the menu Element from a menu class.
     */
    private static Element getMenuElementFromClass(Class<?> menuClass, String menuName) throws Exception {
        String sourceLocation = CLASS_LOCATION_PREFIX + menuClass.getName();
        MenuAnnotationReader reader = new MenuAnnotationReader(null);

        // Check for @MenuList
        com.ilscipio.scipio.widget.def.menu.MenuList menuList =
                menuClass.getAnnotation(com.ilscipio.scipio.widget.def.menu.MenuList.class);
        if (menuList != null) {
            for (com.ilscipio.scipio.widget.def.menu.Menu menuDef : menuList.value()) {
                if (menuName.equals(menuDef.name())) {
                    Document doc = reader.buildMenuDocument(menuDef, menuClass, sourceLocation);
                    // Set resource location for WidgetDocumentInfo
                    WidgetDocumentInfo.retrieveAlways(doc).setResourceLocation(sourceLocation);
                    return findMenuElement(doc, menuName);
                }
            }
        }

        // Check for single @Menu
        com.ilscipio.scipio.widget.def.menu.Menu menuDef =
                menuClass.getAnnotation(com.ilscipio.scipio.widget.def.menu.Menu.class);
        if (menuDef != null && menuName.equals(menuDef.name())) {
            Document doc = reader.buildMenuDocument(menuDef, menuClass, sourceLocation);
            // Set resource location for WidgetDocumentInfo
            WidgetDocumentInfo.retrieveAlways(doc).setResourceLocation(sourceLocation);
            return findMenuElement(doc, menuName);
        }

        // Check nested interfaces/classes (for Menus$NestedMenu pattern)
        for (Class<?> nestedClass : menuClass.getDeclaredClasses()) {
            com.ilscipio.scipio.widget.def.menu.Menu nestedMenuDef =
                    nestedClass.getAnnotation(com.ilscipio.scipio.widget.def.menu.Menu.class);
            if (nestedMenuDef != null && menuName.equals(nestedMenuDef.name())) {
                String nestedSourceLocation = CLASS_LOCATION_PREFIX + nestedClass.getName();
                Document doc = reader.buildMenuDocument(nestedMenuDef, nestedClass, nestedSourceLocation);
                // Set resource location for WidgetDocumentInfo
                WidgetDocumentInfo.retrieveAlways(doc).setResourceLocation(nestedSourceLocation);
                return findMenuElement(doc, menuName);
            }
        }

        // Menus from one source XML become sibling nested classes in a container class, so a
        // same-document reference resolved against one nested class must search the enclosing
        // container (covers include-elements/extends with no resource)
        Class<?> enclosingClass = menuClass.getEnclosingClass();
        if (enclosingClass != null) {
            return getMenuElementFromClass(enclosingClass, menuName);
        }

        return null;
    }

    /**
     * SCIPIO: 4.0.0: Finds a menu element by name in a document.
     */
    private static Element findMenuElement(Document doc, String menuName) {
        if (doc == null) {
            return null;
        }
        Element rootElement = doc.getDocumentElement();
        if (rootElement == null) {
            return null;
        }
        // Check if root is "menus" or directly a "menu"
        if ("menus".equalsIgnoreCase(rootElement.getTagName())) {
            for (Element menuElem : UtilXml.childElementList(rootElement, "menu")) {
                if (menuName.equals(menuElem.getAttribute("name"))) {
                    return menuElem;
                }
            }
        } else if ("menu".equalsIgnoreCase(rootElement.getTagName())) {
            if (menuName.equals(rootElement.getAttribute("name"))) {
                return rootElement;
            }
        }
        return null;
    }

    /**
     * SCIPIO: 4.0.0: Gets all annotation-based menus.
     */
    public static Map<String, ModelMenu> getAnnotationMenus() {
        loadAnnotationMenus();
        return annotationMenuCache != null ? annotationMenuCache : new HashMap<>();
    }

    /**
     * SCIPIO: 4.0.0: Gets all annotation-based menus from a specific location.
     * Uses the location aliases map populated during annotation loading.
     */
    public static Map<String, ModelMenu> getAnnotationMenusByLocation(String location) {
        buildPendingMenusForLocation(location); // SCIPIO: 4.0.0: on-demand build while loading
        loadAnnotationMenus();
        if (menuLocationAliases == null) {
            Debug.logWarning("getAnnotationMenusByLocation: menuLocationAliases is null for [" + location + "]", module);
            return new HashMap<>();
        }
        Map<String, ModelMenu> menusAtLocation = menuLocationAliases.get(location);
        if (menusAtLocation == null) {
            Debug.logInfo("getAnnotationMenusByLocation: No menus found for location [" + location +
                    "], available locations: " + menuLocationAliases.keySet(), module);
            return new HashMap<>();
        }
        Debug.logInfo("getAnnotationMenusByLocation: Found [" + menusAtLocation.size() +
                "] menus for location [" + location + "]: " + menusAtLocation.keySet(), module);
        return new HashMap<>(menusAtLocation);
    }

    /**
     * SCIPIO: 4.0.0: Gets a menu from a location alias.
     *
     * <p>This allows component:// style URLs to resolve to annotation-based menus
     * when the @Menu annotation specifies a location or locations attribute.</p>
     *
     * @param location The component:// style location (e.g., "component://setup/widget/Menus.xml")
     * @param menuName The menu name within that location
     * @return The ModelMenu, or null if no alias is registered for this location/name
     */
    public static ModelMenu getMenuFromLocationAlias(String location, String menuName) {
        // SCIPIO: 4.0.0: Load annotation menus to populate location aliases
        // The double-check locking in loadAnnotationMenus() prevents infinite recursion
        loadAnnotationMenus();
        if (menuLocationAliases == null) {
            return null;
        }
        Map<String, ModelMenu> menusAtLocation = menuLocationAliases.get(location);
        ModelMenu menu = (menusAtLocation != null) ? menusAtLocation.get(menuName) : null;
        if (menu == null && buildPendingMenu(location, menuName)) { // SCIPIO: 4.0.0: on-demand build
            menusAtLocation = menuLocationAliases.get(location);
            menu = (menusAtLocation != null) ? menusAtLocation.get(menuName) : null;
        }
        return menu;
    }

    /**
     * SCIPIO: 4.0.0: Gets a menu Element from a location with annotation fallback.
     *
     * <p>This method is used by SubMenu include resolution when the XML file is missing.
     * It tries:
     * <ol>
     *   <li>Location aliases - annotation menus registered with matching location</li>
     *   <li>Annotation menu by name - fallback search by menu name only</li>
     * </ol>
     * </p>
     *
     * @param resourceName The component:// style location
     * @param menuName The menu name to find
     * @return The menu Element, or null if not found
     */
    public static Element getMenuElementFromLocation(String resourceName, String menuName) {
        // SCIPIO: 4.0.0: include-elements/include-menu-items only need the DOM element; serve it from the pending
        // annotation document without building the ModelMenu (avoids build cycles between sibling menus)
        Map<String, MenuDocumentInfo> pending = pendingMenuDocuments;
        if (pending != null && resourceName != null && menuName != null) {
            String key = findPendingMenuKeyByLocation(pending, resourceName, menuName);
            if (key != null && pending.get(key) != null && pending.get(key).document != null) {
                Element pendingElem = findMenuElement(pending.get(key).document, menuName);
                if (pendingElem != null) {
                    return pendingElem;
                }
            }
        }
        // 1. Check location aliases first
        ModelMenu aliasMenu = getMenuFromLocationAlias(resourceName, menuName);
        if (aliasMenu != null) {
            return getMenuElementForModel(aliasMenu, menuName);
        }

        // 2. Fallback to annotation menu by name
        ModelMenu annMenu = getAnnotationMenu(menuName);
        if (annMenu != null) {
            return getMenuElementForModel(annMenu, menuName);
        }

        return null;
    }

    /**
     * SCIPIO: 4.0.0: Gets the menu Element for a ModelMenu.
     *
     * <p>Builds a synthetic XML Element from the annotation-based ModelMenu.</p>
     */
    private static Element getMenuElementForModel(ModelMenu modelMenu, String menuName) {
        if (modelMenu == null) {
            return null;
        }
        String menuLocation = modelMenu.getMenuLocation();
        if (menuLocation != null && menuLocation.startsWith(CLASS_LOCATION_PREFIX)) {
            return getMenuElementFromClass(menuLocation, menuName);
        }
        return null;
    }

    /**
     * SCIPIO: 4.0.0: Checks if a location has any registered menu aliases.
     */
    public static boolean hasLocationAlias(String location) {
        loadAnnotationMenus();
        return menuLocationAliases != null && menuLocationAliases.containsKey(location);
    }

    /**
     * SCIPIO: 4.0.0: Creates a ModelMenu from a synthetic XML document.
     * Used by MenuAnnotationReader to create menus from annotations.
     */
    public static ModelMenu createModelMenu(Document menuFileDoc, String menuLocation, String menuName) {
        if (menuFileDoc == null) {
            return null;
        }
        // Save original location as user data in Document
        WidgetDocumentInfo.retrieveAlways(menuFileDoc).setResourceLocation(menuLocation);
        Map<String, ModelMenu> menuMap = readMenuDocument(menuFileDoc, menuLocation);
        return menuMap.get(menuName);
    }

    public static ModelMenu getMenuFromWebappContext(String resourceName, String menuName, HttpServletRequest request)
            throws IOException, SAXException, ParserConfigurationException {
        String webappName = UtilHttp.getApplicationName(request);
        String cacheKey = webappName + "::" + resourceName;

        Map<String, ModelMenu> modelMenuMap = menuWebappCache.get(cacheKey);
        if (modelMenuMap == null) {
            // SCIPIO: refactored
            synchronized (MenuFactory.class) {
                modelMenuMap = menuWebappCache.get(cacheKey);
                if (modelMenuMap == null) {
                    ServletContext servletContext = request.getServletContext(); // SCIPIO: get context using servlet API 3.0
                    URL menuFileUrl = servletContext.getResource(resourceName);
                    if (menuFileUrl == null) {
                        throw new IllegalArgumentException("Could not resolve menu file location [" + resourceName + "] in the webapp [" + webappName + "]");
                    }
                    Document menuFileDoc = UtilXml.readXmlDocument(menuFileUrl, true, true);
                    if (menuFileDoc == null) {
                        throw new IllegalArgumentException("Could not read menu file at location [" + resourceName + "] in the webapp [" + webappName + "]");
                    }
                    // SCIPIO: New: Save original location as user data in Document
                    WidgetDocumentInfo.retrieveAlways(menuFileDoc).setResourceLocation(resourceName);
                    modelMenuMap = readMenuDocument(menuFileDoc, cacheKey);
                    menuWebappCache.put(cacheKey, modelMenuMap);
                }
            }
        }

        ModelMenu modelMenu = modelMenuMap.get(menuName);
        if (modelMenu == null) {
            throw new IllegalArgumentException("Could not find menu with name [" + menuName + "] in webapp resource [" + resourceName + "] in the webapp [" + webappName + "]");
        }
        return modelMenu;
    }

    public static Map<String, ModelMenu> readMenuDocument(Document menuFileDoc, String menuLocation) {
        Map<String, ModelMenu> modelMenuMap = new HashMap<>();
        if (menuFileDoc != null) {
            // read document and construct ModelMenu for each menu element
            Element rootElement = menuFileDoc.getDocumentElement();
            if (!"menus".equalsIgnoreCase(rootElement.getTagName())) {
                rootElement = UtilXml.firstChildElement(rootElement, "menus");
            }
            for (Element menuElement: UtilXml.childElementList(rootElement, "menu")){
                ModelMenu modelMenu = new ModelMenu(menuElement, menuLocation);
                modelMenuMap.put(modelMenu.getName(), modelMenu);
            }
         }
        return modelMenuMap;
    }

    /**
     * Gets widget from location or exception.
     * <p>
     * SCIPIO: now delegating.
     */
    public static ModelMenu getMenuFromLocation(String resourceName, String menuName) throws IOException, SAXException, ParserConfigurationException {
        Debug.logInfo("getMenuFromLocation: Requested [" + resourceName + "#" + menuName + "]", module);
        // SCIPIO: 4.0.0: Handle annotation-based menus with class:// prefix
        // Uses on-demand loading - if menu not in cache, load it directly from the class
        if (isClassLocation(resourceName)) {
            ModelMenu modelMenu = getAnnotationMenuFromClass(resourceName, menuName);
            if (modelMenu == null) {
                throw new IllegalArgumentException("Could not find annotation-based menu with name [" + menuName + "] from class [" + resourceName + "]");
            }
            Debug.logInfo("getMenuFromLocation: Found via class:// location [" + menuName + "]", module);
            return modelMenu;
        }
        ModelMenu modelMenu = getMenuFromLocationOrNull(resourceName, menuName);
        if (modelMenu == null) {
            // SCIPIO: 4.0.0: Final fallback - try to find menu by name in all annotation menus
            // This handles cases where XML was deleted and the alias lookup fails due to loading order
            modelMenu = getAnnotationMenu(menuName);
            if (modelMenu != null) {
                Debug.logWarning("Menu [" + menuName + "] not found at [" + resourceName +
                    "] but found as annotation menu; consider updating the reference to use class:// location", module);
                return modelMenu;
            }
            throw new IllegalArgumentException("Could not find menu with name [" + menuName + "] in location [" + resourceName + "]");
        }
        Debug.logInfo("getMenuFromLocation: Found [" + menuName + "] from [" + resourceName + "]", module);
        return modelMenu;
    }

    /**
     * SCIPIO: Gets widget from location or null if name not within the location.
     */
    public static ModelMenu getMenuFromLocationOrNull(String resourceName, String menuName) throws IOException, SAXException, ParserConfigurationException {
        // SCIPIO: 4.0.0: Handle annotation-based menus with class:// prefix
        // Uses on-demand loading - if menu not in cache, load it directly from the class
        if (isClassLocation(resourceName)) {
            return getAnnotationMenuFromClass(resourceName, menuName);
        }
        Map<String, ModelMenu> modelMenuMap = menuLocationCache.get(resourceName);
        if (modelMenuMap == null) {
            // SCIPIO: refactored
            synchronized (MenuFactory.class) {
                modelMenuMap = menuLocationCache.get(resourceName);
                if (modelMenuMap == null) {
                    // SCIPIO: 4.0.0: Use unified WidgetLocationResolver for hashtag handling and fallback logic
                    // Try XML file first, then fall back to annotation-based menus if file doesn't exist
                    URL menuFileUrl = WidgetLocationResolver.resolveWidgetLocation(resourceName, "menu");

                    // SCIPIO: 4.0.0: If no XML file, try annotation-based menus by location alias
                    if (menuFileUrl == null) {
                        // SCIPIO: 4.0.0: annotation-only location: resolve the single requested menu (built on demand)
                        // and never snapshot the alias map into menuLocationCache (menus register incrementally)
                        if (hasLocationAlias(resourceName) || pendingMenuDocuments != null) {
                            ModelMenu aliasMenu = getMenuFromLocationAlias(resourceName, menuName);
                            if (aliasMenu != null) {
                                return aliasMenu;
                            }
                        }
                        Map<String, ModelMenu> annotationMenusByLocation = getAnnotationMenusByLocation(resourceName);
                        if (!annotationMenusByLocation.isEmpty()) {
                            Debug.logInfo("Menu location [" + resourceName + "] not found as file, using [" +
                                    annotationMenusByLocation.size() + "] annotation-based menu(s)", module);
                            return annotationMenusByLocation.get(menuName); // SCIPIO: 4.0.0: no snapshot caching
                        }
                        throw new IllegalArgumentException("Could not resolve menu file location [" + resourceName + "]");
                    }
                    Document menuFileDoc = UtilXml.readXmlDocument(menuFileUrl, true, true);
                    if (menuFileDoc == null) {
                        throw new IllegalArgumentException("Could not read menu file at location [" + resourceName + "]");
                    }
                    // SCIPIO: New: Save original location as user data in Document
                    WidgetDocumentInfo.retrieveAlways(menuFileDoc).setResourceLocation(resourceName);
                    modelMenuMap = readMenuDocument(menuFileDoc, resourceName);
                    menuLocationCache.put(resourceName, modelMenuMap);
                }
            }
        }
        // SCIPIO: 4.0.0: First try to get from the loaded map
        ModelMenu modelMenu = modelMenuMap.get(menuName);
        if (modelMenu != null) {
            return modelMenu;
        }

        // SCIPIO: 4.0.0: If not found in XML file, try location aliases as fallback
        // This handles the case where an XML file exists but the menu was migrated to annotations
        ModelMenu aliasMenu = getMenuFromLocationAlias(resourceName, menuName);
        if (aliasMenu != null) {
            Debug.logInfo("Menu [" + menuName + "] not found in XML file [" + resourceName +
                    "], using annotation-based menu from location alias", module);
            return aliasMenu;
        }

        return null;
    }

    @Override
    public ModelMenu getWidgetFromLocation(ModelLocation modelLoc) throws IOException, IllegalArgumentException { // SCIPIO
        try {
            return getMenuFromLocation(modelLoc.getResource(), modelLoc.getName());
        } catch (SAXException e) {
            throw new IOException(e);
        } catch (ParserConfigurationException e) {
            throw new IOException(e);
        }
    }

    @Override
    public ModelMenu getWidgetFromLocationOrNull(ModelLocation modelLoc) throws IOException { // SCIPIO
        try {
            return getMenuFromLocationOrNull(modelLoc.getResource(), modelLoc.getName());
        } catch (SAXException e) {
            throw new IOException(e);
        } catch (ParserConfigurationException e) {
            throw new IOException(e);
        }
    }
}
