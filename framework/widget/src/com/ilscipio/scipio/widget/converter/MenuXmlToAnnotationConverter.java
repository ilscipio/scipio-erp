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
package com.ilscipio.scipio.widget.converter;

import java.io.File;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import com.ilscipio.scipio.widget.def.condition.impl.*;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

/**
 * Converts Menu XML definitions to Java annotation source code.
 *
 * <p>SCIPIO: 4.0.0: Added for XML-to-Annotation conversion support.</p>
 */
public class MenuXmlToAnnotationConverter extends XmlToAnnotationConverter {

    private int scriptCounter = 0;

    /**
     * SCIPIO: 4.0.0: Maps XML menu name -> resolved (collision-disambiguated) Java
     * interface name, for the document currently being converted. Populated up
     * front in {@link #convert(Document)} so that same-file self-references
     * (include/model without an external resource) use the SAME identifier that
     * ends up on the actual {@code public interface} declaration, even when that
     * declaration had to be renamed to avoid a case-insensitive collision.
     */
    private final Map<String, String> menuNameToInterfaceName = new HashMap<>();

    public MenuXmlToAnnotationConverter(String packageName, String className, String componentName,
                                        File outputDir, File scriptOutputDir) {
        super(packageName, className, componentName, outputDir, scriptOutputDir);
    }

    @Override
    protected String getImports() {
        // Import menu annotations, screen annotations (for action types like ScriptAction, SetAction, etc.),
        // and condition annotations for functional conditions
        return "import com.ilscipio.scipio.widget.def.menu.*;" + NEWLINE +
               "import com.ilscipio.scipio.widget.def.screen.*;" + NEWLINE +
               "import com.ilscipio.scipio.widget.def.condition.Condition;" + NEWLINE +
               "import com.ilscipio.scipio.widget.def.condition.ConditionNode;" + NEWLINE +
               "import com.ilscipio.scipio.widget.def.condition.NestedCondition;" + NEWLINE +
               "import com.ilscipio.scipio.widget.def.condition.NestedCondition2;" + NEWLINE +
               "import com.ilscipio.scipio.widget.def.condition.impl.*;" + NEWLINE;
    }

    @Override
    public String convert(Document doc) {
        StringBuilder sb = new StringBuilder();

        sb.append(generateClassHeader());
        sb.append(NEWLINE);

        // SCIPIO: 4.0.0: Reset case-insensitive interface-name collision tracking for this class
        resetInterfaceNameTracking();
        menuNameToInterfaceName.clear();

        Element root = doc.getDocumentElement();
        List<Element> menuElements = childElementList(root, "menu");

        // SCIPIO: 4.0.0: Sort menus by dependency order (base menus before extending menus)
        List<Element> sortedMenus = sortMenusByDependency(menuElements);

        // SCIPIO: 4.0.0: Pre-resolve every menu's interface name up front (in emission order) so
        // that same-file include/model self-references below can reuse the SAME disambiguated
        // name as the actual declaration, instead of independently re-deriving it.
        for (Element menuElement : sortedMenus) {
            String menuName = getAttr(menuElement, "name");
            if (isNotEmpty(menuName)) {
                menuNameToInterfaceName.put(menuName, resolveUniqueInterfaceName(toInterfaceName(menuName)));
            }
        }

        for (Element menuElement : sortedMenus) {
            sb.append(convertMenu(menuElement));
            sb.append(NEWLINE);
        }

        sb.append(generateClassFooter());

        return sb.toString();
    }

    /**
     * SCIPIO: 4.0.0: Sorts menu elements by dependency order using topological sort.
     * Menus that are extended by others come first.
     */
    protected List<Element> sortMenusByDependency(List<Element> menuElements) {
        // Build name -> element map
        Map<String, Element> menuMap = new LinkedHashMap<>();
        for (Element elem : menuElements) {
            String name = getAttr(elem, "name");
            if (isNotEmpty(name)) {
                menuMap.put(name, elem);
            }
        }

        // Build dependency graph (menu -> list of menus it depends on)
        Map<String, String> dependsOn = new HashMap<>();
        for (Element elem : menuElements) {
            String name = getAttr(elem, "name");
            String extendsMenu = getAttr(elem, "extends");
            String extendsResource = getAttr(elem, "extends-resource");
            // Only track local dependencies (same file)
            if (isNotEmpty(name) && isNotEmpty(extendsMenu) && !isNotEmpty(extendsResource)) {
                dependsOn.put(name, extendsMenu);
            }
        }

        // Topological sort using Kahn's algorithm
        List<Element> result = new ArrayList<>();
        Set<String> processed = new HashSet<>();

        // Process menus with no local dependencies first, then their dependents
        while (result.size() < menuElements.size()) {
            boolean progress = false;
            for (Element elem : menuElements) {
                String name = getAttr(elem, "name");
                if (processed.contains(name)) continue;

                String dep = dependsOn.get(name);
                // Can process if: no dependency, dependency is external, or dependency already processed
                if (dep == null || !menuMap.containsKey(dep) || processed.contains(dep)) {
                    result.add(elem);
                    processed.add(name);
                    progress = true;
                }
            }
            // If no progress, there's a cycle - just add remaining in original order
            if (!progress) {
                for (Element elem : menuElements) {
                    String name = getAttr(elem, "name");
                    if (!processed.contains(name)) {
                        result.add(elem);
                        processed.add(name);
                    }
                }
            }
        }

        return result;
    }

    /**
     * Converts a single menu element to annotations.
     */
    protected String convertMenu(Element menuElement) {
        StringBuilder sb = new StringBuilder();
        String menuName = getAttr(menuElement, "name");
        scriptCounter = 0;

        // Generate @Menu annotation
        sb.append(indent(1)).append(generateMenuAnnotation(menuElement, menuName));
        sb.append(NEWLINE);

        // Generate interface declaration
        // SCIPIO: 4.0.0: Use the pre-resolved (collision-disambiguated) name so the declaration
        // matches what same-file include/model self-references point to; falls back to a fresh
        // resolution if this menu wasn't in the pre-pass (e.g. empty name was filtered out there).
        String menuInterfaceName = menuNameToInterfaceName.get(menuName);
        if (menuInterfaceName == null) {
            menuInterfaceName = resolveUniqueInterfaceName(toInterfaceName(menuName));
        }
        sb.append(indent(1)).append("public interface ").append(menuInterfaceName).append(" {}");
        sb.append(NEWLINE);

        return sb.toString();
    }

    /**
     * Generates the @Menu annotation with all attributes.
     */
    protected String generateMenuAnnotation(Element menuElement, String menuName) {
        StringBuilder sb = new StringBuilder();
        sb.append("@Menu(").append(NEWLINE);

        List<String> attrs = new ArrayList<>();

        // Required attributes
        attrs.add(indent(2) + attrIfNotEmpty("name", menuName));

        // SCIPIO: 4.0.0: Add location attribute for backward compatibility with XML references
        if (isNotEmpty(sourceLocation)) {
            attrs.add(indent(2) + attrIfNotEmpty("location", sourceLocation));
        }

        // Menu type
        String type = getAttr(menuElement, "type");
        if (isNotEmpty(type) && !"simple".equals(type)) {
            attrs.add(indent(2) + "type = MenuType." + type.toUpperCase());
        }

        // Basic attributes
        addAttrIfNotEmpty(attrs, menuElement, "id", "id");
        addAttrIfNotEmpty(attrs, menuElement, "title", "title");
        addAttrIfNotEmpty(attrs, menuElement, "title-style", "titleStyle");
        addAttrIfNotEmpty(attrs, menuElement, "tooltip", "tooltip");
        addAttrIfNotEmpty(attrs, menuElement, "default-entity-name", "defaultEntityName");

        // Default styles
        addAttrIfNotEmpty(attrs, menuElement, "default-title-style", "defaultTitleStyle");
        addAttrIfNotEmpty(attrs, menuElement, "default-widget-style", "defaultWidgetStyle");
        addAttrIfNotEmpty(attrs, menuElement, "default-link-style", "defaultLinkStyle");
        addAttrIfNotEmpty(attrs, menuElement, "default-tooltip-style", "defaultTooltipStyle");
        addAttrIfNotEmpty(attrs, menuElement, "default-selected-style", "defaultSelectedStyle");
        addAttrIfNotEmpty(attrs, menuElement, "default-selected-ancestor-style", "defaultSelectedAncestorStyle");
        addAttrIfNotEmpty(attrs, menuElement, "default-align-style", "defaultAlignStyle");
        addAttrIfNotEmpty(attrs, menuElement, "default-disabled-title-style", "defaultDisabledTitleStyle");

        // Layout
        String orientation = getAttr(menuElement, "orientation");
        if (isNotEmpty(orientation) && !"horizontal".equals(orientation)) {
            attrs.add(indent(2) + "orientation = Orientation." + orientation.toUpperCase());
        }

        String defaultAlign = getAttr(menuElement, "default-align");
        if (isNotEmpty(defaultAlign) && !"left".equals(defaultAlign)) {
            attrs.add(indent(2) + "defaultAlign = Align." + defaultAlign.toUpperCase());
        }

        addAttrIfNotEmpty(attrs, menuElement, "menu-width", "menuWidth");
        addAttrIfNotEmpty(attrs, menuElement, "default-cell-width", "defaultCellWidth");
        addAttrIfNotEmpty(attrs, menuElement, "menu-container-style", "menuContainerStyle");
        addAttrIfNotEmpty(attrs, menuElement, "fill-style", "fillStyle");
        addAttrIfNotEmpty(attrs, menuElement, "extra-index", "extraIndex");

        // Extension
        addAttrIfNotEmpty(attrs, menuElement, "extends", "extendsMenu");
        addAttrIfNotEmpty(attrs, menuElement, "extends-resource", "extendsResource");

        // SCIPIO: 4.0.0: Process include-elements
        List<Element> includeElementsElems = childElementList(menuElement, "include-elements");
        if (!includeElementsElems.isEmpty()) {
            StringBuilder includeBuilder = new StringBuilder();
            includeBuilder.append(indent(2)).append("includeElements = {").append(NEWLINE);
            boolean first = true;
            for (Element includeElem : includeElementsElems) {
                if (!first) includeBuilder.append(",").append(NEWLINE);
                includeBuilder.append(indent(3)).append(generateIncludeElements(includeElem));
                first = false;
            }
            includeBuilder.append(NEWLINE).append(indent(2)).append("}");
            attrs.add(includeBuilder.toString());
        }

        // Selection
        addAttrIfNotEmpty(attrs, menuElement, "default-menu-item-name", "defaultMenuItemName");
        addAttrIfNotEmpty(attrs, menuElement, "default-associated-content-id", "defaultAssociatedContentId");
        addBoolAttr(attrs, menuElement, "default-hide-if-selected", "defaultHideIfSelected", false);
        addAttrIfNotEmpty(attrs, menuElement, "selected-menuitem-context-field-name", "selectedMenuItemContextFieldName");
        addAttrIfNotEmpty(attrs, menuElement, "selected-menu-context-field-name", "selectedMenuContextFieldName");

        // Permissions
        addAttrIfNotEmpty(attrs, menuElement, "default-permission-operation", "defaultPermissionOperation");
        addAttrIfNotEmpty(attrs, menuElement, "default-permission-entity-action", "defaultPermissionEntityAction");

        // SCIPIO-specific
        addAttrIfNotEmpty(attrs, menuElement, "items-sort-mode", "itemsSortMode");
        addAttrIfNotEmpty(attrs, menuElement, "auto-sub-menu-names", "autoSubMenuNames");
        addAttrIfNotEmpty(attrs, menuElement, "default-sub-menu-model-scope", "defaultSubMenuModelScope");
        addAttrIfNotEmpty(attrs, menuElement, "default-sub-menu-include-scope", "defaultSubMenuIncludeScope");
        addAttrIfNotEmpty(attrs, menuElement, "always-expand-selected-or-ancestor", "alwaysExpandSelectedOrAncestor");
        addAttrIfNotEmpty(attrs, menuElement, "separate-menu-type", "separateMenuType");
        addAttrIfNotEmpty(attrs, menuElement, "separate-menu-target-style", "separateMenuTargetStyle");
        addAttrIfNotEmpty(attrs, menuElement, "item-condition-mode", "itemConditionMode");
        addAttrIfNotEmpty(attrs, menuElement, "force-extends-sub-menu-model-scope", "forceExtendsSubMenuModelScope");
        addAttrIfNotEmpty(attrs, menuElement, "force-all-sub-menu-model-scope", "forceAllSubMenuModelScope");
        addAttrIfNotEmpty(attrs, menuElement, "separate-menu-target-preference", "separateMenuTargetPreference");
        addAttrIfNotEmpty(attrs, menuElement, "separate-menu-target-original-action", "separateMenuTargetOriginalAction");

        // Process actions
        Element actionsElement = firstChildElement(menuElement, "actions");
        if (actionsElement != null) {
            String actionsCode = generateMenuActions(actionsElement, menuName);
            if (isNotEmpty(actionsCode)) {
                attrs.add(indent(2) + "actions = " + actionsCode);
            }
        }

        // Process menu items
        List<Element> menuItemElements = childElementList(menuElement, "menu-item");
        if (!menuItemElements.isEmpty()) {
            StringBuilder itemsBuilder = new StringBuilder();
            itemsBuilder.append(indent(2)).append("items = {").append(NEWLINE);
            boolean first = true;
            for (Element item : menuItemElements) {
                if (!first) itemsBuilder.append(",").append(NEWLINE);
                itemsBuilder.append(indent(3)).append(generateMenuItem(item, menuName));
                first = false;
            }
            itemsBuilder.append(NEWLINE).append(indent(2)).append("}");
            attrs.add(itemsBuilder.toString());
        }

        // Join all attributes
        sb.append(String.join("," + NEWLINE, attrs));
        sb.append(NEWLINE).append(indent(1)).append(")");

        return sb.toString();
    }

    /**
     * Helper to add string attribute if not empty.
     */
    protected void addAttrIfNotEmpty(List<String> attrs, Element element, String xmlAttr, String annotationAttr) {
        String value = getAttr(element, xmlAttr);
        if (isNotEmpty(value)) {
            attrs.add(indent(2) + annotationAttr + " = " + toStringValue(value));
        }
    }

    /**
     * Helper to add boolean attribute if different from default.
     */
    protected void addBoolAttr(List<String> attrs, Element element, String xmlAttr, String annotationAttr, boolean defaultValue) {
        String value = getAttr(element, xmlAttr);
        if (isNotEmpty(value)) {
            boolean boolVal = "true".equalsIgnoreCase(value) || "Y".equalsIgnoreCase(value);
            if (boolVal != defaultValue) {
                attrs.add(indent(2) + annotationAttr + " = " + boolVal);
            }
        }
    }

    /**
     * Generates @MenuActions annotation.
     * Groups actions by type according to MenuActions annotation structure.
     */
    protected String generateMenuActions(Element actionsElement, String menuName) {
        List<String> setActions = new ArrayList<>();
        List<String> scriptActions = new ArrayList<>();
        List<String> entityOneActions = new ArrayList<>();
        List<String> entityConditionActions = new ArrayList<>();
        List<String> serviceActions = new ArrayList<>();
        List<String> propertyToFieldActions = new ArrayList<>();

        for (Element child : childElementList(actionsElement)) {
            String tagName = child.getNodeName();
            switch (tagName) {
                case "set":
                    setActions.add(generateSetAction(child));
                    break;
                case "script":
                    scriptActions.add(generateScriptAction(child, menuName));
                    break;
                case "entity-one":
                    entityOneActions.add(generateEntityOneAction(child));
                    break;
                case "entity-and":
                case "entity-condition":
                    // Both entity-and and entity-condition map to @EntityConditionAction
                    entityConditionActions.add(generateEntityConditionAction(child));
                    break;
                case "service":
                    serviceActions.add(generateServiceAction(child));
                    break;
                case "property-to-field":
                    propertyToFieldActions.add(generatePropertyToFieldAction(child));
                    break;
            }
        }

        List<String> attrs = new ArrayList<>();
        if (!setActions.isEmpty()) {
            attrs.add("set = {" + String.join(", ", setActions) + "}");
        }
        if (!scriptActions.isEmpty()) {
            attrs.add("script = {" + String.join(", ", scriptActions) + "}");
        }
        if (!entityOneActions.isEmpty()) {
            attrs.add("entityOne = {" + String.join(", ", entityOneActions) + "}");
        }
        if (!entityConditionActions.isEmpty()) {
            attrs.add("entityCondition = {" + String.join(", ", entityConditionActions) + "}");
        }
        if (!serviceActions.isEmpty()) {
            attrs.add("service = {" + String.join(", ", serviceActions) + "}");
        }
        if (!propertyToFieldActions.isEmpty()) {
            attrs.add("propertyToField = {" + String.join(", ", propertyToFieldActions) + "}");
        }

        if (attrs.isEmpty()) {
            return "";
        }

        return "@MenuActions(" + joinAttrs(attrs.toArray(new String[0])) + ")";
    }

    protected String generatePropertyToFieldAction(Element element) {
        String resource = getAttr(element, "resource");
        String property = getAttr(element, "property");
        String field = getAttr(element, "field");
        String defaultValue = getAttr(element, "default");
        String noLocale = getAttr(element, "no-locale");
        String argListName = getAttr(element, "arg-list-name");

        List<String> attrs = new ArrayList<>();
        if (isNotEmpty(resource)) attrs.add(attrIfNotEmpty("resource", resource));
        if (isNotEmpty(property)) attrs.add(attrIfNotEmpty("property", property));
        if (isNotEmpty(field)) attrs.add(attrIfNotEmpty("field", field));
        if (isNotEmpty(defaultValue)) attrs.add(attrIfNotEmpty("defaultValue", defaultValue));
        if ("true".equalsIgnoreCase(noLocale)) attrs.add("noLocale = true");
        if (isNotEmpty(argListName)) attrs.add(attrIfNotEmpty("argListName", argListName));

        return "@PropertyToFieldAction(" + joinAttrs(attrs.toArray(new String[0])) + ")";
    }

    /**
     * Generates a single action annotation.
     */
    protected String generateActionAnnotation(Element element, String tagName, String menuName) {
        switch (tagName) {
            case "set":
                return generateSetAction(element);
            case "script":
                return generateScriptAction(element, menuName);
            case "entity-one":
                return generateEntityOneAction(element);
            case "entity-and":
                return generateEntityAndAction(element);
            case "service":
                return generateServiceAction(element);
            default:
                return "// TODO: Unsupported action: " + tagName;
        }
    }

    protected String generateSetAction(Element element) {
        String field = getAttr(element, "field");
        String value = getAttr(element, "value");
        String fromField = getAttr(element, "from-field");
        String type = getAttr(element, "type");
        String defaultValue = getAttr(element, "default-value");
        String global = getAttr(element, "global");

        List<String> attrs = new ArrayList<>();
        if (isNotEmpty(field)) attrs.add(attrIfNotEmpty("field", field));
        if (isNotEmpty(value)) attrs.add(attrIfNotEmpty("value", value));
        if (isNotEmpty(fromField)) attrs.add(attrIfNotEmpty("fromField", fromField));
        if (isNotEmpty(type)) attrs.add(attrIfNotEmpty("type", type));
        if (isNotEmpty(defaultValue)) attrs.add(attrIfNotEmpty("defaultValue", defaultValue));
        if ("true".equalsIgnoreCase(global)) attrs.add("global = true");

        return "@SetAction(" + joinAttrs(attrs.toArray(new String[0])) + ")";
    }

    protected String generateScriptAction(Element element, String menuName) {
        String location = getAttr(element, "location");
        String lang = getAttr(element, "lang", "groovy");

        // Check for inline script (CDATA)
        String code = element.getTextContent();
        if (isNotEmpty(code) && code.trim().length() > 0) {
            scriptCounter++;
            location = extractScript(menuName, scriptCounter, lang, code.trim());
        }

        List<String> attrs = new ArrayList<>();
        if (isNotEmpty(location)) attrs.add(attrIfNotEmpty("location", location));
        if (!"groovy".equals(lang)) attrs.add(attrIfNotEmpty("lang", lang));

        return "@ScriptAction(" + joinAttrs(attrs.toArray(new String[0])) + ")";
    }

    protected String generateEntityOneAction(Element element) {
        String entityName = getAttr(element, "entity-name");
        String valueField = getAttr(element, "value-field");
        String useCache = getAttr(element, "use-cache");
        String autoFieldMap = getAttr(element, "auto-field-map");

        List<String> attrs = new ArrayList<>();
        if (isNotEmpty(entityName)) attrs.add(attrIfNotEmpty("entityName", entityName));
        if (isNotEmpty(valueField)) attrs.add(attrIfNotEmpty("valueField", valueField));
        if ("true".equalsIgnoreCase(useCache)) attrs.add("useCache = true");
        if ("false".equalsIgnoreCase(autoFieldMap)) attrs.add("autoFieldMap = false");

        return "@EntityOneAction(" + joinAttrs(attrs.toArray(new String[0])) + ")";
    }

    protected String generateEntityAndAction(Element element) {
        String entityName = getAttr(element, "entity-name");
        String list = getAttr(element, "list");
        String useCache = getAttr(element, "use-cache");
        String filterByDate = getAttr(element, "filter-by-date");

        List<String> attrs = new ArrayList<>();
        if (isNotEmpty(entityName)) attrs.add(attrIfNotEmpty("entityName", entityName));
        if (isNotEmpty(list)) attrs.add(attrIfNotEmpty("list", list));
        if ("true".equalsIgnoreCase(useCache)) attrs.add("useCache = true");
        if (isNotEmpty(filterByDate)) attrs.add(attrIfNotEmpty("filterByDate", filterByDate));

        return "@EntityAndAction(" + joinAttrs(attrs.toArray(new String[0])) + ")";
    }

    /**
     * SCIPIO: 4.0.0: Generates @EntityConditionAction for entity-condition elements.
     */
    protected String generateEntityConditionAction(Element element) {
        String entityName = getAttr(element, "entity-name");
        String list = getAttr(element, "list");
        String useCache = getAttr(element, "use-cache");
        String filterByDate = getAttr(element, "filter-by-date");
        String distinct = getAttr(element, "distinct");
        String delegatorName = getAttr(element, "delegator-name");

        List<String> attrs = new ArrayList<>();
        if (isNotEmpty(entityName)) attrs.add(attrIfNotEmpty("entityName", entityName));
        if (isNotEmpty(list)) attrs.add(attrIfNotEmpty("list", list));
        if ("true".equalsIgnoreCase(useCache)) attrs.add("useCache = true");
        if ("true".equalsIgnoreCase(filterByDate)) attrs.add("filterByDate = true");
        if ("true".equalsIgnoreCase(distinct)) attrs.add("distinct = true");
        if (isNotEmpty(delegatorName)) attrs.add(attrIfNotEmpty("delegatorName", delegatorName));

        // Handle select-field elements
        List<Element> selectFields = childElementList(element, "select-field");
        if (!selectFields.isEmpty()) {
            StringBuilder selectBuilder = new StringBuilder("selectFields = {");
            boolean first = true;
            for (Element selectField : selectFields) {
                String fieldName = getAttr(selectField, "field-name");
                if (isNotEmpty(fieldName)) {
                    if (!first) selectBuilder.append(", ");
                    selectBuilder.append(toStringValue(fieldName));
                    first = false;
                }
            }
            selectBuilder.append("}");
            attrs.add(selectBuilder.toString());
        }

        // Handle order-by elements
        List<Element> orderByFields = childElementList(element, "order-by");
        if (!orderByFields.isEmpty()) {
            StringBuilder orderBuilder = new StringBuilder("orderBy = {");
            boolean first = true;
            for (Element orderBy : orderByFields) {
                String fieldName = getAttr(orderBy, "field-name");
                if (isNotEmpty(fieldName)) {
                    if (!first) orderBuilder.append(", ");
                    orderBuilder.append(toStringValue(fieldName));
                    first = false;
                }
            }
            orderBuilder.append("}");
            attrs.add(orderBuilder.toString());
        }

        // Handle condition-expr elements, including those wrapped in a condition-list
        // SCIPIO: 4.0.0: condition-list children were dropped, so the query ran unconditioned
        List<Element> conditionExprs = new ArrayList<>(childElementList(element, "condition-expr"));
        for (Element condList : childElementList(element, "condition-list")) {
            conditionExprs.addAll(childElementList(condList, "condition-expr"));
        }
        if (!conditionExprs.isEmpty()) {
            StringBuilder condBuilder = new StringBuilder("conditions = {");
            boolean first = true;
            for (Element condExpr : conditionExprs) {
                if (!first) condBuilder.append(", ");
                condBuilder.append(generateConditionExpr(condExpr));
                first = false;
            }
            condBuilder.append("}");
            attrs.add(condBuilder.toString());
        }

        return "@EntityConditionAction(" + joinAttrs(attrs.toArray(new String[0])) + ")";
    }

    /**
     * SCIPIO: 4.0.0: Generates @ConditionExpr for condition-expr elements.
     */
    protected String generateConditionExpr(Element element) {
        String fieldName = getAttr(element, "field-name");
        String operator = getAttr(element, "operator");
        String value = getAttr(element, "value");
        String fromField = getAttr(element, "from-field");
        String envName = getAttr(element, "env-name");
        String ignoreIfNull = getAttr(element, "ignore-if-null");
        String ignoreIfEmpty = getAttr(element, "ignore-if-empty");

        List<String> attrs = new ArrayList<>();
        if (isNotEmpty(fieldName)) attrs.add(attrIfNotEmpty("fieldName", fieldName));
        if (isNotEmpty(operator) && !"equals".equals(operator)) attrs.add(attrIfNotEmpty("operator", operator));
        if (isNotEmpty(value)) attrs.add(attrIfNotEmpty("value", value));
        if (isNotEmpty(fromField)) attrs.add(attrIfNotEmpty("fromField", fromField));
        if (isNotEmpty(envName)) attrs.add(attrIfNotEmpty("envName", envName));
        if ("true".equalsIgnoreCase(ignoreIfNull)) attrs.add("ignoreIfNull = true");
        if ("true".equalsIgnoreCase(ignoreIfEmpty)) attrs.add("ignoreIfEmpty = true");

        return "@ConditionExpr(" + joinAttrs(attrs.toArray(new String[0])) + ")";
    }

    protected String generateServiceAction(Element element) {
        String serviceName = getAttr(element, "service-name");
        String resultMapName = getAttr(element, "result-map-name");
        String autoFieldMap = getAttr(element, "auto-field-map");

        List<String> attrs = new ArrayList<>();
        if (isNotEmpty(serviceName)) attrs.add(attrIfNotEmpty("serviceName", serviceName));
        if (isNotEmpty(resultMapName)) attrs.add(attrIfNotEmpty("resultMapName", resultMapName));
        if ("false".equalsIgnoreCase(autoFieldMap)) attrs.add("autoFieldMap = false");

        return "@ServiceAction(" + joinAttrs(attrs.toArray(new String[0])) + ")";
    }

    /**
     * Generates @MenuItem annotation.
     */
    protected String generateMenuItem(Element itemElement, String menuName) {
        StringBuilder sb = new StringBuilder();
        sb.append("@MenuItem(");

        List<String> attrs = new ArrayList<>();

        // Required name
        String itemName = getAttr(itemElement, "name");
        attrs.add(attrIfNotEmpty("name", itemName));

        // Common attributes
        String title = getAttr(itemElement, "title");
        String tooltip = getAttr(itemElement, "tooltip");
        String titleStyle = getAttr(itemElement, "title-style");
        String widgetStyle = getAttr(itemElement, "widget-style");
        String linkStyle = getAttr(itemElement, "link-style");
        String alignStyle = getAttr(itemElement, "align-style");
        String tooltipStyle = getAttr(itemElement, "tooltip-style");
        String selectedStyle = getAttr(itemElement, "selected-style");
        String selectedAncestorStyle = getAttr(itemElement, "selected-ancestor-style");
        String disabledTitleStyle = getAttr(itemElement, "disabled-title-style");
        String position = getAttr(itemElement, "position");
        String align = getAttr(itemElement, "align");
        String cellWidth = getAttr(itemElement, "cell-width");
        String associatedContentId = getAttr(itemElement, "associated-content-id");
        String hideIfSelected = getAttr(itemElement, "hide-if-selected");
        String targetWindow = getAttr(itemElement, "target-window");
        String disabled = getAttr(itemElement, "disabled");
        String disableIfEmpty = getAttr(itemElement, "disable-if-empty");
        String overrideMode = getAttr(itemElement, "override-mode");
        String sortMode = getAttr(itemElement, "sort-mode");
        String alwaysExpandSelectedOrAncestor = getAttr(itemElement, "always-expand-selected-or-ancestor");

        if (isNotEmpty(title)) attrs.add(attrIfNotEmpty("title", title));
        if (isNotEmpty(tooltip)) attrs.add(attrIfNotEmpty("tooltip", tooltip));
        if (isNotEmpty(titleStyle)) attrs.add(attrIfNotEmpty("titleStyle", titleStyle));
        if (isNotEmpty(widgetStyle)) attrs.add(attrIfNotEmpty("widgetStyle", widgetStyle));
        if (isNotEmpty(linkStyle)) attrs.add(attrIfNotEmpty("linkStyle", linkStyle));
        if (isNotEmpty(alignStyle)) attrs.add(attrIfNotEmpty("alignStyle", alignStyle));
        if (isNotEmpty(tooltipStyle)) attrs.add(attrIfNotEmpty("tooltipStyle", tooltipStyle));
        if (isNotEmpty(selectedStyle)) attrs.add(attrIfNotEmpty("selectedStyle", selectedStyle));
        if (isNotEmpty(selectedAncestorStyle)) attrs.add(attrIfNotEmpty("selectedAncestorStyle", selectedAncestorStyle));
        if (isNotEmpty(disabledTitleStyle)) attrs.add(attrIfNotEmpty("disabledTitleStyle", disabledTitleStyle));
        if (isNotEmpty(position) && !"1".equals(position)) attrs.add(attrIfNotEmpty("position", position));
        if (isNotEmpty(align) && !"left".equals(align)) {
            attrs.add("align = Align." + align.toUpperCase());
        }
        if (isNotEmpty(cellWidth)) attrs.add(attrIfNotEmpty("cellWidth", cellWidth));
        if (isNotEmpty(associatedContentId)) attrs.add(attrIfNotEmpty("associatedContentId", associatedContentId));
        if (isNotEmpty(hideIfSelected)) attrs.add(attrIfNotEmpty("hideIfSelected", hideIfSelected));
        if (isNotEmpty(targetWindow)) attrs.add(attrIfNotEmpty("targetWindow", targetWindow));
        if (isNotEmpty(disabled)) attrs.add(attrIfNotEmpty("disabled", disabled));
        if (isNotEmpty(disableIfEmpty)) attrs.add(attrIfNotEmpty("disableIfEmpty", disableIfEmpty));
        if (isNotEmpty(overrideMode)) attrs.add(attrIfNotEmpty("overrideMode", overrideMode));
        if (isNotEmpty(sortMode)) attrs.add(attrIfNotEmpty("sortMode", sortMode));
        if (isNotEmpty(alwaysExpandSelectedOrAncestor)) attrs.add(attrIfNotEmpty("alwaysExpandSelectedOrAncestor", alwaysExpandSelectedOrAncestor));

        // Process condition
        Element conditionElement = firstChildElement(itemElement, "condition");
        if (conditionElement != null) {
            String conditionCode = generateMenuItemCondition(conditionElement);
            if (isNotEmpty(conditionCode)) {
                attrs.add("condition = " + conditionCode);
            }
        }

        // Process actions
        Element actionsElement = firstChildElement(itemElement, "actions");
        if (actionsElement != null) {
            String actionsCode = generateMenuActions(actionsElement, menuName + "_" + itemName);
            if (isNotEmpty(actionsCode)) {
                attrs.add("itemActions = " + actionsCode);
            }
        }

        // Process link
        Element linkElement = firstChildElement(itemElement, "link");
        if (linkElement != null) {
            String linkCode = generateMenuLink(linkElement);
            if (isNotEmpty(linkCode)) {
                attrs.add("link = " + linkCode);
            }
        }

        // Process sub-menu
        List<Element> subMenuElements = childElementList(itemElement, "sub-menu");
        if (!subMenuElements.isEmpty()) {
            StringBuilder subBuilder = new StringBuilder();
            subBuilder.append("subMenus = {");
            boolean first = true;
            for (Element subMenu : subMenuElements) {
                if (!first) subBuilder.append(", ");
                subBuilder.append(generateSubMenu(subMenu));
                first = false;
            }
            subBuilder.append("}");
            attrs.add(subBuilder.toString());
        }

        sb.append(joinAttrs(attrs.toArray(new String[0])));
        sb.append(")");

        return sb.toString();
    }

    /**
     * Generates @MenuItemCondition annotation.
     * Uses functional @Condition annotations for all condition types.
     *
     * <p>Special handling for composite conditions:</p>
     * <ul>
     *   <li>And: Children are flattened into the conditions array</li>
     *   <li>Or/Xor: Cannot be represented, generate TODO</li>
     *   <li>Not + if-empty: Converted to NotEmpty</li>
     * </ul>
     */
    protected String generateMenuItemCondition(Element conditionElement) {
        List<String> attrs = new ArrayList<>();

        // Check for condition mode/pass-style/disabled-style attributes
        String mode = getAttr(conditionElement, "mode");
        String passStyle = getAttr(conditionElement, "pass-style");
        String disabledStyle = getAttr(conditionElement, "disabled-style");

        if (isNotEmpty(mode)) attrs.add(attrIfNotEmpty("mode", mode));
        if (isNotEmpty(passStyle)) attrs.add(attrIfNotEmpty("passStyle", passStyle));
        if (isNotEmpty(disabledStyle)) attrs.add(attrIfNotEmpty("disabledStyle", disabledStyle));

        // Process child conditions - flatten And elements
        List<String> conditionCodes = new ArrayList<>();
        // SCIPIO: 4.0.0: single top-level <not> with representable content -> not = true; <or>/<xor> groups of
        // simple conditions -> or/xor attributes (previously collapsed to Always = item always shown)
        pendingOrCodes = new ArrayList<>();
        pendingXorCodes = new ArrayList<>();
        boolean notWrapper = false;
        Element condRoot = conditionElement;
        List<? extends Element> topChildren = childElementList(conditionElement);
        if (topChildren.size() == 1 && "not".equals(topChildren.get(0).getNodeName())) {
            List<? extends Element> inner = childElementList(topChildren.get(0));
            boolean representable = !inner.isEmpty();
            for (Element e : inner) {
                String t = e.getNodeName();
                if ("or".equals(t) || "xor".equals(t) || "not".equals(t)) representable = false;
            }
            if (representable) {
                notWrapper = true;
                condRoot = topChildren.get(0);
            }
        }
        collectConditions(condRoot, conditionCodes);
        if (notWrapper) attrs.add("not = true");
        if (!pendingOrCodes.isEmpty()) attrs.add("or = {" + String.join(", ", pendingOrCodes) + "}");
        if (!pendingXorCodes.isEmpty()) attrs.add("xor = {" + String.join(", ", pendingXorCodes) + "}");

        if (!conditionCodes.isEmpty()) {
            StringBuilder conditionsBuilder = new StringBuilder();
            conditionsBuilder.append("conditions = {");
            boolean first = true;
            for (String code : conditionCodes) {
                if (!first) conditionsBuilder.append(", ");
                conditionsBuilder.append(code);
                first = false;
            }
            conditionsBuilder.append("}");
            attrs.add(conditionsBuilder.toString());
        }

        if (attrs.isEmpty()) {
            return "";
        }

        return "@MenuItemCondition(" + joinAttrs(attrs.toArray(new String[0])) + ")";
    }

    /**
     * Collects condition codes, flattening And elements.
     */
    protected List<String> pendingOrCodes = new ArrayList<>();
    protected List<String> pendingXorCodes = new ArrayList<>();

    protected void collectConditions(Element parentElement, List<String> conditionCodes) {
        for (Element child : childElementList(parentElement)) {
            String tagName = child.getNodeName();
            if ("or".equals(tagName) || "xor".equals(tagName)) {
                String orCode = generateOrXorCondition(child);
                if (orCode != null) {
                    ("or".equals(tagName) ? pendingOrCodes : pendingXorCodes).add(orCode);
                    continue;
                }
            }
            if ("and".equals(tagName)) {
                // Flatten And element - recurse into its children
                collectConditions(child, conditionCodes);
            } else {
                // Other conditions - generate normally
                String code = generateFunctionalCondition(child);
                if (isNotEmpty(code) && !code.startsWith("FLATTEN:")) {
                    conditionCodes.add(code);
                }
            }
        }
    }

    /**
     * Generates a functional @Condition annotation from an XML condition element.
     *
     * <p>Note: Due to Java's cyclic annotation limitations, composite conditions (And, Or, Xor, Not)
     * cannot have nested children in individual @Condition annotations. Instead:</p>
     * <ul>
     *   <li>And: Children are flattened (since multiple conditions are AND-ed together)</li>
     *   <li>Or/Xor: Generate TODO comment (not directly representable)</li>
     *   <li>Not + if-empty: Converted to NotEmpty condition</li>
     *   <li>Other Not: Generate TODO comment</li>
     * </ul>
     */
    /**
     * SCIPIO: 4.0.0: Builds an OrCondition/XorCondition from simple children (not(simple) is folded); null if unrepresentable.
     */
    protected String generateOrXorCondition(Element element) {
        String ann = "xor".equals(element.getNodeName()) ? "@com.ilscipio.scipio.widget.def.screen.XorCondition(" : "@com.ilscipio.scipio.widget.def.screen.OrCondition(";
        List<String> ifEmpty = new ArrayList<>(), ifNotEmpty = new ArrayList<>(), ifTrue = new ArrayList<>(),
                ifFalse = new ArrayList<>(), ifCompare = new ArrayList<>(), ifHasPerm = new ArrayList<>();
        for (Element child : childElementList(element)) {
            Element c = child;
            String tag = c.getNodeName();
            boolean neg = false;
            if ("not".equals(tag)) {
                List<? extends Element> inner = childElementList(child);
                if (inner.size() != 1) return null;
                c = inner.get(0);
                tag = c.getNodeName();
                neg = true;
            }
            String field = getAttr(c, "field");
            switch (tag) {
                case "if-empty": (neg ? ifNotEmpty : ifEmpty).add(toStringValue(field)); break;
                case "if-not-empty": (neg ? ifEmpty : ifNotEmpty).add(toStringValue(field)); break;
                // SCIPIO: 4.0.0: not(if-true x) is not if-false x (an unset field is neither); such a child and a
                // value expression go to the tree form (TimesheetSubTabBar hid its create item)
                case "if-true":
                    if (neg || isEmpty(field)) return null;
                    ifTrue.add(toStringValue(field));
                    break;
                case "if-false":
                    if (neg || isEmpty(field)) return null;
                    ifFalse.add(toStringValue(field));
                    break;
                case "if-compare": {
                    String op = getAttr(c, "operator");
                    if (isEmpty(op)) op = "equals";
                    if (neg) {
                        op = negateCompareOperator(op);
                        if (op == null) return null;
                    }
                    List<String> a = new ArrayList<>();
                    a.add("field = " + toStringValue(field));
                    a.add("operator = " + toStringValue(op));
                    a.add("value = " + toStringValue(getAttr(c, "value")));
                    if (isNotEmpty(getAttr(c, "type"))) a.add("type = " + toStringValue(getAttr(c, "type")));
                    if (isNotEmpty(getAttr(c, "format"))) a.add("format = " + toStringValue(getAttr(c, "format")));
                    ifCompare.add("@com.ilscipio.scipio.widget.def.screen.IfCompare(" + String.join(", ", a) + ")");
                    break;
                }
                case "if-has-permission": {
                    if (neg) return null;
                    String action = getAttr(c, "action");
                    ifHasPerm.add("@com.ilscipio.scipio.widget.def.screen.IfHasPermission(permission = " + toStringValue(getAttr(c, "permission"))
                            + (isNotEmpty(action) ? ", action = " + toStringValue(action) : "") + ")");
                    break;
                }
                default:
                    return null;
            }
        }
        List<String> attrs = new ArrayList<>();
        if (!ifEmpty.isEmpty()) attrs.add("ifEmpty = {" + String.join(", ", ifEmpty) + "}");
        if (!ifNotEmpty.isEmpty()) attrs.add("ifNotEmpty = {" + String.join(", ", ifNotEmpty) + "}");
        if (!ifTrue.isEmpty()) attrs.add("ifTrue = {" + String.join(", ", ifTrue) + "}");
        if (!ifFalse.isEmpty()) attrs.add("ifFalse = {" + String.join(", ", ifFalse) + "}");
        if (!ifCompare.isEmpty()) attrs.add("ifCompare = {" + String.join(", ", ifCompare) + "}");
        if (!ifHasPerm.isEmpty()) attrs.add("ifHasPermission = {" + String.join(", ", ifHasPerm) + "}");
        if (attrs.isEmpty()) return null;
        return ann + String.join(", ", attrs) + ")";
    }

    protected String negateCompareOperator(String op) {
        switch (op) {
            case "equals": return "not-equals";
            case "not-equals": return "equals";
            case "greater": return "less-equals";
            case "less-equals": return "greater";
            case "less": return "greater-equals";
            case "greater-equals": return "less";
            default: return null;
        }
    }

    /**
     * SCIPIO: 4.0.0: Generates a composite @Condition (Or/Xor/And) with its children as @NestedCondition.
     *
     * <p>Returns null when a child is itself composite, because a NestedCondition holds no further nesting.</p>
     */
    /**
     * SCIPIO: 4.0.0: Collects the @NestedCondition members of a composite, flattening a child composite
     * of the same kind (or(a, or(b, c)) == or(a, b, c)). Returns false when a child cannot be represented.
     */
    /**
     * SCIPIO: 4.0.0: Generates a composite @NestedCondition whose members are @NestedCondition2.
     *
     * <p>This is the deepest level the annotation types allow; returns null when a member is
     * itself composite, or when the whole group would have to be negated.</p>
     */
    /**
     * SCIPIO: 4.0.0: Generates a composite @Condition using the flat Condition.tree() form.
     *
     * <p>Replaces the NestedCondition chain, which stopped at a fixed depth and made anything
     * deeper fall back to an always-true condition. Nodes are emitted in pre-order, each naming
     * its parent by index, so the tree carries any depth.</p>
     */
    protected String generateConditionTree(Element element, String compositeType, boolean negateChildren) {
        List<String> nodes = new ArrayList<>();
        if (!collectConditionTreeNodes(element, -1, negateChildren, nodes)) {
            return null;
        }
        if (nodes.isEmpty()) {
            return null;
        }
        return "@Condition(type = " + compositeType + ".class, tree = {" + String.join(", ", nodes) + "})";
    }

    /**
     * SCIPIO: 4.0.0: Appends the children of a composite element to the flat node list.
     *
     * <p>Returns false when a child cannot be represented at all, so the caller can fall back
     * rather than emit a condition that silently means something else.</p>
     */
    protected boolean collectConditionTreeNodes(Element element, int parentIndex, boolean negateChildren,
            List<String> nodes) {
        for (Element child : childElementList(element)) {
            String tag = child.getNodeName();
            boolean negate = negateChildren;
            Element target = child;
            if ("not".equals(tag)) {
                List<? extends Element> inner = childElementList(child);
                if (inner.size() != 1) {
                    return false;
                }
                // A double negation cancels; otherwise the not flag carries it.
                negate = !negate;
                target = inner.get(0);
                tag = target.getNodeName();
            }
            if ("and".equals(tag) || "or".equals(tag) || "xor".equals(tag)) {
                String compositeType = "and".equals(tag) ? "And" : ("or".equals(tag) ? "Or" : "Xor");
                nodes.add("@ConditionNode(" + conditionNodeHeader(parentIndex, negate)
                        + "type = " + compositeType + ".class)");
                int myIndex = nodes.size() - 1;
                if (!collectConditionTreeNodes(target, myIndex, false, nodes)) {
                    return false;
                }
                continue;
            }
            if ("not".equals(tag)) {
                return false; // not(not(...)) beyond one level
            }
            String code = generateFunctionalCondition(target);
            if (code == null || !code.startsWith("@Condition(")) {
                return false;
            }
            nodes.add("@ConditionNode(" + conditionNodeHeader(parentIndex, negate)
                    + code.substring("@Condition(".length()));
        }
        return true;
    }

    /** SCIPIO: 4.0.0: Renders the parent/not prefix shared by every @ConditionNode. */
    protected String conditionNodeHeader(int parentIndex, boolean negate) {
        StringBuilder sb = new StringBuilder();
        if (parentIndex >= 0) {
            sb.append("parent = ").append(parentIndex).append(", ");
        }
        if (negate) {
            sb.append("not = true, ");
        }
        return sb.toString();
    }


    protected String generateFunctionalCondition(Element element) {
        String tagName = element.getNodeName();

        switch (tagName) {
            // Composite conditions - special handling due to annotation limitations
            case "and":
                // And conditions are flattened - children returned as array
                return generateAndCondition(element);
            case "or":
            case "xor":
                // SCIPIO: Or/Xor cannot be directly represented - use Always as fallback (always passes)
                // The original condition semantics are lost, but at least the UI renders
                String composite = generateConditionTree(element, "or".equals(tagName) ? "Or" : "Xor", false);
                if (composite != null) {
                    return composite;
                }
                System.err.println("WARN: Menu condition '" + tagName + "' cannot be represented - using Always fallback");
                return "@Condition(type = Always.class)";
            case "not":
                return generateNotCondition(element);

            // Permission conditions
            case "if-has-permission":
                return generatePermissionCondition(element);
            case "if-service-permission":
                return generateServicePermissionCondition(element);
            case "if-entity-permission":
                return generateEntityPermissionCondition(element);

            // Comparison conditions
            case "if-empty":
                return generateEmptyCondition(element);
            case "if-compare":
                return generateCompareCondition(element);
            case "if-compare-field":
                return generateCompareFieldCondition(element);
            case "if-regexp":
                return generateRegexpCondition(element);

            // Validation conditions
            case "if-validate-method":
                return generateValidateMethodCondition(element);
            case "if-true":
                return generateTrueCondition(element);
            case "if-false":
                return generateFalseCondition(element);

            // System conditions
            case "if-widget":
                return generateWidgetDefinedCondition(element);
            case "if-component":
                return generateComponentEnabledCondition(element);
            case "if-entity":
                return generateEntityDefinedCondition(element);
            case "if-service":
                return generateServiceDefinedCondition(element);
            case "if-empty-section":
                return generateEmptySectionCondition(element);

            default:
                // Fallback for unknown conditions - generate a comment
                return "@Condition(type = Empty.class, params = {\"/* UNSUPPORTED: " + escapeString(tagName) + " */\"})";
        }
    }

    /**
     * Generates conditions for an And element.
     * Since multiple conditions in MenuItemCondition are AND-ed together,
     * this method is called from generateMenuItemCondition when processing the
     * And element directly at the condition level.
     *
     * <p>When And is nested inside another composite, this returns FLATTEN_MARKER
     * to indicate the children should be flattened into the parent array.</p>
     */
    protected String generateAndCondition(Element element) {
        // And with single child - just return the child condition
        List<Element> children = childElementList(element);
        if (children.size() == 1) {
            return generateFunctionalCondition(children.get(0));
        }
        // And with multiple children - these will be handled at the container level
        // Return a marker that indicates flattening is needed
        return "FLATTEN:" + children.size();
    }

    /**
     * Generates a Not condition.
     *
     * <p>Special cases handled:</p>
     * <ul>
     *   <li>not + if-empty = NotEmpty condition</li>
     *   <li>Other negations = TODO comment (cannot be represented without nested annotations)</li>
     * </ul>
     */
    protected String generateNotCondition(Element element) {
        Element child = firstChildElement(element, null);
        if (child == null) {
            return "@Condition(type = Empty.class, params = {\"/* TODO: empty not condition */\"})";
        }

        String childTagName = child.getNodeName();

        // Special case: not + if-empty = NotEmpty
        if ("if-empty".equals(childTagName)) {
            String field = getAttr(child, "field");
            return "@Condition(type = NotEmpty.class, params = {" + toStringValue(field) + "})";
        }

        // SCIPIO: 4.0.0: not + if-compare = if-compare with the negated operator
        if ("if-compare".equals(childTagName)) {
            String op = getAttr(child, "operator");
            String negOp = negateCompareOperator(isEmpty(op) ? "equals" : op);
            if (negOp != null) {
                Element clone = (Element) child.cloneNode(true);
                clone.setAttribute("operator", negOp);
                return generateCompareCondition(clone);
            }
        }
        // SCIPIO: Other negations cannot be directly represented - use Always as fallback
        // SCIPIO: 4.0.0: De Morgan rewrite keeps not(or)/not(and) representable (were always-true fallbacks)
        if ("or".equals(childTagName) || "and".equals(childTagName)) {
            // De Morgan: not(or(a, b)) == and(not a, not b), and the mirror for and.
            String composite = generateConditionTree(child, "or".equals(childTagName) ? "And" : "Or", true);
            if (composite != null) {
                return composite;
            }
        }
        System.err.println("WARN: Menu condition 'not(" + childTagName + ")' cannot be represented in annotations - using Always fallback");
        return "@Condition(type = Always.class)";
    }

    /**
     * Generates a HasPermission condition.
     */
    protected String generatePermissionCondition(Element element) {
        String permission = getAttr(element, "permission");
        String action = getAttr(element, "action");

        List<String> params = new ArrayList<>();
        if (isNotEmpty(permission)) params.add(toStringValue(permission));
        if (isNotEmpty(action)) params.add(toStringValue(action));

        return "@Condition(type = HasPermission.class, params = {" + String.join(", ", params) + "})";
    }

    /**
     * Generates a ServicePermission condition.
     */
    protected String generateServicePermissionCondition(Element element) {
        String serviceName = getAttr(element, "service-name");
        String mainAction = getAttr(element, "main-action");
        String contextMap = getAttr(element, "context-map");
        String resourceDescription = getAttr(element, "resource-description");

        List<String> params = new ArrayList<>();
        if (isNotEmpty(serviceName)) params.add(toStringValue(serviceName));
        if (isNotEmpty(mainAction)) params.add(toStringValue(mainAction));
        if (isNotEmpty(contextMap)) params.add(toStringValue(contextMap));
        if (isNotEmpty(resourceDescription)) params.add(toStringValue(resourceDescription));

        return "@Condition(type = ServicePermission.class, params = {" + String.join(", ", params) + "})";
    }

    /**
     * Generates an EntityPermission condition.
     */
    protected String generateEntityPermissionCondition(Element element) {
        String entityName = getAttr(element, "entity-name");
        String entityId = getAttr(element, "entity-id");
        String targetOperation = getAttr(element, "target-operation");

        List<String> params = new ArrayList<>();
        if (isNotEmpty(entityName)) params.add(toStringValue(entityName));
        if (isNotEmpty(entityId)) params.add(toStringValue(entityId));
        if (isNotEmpty(targetOperation)) params.add(toStringValue(targetOperation));

        return "@Condition(type = EntityPermission.class, params = {" + String.join(", ", params) + "})";
    }

    /**
     * Generates an Empty condition.
     */
    protected String generateEmptyCondition(Element element) {
        String field = getAttr(element, "field");
        return "@Condition(type = Empty.class, params = {" + toStringValue(field) + "})";
    }

    /**
     * Generates a Compare condition.
     */
    protected String generateCompareCondition(Element element) {
        String field = getAttr(element, "field");
        String operator = getAttr(element, "operator");
        String value = getAttr(element, "value");
        String type = getAttr(element, "type");
        String format = getAttr(element, "format");

        List<String> params = new ArrayList<>();
        if (isNotEmpty(field)) params.add(toStringValue(field));
        if (isNotEmpty(operator)) params.add(toStringValue(operator));
        if (isNotEmpty(value)) params.add(toStringValue(value));
        if (isNotEmpty(type)) params.add(toStringValue(type));
        if (isNotEmpty(format)) params.add(toStringValue(format));

        return "@Condition(type = Compare.class, params = {" + String.join(", ", params) + "})";
    }

    /**
     * Generates a CompareField condition.
     */
    protected String generateCompareFieldCondition(Element element) {
        String field = getAttr(element, "field");
        String operator = getAttr(element, "operator");
        String toField = getAttr(element, "to-field");
        String type = getAttr(element, "type");
        String format = getAttr(element, "format");

        List<String> params = new ArrayList<>();
        if (isNotEmpty(field)) params.add(toStringValue(field));
        if (isNotEmpty(operator)) params.add(toStringValue(operator));
        if (isNotEmpty(toField)) params.add(toStringValue(toField));
        if (isNotEmpty(type)) params.add(toStringValue(type));
        if (isNotEmpty(format)) params.add(toStringValue(format));

        return "@Condition(type = CompareField.class, params = {" + String.join(", ", params) + "})";
    }

    /**
     * Generates a Regexp condition.
     */
    protected String generateRegexpCondition(Element element) {
        String field = getAttr(element, "field");
        String expr = getAttr(element, "expr");

        List<String> params = new ArrayList<>();
        if (isNotEmpty(field)) params.add(toStringValue(field));
        if (isNotEmpty(expr)) params.add(toStringValue(expr));

        return "@Condition(type = Regexp.class, params = {" + String.join(", ", params) + "})";
    }

    /**
     * Generates a ValidateMethod condition.
     */
    protected String generateValidateMethodCondition(Element element) {
        String field = getAttr(element, "field");
        String method = getAttr(element, "method");
        String className = getAttr(element, "class");

        List<String> params = new ArrayList<>();
        if (isNotEmpty(field)) params.add(toStringValue(field));
        if (isNotEmpty(method)) params.add(toStringValue(method));
        if (isNotEmpty(className)) params.add(toStringValue(className));

        return "@Condition(type = ValidateMethod.class, params = {" + String.join(", ", params) + "})";
    }

    /**
     * Generates a True condition.
     */
    protected String generateTrueCondition(Element element) {
        String field = getAttr(element, "field");
        return "@Condition(type = True.class, params = {" + toStringValue(field) + "})";
    }

    /**
     * Generates a False condition.
     */
    protected String generateFalseCondition(Element element) {
        String field = getAttr(element, "field");
        return "@Condition(type = False.class, params = {" + toStringValue(field) + "})";
    }

    /**
     * Generates a WidgetDefined condition.
     */
    protected String generateWidgetDefinedCondition(Element element) {
        String name = getAttr(element, "name");
        String location = getAttr(element, "location");
        String type = getAttr(element, "type");

        List<String> params = new ArrayList<>();
        if (isNotEmpty(name)) params.add(toStringValue(name));
        if (isNotEmpty(location)) params.add(toStringValue(location));
        if (isNotEmpty(type)) params.add(toStringValue(type));

        return "@Condition(type = WidgetDefined.class, params = {" + String.join(", ", params) + "})";
    }

    /**
     * Generates a ComponentEnabled condition.
     */
    protected String generateComponentEnabledCondition(Element element) {
        String componentName = getAttr(element, "component-name");
        return "@Condition(type = ComponentEnabled.class, params = {" + toStringValue(componentName) + "})";
    }

    /**
     * Generates an EntityDefined condition.
     */
    protected String generateEntityDefinedCondition(Element element) {
        String entityName = getAttr(element, "entity-name");
        return "@Condition(type = EntityDefined.class, params = {" + toStringValue(entityName) + "})";
    }

    /**
     * Generates a ServiceDefined condition.
     */
    protected String generateServiceDefinedCondition(Element element) {
        String serviceName = getAttr(element, "service-name");
        return "@Condition(type = ServiceDefined.class, params = {" + toStringValue(serviceName) + "})";
    }

    /**
     * Generates an EmptySection condition.
     */
    protected String generateEmptySectionCondition(Element element) {
        String sectionName = getAttr(element, "section-name");
        return "@Condition(type = EmptySection.class, params = {" + toStringValue(sectionName) + "})";
    }

    /**
     * Generates @MenuLink annotation.
     */
    protected String generateMenuLink(Element linkElement) {
        List<String> attrs = new ArrayList<>();

        String target = getAttr(linkElement, "target");
        String targetWindow = getAttr(linkElement, "target-window");
        String linkType = getAttr(linkElement, "link-type");
        String urlMode = getAttr(linkElement, "url-mode");
        String text = getAttr(linkElement, "text");
        String id = getAttr(linkElement, "id");
        String style = getAttr(linkElement, "style");
        String name = getAttr(linkElement, "name");
        String title = getAttr(linkElement, "title");
        String prefix = getAttr(linkElement, "prefix");
        String width = getAttr(linkElement, "width");
        String height = getAttr(linkElement, "height");
        String fullPath = getAttr(linkElement, "full-path");
        String secure = getAttr(linkElement, "secure");
        String encode = getAttr(linkElement, "encode");
        String requestConfirmation = getAttr(linkElement, "request-confirmation");
        String confirmationMessage = getAttr(linkElement, "confirmation-message");
        String useWhen = getAttr(linkElement, "use-when");
        String size = getAttr(linkElement, "size");

        if (isNotEmpty(target)) attrs.add(attrIfNotEmpty("target", target));
        if (isNotEmpty(targetWindow)) attrs.add(attrIfNotEmpty("targetWindow", targetWindow));
        if (isNotEmpty(linkType) && !"auto".equals(linkType)) {
            String enumValue = linkType.replace("-", "_").toUpperCase();
            attrs.add("linkType = LinkType." + enumValue);
        }
        if (isNotEmpty(urlMode) && !"intra-app".equals(urlMode)) {
            String enumValue = urlMode.replace("-", "_").toUpperCase();
            attrs.add("urlMode = UrlMode." + enumValue);
        }
        if (isNotEmpty(text)) attrs.add(attrIfNotEmpty("text", text));
        if (isNotEmpty(id)) attrs.add(attrIfNotEmpty("id", id));
        if (isNotEmpty(style)) attrs.add(attrIfNotEmpty("style", style));
        if (isNotEmpty(name)) attrs.add(attrIfNotEmpty("name", name));
        if (isNotEmpty(title)) attrs.add(attrIfNotEmpty("title", title));
        if (isNotEmpty(prefix)) attrs.add(attrIfNotEmpty("prefix", prefix));
        if (isNotEmpty(width)) attrs.add(attrIfNotEmpty("width", width));
        if (isNotEmpty(height)) attrs.add(attrIfNotEmpty("height", height));
        if (isNotEmpty(fullPath)) attrs.add(attrIfNotEmpty("fullPath", fullPath));
        if (isNotEmpty(secure)) attrs.add(attrIfNotEmpty("secure", secure));
        if (isNotEmpty(encode)) attrs.add(attrIfNotEmpty("encode", encode));
        if ("true".equalsIgnoreCase(requestConfirmation)) attrs.add("requestConfirmation = true");
        if (isNotEmpty(confirmationMessage)) attrs.add(attrIfNotEmpty("confirmationMessage", confirmationMessage));
        if (isNotEmpty(useWhen)) attrs.add(attrIfNotEmpty("useWhen", useWhen));
        if (isNotEmpty(size)) {
            try {
                int s = Integer.parseInt(size);
                if (s != 0) attrs.add("size = " + s);
            } catch (NumberFormatException e) { /* ignore */ }
        }

        // Process parameters
        List<String> params = generateLinkParameters(linkElement);
        if (!params.isEmpty()) {
            attrs.add("parameters = {" + String.join(", ", params) + "}");
        }

        // Process parameter-map (note: annotation doesn't directly support this, so we add a comment)
        Element paramMapElement = firstChildElement(linkElement, "parameter-map");
        if (paramMapElement != null) {
            String fromField = getAttr(paramMapElement, "from-field");
            if (isNotEmpty(fromField)) {
                // parameter-map from-field is not directly supported by annotations
                // The MenuAnnotationReader handles this via useWhen or special processing
                // For now, add as a TODO comment in the useWhen attribute
                if (isEmpty(useWhen)) {
                    attrs.add("useWhen = \"/* TODO: parameter-map from-field=" + escapeString(fromField) + " */\"");
                }
            }
        }

        // Process image
        Element imageElement = firstChildElement(linkElement, "image");
        if (imageElement != null) {
            String imageCode = generateMenuImage(imageElement);
            if (isNotEmpty(imageCode)) {
                attrs.add("image = " + imageCode);
            }
        }

        if (attrs.isEmpty()) {
            return "@MenuLink";
        }
        return "@MenuLink(" + joinAttrs(attrs.toArray(new String[0])) + ")";
    }

    /**
     * Generates link parameters.
     */
    protected List<String> generateLinkParameters(Element linkElement) {
        List<String> params = new ArrayList<>();
        for (Element param : childElementList(linkElement, "parameter")) {
            String paramName = getAttr(param, "param-name");
            String value = getAttr(param, "value");
            String fromField = getAttr(param, "from-field");

            List<String> paramAttrs = new ArrayList<>();
            if (isNotEmpty(paramName)) {
                paramAttrs.add(attrIfNotEmpty("paramName", paramName));
            }
            if (isNotEmpty(value)) {
                paramAttrs.add(attrIfNotEmpty("value", value));
            }
            if (isNotEmpty(fromField)) {
                paramAttrs.add(attrIfNotEmpty("fromField", fromField));
            } else if (isEmpty(value) && isNotEmpty(paramName)) {
                // When only param-name is specified, the value comes from a field with the same name
                paramAttrs.add(attrIfNotEmpty("fromField", paramName));
            }
            params.add("@MenuParameter(" + joinAttrs(paramAttrs.toArray(new String[0])) + ")");
        }
        return params;
    }

    /**
     * Generates @MenuImage annotation.
     */
    protected String generateMenuImage(Element imageElement) {
        String src = getAttr(imageElement, "src");
        String id = getAttr(imageElement, "id");
        String style = getAttr(imageElement, "style");
        String title = getAttr(imageElement, "title");
        String width = getAttr(imageElement, "width");
        String height = getAttr(imageElement, "height");
        String urlMode = getAttr(imageElement, "url-mode");

        List<String> attrs = new ArrayList<>();
        if (isNotEmpty(src)) attrs.add(attrIfNotEmpty("src", src));
        if (isNotEmpty(id)) attrs.add(attrIfNotEmpty("id", id));
        if (isNotEmpty(style)) attrs.add(attrIfNotEmpty("style", style));
        if (isNotEmpty(title)) attrs.add(attrIfNotEmpty("title", title));
        if (isNotEmpty(width)) attrs.add(attrIfNotEmpty("width", width));
        if (isNotEmpty(height)) attrs.add(attrIfNotEmpty("height", height));
        if (isNotEmpty(urlMode) && !"content".equals(urlMode)) attrs.add(attrIfNotEmpty("urlMode", urlMode));

        if (attrs.isEmpty()) {
            return "@MenuImage";
        }
        return "@MenuImage(" + joinAttrs(attrs.toArray(new String[0])) + ")";
    }

    /**
     * Generates @SubMenu annotation.
     */
    protected String generateSubMenu(Element subMenuElement) {
        String name = getAttr(subMenuElement, "name");
        String include = getAttr(subMenuElement, "include");
        String includeResource = getAttr(subMenuElement, "include-resource");
        String model = getAttr(subMenuElement, "model");
        String modelResource = getAttr(subMenuElement, "model-resource");
        String modelScope = getAttr(subMenuElement, "model-scope");
        String includeScope = getAttr(subMenuElement, "include-scope");

        List<String> attrs = new ArrayList<>();
        if (isNotEmpty(name)) attrs.add(attrIfNotEmpty("name", name));

        // Handle include - combine include-resource and include into resource#name format
        if (isNotEmpty(include)) {
            String includeValue;
            // SCIPIO: 4.0.0: Handle combined resource#menuName format in include attribute
            String effectiveResource = includeResource;
            String effectiveMenuName = include;
            if (isEmpty(includeResource) && include.contains("#")) {
                // Split combined format: component://path/File.xml#MenuName
                int hashPos = include.indexOf('#');
                effectiveResource = include.substring(0, hashPos);
                effectiveMenuName = include.substring(hashPos + 1);
            }

            if (isNotEmpty(effectiveResource)) {
                // SCIPIO: 4.0.0: For external includes, try to map to class:// reference if possible
                String classRef = mapResourceToClassLocation(effectiveResource, effectiveMenuName);
                if (classRef != null) {
                    includeValue = classRef;
                } else {
                    // Fall back to component:// format
                    includeValue = effectiveResource + "#" + effectiveMenuName;
                }
            } else {
                // Same file - generate class-based location: class://package.Class$MenuName
                // SCIPIO: 4.0.0: Use the pre-resolved name so this reference matches the actual
                // declaration even if it was renamed to avoid a case-insensitive collision.
                // SCIPIO: 4.0.0: prefer the location alias (the generated class name follows the task naming scheme,
                // e.g. CatalogCatalogMenus, which the class:// form above did not match)
                includeValue = isNotEmpty(sourceLocation) ? sourceLocation + "#" + effectiveMenuName
                        : "class://" + packageName + "." + className + "$"
                        + menuNameToInterfaceName.getOrDefault(effectiveMenuName, toInterfaceName(effectiveMenuName));
            }
            attrs.add(attrIfNotEmpty("include", includeValue));
        }

        // Handle model - combine model-resource and model into resource#name format
        if (isNotEmpty(model)) {
            String modelValue;
            // SCIPIO: 4.0.0: Handle combined resource#menuName format in model attribute
            String effectiveModelResource = modelResource;
            String effectiveModelName = model;
            if (isEmpty(modelResource) && model.contains("#")) {
                int hashPos = model.indexOf('#');
                effectiveModelResource = model.substring(0, hashPos);
                effectiveModelName = model.substring(hashPos + 1);
            }

            if (isNotEmpty(effectiveModelResource)) {
                String classRef = mapResourceToClassLocation(effectiveModelResource, effectiveModelName);
                if (classRef != null) {
                    modelValue = classRef;
                } else {
                    modelValue = effectiveModelResource + "#" + effectiveModelName;
                }
            } else {
                // SCIPIO: 4.0.0: Use the pre-resolved name so this reference matches the actual
                // declaration even if it was renamed to avoid a case-insensitive collision.
                modelValue = "class://" + packageName + "." + className + "$"
                        + menuNameToInterfaceName.getOrDefault(effectiveModelName, toInterfaceName(effectiveModelName));
            }
            attrs.add(attrIfNotEmpty("model", modelValue));
        }

        if (isNotEmpty(modelScope)) attrs.add(attrIfNotEmpty("modelScope", modelScope));
        if (isNotEmpty(includeScope)) attrs.add(attrIfNotEmpty("includeScope", includeScope));

        return "@SubMenu(" + joinAttrs(attrs.toArray(new String[0])) + ")";
    }

    /**
     * SCIPIO: 4.0.0: Generates @IncludeElements annotation.
     */
    protected String generateIncludeElements(Element element) {
        List<String> attrs = new ArrayList<>();

        // menu-name attribute
        String menuName = getAttr(element, "menu-name");
        if (isNotEmpty(menuName)) {
            attrs.add(attrIfNotEmpty("menuName", menuName));
        }

        // resource attribute
        String resource = getAttr(element, "resource");
        if (isNotEmpty(resource)) {
            attrs.add(attrIfNotEmpty("resource", resource));
        }

        // menu-ref attribute
        String menuRef = getAttr(element, "menu-ref");
        if (isNotEmpty(menuRef)) {
            attrs.add(attrIfNotEmpty("menuRef", menuRef));
        }

        // recursive attribute - only add if not "full" (the default)
        String recursive = getAttr(element, "recursive");
        if (isNotEmpty(recursive) && !"full".equals(recursive)) {
            String enumValue = recursive.toUpperCase().replace("-", "_");
            attrs.add("recursive = RecursiveMode." + enumValue);
        }

        // sub-menus attribute
        String subMenus = getAttr(element, "sub-menus");
        if (isNotEmpty(subMenus)) {
            attrs.add(attrIfNotEmpty("subMenus", subMenus));
        }

        // force-sub-menu-model-scope attribute
        String forceSubMenuModelScope = getAttr(element, "force-sub-menu-model-scope");
        if (isNotEmpty(forceSubMenuModelScope)) {
            attrs.add(attrIfNotEmpty("forceSubMenuModelScope", forceSubMenuModelScope));
        }

        // include-menu-item-aliases attribute - only add if false (default is true)
        String includeMenuItemAliases = getAttr(element, "include-menu-item-aliases");
        if ("false".equalsIgnoreCase(includeMenuItemAliases)) {
            attrs.add("includeMenuItemAliases = false");
        }

        // exclude-item elements - collect all into array
        List<Element> excludeItems = childElementList(element, "exclude-item");
        if (!excludeItems.isEmpty()) {
            StringBuilder excludeBuilder = new StringBuilder();
            excludeBuilder.append("excludeItems = {");
            boolean first = true;
            for (Element excludeItem : excludeItems) {
                String itemName = getAttr(excludeItem, "name");
                if (isNotEmpty(itemName)) {
                    if (!first) excludeBuilder.append(", ");
                    excludeBuilder.append(toStringValue(itemName));
                    first = false;
                }
            }
            excludeBuilder.append("}");
            attrs.add(excludeBuilder.toString());
        }

        return "@IncludeElements(" + joinAttrs(attrs.toArray(new String[0])) + ")";
    }

    /**
     * SCIPIO: 4.0.0: Maps a component:// resource to a class:// location if the target
     * annotation class can be determined.
     *
     * <p>Maps: component://{component}/widget/{file}.xml
     * To: class://com.ilscipio.scipio.{component}.widget.{File}$MenuName</p>
     *
     * @param resource The component:// resource path
     * @param menuName The menu name
     * @return The class:// location, or null if mapping cannot be determined
     */
    protected String mapResourceToClassLocation(String resource, String menuName) {
        // SCIPIO: 4.0.0: keep component:// location aliases; the class naming scheme of the generated files
        // (e.g. CatalogCatalogMenus) does not match the guessed class:// form
        if (resource != null) return null;
        if (resource == null || !resource.startsWith("component://")) {
            return null;
        }

        // Parse: component://{component}/widget/{subdirs/}filename.xml
        String path = resource.substring("component://".length());
        int slashIdx = path.indexOf('/');
        if (slashIdx < 0) {
            return null;
        }

        String component = path.substring(0, slashIdx);
        String rest = path.substring(slashIdx + 1);

        // Check if it's in a widget directory
        if (!rest.startsWith("widget/")) {
            return null;
        }

        // Extract filename (remove .xml extension)
        String widgetPath = rest.substring("widget/".length());
        if (!widgetPath.endsWith(".xml")) {
            return null;
        }

        // Remove .xml and get filename (may include subdirs)
        String filePathWithoutExt = widgetPath.substring(0, widgetPath.length() - 4);

        // Convert path separators to package separators and extract class name
        // e.g., "ordermgr/OrderMenus" -> package suffix = "ordermgr", class = "OrderMenus"
        String packageSuffix = "";
        String fileName = filePathWithoutExt;
        int lastSlash = filePathWithoutExt.lastIndexOf('/');
        if (lastSlash >= 0) {
            packageSuffix = "." + filePathWithoutExt.substring(0, lastSlash).replace('/', '.');
            fileName = filePathWithoutExt.substring(lastSlash + 1);
        }

        // Build class location: class://com.ilscipio.scipio.{component}.widget{.subpackage}.{ClassName}${MenuName}
        String classLocation = "class://com.ilscipio.scipio." + component + ".widget" + packageSuffix + "." + fileName + "$" + toInterfaceName(menuName);

        return classLocation;
    }

    /**
     * Converts a menu name to a valid Java interface name.
     */
    protected String toInterfaceName(String menuName) {
        String result = menuName.replaceAll("[^a-zA-Z0-9_]", "_");
        if (result.length() > 0 && Character.isDigit(result.charAt(0))) {
            result = "_" + result;
        }
        return result;
    }
}
