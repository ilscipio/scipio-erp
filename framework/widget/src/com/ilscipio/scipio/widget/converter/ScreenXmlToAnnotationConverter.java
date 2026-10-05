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
import java.util.Arrays;
import java.util.Collections;
import java.util.IdentityHashMap;
import java.util.List;
import java.util.Map;

import org.w3c.dom.Document;
import org.w3c.dom.Element;

/**
 * Converts Screen XML definitions to Java annotation source code.
 *
 * <p>SCIPIO: 4.0.0: Added for XML-to-Annotation conversion support.</p>
 */
public class ScreenXmlToAnnotationConverter extends XmlToAnnotationConverter {

    private int scriptCounter = 0;

    public ScreenXmlToAnnotationConverter(String packageName, String className, String componentName,
                                          File outputDir, File scriptOutputDir) {
        super(packageName, className, componentName, outputDir, scriptOutputDir);
    }

    @Override
    protected String getImports() {
        return "import com.ilscipio.scipio.widget.def.screen.*;" + NEWLINE +
               "import com.ilscipio.scipio.widget.def.condition.Condition;" + NEWLINE +
               "import com.ilscipio.scipio.widget.def.condition.ConditionNode;" + NEWLINE +
               "import com.ilscipio.scipio.widget.def.condition.NestedCondition;" + NEWLINE +
               "import com.ilscipio.scipio.widget.def.condition.NestedCondition2;" + NEWLINE +
               "import com.ilscipio.scipio.widget.def.condition.impl.*;" + NEWLINE;
    }

    /**
     * SCIPIO: 4.0.0: A Widget cannot hold the section of an iterate-section, so each one becomes a helper
     * screen of the same class; the iterate widget includes it with a shared scope. These track the screen
     * being converted, the helpers still to convert and the helper of each iterate-section element.
     */
    protected String currentScreenName;
    protected int iterateCounter;
    protected final List<Element> pendingHelperScreens = new ArrayList<>();
    protected final Map<Element, String> iterateHelperNames = new IdentityHashMap<>();
    protected int partCounter;
    protected boolean decoratorsToHelpers;

    @Override
    public String convert(Document doc) {
        StringBuilder sb = new StringBuilder();

        sb.append(generateClassHeader());
        sb.append(NEWLINE);

        // SCIPIO: 4.0.0: Reset case-insensitive interface-name collision tracking for this class
        resetInterfaceNameTracking();

        Element root = doc.getDocumentElement();
        List<Element> screenElements = childElementList(root, "screen");

        for (Element screenElement : screenElements) {
            sb.append(convertScreen(screenElement));
            sb.append(NEWLINE);
            // SCIPIO: 4.0.0: the helper screens of its iterate-sections (and of theirs) follow the screen
            while (!pendingHelperScreens.isEmpty()) {
                sb.append(convertScreen(pendingHelperScreens.remove(0)));
                sb.append(NEWLINE);
            }
        }

        sb.append(generateClassFooter());

        return sb.toString();
    }

    /**
     * Converts a single screen element to annotations.
     */
    protected String convertScreen(Element screenElement) {
        StringBuilder sb = new StringBuilder();
        String screenName = getAttr(screenElement, "name");

        // SCIPIO: 4.0.0: Skip screens with empty or missing names
        if (screenName == null || screenName.trim().isEmpty()) {
            System.err.println("Warning: Skipping screen with empty name in " + sourceLocation);
            return "";
        }

        scriptCounter = 0;
        currentScreenName = screenName;
        iterateCounter = 0;
        partCounter = 0;

        // Collect all annotations for this screen
        List<String> annotations = new ArrayList<>();

        // Main @Screen annotation
        annotations.add(generateScreenAnnotation(screenElement));

        // Process section element (screens have a single root section)
        Element sectionElement = firstChildElement(screenElement, "section");
        if (sectionElement != null) {
            // Process actions inside section
            Element actionsElement = firstChildElement(sectionElement, "actions");
            if (actionsElement != null) {
                annotations.addAll(generateActionAnnotations(actionsElement, screenName));
            }

            // Process widgets inside section
            Element widgetsElement = firstChildElement(sectionElement, "widgets");
            if (widgetsElement != null) {
                List<String> widgetAnnotations = generateWidgetAnnotations(widgetsElement, screenName);
                // SCIPIO: 4.0.0: the root section's own attributes were dropped; name and contains
                // drive the render-target expressions (e.g. Global-Column-Main), so losing them
                // breaks the layout of every screen that targets the section.
                applyRootSectionAttributes(widgetAnnotations, sectionElement);
                annotations.addAll(widgetAnnotations);
            }
        } else {
            // SCIPIO: 4.0.0: Handle screens with actions/widgets directly (no section wrapper)
            // This pattern is used for action-only screens like webapp-common-actions, static-common-actions
            Element actionsElement = firstChildElement(screenElement, "actions");
            if (actionsElement != null) {
                annotations.addAll(generateActionAnnotations(actionsElement, screenName));
            }

            Element widgetsElement = firstChildElement(screenElement, "widgets");
            if (widgetsElement != null) {
                annotations.addAll(generateWidgetAnnotations(widgetsElement, screenName));
            }
        }

        // Write all annotations
        for (String annotation : annotations) {
            sb.append(indent(1)).append(annotation).append(NEWLINE);
        }

        // Generate interface declaration
        // SCIPIO: 4.0.0: Disambiguate names that collide case-insensitively (Windows filesystem defect)
        sb.append(indent(1)).append("public interface ").append(resolveUniqueInterfaceName(toInterfaceName(screenName))).append(" {}");
        sb.append(NEWLINE);

        return sb.toString();
    }

    /**
     * Generates the main @Screen annotation.
     */
    protected String generateScreenAnnotation(Element screenElement) {
        String name = getAttr(screenElement, "name");
        String useTransaction = getAttr(screenElement, "use-transaction");
        String requireLogin = getAttr(screenElement, "require-login");
        String transactionTimeout = getAttr(screenElement, "transaction-timeout");

        List<String> attrs = new ArrayList<>();
        attrs.add(attrIfNotEmpty("name", name));

        // SCIPIO: 4.0.0: Location attribute now enabled - annotation system supports
        // all XML widget features (use-when, if-empty-section, decorator-section-include, fail-widgets).
        // Location aliases allow annotation-based screens to shadow XML versions.
        if (isNotEmpty(sourceLocation)) {
            attrs.add(attrIfNotEmpty("location", sourceLocation));
        }

        if (isNotEmpty(useTransaction) && !"true".equals(useTransaction)) {
            attrs.add("useTransaction = false");
        }
        if (isNotEmpty(requireLogin) && !"true".equals(requireLogin)) {
            attrs.add("requireLogin = false");
        }
        attrs.add(attrIfNotEmpty("transactionTimeout", transactionTimeout));
        // SCIPIO: 4.0.0: root section condition and fail-widgets
        Element rootSection = firstChildElement(screenElement, "section");
        if (rootSection != null) {
            Element rootCondition = firstChildElement(rootSection, "condition");
            if (rootCondition != null) {
                String conditionCode = generateConditionAnnotation(rootCondition);
                if (conditionCode != null) {
                    attrs.add("condition = " + conditionCode);
                } else {
                    System.err.println("Warning: unsupported root section condition in screen " + name);
                }
            }
            Element rootFailWidgets = firstChildElement(rootSection, "fail-widgets");
            if (rootFailWidgets != null) {
                String w = generateWidgetsForContainer(rootFailWidgets, 1);
                if (w != null) {
                    attrs.add("failWidgets = " + w);
                }
            }
            // SCIPIO: 4.0.0: were dropped, so e.g. FindApInvoices never closed its list iterator
            for (String[] block : new String[][] {{"catch-actions", "catchActions"}, {"finally-actions", "finallyActions"}}) {
                Element blockElement = firstChildElement(rootSection, block[0]);
                if (blockElement != null) {
                    List<String> blockCode = generateActionsNested(blockElement);
                    if (!blockCode.isEmpty()) {
                        attrs.add(block[1] + " = @Actions(" + String.join(", ", blockCode) + ")");
                    }
                }
            }
        }

        return "@Screen(" + joinAttrs(attrs.toArray(new String[0])) + ")";
    }

    /**
     * Generates action annotations from actions element.
     */
    protected List<String> generateActionAnnotations(Element actionsElement, String screenName) {
        List<String> annotations = new ArrayList<>();

        // SCIPIO: Screen-level actions are emitted as repeated unified @Action annotations.
        // Typed annotations (@SetAction, @ServiceAction, ...) CANNOT preserve cross-type
        // execution order at runtime - Java reflection groups annotations by type, so all
        // sets ran before all services/lookups regardless of XML order, silently nulling
        // any set that reads a lookup result. The unified @Action list (ActionList container)
        // preserves declaration order exactly.
        List<Element> children = childElementList(actionsElement);
        // When a screen mixes @Action with @IfAction, reflection cannot recover the
        // cross-type declaration order - stamp explicit order indexes on every entry.
        boolean hasIf = false;
        for (Element child : children) {
            if ("if".equals(child.getNodeName())) {
                hasIf = true;
                break;
            }
        }
        int order = 0;
        for (Element child : children) {
            String tagName = child.getNodeName();
            if ("if".equals(tagName)) {
                String ifCode = generateIfActionAnnotation(child, screenName, order);
                annotations.add(ifCode != null ? ifCode : "// TODO: Unsupported action type: if");
            } else {
                String unified = generateUnifiedAction(child, tagName, screenName);
                if (unified != null) {
                    if (hasIf && unified.startsWith("@Action(")) {
                        unified = "@Action(order = " + order + ", " + unified.substring("@Action(".length());
                    }
                    annotations.add(unified);
                } else {
                    // Add comment for unsupported action types
                    annotations.add("// TODO: Unsupported action type: " + tagName);
                }
            }
            order++;
        }

        return annotations;
    }

    /**
     * Generates an @IfAction annotation from an XML &lt;if&gt; action element
     * (condition / then / else-if* / else). Returns null when the structure cannot
     * be represented (e.g. a nested &lt;if&gt; inside a branch).
     */
    protected String generateIfActionAnnotation(Element ifElement, String screenName, int order) {
        Element conditionElement = firstChildElement(ifElement, "condition");
        String conditionCode = (conditionElement != null) ? generateConditionAnnotation(conditionElement) : null;
        if (conditionCode != null && conditionCode.contains("Always.class")) {
            System.err.println("WARN: <if> condition in screen '" + screenName + "' cannot be represented in annotations - if block dropped (TODO)");
            return null;
        }
        String conditionExpr = getAttr(ifElement, "condition");
        if (conditionCode == null && isEmpty(conditionExpr)) {
            return null;
        }

        Element thenElement = firstChildElement(ifElement, "then");
        String thenCode = generateBranchActions(thenElement, screenName);
        if (thenCode == null) {
            return null;
        }

        List<String> attrs = new ArrayList<>();
        attrs.add("order = " + order);
        if (conditionCode != null) {
            attrs.add("condition = " + conditionCode);
        } else {
            attrs.add(attrIfNotEmpty("conditionExpr", conditionExpr));
        }
        attrs.add("then = " + thenCode);

        List<String> elseIfCodes = new ArrayList<>();
        for (Element elseIfElement : childElementList(ifElement, "else-if")) {
            Element eiCondition = firstChildElement(elseIfElement, "condition");
            String eiConditionCode = (eiCondition != null) ? generateConditionAnnotation(eiCondition) : null;
            if (eiConditionCode != null && eiConditionCode.contains("Always.class")) {
                System.err.println("WARN: <else-if> condition in screen '" + screenName + "' cannot be represented in annotations - if block dropped (TODO)");
                return null;
            }
            if (eiConditionCode == null) {
                return null;
            }
            String eiThenCode = generateBranchActions(firstChildElement(elseIfElement, "then"), screenName);
            if (eiThenCode == null) {
                return null;
            }
            elseIfCodes.add("@ElseIfBlock(condition = " + eiConditionCode + ", then = " + eiThenCode + ")");
        }
        if (!elseIfCodes.isEmpty()) {
            attrs.add("elseIf = {" + String.join(", ", elseIfCodes) + "}");
        }

        Element elseElement = firstChildElement(ifElement, "else");
        if (elseElement != null) {
            String elseCode = generateBranchActions(elseElement, screenName);
            if (elseCode == null) {
                return null;
            }
            attrs.add("elseActions = " + elseCode);
        }

        return "@IfAction(" + joinAttrs(attrs.toArray(new String[0])) + ")";
    }

    /**
     * Generates the @Actions(value = {...}) code for one if/else branch.
     * Returns null when a branch child cannot be represented as a unified @Action.
     */
    protected String generateBranchActions(Element branchElement, String screenName) {
        List<String> actionCodes = new ArrayList<>();
        List<String> ifCodes = new ArrayList<>();
        if (branchElement != null) {
            List<Element> children = childElementList(branchElement);
            boolean hasNestedIf = false;
            for (Element child : children) {
                if ("if".equals(child.getNodeName())) {
                    hasNestedIf = true;
                    break;
                }
            }
            int order = 0;
            for (Element child : children) {
                String tagName = child.getNodeName();
                if ("if".equals(tagName)) {
                    // SCIPIO: 4.0.0: one nested level via @IfAction2 (leaf branches)
                    String nested = generateIfAction2Annotation(child, screenName, order);
                    if (nested == null) {
                        return null;
                    }
                    ifCodes.add(nested);
                } else {
                    String unified = generateUnifiedAction(child, tagName, screenName);
                    if (unified == null) {
                        return null;
                    }
                    if (hasNestedIf && unified.startsWith("@Action(")) {
                        unified = "@Action(order = " + order + ", " + unified.substring("@Action(".length());
                    }
                    actionCodes.add(unified);
                }
                order++;
            }
        }
        List<String> attrs = new ArrayList<>();
        if (!actionCodes.isEmpty()) {
            attrs.add("value = {" + String.join(", ", actionCodes) + "}");
        }
        if (!ifCodes.isEmpty()) {
            attrs.add("ifs = {" + String.join(", ", ifCodes) + "}");
        }
        return "@Actions(" + String.join(", ", attrs) + ")";
    }

    /** SCIPIO: 4.0.0: Generates an @IfAction2 for an &lt;if&gt; nested inside an if branch; null if not representable. */
    protected String generateIfAction2Annotation(Element ifElement, String screenName, int order) {
        Element conditionElement = firstChildElement(ifElement, "condition");
        String conditionCode = (conditionElement != null) ? generateConditionAnnotation(conditionElement) : null;
        if (conditionCode != null && conditionCode.contains("Always.class")) {
            return null;
        }
        String conditionExpr = getAttr(ifElement, "condition");
        if (conditionCode == null && isEmpty(conditionExpr)) {
            return null;
        }
        String thenCode = generateLeafBranchActions(firstChildElement(ifElement, "then"), screenName);
        if (thenCode == null) {
            return null;
        }
        List<String> attrs = new ArrayList<>();
        attrs.add("order = " + order);
        if (conditionCode != null) {
            attrs.add("condition = " + conditionCode);
        } else {
            attrs.add(attrIfNotEmpty("conditionExpr", conditionExpr));
        }
        attrs.add("then = " + thenCode);
        List<String> elseIfCodes = new ArrayList<>();
        for (Element elseIfElement : childElementList(ifElement, "else-if")) {
            Element eiCondition = firstChildElement(elseIfElement, "condition");
            String eiConditionCode = (eiCondition != null) ? generateConditionAnnotation(eiCondition) : null;
            if (eiConditionCode == null || eiConditionCode.contains("Always.class")) {
                return null;
            }
            String eiThenCode = generateLeafBranchActions(firstChildElement(elseIfElement, "then"), screenName);
            if (eiThenCode == null) {
                return null;
            }
            elseIfCodes.add("@ElseIfBlock2(condition = " + eiConditionCode + ", then = " + eiThenCode + ")");
        }
        if (!elseIfCodes.isEmpty()) {
            attrs.add("elseIf = {" + String.join(", ", elseIfCodes) + "}");
        }
        Element elseElement = firstChildElement(ifElement, "else");
        if (elseElement != null) {
            String elseCode = generateLeafBranchActions(elseElement, screenName);
            if (elseCode == null) {
                return null;
            }
            attrs.add("elseActions = " + elseCode);
        }
        return "@IfAction2(" + joinAttrs(attrs.toArray(new String[0])) + ")";
    }

    /** SCIPIO: 4.0.0: Leaf branch (@Actions2): unified actions only, no further nesting. */
    protected String generateLeafBranchActions(Element branchElement, String screenName) {
        List<String> actionCodes = new ArrayList<>();
        if (branchElement != null) {
            for (Element child : childElementList(branchElement)) {
                String tagName = child.getNodeName();
                if ("if".equals(tagName)) {
                    return null;
                }
                String unified = generateUnifiedAction(child, tagName, screenName);
                if (unified == null) {
                    return null;
                }
                actionCodes.add(unified);
            }
        }
        return "@Actions2(value = {" + String.join(", ", actionCodes) + "})";
    }

    protected String generateSetAction(Element element) {
        String field = getAttr(element, "field");
        String value = getAttr(element, "value");
        String fromField = getAttr(element, "from-field");
        String fromScope = getAttr(element, "from-scope");
        String type = getAttr(element, "type");
        String defaultValue = getAttr(element, "default-value");
        String global = getAttr(element, "global");

        List<String> attrs = new ArrayList<>();
        attrs.add(attrIfNotEmpty("field", field));
        attrs.add(attrIfNotEmpty("value", value));
        attrs.add(attrIfNotEmpty("fromField", fromField));
        attrs.add(attrIfNotEmpty("fromScope", fromScope));
        attrs.add(attrIfNotEmpty("type", type));
        attrs.add(attrIfNotEmpty("defaultValue", defaultValue));
        if ("true".equals(global)) {
            attrs.add("global = true");
        }

        return "@SetAction(" + joinAttrs(attrs.toArray(new String[0])) + ")";
    }

    protected String generatePropertyToFieldAction(Element element) {
        String field = getAttr(element, "field");
        String resource = getAttr(element, "resource");
        String property = getAttr(element, "property");
        String defaultValue = getAttr(element, "default");

        List<String> attrs = new ArrayList<>();
        attrs.add(attrIfNotEmpty("field", field));
        attrs.add(attrIfNotEmpty("resource", resource));
        attrs.add(attrIfNotEmpty("property", property));
        attrs.add(attrIfNotEmpty("defaultValue", defaultValue));

        return "@PropertyToFieldAction(" + joinAttrs(attrs.toArray(new String[0])) + ")";
    }

    protected String generateScriptAction(Element element, String screenName) {
        String location = getAttr(element, "location");
        String lang = getAttr(element, "lang", "groovy");

        // Check for inline script (CDATA)
        String code = element.getTextContent();
        if (isNotEmpty(code) && code.trim().length() > 0) {
            // Extract inline script to external file
            scriptCounter++;
            location = extractScript(screenName, scriptCounter, lang, code.trim());
        }

        List<String> attrs = new ArrayList<>();
        attrs.add(attrIfNotEmpty("location", location));
        if (!"groovy".equals(lang)) {
            attrs.add(attrIfNotEmpty("lang", lang));
        }

        return "@ScriptAction(" + joinAttrs(attrs.toArray(new String[0])) + ")";
    }

    protected String generateEntityOneAction(Element element) {
        String entityName = getAttr(element, "entity-name");
        String valueField = getAttr(element, "value-field");
        String useCache = getAttr(element, "use-cache");
        String autoFieldMap = getAttr(element, "auto-field-map");

        List<String> attrs = new ArrayList<>();
        attrs.add(attrIfNotEmpty("entityName", entityName));
        attrs.add(attrIfNotEmpty("valueField", valueField));
        if ("true".equals(useCache)) {
            attrs.add("useCache = true");
        }
        if ("false".equals(autoFieldMap)) {
            attrs.add("autoFieldMap = false");
        }

        // Handle field-map children
        List<Element> fieldMaps = childElementList(element, "field-map");
        if (!fieldMaps.isEmpty()) {
            StringBuilder fmBuilder = new StringBuilder();
            fmBuilder.append("fieldMaps = {");
            boolean first = true;
            for (Element fm : fieldMaps) {
                if (!first) fmBuilder.append(", ");
                fmBuilder.append(generateFieldMap(fm));
                first = false;
            }
            fmBuilder.append("}");
            attrs.add(fmBuilder.toString());
        }

        return "@EntityOneAction(" + joinAttrs(attrs.toArray(new String[0])) + ")";
    }

    protected String generateEntityAndAction(Element element) {
        String entityName = getAttr(element, "entity-name");
        String list = getAttr(element, "list");
        String useCache = getAttr(element, "use-cache");
        String filterByDate = getAttr(element, "filter-by-date");

        List<String> attrs = new ArrayList<>();
        attrs.add(attrIfNotEmpty("entityName", entityName));
        attrs.add(attrIfNotEmpty("list", list));
        if ("true".equals(useCache)) {
            attrs.add("useCache = true");
        }
        if ("true".equals(filterByDate)) {
            attrs.add("filterByDate = true");
        }

        // Handle field-map children
        List<Element> fieldMaps = childElementList(element, "field-map");
        if (!fieldMaps.isEmpty()) {
            StringBuilder fmBuilder = new StringBuilder();
            fmBuilder.append("fieldMaps = {");
            boolean first = true;
            for (Element fm : fieldMaps) {
                if (!first) fmBuilder.append(", ");
                fmBuilder.append(generateFieldMap(fm));
                first = false;
            }
            fmBuilder.append("}");
            attrs.add(fmBuilder.toString());
        }

        return "@EntityAndAction(" + joinAttrs(attrs.toArray(new String[0])) + ")";
    }

    protected String generateEntityConditionAction(Element element) {
        String entityName = getAttr(element, "entity-name");
        String list = getAttr(element, "list");
        String useCache = getAttr(element, "use-cache");
        String filterByDate = getAttr(element, "filter-by-date");

        List<String> attrs = new ArrayList<>();
        attrs.add(attrIfNotEmpty("entityName", entityName));
        attrs.add(attrIfNotEmpty("list", list));
        if ("true".equals(useCache)) {
            attrs.add("useCache = true");
        }
        if ("true".equals(filterByDate)) {
            attrs.add("filterByDate = true");
        }
        // SCIPIO: 4.0.0: distinct/delegator-name were dropped
        if ("true".equals(getAttr(element, "distinct"))) {
            attrs.add("distinct = true");
        }
        attrs.add(attrIfNotEmpty("delegatorName", getAttr(element, "delegator-name")));

        // SCIPIO: 4.0.0: conditions wrapped in condition-list, order-by and select-field were dropped
        String conditions = generateConditionsArray(element);
        if (conditions != null) {
            attrs.add("conditions = " + conditions);
        }
        String orderBy = generateOrderByArray(element);
        if (orderBy != null) {
            attrs.add("orderBy = " + orderBy);
        }
        String selectFields = generateSelectFieldsArray(element);
        if (selectFields != null) {
            attrs.add("selectFields = " + selectFields);
        }

        return "@EntityConditionAction(" + joinAttrs(attrs.toArray(new String[0])) + ")";
    }

    protected String generateServiceAction(Element element) {
        String serviceName = getAttr(element, "service-name");
        String resultMapName = getAttr(element, "result-map-name");
        if (isEmpty(resultMapName)) {
            resultMapName = getAttr(element, "result-map");
        }
        String resultMapList = getAttr(element, "result-map-list");
        String autoFieldMap = getAttr(element, "auto-field-map");

        List<String> attrs = new ArrayList<>();
        attrs.add(attrIfNotEmpty("serviceName", serviceName));
        attrs.add(attrIfNotEmpty("resultMapName", resultMapName));
        attrs.add(attrIfNotEmpty("resultMapList", resultMapList));
        if ("false".equals(autoFieldMap)) {
            attrs.add("autoFieldMap = false");
        }

        List<Element> fieldMaps = childElementList(element, "field-map");
        if (!fieldMaps.isEmpty()) {
            StringBuilder fmBuilder = new StringBuilder();
            fmBuilder.append("fieldMaps = {");
            boolean first = true;
            for (Element fm : fieldMaps) {
                if (!first) fmBuilder.append(", ");
                fmBuilder.append(generateFieldMap(fm));
                first = false;
            }
            fmBuilder.append("}");
            attrs.add(fmBuilder.toString());
        }

        return "@ServiceAction(" + joinAttrs(attrs.toArray(new String[0])) + ")";
    }

    protected String generateGetRelatedOneAction(Element element) {
        String valueName = getAttr(element, "value-field");
        String relationName = getAttr(element, "relation-name");
        String toValueField = getAttr(element, "to-value-field");
        String useCache = getAttr(element, "use-cache");

        List<String> attrs = new ArrayList<>();
        attrs.add(attrIfNotEmpty("valueField", valueName));
        attrs.add(attrIfNotEmpty("relationName", relationName));
        attrs.add(attrIfNotEmpty("toValueField", toValueField));
        if ("true".equals(useCache)) {
            attrs.add("useCache = true");
        }

        return "@GetRelatedOneAction(" + joinAttrs(attrs.toArray(new String[0])) + ")";
    }

    protected String generateGetRelatedAction(Element element) {
        String valueName = getAttr(element, "value-field");
        String relationName = getAttr(element, "relation-name");
        String list = getAttr(element, "list");
        String useCache = getAttr(element, "use-cache");

        List<String> attrs = new ArrayList<>();
        attrs.add(attrIfNotEmpty("valueField", valueName));
        attrs.add(attrIfNotEmpty("relationName", relationName));
        attrs.add(attrIfNotEmpty("list", list));
        if ("true".equals(useCache)) {
            attrs.add("useCache = true");
        }

        return "@GetRelatedAction(" + joinAttrs(attrs.toArray(new String[0])) + ")";
    }

    protected String generatePropertyMapAction(Element element) {
        String resource = getAttr(element, "resource");
        String mapName = getAttr(element, "map-name");
        String global = getAttr(element, "global");

        List<String> attrs = new ArrayList<>();
        attrs.add(attrIfNotEmpty("resource", resource));
        attrs.add(attrIfNotEmpty("mapName", mapName));
        if ("true".equals(global)) {
            attrs.add("global = true");
        }

        return "@PropertyMapAction(" + joinAttrs(attrs.toArray(new String[0])) + ")";
    }

    protected String generateIncludeScreenActionsAction(Element element) {
        String name = getAttr(element, "name");
        String location = getAttr(element, "location");

        List<String> attrs = new ArrayList<>();
        attrs.add(attrIfNotEmpty("name", name));
        attrs.add(attrIfNotEmpty("location", location));

        return "@IncludeScreenActionsAction(" + joinAttrs(attrs.toArray(new String[0])) + ")";
    }

    protected String generateConditionToFieldAction(Element element) {
        String field = getAttr(element, "field");
        String type = getAttr(element, "type");
        String onlyIfField = getAttr(element, "only-if-field");

        List<String> attrs = new ArrayList<>();
        attrs.add(attrIfNotEmpty("field", field));
        if (isNotEmpty(type)) {
            attrs.add("type = \"" + type + "\"");
        }
        if (isNotEmpty(onlyIfField)) {
            attrs.add("onlyIfField = \"" + onlyIfField + "\"");
        }

        // Handle condition child - generate wrapped @Condition with functionalConditions
        Element conditionElement = firstChildElement(element, null);
        if (conditionElement != null) {
            String functionalCondition = generateFunctionalCondition(conditionElement);
            if (isNotEmpty(functionalCondition)) {
                // Wrap in screen.Condition(functionalConditions = {...}) - must use FQN to avoid import conflict
                attrs.add("condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {" + functionalCondition + "})");
            }
        }

        return "@ConditionToFieldAction(" + joinAttrs(attrs.toArray(new String[0])) + ")";
    }

    protected String generateClearFieldAction(Element element) {
        String field = getAttr(element, "field");

        List<String> attrs = new ArrayList<>();
        attrs.add(attrIfNotEmpty("field", field));

        return "@ClearFieldAction(" + joinAttrs(attrs.toArray(new String[0])) + ")";
    }

    protected String generateIncludeFormActionsAction(Element element) {
        String name = getAttr(element, "name");
        String location = getAttr(element, "location");

        List<String> attrs = new ArrayList<>();
        attrs.add(attrIfNotEmpty("name", name));
        attrs.add(attrIfNotEmpty("location", location));

        return "@IncludeFormActionsAction(" + joinAttrs(attrs.toArray(new String[0])) + ")";
    }

    protected String generateIncludeFormRowActionsAction(Element element) {
        String name = getAttr(element, "name");
        String location = getAttr(element, "location");

        List<String> attrs = new ArrayList<>();
        attrs.add(attrIfNotEmpty("name", name));
        attrs.add(attrIfNotEmpty("location", location));

        return "@IncludeFormRowActionsAction(" + joinAttrs(attrs.toArray(new String[0])) + ")";
    }

    protected String generateIncludeMenuActionsAction(Element element) {
        String name = getAttr(element, "name");
        String location = getAttr(element, "location");

        List<String> attrs = new ArrayList<>();
        attrs.add(attrIfNotEmpty("name", name));
        attrs.add(attrIfNotEmpty("location", location));

        return "@IncludeMenuActionsAction(" + joinAttrs(attrs.toArray(new String[0])) + ")";
    }

    protected String generateIncludeTreeActionsAction(Element element) {
        String name = getAttr(element, "name");
        String location = getAttr(element, "location");

        List<String> attrs = new ArrayList<>();
        attrs.add(attrIfNotEmpty("name", name));
        attrs.add(attrIfNotEmpty("location", location));

        return "@IncludeTreeActionsAction(" + joinAttrs(attrs.toArray(new String[0])) + ")";
    }

    protected String generateCloseObjectAction(Element element) {
        String field = getAttr(element, "field");

        List<String> attrs = new ArrayList<>();
        attrs.add(attrIfNotEmpty("field", field));

        return "@CloseObjectAction(" + joinAttrs(attrs.toArray(new String[0])) + ")";
    }

    protected String generateThrowExceptionAction(Element element) {
        String field = getAttr(element, "field");

        List<String> attrs = new ArrayList<>();
        attrs.add(attrIfNotEmpty("field", field));

        return "@ThrowExceptionAction(" + joinAttrs(attrs.toArray(new String[0])) + ")";
    }

    protected String generateFieldMap(Element element) {
        String fieldName = getAttr(element, "field-name");
        String fromField = getAttr(element, "from-field");
        String value = getAttr(element, "value");

        List<String> attrs = new ArrayList<>();
        attrs.add(attrIfNotEmpty("fieldName", fieldName));
        attrs.add(attrIfNotEmpty("fromField", fromField));
        attrs.add(attrIfNotEmpty("value", value));

        return "@FieldMap(" + joinAttrs(attrs.toArray(new String[0])) + ")";
    }

    protected String generateConditionExpr(Element element) {
        String fieldName = getAttr(element, "field-name");
        String operator = getAttr(element, "operator", "equals");
        String value = getAttr(element, "value");
        String fromField = getAttr(element, "from-field");
        String envName = getAttr(element, "env-name");

        List<String> attrs = new ArrayList<>();
        attrs.add(attrIfNotEmpty("fieldName", fieldName));
        if (!"equals".equals(operator)) {
            attrs.add(attrIfNotEmpty("operator", operator));
        }
        attrs.add(attrIfNotEmpty("value", value));
        attrs.add(attrIfNotEmpty("fromField", fromField));
        attrs.add(attrIfNotEmpty("envName", envName));
        if ("true".equalsIgnoreCase(getAttr(element, "ignore-if-null"))) attrs.add("ignoreIfNull = true");
        if ("true".equalsIgnoreCase(getAttr(element, "ignore-if-empty"))) attrs.add("ignoreIfEmpty = true");
        if ("true".equalsIgnoreCase(getAttr(element, "ignore-case"))) attrs.add("ignoreCase = true");

        return "@ConditionExpr(" + joinAttrs(attrs.toArray(new String[0])) + ")";
    }

    /**
     * Generates widget annotations from widgets element.
     *
     * <p>SCIPIO: 4.0.0: Updated to use unified Widget format when widgets are not inside
     * a decorator-screen. This fixes the issue where multiple @IncludeForm or @Label
     * annotations would cause "not a repeatable annotation type" compile errors.</p>
     */
    /**
     * SCIPIO: 4.0.0: Copies the root section's own attributes onto the generated root @Section.
     *
     * <p>Only the root section's condition and fail-widgets were hoisted onto @Screen; its
     * name, contains, id, style and share-scope were lost.</p>
     */
    protected void applyRootSectionAttributes(List<String> annotations, Element sectionElement) {
        List<String> sectionAttrs = new ArrayList<>();
        sectionAttrs.add(attrIfNotEmpty("name", getAttr(sectionElement, "name")));
        sectionAttrs.add(attrIfNotEmpty("contains", getAttr(sectionElement, "contains")));
        sectionAttrs.add(attrIfNotEmpty("id", getAttr(sectionElement, "id")));
        sectionAttrs.add(attrIfNotEmpty("style", getAttr(sectionElement, "style")));
        if ("true".equals(getAttr(sectionElement, "share-scope"))) {
            sectionAttrs.add("shareScope = true");
        }
        String prefix = joinAttrs(sectionAttrs.toArray(new String[0]));
        if (isEmpty(prefix)) {
            return;
        }
        // Only the @Section that stands for the root section itself may take these attributes; a
        // @Section generated for a child section carries its own, and a @DecoratorScreen takes none.
        for (int i = 0; i < annotations.size(); i++) {
            String annotation = annotations.get(i);
            if (annotation != null && annotation.startsWith("@Section(widgets = ")) {
                annotations.set(i, "@Section(" + prefix + ", " + annotation.substring("@Section(".length()));
                return;
            }
        }
    }

    protected List<String> generateWidgetAnnotations(Element widgetsElement, String screenName) {
        List<String> annotations = new ArrayList<>();
        List<Element> children = childElementList(widgetsElement);

        // SCIPIO: 4.0.0: only decorator-screens and sections become type-level annotations. A mix of sections
        // and other widgets goes into one @Section below, in document order; its sections used to be emitted
        // here as well, so their content rendered twice (configOptions, AjaxGlobalDecorator, dayCalendar).
        boolean hasDecoratorScreen = false;
        boolean onlyDecoratorsAndSections = true;
        for (Element child : children) {
            if ("decorator-screen".equals(child.getNodeName())) {
                hasDecoratorScreen = true;
            } else if (!"section".equals(child.getNodeName())) {
                onlyDecoratorsAndSections = false;
            }
        }
        if (onlyDecoratorsAndSections) {
            for (Element child : children) {
                if ("decorator-screen".equals(child.getNodeName())) {
                    annotations.add(generateDecoratorScreen(child));
                } else if ("section".equals(child.getNodeName())) {
                    // Nested section with its own widgets - generate as @Section
                    annotations.add(generateSection(child, screenName));
                }
            }
            return annotations;
        }

        // Otherwise, collect non-decorator widgets and wrap in @Section with @Widgets
        // This uses the unified Widget format to preserve order. SCIPIO: 4.0.0: a decorator-screen in such a mix
        // moves into a helper screen; its siblings were dropped before (DemoExtraWidget1).
        decoratorsToHelpers = hasDecoratorScreen;
        String inlineWidgets = generateInlineWidgetsNested(widgetsElement);
        if (isNotEmpty(inlineWidgets)) {
            // @Section.widgets() takes @Widgets, not @InlineWidgets
            String widgetsCode = inlineWidgets.replace("@InlineWidgets", "@Widgets");
            annotations.add("@Section(widgets = " + widgetsCode + ")");
        }

        return annotations;
    }

    protected String generateDecoratorScreen(Element element) {
        String name = getAttr(element, "name");
        String location = getAttr(element, "location");
        // SCIPIO: 4.0.0: When no location is specified, use sourceLocation (the original XML file)
        // This is needed because annotation-based screens are loaded separately, so "same file" lookups don't work
        if (isEmpty(location) && isNotEmpty(sourceLocation)) {
            location = sourceLocation;
        }

        // SCIPIO: 4.0.0: Validate required attributes - decorator-screen must have a name
        if (isEmpty(name)) {
            return null;
        }

        StringBuilder sb = new StringBuilder();
        sb.append("@DecoratorScreen(").append(NEWLINE);
        sb.append(indent(2)).append("name = \"").append(escapeString(name)).append("\"");
        if (isNotEmpty(location)) {
            sb.append(",").append(NEWLINE);
            sb.append(indent(2)).append("location = \"").append(escapeString(location)).append("\"");
        }

        // Handle decorator-section children
        List<Element> sections = childElementList(element, "decorator-section");
        if (!sections.isEmpty()) {
            sb.append(",").append(NEWLINE);
            sb.append(indent(2)).append("sections = {").append(NEWLINE);
            boolean first = true;
            for (Element section : sections) {
                String sectionCode = generateDecoratorSection(section);
                // SCIPIO: 4.0.0: Skip null sections (empty sections are now skipped)
                if (sectionCode == null) {
                    continue;
                }
                if (!first) sb.append(",").append(NEWLINE);
                sb.append(indent(3)).append(sectionCode);
                first = false;
            }
            sb.append(NEWLINE).append(indent(2)).append("}");
        }

        sb.append(NEWLINE).append(indent(1)).append(")");
        return sb.toString();
    }

    /**
     * Generates a @DecoratorSection annotation using the unified Widget[] value array.
     *
     * <p>SCIPIO: 4.0.0: Updated to use unified @Widget format for order preservation.
     * This ensures widgets are rendered in XML document order.</p>
     *
     * <p>IMPORTANT: If ANY widget requires legacy format (containers/screenlets with nested
     * content), we fall back to FULLY legacy mode to preserve ordering. The reader only
     * processes one mode - mixing modes causes legacy widgets to be ignored.</p>
     */
    protected String generateDecoratorSection(Element element) {
        String name = getAttr(element, "name");
        String useWhen = getAttr(element, "use-when");
        String fallbackAutoInclude = getAttr(element, "fallback-auto-include");
        String overrideByAutoInclude = getAttr(element, "override-by-auto-include");
        String contains = getAttr(element, "contains");

        StringBuilder sb = new StringBuilder();
        sb.append("@DecoratorSection(");
        sb.append(attrIfNotEmpty("name", name));

        if (isNotEmpty(useWhen)) {
            sb.append(", ").append(attrIfNotEmpty("useWhen", useWhen));
        }
        if ("true".equals(fallbackAutoInclude)) {
            sb.append(", fallbackAutoInclude = true");
        }
        if ("true".equals(overrideByAutoInclude)) {
            sb.append(", overrideByAutoInclude = true");
        }
        if (isNotEmpty(contains)) {
            sb.append(", ").append(attrIfNotEmpty("contains", contains));
        }

        List<Element> children = childElementList(element);
        List<String> inlineSectionsCode = new ArrayList<>();

        // SCIPIO: 4.0.0: Mixed mode - unified widgets for simple elements, legacy arrays for nested containers/screenlets
        // This preserves order for simple widgets while supporting complex nested structures
        List<String> unifiedWidgets = new ArrayList<>();
        List<String> legacyContainers = new ArrayList<>();
        List<String> legacyScreenlets = new ArrayList<>();
        List<String> decoratorsCode = new ArrayList<>();
        // SCIPIO: 4.0.0: the reader emits the arrays in a fixed order (unified widgets, screenlets,
        // containers, sections, decorators), so a screenlet above a template swapped places. Each
        // emitted child's rank and slot; positions are written only when the XML order differs.
        List<Integer> ranks = new ArrayList<>();
        List<List<String>> targets = new ArrayList<>();

        for (Element child : children) {
            String tagName = child.getNodeName();

            if ("section".equals(tagName)) {
                inlineSectionsCode.add(generateInlineSection(child));
                ranks.add(3);
                targets.add(inlineSectionsCode);
                continue;
            }
            // SCIPIO: 4.0.0: a nested decorator-screen (FindScreenDecorator in a body) was dropped
            if ("decorator-screen".equals(tagName)) {
                decoratorsCode.add(generateDecoratorScreenNested(child));
                ranks.add(4);
                targets.add(decoratorsCode);
                continue;
            }

            // Handle containers/screenlets with nested content separately (legacy arrays)
            if ("container".equals(tagName) && hasNestedWidgets(child)) {
                legacyContainers.add(generateContainerNested(child));
                ranks.add(2);
                targets.add(legacyContainers);
                continue;
            }
            if ("screenlet".equals(tagName) && hasNestedWidgets(child)) {
                // DecoratorSection.screenlets expects Screenlet[], not ScreenletNested[]
                legacyScreenlets.add(generateScreenlet(child, ""));
                ranks.add(1);
                targets.add(legacyScreenlets);
                continue;
            }

            // All other widgets (including simple containers/screenlets) go in unified array
            String unifiedWidget = generateUnifiedWidgetForDecoratorSection(child, tagName);
            if (unifiedWidget != null) {
                unifiedWidgets.add(unifiedWidget);
                ranks.add(0);
                targets.add(unifiedWidgets);
            }
        }
        List<Integer> readerOrder = new ArrayList<>(ranks);
        Collections.sort(readerOrder);
        if (!readerOrder.equals(ranks)) {
            Map<List<String>, Integer> seen = new IdentityHashMap<>();
            for (int slot = 0; slot < ranks.size(); slot++) {
                List<String> target = targets.get(slot);
                int index = seen.merge(target, 1, Integer::sum) - 1;
                if (ranks.get(slot) > 0) {
                    target.set(index, withPosition(target.get(index), slot));
                }
            }
        }

        // Output unified widgets first (preserves order)
        if (!unifiedWidgets.isEmpty()) {
            sb.append(", value = {");
            sb.append(String.join(", ", unifiedWidgets));
            sb.append("}");
        }

        // Output legacy containers (with nested content) - appended after unified widgets
        if (!legacyContainers.isEmpty()) {
            sb.append(", containers = {");
            sb.append(String.join(", ", legacyContainers));
            sb.append("}");
        }

        // Output legacy screenlets (with nested content) - appended after unified widgets
        if (!legacyScreenlets.isEmpty()) {
            sb.append(", screenlets = {");
            sb.append(String.join(", ", legacyScreenlets));
            sb.append("}");
        }

        // Add inline sections (always processed regardless of mode)
        if (!inlineSectionsCode.isEmpty()) {
            sb.append(", sections = {");
            sb.append(String.join(", ", inlineSectionsCode));
            sb.append("}");
        }

        if (!decoratorsCode.isEmpty()) {
            sb.append(", decorators = {");
            sb.append(String.join(", ", decoratorsCode));
            sb.append("}");
        }

        sb.append(")");
        // SCIPIO: 4.0.0: Format long annotations to prevent parsing issues
        return formatAnnotation(sb.toString(), 3, 120);
    }

    /**
     * SCIPIO: 4.0.0: The reader emits the typed arrays of a parent in a fixed order, so a child could land
     * before a sibling the XML put first (BalanceSheet printed every heading after all its forms). This
     * records the array each XML child's code went to; {@link #applyPositions()} gives every entry its XML
     * slot as position when that order differs from the reader's order.
     */
    /**
     * SCIPIO: 4.0.0: The ChildOrder of the generator that is flattening a section into its own lists; the
     * flatten helpers split it per flattened child so each keeps its own slot. A split only records growth
     * of that generator's lists, so a nested generator that leaves this set cannot corrupt it.
     */
    protected ChildOrder flattenOrder;

    protected void splitFlattenOrder() {
        if (flattenOrder != null) {
            flattenOrder.after();
            flattenOrder.before();
        }
    }

    protected final class ChildOrder {
        private final List<List<String>> readerOrder;
        private final List<List<List<String>>> otherOrders = new ArrayList<>();
        private final List<List<String>> slots = new ArrayList<>();
        private int[] sizes;

        /** @param readerOrder the code lists in the order the reader emits their arrays */
        @SafeVarargs
        protected ChildOrder(List<String>... readerOrder) {
            this.readerOrder = Arrays.asList(readerOrder);
        }

        /** Also adds positions when the order differs from this second reader order (same lists). */
        @SafeVarargs
        protected final ChildOrder alsoFor(List<String>... otherOrder) {
            otherOrders.add(Arrays.asList(otherOrder));
            return this;
        }

        protected void before() {
            sizes = new int[readerOrder.size()];
            for (int rank = 0; rank < sizes.length; rank++) {
                sizes[rank] = readerOrder.get(rank).size();
            }
        }

        protected void after() {
            for (int rank = 0; rank < sizes.length; rank++) {
                for (int i = sizes[rank]; i < readerOrder.get(rank).size(); i++) {
                    slots.add(readerOrder.get(rank));
                }
                // a nested before()/after() pair (a folded section) must not be recorded twice
                sizes[rank] = readerOrder.get(rank).size();
            }
        }

        protected void applyPositions() {
            boolean differs = differs(readerOrder);
            for (List<List<String>> other : otherOrders) {
                differs |= differs(other);
            }
            if (!differs) {
                return;
            }
            Map<List<String>, Integer> seen = new IdentityHashMap<>();
            for (int slot = 0; slot < slots.size(); slot++) {
                List<String> target = slots.get(slot);
                int index = seen.merge(target, 1, Integer::sum) - 1;
                target.set(index, withPosition(target.get(index), slot));
            }
        }

        private boolean differs(List<List<String>> order) {
            int last = -1;
            for (List<String> slot : slots) {
                int rank = -1;
                for (int i = 0; i < order.size(); i++) {
                    if (order.get(i) == slot) {
                        rank = i;
                    }
                }
                if (rank < last) {
                    return true;
                }
                last = rank;
            }
            return false;
        }
    }

    /**
     * SCIPIO: 4.0.0: The annotation types nest to a fixed depth (a type cannot contain itself). An element
     * beyond it, such as a container in a Container4 or a screenlet in a SectionLeaf, was dropped or folded
     * into its parent (a folded section rendered both of its branches). It moves into a helper screen of this
     * class instead; the returned widget includes it with a shared scope, which renders it in place.
     */
    protected String helperScreenInclude(Element element) {
        String helperName = iterateHelperNames.get(element);
        if (helperName == null) {
            helperName = currentScreenName + "-part" + (++partCounter);
            iterateHelperNames.put(element, helperName);
            Document owner = element.getOwnerDocument();
            Element helper = owner.createElement("screen");
            helper.setAttribute("name", helperName);
            Element section = owner.createElement("section");
            Element widgets = owner.createElement("widgets");
            widgets.appendChild(element.cloneNode(true));
            section.appendChild(widgets);
            helper.appendChild(section);
            pendingHelperScreens.add(helper);
        }
        return "@Widget(type = WidgetType.INCLUDE_SCREEN, name = " + toStringValue(helperName)
                + ", location = " + toStringValue(sourceLocation) + ", shareScope = true)";
    }

    /** SCIPIO: 4.0.0: The field of an if-true/if-false, or its value expression when it has no field. */
    protected String fieldOrValue(Element element) {
        String field = getAttr(element, "field");
        return isNotEmpty(field) ? field : getAttr(element, "value");
    }

    /** SCIPIO: 4.0.0: The XML and the runtime name it dataresource-id; data-resource-id was read, so it was always empty. */
    protected String getDataResourceIdAttr(Element element) {
        String value = getAttr(element, "dataresource-id");
        return isNotEmpty(value) ? value : getAttr(element, "data-resource-id");
    }

    /** SCIPIO: 4.0.0: Adds a position member to generated annotation code of the form @Name(...). */
    protected String withPosition(String code, int position) {
        String trimmed = code.trim();
        if (!trimmed.endsWith(")")) {
            throw new IllegalStateException("Cannot add a position to " + trimmed);
        }
        String head = trimmed.substring(0, trimmed.length() - 1);
        return head + (head.endsWith("(") ? "" : ", ") + "position = " + position + ")";
    }

    /**
     * Generates a unified @Widget annotation for a widget inside a decorator section.
     *
     * <p>This handles complex nested widgets like containers and screenlets that have
     * their own nested content. Unlike the simple generateUnifiedWidget(), this method
     * supports the full range of nested widget structures.</p>
     *
     * <p>SCIPIO: 4.0.0: Added for unified decorator section widget generation.</p>
     */
    protected String generateUnifiedWidgetForDecoratorSection(Element element, String tagName) {
        List<String> attrs = new ArrayList<>();

        switch (tagName) {
            // SCIPIO: 4.0.0: both were dropped in a decorator section
            case "iterate-section":
            case "include-portal-page":
                return generateUnifiedWidget(element, tagName);
            case "include-screen": {
                String screenLoc = getAttr(element, "location");
                // SCIPIO: 4.0.0: When no location is specified, use sourceLocation (the original XML file)
                if (isEmpty(screenLoc) && isNotEmpty(sourceLocation)) {
                    screenLoc = sourceLocation;
                }
                attrs.add("type = WidgetType.INCLUDE_SCREEN");
                attrs.add(attrIfNotEmpty("name", getAttr(element, "name")));
                attrs.add(attrIfNotEmpty("location", screenLoc));
                if ("true".equals(getAttr(element, "share-scope"))) {
                    attrs.add("shareScope = true");
                }
                break;
            }

            case "include-form":
                attrs.add("type = WidgetType.INCLUDE_FORM");
                attrs.add(attrIfNotEmpty("name", getAttr(element, "name")));
                attrs.add(attrIfNotEmpty("location", getAttr(element, "location")));
                if ("true".equals(getAttr(element, "share-scope"))) {
                    attrs.add("shareScope = true");
                }
                break;

            case "include-menu":
                attrs.add("type = WidgetType.INCLUDE_MENU");
                attrs.add(attrIfNotEmpty("name", getAttr(element, "name")));
                attrs.add(attrIfNotEmpty("location", getAttr(element, "location")));
                break;

            case "include-grid":
                attrs.add("type = WidgetType.INCLUDE_GRID");
                attrs.add(attrIfNotEmpty("name", getAttr(element, "name")));
                attrs.add(attrIfNotEmpty("location", getAttr(element, "location")));
                break;

            case "include-tree":
                attrs.add("type = WidgetType.INCLUDE_TREE");
                attrs.add(attrIfNotEmpty("name", getAttr(element, "name")));
                attrs.add(attrIfNotEmpty("location", getAttr(element, "location")));
                break;

            case "label":
                attrs.add("type = WidgetType.LABEL");
                // SCIPIO: 4.0.0: label text may be element content, not the text attribute (was dropped)
                String labelText = getAttr(element, "text");
                if (isEmpty(labelText)) {
                    labelText = element.getTextContent();
                }
                attrs.add(attrIfNotEmpty("text", labelText));
                attrs.add(attrIfNotEmpty("style", getAttr(element, "style")));
                attrs.add(attrIfNotEmpty("id", getAttr(element, "id")));
                break;

            case "decorator-section-include":
                attrs.add("type = WidgetType.DECORATOR_SECTION_INCLUDE");
                attrs.add(attrIfNotEmpty("name", getAttr(element, "name")));
                break;

            case "horizontal-separator":
                attrs.add("type = WidgetType.HORIZONTAL_SEPARATOR");
                attrs.add(attrIfNotEmpty("style", getAttr(element, "style")));
                attrs.add(attrIfNotEmpty("id", getAttr(element, "id")));
                break;

            case "image":
                attrs.add("type = WidgetType.IMAGE");
                attrs.add(attrIfNotEmpty("src", getAttr(element, "src")));
                attrs.add(attrIfNotEmpty("alt", getAttr(element, "alt")));
                attrs.add(attrIfNotEmpty("style", getAttr(element, "style")));
                attrs.add(attrIfNotEmpty("id", getAttr(element, "id")));
                attrs.add(attrIfNotEmpty("title", getAttr(element, "title")));
                attrs.add(attrIfNotEmpty("width", getAttr(element, "width")));
                attrs.add(attrIfNotEmpty("height", getAttr(element, "height")));
                break;

            case "link":
                attrs.add("type = WidgetType.LINK");
                attrs.add(attrIfNotEmpty("text", getAttr(element, "text")));
                attrs.add(attrIfNotEmpty("style", getAttr(element, "style")));
                attrs.add(attrIfNotEmpty("target", getAttr(element, "target")));
                attrs.add(attrIfNotEmpty("targetWindow", getAttr(element, "target-window")));
                attrs.add(attrIfNotEmpty("urlMode", getAttr(element, "url-mode")));
                break;

            case "content":
                attrs.add("type = WidgetType.CONTENT");
                attrs.add(attrIfNotEmpty("contentId", getAttr(element, "content-id")));
                attrs.add(attrIfNotEmpty("dataResourceId", getDataResourceIdAttr(element)));
                attrs.add(attrIfNotEmpty("editRequest", getAttr(element, "edit-request")));
                attrs.add(attrIfNotEmpty("editContainerStyle", getAttr(element, "edit-container-style")));
                attrs.add(attrIfNotEmpty("enableEditName", getAttr(element, "enable-edit-name")));
                attrs.add(attrIfNotEmpty("enableEditValue", getAttr(element, "enable-edit-value")));
                break;

            case "sub-content":
                attrs.add("type = WidgetType.SUB_CONTENT");
                attrs.add(attrIfNotEmpty("contentId", getAttr(element, "content-id")));
                attrs.add(attrIfNotEmpty("mapKey", getAttr(element, "map-key")));
                attrs.add(attrIfNotEmpty("assocName", getAttr(element, "assoc-name")));
                attrs.add(attrIfNotEmpty("editRequest", getAttr(element, "edit-request")));
                attrs.add(attrIfNotEmpty("editContainerStyle", getAttr(element, "edit-container-style")));
                attrs.add(attrIfNotEmpty("enableEditName", getAttr(element, "enable-edit-name")));
                attrs.add(attrIfNotEmpty("enableEditValue", getAttr(element, "enable-edit-value")));
                break;

            case "platform-specific": {
                // SCIPIO: 4.0.0: every branch (html, xsl-fo, text, xml) and every template in it becomes a
                // @Widget(type = HTML_TEMPLATE); a branch other than html carries platform. Several widgets
                // are returned joined by ", " because the caller puts the result into an array literal.
                List<String> platformWidgets = platformTemplateWidgets(element);
                if (platformWidgets.isEmpty()) {
                    return null;
                }
                return String.join(", ", platformWidgets);
            }

            case "container":
                // SCIPIO: 4.0.0: Simple containers (no nested widgets) use unified mode
                // Containers with nested content are handled separately in legacy arrays
                attrs.add("type = WidgetType.CONTAINER");
                attrs.add(attrIfNotEmpty("style", getAttr(element, "style")));
                attrs.add(attrIfNotEmpty("id", getAttr(element, "id")));
                attrs.add(attrIfNotEmpty("autoUpdateTargetId", getAttr(element, "auto-update-target-id")));
                attrs.add(attrIfNotEmpty("autoUpdateInterval", getAttr(element, "auto-update-interval")));
                attrs.add(attrIfNotEmpty("contains", getAttr(element, "contains")));
                // Note: Nested widgets can't be in unified @Widget due to Java annotation cycle limitations
                break;

            case "screenlet":
                // SCIPIO: 4.0.0: Simple screenlets (no nested widgets) use unified mode
                // Screenlets with nested content are handled separately in legacy arrays
                attrs.add("type = WidgetType.SCREENLET");
                attrs.add(attrIfNotEmpty("name", getAttr(element, "name")));
                attrs.add(attrIfNotEmpty("title", getAttr(element, "title")));
                attrs.add(attrIfNotEmpty("id", getAttr(element, "id")));
                // Note: Nested widgets can't be in unified @Widget due to Java annotation cycle limitations
                break;

            default:
                // Unknown widget type - skip
                return null;
        }

        // Filter out empty attributes
        attrs.removeIf(attr -> attr == null || attr.isEmpty());

        if (attrs.isEmpty()) {
            return null;
        }

        return "@Widget(" + String.join(", ", attrs) + ")";
    }

    /**
     * Generates the nested widgets content for a container element.
     */
    protected String generateContainerNestedWidgets(Element containerElement) {
        List<String> nestedWidgets = new ArrayList<>();
        for (Element child : childElementList(containerElement)) {
            String tagName = child.getNodeName();
            String nestedWidget = generateUnifiedWidgetForDecoratorSection(child, tagName);
            if (nestedWidget != null) {
                nestedWidgets.add(nestedWidget);
            }
        }
        if (nestedWidgets.isEmpty()) {
            return "";
        }
        return "@InlineWidgets(value = {" + String.join(", ", nestedWidgets) + "})";
    }

    /**
     * Generates a nested {@code @Widget[]} array string for container/screenlet children.
     * Returns null if no nested widgets.
     * SCIPIO: 4.0.0: Added for unified nested widget generation.
     */
    protected String generateNestedWidgetsArray(Element containerElement) {
        List<String> nestedWidgets = new ArrayList<>();
        for (Element child : childElementList(containerElement)) {
            String tagName = child.getNodeName();
            String nestedWidget = generateUnifiedWidgetForDecoratorSection(child, tagName);
            if (nestedWidget != null) {
                nestedWidgets.add(nestedWidget);
            }
        }
        if (nestedWidgets.isEmpty()) {
            return null;
        }
        return "{" + String.join(", ", nestedWidgets) + "}";
    }

    /**
     * Checks if an element (container or screenlet) has nested widget children.
     */
    protected boolean hasNestedWidgets(Element element) {
        for (Element child : childElementList(element)) {
            String tagName = child.getNodeName();
            // Check for any widget child element
            switch (tagName) {
                case "include-screen":
                case "include-form":
                case "include-menu":
                case "include-grid":
                case "include-tree":
                case "iterate-section":
                case "include-portal-page":
                case "label":
                case "container":
                case "screenlet":
                case "platform-specific":
                case "content":
                case "sub-content":
                case "image":
                case "link":
                case "decorator-section-include":
                case "decorator-screen":
                case "horizontal-separator":
                case "section":
                    return true;
            }
        }
        return false;
    }

    /**
     * Generates an @InlineSection annotation for a nested section inside decorator-section.
     * This preserves conditional logic (if-empty-section, fail-widgets, etc.) that was
     * previously lost when flattening sections.
     *
     * <p>SCIPIO: 4.0.0: Added for nested section support.</p>
     */
    protected String generateInlineSection(Element sectionElement) {
        List<String> attrs = new ArrayList<>();

        // Optional attributes
        String sectionName = getAttr(sectionElement, "name");
        if (isNotEmpty(sectionName)) {
            attrs.add("name = \"" + escapeString(sectionName) + "\"");
        }

        String shareScope = getAttr(sectionElement, "share-scope");
        if ("true".equals(shareScope)) {
            attrs.add("shareScope = true");
        }

        String id = getAttr(sectionElement, "id");
        if (isNotEmpty(id)) {
            attrs.add("id = \"" + escapeString(id) + "\"");
        }

        String style = getAttr(sectionElement, "style");
        if (isNotEmpty(style)) {
            attrs.add("style = \"" + escapeString(style) + "\"");
        }

        String contains = getAttr(sectionElement, "contains");
        if (isNotEmpty(contains)) {
            attrs.add("contains = \"" + escapeString(contains) + "\"");
        }

        // Process condition element
        Element conditionElement = firstChildElement(sectionElement, "condition");
        if (conditionElement != null) {
            String conditionCode = generateConditionAnnotation(conditionElement);
            if (isNotEmpty(conditionCode)) {
                attrs.add("condition = " + conditionCode);
            }
        }

        // Process actions element
        Element actionsElement = firstChildElement(sectionElement, "actions");
        if (actionsElement != null) {
            List<String> actionsCode = generateActionsNested(actionsElement);
            if (!actionsCode.isEmpty()) {
                attrs.add("actions = @Actions(" + String.join(", ", actionsCode) + ")");
            }
        }

        // Process widgets element
        Element widgetsElement = firstChildElement(sectionElement, "widgets");
        if (widgetsElement != null) {
            String widgetsCode = generateInlineWidgetsNested(widgetsElement);
            if (isNotEmpty(widgetsCode)) {
                attrs.add("widgets = " + widgetsCode);
            }
        }

        // Process fail-widgets element
        Element failWidgetsElement = firstChildElement(sectionElement, "fail-widgets");
        if (failWidgetsElement != null) {
            String failWidgetsCode = generateInlineWidgetsNested(failWidgetsElement);
            if (isNotEmpty(failWidgetsCode)) {
                attrs.add("failWidgets = " + failWidgetsCode);
            }
        }

        return "@InlineSection(" + String.join(", ", attrs) + ")";
    }

    /**
     * Generates an @InlineWidgets annotation for widgets inside an InlineSection.
     * Uses the unified Widget format with value array to preserve document order.
     *
     * <p>SCIPIO: 4.0.0: Updated to use unified Widget format for order preservation.</p>
     */
    protected String generateInlineWidgetsNested(Element widgetsElement) {
        // SCIPIO: 4.0.0: only the root call of generateWidgetAnnotations moves decorators into helper screens
        boolean decoratorHelpers = decoratorsToHelpers;
        decoratorsToHelpers = false;
        // Generate unified @Widget annotations in document order (preserves render order)
        List<String> unifiedWidgets = new ArrayList<>();

        // SCIPIO: 4.0.0: Collect complex widgets that can't be represented in unified format
        List<String> screenletsCode = new ArrayList<>();
        List<String> containersCode = new ArrayList<>();
        List<String> htmlTemplatesCode = new ArrayList<>();
        List<String> decoratorCode = new ArrayList<>();
        List<String> sectionsCode = new ArrayList<>();
        // SCIPIO: 4.0.0: the reader order of InlineWidgets (addInlineWidgetsContent); generateWidgetAnnotations
        // also renames the result to @Widgets, whose reader (addWidgetsContent) puts sections first
        ChildOrder order = new ChildOrder(decoratorCode, unifiedWidgets, screenletsCode, containersCode,
                htmlTemplatesCode, sectionsCode)
                .alsoFor(decoratorCode, unifiedWidgets, sectionsCode, screenletsCode, containersCode, htmlTemplatesCode);

        for (Element child : childElementList(widgetsElement)) {
            order.before();
            String tagName = child.getNodeName();
            // SCIPIO: 4.0.0: a nested decorator-screen was dropped (e.g. FindBillingAccount)
            if ("decorator-screen".equals(tagName)) {
                if (decoratorHelpers) {
                    unifiedWidgets.add(helperScreenInclude(child));
                } else if (decoratorCode.isEmpty()) {
                    decoratorCode.add(generateDecoratorScreenNested(child));
                } else {
                    System.err.println("WARN: second decorator-screen in one inline widgets block cannot be represented (dropped)");
                }
            } else {
                String unifiedWidget = generateUnifiedWidget(child, tagName);
                if (unifiedWidget != null) {
                    unifiedWidgets.add(unifiedWidget);
                } else if ("section".equals(tagName)) {
                    // SCIPIO: 4.0.0: a section with a condition, actions, name or contains was skipped, which
                    // dropped e.g. the whole body of CommonInvoicesDecorator; it is a @SectionNested now
                    if (firstChildElement(child, "condition") != null || firstChildElement(child, "actions") != null
                            || isNotEmpty(getAttr(child, "name")) || isNotEmpty(getAttr(child, "contains"))) {
                        sectionsCode.add(generateSectionNestedAtDepth(child, 1));
                    } else {
                        ChildOrder savedOrder = flattenOrder;
                        flattenOrder = order;
                        flattenSectionWidgetsExtended(child, unifiedWidgets, screenletsCode, containersCode, htmlTemplatesCode);
                        flattenOrder = savedOrder;
                    }
                } else if ("screenlet".equals(tagName)) {
                    // SCIPIO: 4.0.0: Handle screenlets that can't be unified (have nested content)
                    String screenletCode = generateScreenlet(child, "");
                    if (isNotEmpty(screenletCode)) {
                        screenletsCode.add(screenletCode);
                    }
                } else if ("container".equals(tagName)) {
                    // SCIPIO: 4.0.0: Handle containers that can't be unified (have nested content)
                    String containerCode = generateContainerNested(child);
                    if (isNotEmpty(containerCode)) {
                        containersCode.add(containerCode);
                    }
                } else if ("platform-specific".equals(tagName)) {
                    // SCIPIO: 4.0.0: Handle html-templates
                    htmlTemplatesCode.addAll(generateHtmlTemplatesNested(child));
                }
            }
            order.after();
        }
        order.applyPositions();

        // Build the @InlineWidgets annotation
        List<String> attrs = new ArrayList<>();
        if (!decoratorCode.isEmpty()) {
            attrs.add("decorator = " + decoratorCode.get(0));
        }
        if (!unifiedWidgets.isEmpty()) {
            attrs.add("value = {" + String.join(", ", unifiedWidgets) + "}");
        }
        // SCIPIO: 4.0.0: Add complex widgets using legacy arrays
        if (!screenletsCode.isEmpty()) {
            attrs.add("screenlets = {" + String.join(", ", screenletsCode) + "}");
        }
        if (!containersCode.isEmpty()) {
            attrs.add("containers = {" + String.join(", ", containersCode) + "}");
        }
        if (!htmlTemplatesCode.isEmpty()) {
            attrs.add("htmlTemplates = {" + String.join(", ", htmlTemplatesCode) + "}");
        }
        if (!sectionsCode.isEmpty()) {
            attrs.add("sections = {" + String.join(", ", sectionsCode) + "}");
        }

        if (attrs.isEmpty()) {
            return "";
        }

        return "@InlineWidgets(" + String.join(", ", attrs) + ")";
    }

    protected String generateDecoratorSectionIncludeNested(Element element) {
        String name = getAttr(element, "name");

        List<String> attrs = new ArrayList<>();
        attrs.add(attrIfNotEmpty("name", name));

        return "@DecoratorSectionInclude(" + joinAttrs(attrs.toArray(new String[0])) + ")";
    }

    /**
     * Recursively extracts widget content from a nested section element.
     *
     * <p>Since Java annotations don't support self-referential types (InlineSection cannot
     * contain InlineSection), we flatten nested section content into the parent's widget lists.
     * This preserves the widget content but loses nested conditional logic.</p>
     *
     * <p>SCIPIO: 4.0.0: Added to support nested sections in decorator-sections.</p>
     */
    protected void extractNestedSectionWidgets(Element sectionElement,
            List<String> includeScreensCode, List<String> includeFormsCode,
            List<String> includeMenusCode, List<String> labelsCode,
            List<String> screenletsCode, List<String> containersCode,
            List<String> htmlTemplatesCode, List<String> imagesCode,
            List<String> horizontalSeparatorsCode, List<String> contentsCode,
            List<String> decoratorSectionIncludesCode) {

        // Process widgets element
        Element widgetsElement = firstChildElement(sectionElement, "widgets");
        if (widgetsElement != null) {
            extractWidgetsContent(widgetsElement, includeScreensCode, includeFormsCode,
                    includeMenusCode, labelsCode, screenletsCode, containersCode,
                    htmlTemplatesCode, imagesCode, horizontalSeparatorsCode,
                    contentsCode, decoratorSectionIncludesCode);
        }

        // Also process fail-widgets element - merge into same lists
        // (loses conditional distinction but preserves content)
        Element failWidgetsElement = firstChildElement(sectionElement, "fail-widgets");
        if (failWidgetsElement != null) {
            extractWidgetsContent(failWidgetsElement, includeScreensCode, includeFormsCode,
                    includeMenusCode, labelsCode, screenletsCode, containersCode,
                    htmlTemplatesCode, imagesCode, horizontalSeparatorsCode,
                    contentsCode, decoratorSectionIncludesCode);
        }
    }

    /**
     * Extracts widget content from a widgets or fail-widgets element into the provided lists.
     * Recursively handles nested sections.
     *
     * <p>SCIPIO: 4.0.0: Added to support nested sections in decorator-sections.</p>
     */
    protected void extractWidgetsContent(Element widgetsElement,
            List<String> includeScreensCode, List<String> includeFormsCode,
            List<String> includeMenusCode, List<String> labelsCode,
            List<String> screenletsCode, List<String> containersCode,
            List<String> htmlTemplatesCode, List<String> imagesCode,
            List<String> horizontalSeparatorsCode, List<String> contentsCode,
            List<String> decoratorSectionIncludesCode) {

        for (Element child : childElementList(widgetsElement)) {
            String tagName = child.getNodeName();
            switch (tagName) {
                case "include-screen":
                    includeScreensCode.add(generateIncludeScreenNested(child));
                    break;
                case "include-form":
                    includeFormsCode.add(generateIncludeFormNested(child));
                    break;
                case "include-menu":
                    includeMenusCode.add(generateIncludeMenuNested(child));
                    break;
                case "label":
                    labelsCode.add(generateLabelNested(child));
                    break;
                case "screenlet":
                    screenletsCode.add(generateScreenletNested(child));
                    break;
                case "container":
                    containersCode.add(generateContainerNested(child));
                    break;
                case "platform-specific":
                    htmlTemplatesCode.addAll(generateHtmlTemplatesNested(child));
                    break;
                case "image":
                    imagesCode.add(generateImageNested(child));
                    break;
                case "horizontal-separator":
                    horizontalSeparatorsCode.add(generateHorizontalSeparatorNested(child));
                    break;
                case "content":
                    contentsCode.add(generateContentNested(child));
                    break;
                case "decorator-section-include":
                    decoratorSectionIncludesCode.add(generateDecoratorSectionIncludeNested(child));
                    break;
                case "section":
                    // Only extract from nested sections that have NO condition
                    // to preserve correct conditional behavior
                    Element innerCondition = firstChildElement(child, "condition");
                    if (innerCondition == null) {
                        extractNestedSectionWidgets(child, includeScreensCode, includeFormsCode,
                                includeMenusCode, labelsCode, screenletsCode, containersCode,
                                htmlTemplatesCode, imagesCode, horizontalSeparatorsCode,
                                contentsCode, decoratorSectionIncludesCode);
                    }
                    else if (containersCode != null) {
                        // SCIPIO: 4.0.0: conditional nested section: wrap in a synthetic container so the condition survives
                        // as @Container(sections = {@SectionNested(condition = ..., widgets = ..., failWidgets = ...)})
                        Element wrapper = child.getOwnerDocument().createElement("container");
                        wrapper.appendChild(child.cloneNode(true));
                        String wrapperCode = generateContainerLevel(wrapper, 1);
                        if (isNotEmpty(wrapperCode)) containersCode.add(wrapperCode);
                    }
                    break;
            }
        }
    }

    protected String generateIncludeScreen(Element element) {
        String name = getAttr(element, "name");
        String location = getAttr(element, "location");
        // SCIPIO: 4.0.0: When no location is specified, use sourceLocation (the original XML file)
        // This is needed because annotation-based screens are loaded separately, so "same file" lookups don't work
        if (isEmpty(location) && isNotEmpty(sourceLocation)) {
            location = sourceLocation;
        }

        List<String> attrs = new ArrayList<>();
        attrs.add(attrIfNotEmpty("name", name));
        attrs.add(attrIfNotEmpty("location", location));

        return "@IncludeScreen(" + joinAttrs(attrs.toArray(new String[0])) + ")";
    }

    protected String generateIncludeScreenNested(Element element) {
        String name = getAttr(element, "name");
        String location = getAttr(element, "location");
        // SCIPIO: 4.0.0: When no location is specified, use sourceLocation (the original XML file)
        // This is needed because annotation-based screens are loaded separately, so "same file" lookups don't work
        if (isEmpty(location) && isNotEmpty(sourceLocation)) {
            location = sourceLocation;
        }

        List<String> attrs = new ArrayList<>();
        attrs.add(attrIfNotEmpty("name", name));
        attrs.add(attrIfNotEmpty("location", location));

        return "@IncludeScreen(" + joinAttrs(attrs.toArray(new String[0])) + ")";
    }

    protected String generateIncludeForm(Element element) {
        String name = getAttr(element, "name");
        String location = getAttr(element, "location");

        List<String> attrs = new ArrayList<>();
        attrs.add(attrIfNotEmpty("name", name));
        attrs.add(attrIfNotEmpty("location", location));

        return "@IncludeForm(" + joinAttrs(attrs.toArray(new String[0])) + ")";
    }

    protected String generateIncludeFormNested(Element element) {
        String name = getAttr(element, "name");
        String location = getAttr(element, "location");

        List<String> attrs = new ArrayList<>();
        attrs.add(attrIfNotEmpty("name", name));
        attrs.add(attrIfNotEmpty("location", location));

        return "@IncludeForm(" + joinAttrs(attrs.toArray(new String[0])) + ")";
    }

    protected String generateIncludeMenu(Element element) {
        String name = getAttr(element, "name");
        String location = getAttr(element, "location");

        List<String> attrs = new ArrayList<>();
        attrs.add(attrIfNotEmpty("name", name));
        attrs.add(attrIfNotEmpty("location", location));

        return "@IncludeMenu(" + joinAttrs(attrs.toArray(new String[0])) + ")";
    }

    protected String generateIncludeMenuNested(Element element) {
        String name = getAttr(element, "name");
        String location = getAttr(element, "location");

        List<String> attrs = new ArrayList<>();
        attrs.add(attrIfNotEmpty("name", name));
        attrs.add(attrIfNotEmpty("location", location));

        return "@IncludeMenu(" + joinAttrs(attrs.toArray(new String[0])) + ")";
    }

    protected String generateScreenlet(Element element, String screenName) {
        String title = getAttr(element, "title");
        String id = getAttr(element, "id");
        String name = getAttr(element, "name");
        String collapsible = getAttr(element, "collapsible");
        String initiallyCollapsed = getAttr(element, "initially-collapsed");
        String saveCollapsed = getAttr(element, "save-collapsed");
        String padded = getAttr(element, "padded");
        String menubar = getAttr(element, "menubar");
        String tabMenu = getAttr(element, "tab-menu");
        String tabMenuId = getAttr(element, "tab-menu-id");

        List<String> attrs = new ArrayList<>();
        attrs.add(attrIfNotEmpty("title", title));
        // Use 'name' attribute, not 'id' - Screenlet annotation uses 'name'
        // Prefer explicit 'name' attribute, fallback to 'id' for backward compatibility
        String screenletName = isNotEmpty(name) ? name : id;
        attrs.add(attrIfNotEmpty("name", screenletName));
        if ("true".equals(collapsible)) {
            attrs.add("collapsible = true");
        }
        if ("true".equals(initiallyCollapsed)) {
            attrs.add("initiallyCollapsed = true");
        }
        if ("true".equals(saveCollapsed)) {
            attrs.add("saveCollapsed = true");
        }
        if ("true".equals(padded)) {
            attrs.add("padded = true");
        }
        attrs.add(attrIfNotEmpty("menubar", menubar));
        attrs.add(attrIfNotEmpty("tabMenu", tabMenu));
        attrs.add(attrIfNotEmpty("tabMenuId", tabMenuId));

        // Screenlet now supports all these widgets directly (no InlineWidgets wrapper needed)
        // Supported: includeForms, includeScreens, includeMenus, labels, htmlTemplates, containers
        List<String> widgetsCode = new ArrayList<>();
        List<String> includeFormsCode = new ArrayList<>();
        List<String> screenletActionsCode = new ArrayList<>(); // SCIPIO: 4.0.0: screenlet/section/actions
        List<String> screenletSectionsCode = new ArrayList<>(); // SCIPIO: 4.0.0: conditional screenlet sections
        List<String> includeScreensCode = new ArrayList<>();
        List<String> includeMenusCode = new ArrayList<>();
        List<String> labelsCode = new ArrayList<>();
        List<String> htmlTemplatesCode = new ArrayList<>();
        List<String> containersCode = new ArrayList<>();
        List<String> screenletsCode = new ArrayList<>();
        List<String> decoratorSectionIncludesCode = new ArrayList<>();

        ChildOrder order = new ChildOrder(widgetsCode, includeFormsCode, includeScreensCode, includeMenusCode, htmlTemplatesCode, labelsCode, containersCode, screenletsCode, screenletSectionsCode, decoratorSectionIncludesCode);
        for (Element child : childElementList(element)) {
            order.before();
            String tagName = child.getNodeName();
            switch (tagName) {
                case "include-form":
                    includeFormsCode.add(generateIncludeFormNested(child));
                    break;
                case "include-screen":
                    includeScreensCode.add(generateIncludeScreenNested(child));
                    break;
                case "include-menu":
                    includeMenusCode.add(generateIncludeMenuNested(child));
                    break;
                case "label":
                    labelsCode.add(generateLabelNested(child));
                    break;
                case "decorator-section-include":
                    decoratorSectionIncludesCode.add(generateDecoratorSectionIncludeNested(child));
                    break;
                case "platform-specific":
                    htmlTemplatesCode.addAll(generateHtmlTemplatesNested(child));
                    break;
                case "container":
                    containersCode.add(generateContainerNested(child));
                    break;
                case "screenlet":
                    // SCIPIO: 4.0.0: a screenlet in a screenlet was dropped (ApprovedProductRequirements)
                    screenletsCode.add(generateScreenletNested(child));
                    break;
                case "section":
                    // SCIPIO: 4.0.0: a section with a condition (or fail-widgets) stays a real section (@SectionNested)
                    if (firstChildElement(child, "condition") != null || firstChildElement(child, "fail-widgets") != null) {
                        String sectionCode = generateSectionNestedAtDepth(child, 1);
                        if (sectionCode != null) {
                            screenletSectionsCode.add(sectionCode);
                        } else {
                            screenletActionsCode.add("/* TODO: Unsupported conditional screenlet section */");
                        }
                        break;
                    }
                    // SCIPIO: 4.0.0: keep the section actions (were silently dropped) as screenlet-level actions
                    Element sectionActions = firstChildElement(child, "actions");
                    if (sectionActions != null) {
                        {
                            for (String frag : generateActionsNested(sectionActions)) {
                                if (frag.startsWith("value = {") && frag.endsWith("}")) {
                                    screenletActionsCode.add(frag.substring("value = {".length(), frag.length() - 1));
                                } else {
                                    screenletActionsCode.add("/* TODO: Unsupported screenlet actions fragment: " + frag.replace("*/", "* /") + " */");
                                }
                            }
                        }
                    }
                    // Flatten sections - collect widgets into appropriate lists
                    ChildOrder savedOrder = flattenOrder;
                    flattenOrder = order;
                    flattenSectionToDecoratorSection(child, includeFormsCode, includeScreensCode,
                            includeMenusCode, null, labelsCode, containersCode, htmlTemplatesCode);
                    flattenOrder = savedOrder;
                    break;
                default: {
                    // SCIPIO: 4.0.0: link, content, include-tree, iterate-section, ... were dropped
                    String widgetCode = generateUnifiedWidget(child, tagName);
                    if (isNotEmpty(widgetCode)) {
                        widgetsCode.add(widgetCode);
                    } else if (hasChildElements(child)) {
                        // e.g. a screenlet in a ScreenletNested, which cannot contain itself
                        widgetsCode.add(helperScreenInclude(child));
                    }
                }
            }
            order.after();
        }
        order.applyPositions();

        // Add all widget attributes directly (Screenlet supports all of these)
        if (!screenletActionsCode.isEmpty()) {
            attrs.add("actions = @Actions(value = {" + String.join(", ", screenletActionsCode) + "})");
        }
        if (!includeFormsCode.isEmpty()) {
            attrs.add("includeForms = {" + String.join(", ", includeFormsCode) + "}");
        }
        if (!includeScreensCode.isEmpty()) {
            attrs.add("includeScreens = {" + String.join(", ", includeScreensCode) + "}");
        }
        if (!includeMenusCode.isEmpty()) {
            attrs.add("includeMenus = {" + String.join(", ", includeMenusCode) + "}");
        }
        if (!labelsCode.isEmpty()) {
            attrs.add("labels = {" + String.join(", ", labelsCode) + "}");
        }
        if (!htmlTemplatesCode.isEmpty()) {
            attrs.add("htmlTemplates = {" + String.join(", ", htmlTemplatesCode) + "}");
        }
        if (!containersCode.isEmpty()) {
            attrs.add("containers = {" + String.join(", ", containersCode) + "}");
        }
        if (!screenletsCode.isEmpty()) {
            attrs.add("screenlets = {" + String.join(", ", screenletsCode) + "}");
        }

        if (!screenletSectionsCode.isEmpty()) {
            attrs.add("sections = {" + String.join(", ", screenletSectionsCode) + "}");
        }
        if (!decoratorSectionIncludesCode.isEmpty()) {
            attrs.add("decoratorSectionIncludes = {" + String.join(", ", decoratorSectionIncludesCode) + "}");
        }

        if (!widgetsCode.isEmpty()) {
            attrs.add("widgets = {" + String.join(", ", widgetsCode) + "}");
        }
        return "@Screenlet(" + joinAttrs(attrs.toArray(new String[0])) + ")";
    }

    /**
     * Generates a @ScreenletNested annotation for use inside containers.
     * Uses Container2 for nested containers to avoid cyclic type references.
     * SCIPIO: 4.0.0: Added for nested screenlet support.
     */
    protected String generateScreenletNested(Element element) {
        String title = getAttr(element, "title");
        String name = getAttr(element, "name");
        String collapsible = getAttr(element, "collapsible");
        String initiallyCollapsed = getAttr(element, "initially-collapsed");
        String saveCollapsed = getAttr(element, "save-collapsed");
        String padded = getAttr(element, "padded");
        String titleStyle = getAttr(element, "title-style");
        String navigationMenuName = getAttr(element, "navigation-menu-name");
        String navigationFormName = getAttr(element, "navigation-form-name");
        String tabMenuName = getAttr(element, "tab-menu-name");

        List<String> attrs = new ArrayList<>();
        attrs.add(attrIfNotEmpty("title", title));
        attrs.add(attrIfNotEmpty("name", name));
        attrs.add(attrIfNotEmpty("id", getAttr(element, "id")));
        if ("true".equals(collapsible)) {
            attrs.add("collapsible = true");
        }
        if ("true".equals(initiallyCollapsed)) {
            attrs.add("initiallyCollapsed = true");
        }
        if ("false".equals(saveCollapsed)) {
            attrs.add("saveCollapsed = false");
        }
        if ("false".equals(padded)) {
            attrs.add("padded = false");
        }
        attrs.add(attrIfNotEmpty("titleStyle", titleStyle));
        attrs.add(attrIfNotEmpty("navigationMenuName", navigationMenuName));
        attrs.add(attrIfNotEmpty("navigationFormName", navigationFormName));
        attrs.add(attrIfNotEmpty("tabMenuName", tabMenuName));

        // SCIPIO: 4.0.0: ScreenletNested now supports containers via ContainerInScreenlet
        List<String> widgetsCode = new ArrayList<>();
        List<String> containersCode = new ArrayList<>();
        List<String> decoratorSectionIncludesCode = new ArrayList<>();
        List<String> includeFormsCode = new ArrayList<>();
        List<String> includeScreensCode = new ArrayList<>();
        List<String> includeMenusCode = new ArrayList<>();
        List<String> labelsCode = new ArrayList<>();
        List<String> htmlTemplatesCode = new ArrayList<>();
        List<String> sectionsCode = new ArrayList<>();

        ChildOrder order = new ChildOrder(widgetsCode, sectionsCode, containersCode, includeFormsCode, includeScreensCode, includeMenusCode, labelsCode, htmlTemplatesCode, decoratorSectionIncludesCode);
        for (Element child : childElementList(element)) {
            order.before();
            String tagName = child.getNodeName();
            switch (tagName) {
                case "include-form":
                    includeFormsCode.add(generateIncludeFormNested(child));
                    break;
                case "include-screen":
                    includeScreensCode.add(generateIncludeScreenNested(child));
                    break;
                case "include-menu":
                    includeMenusCode.add(generateIncludeMenuNested(child));
                    break;
                case "label":
                    labelsCode.add(generateLabelNested(child));
                    break;
                // SCIPIO: 4.0.0: a section here was flattened, losing its condition and actions
                case "section":
                    sectionsCode.add(generateSectionLeaf(child));
                    break;
                case "decorator-section-include":
                    decoratorSectionIncludesCode.add(generateDecoratorSectionIncludeNested(child));
                    break;
                case "platform-specific":
                    htmlTemplatesCode.addAll(generateHtmlTemplatesNested(child));
                    break;
                case "container":
                    // SCIPIO: 4.0.0: Use ContainerInScreenlet for proper container support
                    containersCode.add(generateContainerInScreenlet(child));
                    break;
                default: {
                    // SCIPIO: 4.0.0: link, content, include-tree, iterate-section, ... were dropped
                    String widgetCode = generateUnifiedWidget(child, tagName);
                    if (isNotEmpty(widgetCode)) {
                        widgetsCode.add(widgetCode);
                    } else if (hasChildElements(child)) {
                        // e.g. a screenlet in a ScreenletNested, which cannot contain itself
                        widgetsCode.add(helperScreenInclude(child));
                    }
                }
            }
            order.after();
        }
        order.applyPositions();

        // Containers first (for proper layout order)
        if (!containersCode.isEmpty()) {
            attrs.add("containers = {" + String.join(", ", containersCode) + "}");
        }
        if (!includeFormsCode.isEmpty()) {
            attrs.add("includeForms = {" + String.join(", ", includeFormsCode) + "}");
        }
        if (!includeScreensCode.isEmpty()) {
            attrs.add("includeScreens = {" + String.join(", ", includeScreensCode) + "}");
        }
        if (!includeMenusCode.isEmpty()) {
            attrs.add("includeMenus = {" + String.join(", ", includeMenusCode) + "}");
        }
        if (!labelsCode.isEmpty()) {
            attrs.add("labels = {" + String.join(", ", labelsCode) + "}");
        }
        if (!htmlTemplatesCode.isEmpty()) {
            attrs.add("htmlTemplates = {" + String.join(", ", htmlTemplatesCode) + "}");
        }

        if (!decoratorSectionIncludesCode.isEmpty()) {
            attrs.add("decoratorSectionIncludes = {" + String.join(", ", decoratorSectionIncludesCode) + "}");
        }

        if (!sectionsCode.isEmpty()) {
            attrs.add("sections = {" + String.join(", ", sectionsCode) + "}");
        }
        if (!widgetsCode.isEmpty()) {
            attrs.add("widgets = {" + String.join(", ", widgetsCode) + "}");
        }
        return formatAnnotation("@ScreenletNested(" + joinAttrs(attrs.toArray(new String[0])) + ")", 4, 120);
    }

    /**
     * Flattens section widgets for use in ScreenletNested.
     * SCIPIO: 4.0.0: Added for nested screenlet support.
     */
    protected void flattenSectionToScreenletNested(Element sectionElement,
            List<String> containersCode, List<String> includeFormsCode, List<String> includeScreensCode,
            List<String> includeMenusCode, List<String> labelsCode, List<String> htmlTemplatesCode) {
        Element widgetsElement = firstChildElement(sectionElement, "widgets");
        if (widgetsElement == null) {
            return;
        }
        for (Element child : childElementList(widgetsElement)) {
            String tagName = child.getNodeName();
            switch (tagName) {
                case "include-form":
                    includeFormsCode.add(generateIncludeFormNested(child));
                    break;
                case "include-screen":
                    includeScreensCode.add(generateIncludeScreenNested(child));
                    break;
                case "include-menu":
                    includeMenusCode.add(generateIncludeMenuNested(child));
                    break;
                case "label":
                    labelsCode.add(generateLabelNested(child));
                    break;
                case "platform-specific":
                    htmlTemplatesCode.addAll(generateHtmlTemplatesNested(child));
                    break;
                case "container":
                    // Preserve container nesting via ContainerInScreenlet
                    containersCode.add(generateContainerInScreenlet(child));
                    break;
                case "section":
                    // Recurse
                    flattenSectionToScreenletNested(child, containersCode, includeFormsCode, includeScreensCode,
                            includeMenusCode, labelsCode, htmlTemplatesCode);
                    break;
            }
        }
    }

    /**
     * Generates a @ContainerInScreenlet annotation for containers inside screenlets.
     * SCIPIO: 4.0.0: Added to support containers inside ScreenletNested.
     */
    /**
     * SCIPIO: 4.0.0: Generates a @SectionLeaf for a section that has run out of SectionNested levels.
     *
     * <p>Used inside a screenlet container and at container depth 4. Both places previously either
     * dropped the section outright or flattened it, which merged its actions with its siblings'.</p>
     */
    protected String generateSectionLeaf(Element element) {
        List<String> attrs = new ArrayList<>();
        attrs.add(attrIfNotEmpty("name", getAttr(element, "name")));
        attrs.add(attrIfNotEmpty("contains", getAttr(element, "contains")));
        attrs.add(attrIfNotEmpty("id", getAttr(element, "id")));
        attrs.add(attrIfNotEmpty("style", getAttr(element, "style")));
        if ("true".equals(getAttr(element, "share-scope"))) {
            attrs.add("shareScope = true");
        }

        Element conditionElement = firstChildElement(element, "condition");
        if (conditionElement != null) {
            List<String> conditionCodes = new ArrayList<>();
            collectFunctionalConditions(conditionElement, conditionCodes);
            if (conditionCodes.size() == 1) {
                attrs.add("condition = " + conditionCodes.get(0));
            } else if (conditionCodes.size() > 1) {
                // Sibling conditions are ANDed; carry them as one And tree.
                String tree = generateConditionTree(conditionElement, "And", false);
                if (tree != null) {
                    attrs.add("condition = " + tree);
                }
            }
        }

        Element actionsElement = firstChildElement(element, "actions");
        if (actionsElement != null) {
            List<String> actionsCode = generateActionsNested(actionsElement);
            if (!actionsCode.isEmpty()) {
                attrs.add("actions = @Actions(" + String.join(", ", actionsCode) + ")");
            }
        }

        Element widgetsElement = firstChildElement(element, "widgets");
        if (widgetsElement != null) {
            String code = generateWidgetsLeaf(widgetsElement);
            if (isNotEmpty(code)) {
                attrs.add("widgets = " + code);
            }
        }
        Element failWidgetsElement = firstChildElement(element, "fail-widgets");
        if (failWidgetsElement != null) {
            String code = generateWidgetsLeaf(failWidgetsElement);
            if (isNotEmpty(code)) {
                attrs.add("failWidgets = " + code);
            }
        }
        return "@SectionLeaf(" + joinAttrs(attrs.toArray(new String[0])) + ")";
    }

    /** SCIPIO: 4.0.0: Generates the @WidgetsLeaf of a @SectionLeaf. */
    protected String generateWidgetsLeaf(Element widgetsElement) {
        List<String> valueCode = new ArrayList<>();
        List<String> containersCode = new ArrayList<>();
        List<String> includeFormsCode = new ArrayList<>();
        List<String> includeScreensCode = new ArrayList<>();
        List<String> includeMenusCode = new ArrayList<>();
        List<String> labelsCode = new ArrayList<>();
        List<String> htmlTemplatesCode = new ArrayList<>();
        List<String> dsiCode = new ArrayList<>();
        ChildOrder order = new ChildOrder(valueCode, containersCode, includeScreensCode, includeFormsCode,
                includeMenusCode, labelsCode, htmlTemplatesCode, dsiCode);
        collectLeafChildren(widgetsElement, valueCode, containersCode, includeFormsCode, includeScreensCode,
                includeMenusCode, labelsCode, htmlTemplatesCode, dsiCode, true, order);
        order.applyPositions();

        List<String> attrs = new ArrayList<>();
        if (!valueCode.isEmpty()) attrs.add("value = {" + String.join(", ", valueCode) + "}");
        if (!containersCode.isEmpty()) attrs.add("containers = {" + String.join(", ", containersCode) + "}");
        if (!includeFormsCode.isEmpty()) attrs.add("includeForms = {" + String.join(", ", includeFormsCode) + "}");
        if (!includeScreensCode.isEmpty()) attrs.add("includeScreens = {" + String.join(", ", includeScreensCode) + "}");
        if (!includeMenusCode.isEmpty()) attrs.add("includeMenus = {" + String.join(", ", includeMenusCode) + "}");
        if (!labelsCode.isEmpty()) attrs.add("labels = {" + String.join(", ", labelsCode) + "}");
        if (!htmlTemplatesCode.isEmpty()) attrs.add("htmlTemplates = {" + String.join(", ", htmlTemplatesCode) + "}");
        if (!dsiCode.isEmpty()) attrs.add("decoratorSectionIncludes = {" + String.join(", ", dsiCode) + "}");
        if (attrs.isEmpty()) {
            return null;
        }
        return "@WidgetsLeaf(" + String.join(", ", attrs) + ")";
    }

    /** SCIPIO: 4.0.0: Generates a @ContainerLeaf, the one container level a @SectionLeaf allows. */
    protected String generateContainerLeaf(Element element) {
        List<String> attrs = new ArrayList<>();
        attrs.add(attrIfNotEmpty("id", getAttr(element, "id")));
        attrs.add(attrIfNotEmpty("style", getAttr(element, "style")));
        attrs.add(attrIfNotEmpty("type", getAttr(element, "type")));
        attrs.add(attrIfNotEmpty("contains", getAttr(element, "contains")));
        attrs.add(attrIfNotEmpty("autoUpdateTargetId", getAttr(element, "auto-update-target-id")));
        attrs.add(attrIfNotEmpty("autoUpdateInterval", getAttr(element, "auto-update-interval")));

        List<String> valueCode = new ArrayList<>();
        List<String> includeFormsCode = new ArrayList<>();
        List<String> includeScreensCode = new ArrayList<>();
        List<String> includeMenusCode = new ArrayList<>();
        List<String> labelsCode = new ArrayList<>();
        List<String> htmlTemplatesCode = new ArrayList<>();
        List<String> dsiCode = new ArrayList<>();
        // A ContainerLeaf holds no further container, so a nested one is folded in place.
        ChildOrder order = new ChildOrder(valueCode, includeFormsCode, includeScreensCode, includeMenusCode,
                labelsCode, htmlTemplatesCode, dsiCode);
        collectLeafChildren(element, valueCode, null, includeFormsCode, includeScreensCode,
                includeMenusCode, labelsCode, htmlTemplatesCode, dsiCode, false, order);
        order.applyPositions();

        if (!valueCode.isEmpty()) attrs.add("widgets = {" + String.join(", ", valueCode) + "}");
        if (!includeFormsCode.isEmpty()) attrs.add("includeForms = {" + String.join(", ", includeFormsCode) + "}");
        if (!includeScreensCode.isEmpty()) attrs.add("includeScreens = {" + String.join(", ", includeScreensCode) + "}");
        if (!includeMenusCode.isEmpty()) attrs.add("includeMenus = {" + String.join(", ", includeMenusCode) + "}");
        if (!labelsCode.isEmpty()) attrs.add("labels = {" + String.join(", ", labelsCode) + "}");
        if (!htmlTemplatesCode.isEmpty()) attrs.add("htmlTemplates = {" + String.join(", ", htmlTemplatesCode) + "}");
        if (!dsiCode.isEmpty()) attrs.add("decoratorSectionIncludes = {" + String.join(", ", dsiCode) + "}");
        return "@ContainerLeaf(" + joinAttrs(attrs.toArray(new String[0])) + ")";
    }

    /**
     * SCIPIO: 4.0.0: Collects the children of a leaf-level widgets or container element.
     *
     * @param containersCode receives @ContainerLeaf code; null folds a container's content in place
     * @param allowContainers false once inside a ContainerLeaf, which holds no further container
     */
    protected void collectLeafChildren(Element parent, List<String> valueCode, List<String> containersCode,
            List<String> includeFormsCode, List<String> includeScreensCode, List<String> includeMenusCode,
            List<String> labelsCode, List<String> htmlTemplatesCode, List<String> dsiCode, boolean allowContainers) {
        collectLeafChildren(parent, valueCode, containersCode, includeFormsCode, includeScreensCode, includeMenusCode,
                labelsCode, htmlTemplatesCode, dsiCode, allowContainers, null);
    }

    /** SCIPIO: 4.0.0: order, when given, records the top-level children; folded levels pass null. */
    protected void collectLeafChildren(Element parent, List<String> valueCode, List<String> containersCode,
            List<String> includeFormsCode, List<String> includeScreensCode, List<String> includeMenusCode,
            List<String> labelsCode, List<String> htmlTemplatesCode, List<String> dsiCode, boolean allowContainers,
            ChildOrder order) {
        for (Element child : childElementList(parent)) {
            if (order != null) {
                order.before();
            }
            String tag = child.getNodeName();
            switch (tag) {
                case "include-form":
                    includeFormsCode.add(generateIncludeFormNested(child));
                    break;
                case "include-screen":
                    includeScreensCode.add(generateIncludeScreenNested(child));
                    break;
                case "include-menu":
                    includeMenusCode.add(generateIncludeMenuNested(child));
                    break;
                case "label":
                    labelsCode.add(generateLabelNested(child));
                    break;
                case "decorator-section-include":
                    dsiCode.add(generateDecoratorSectionIncludeNested(child));
                    break;
                case "platform-specific": {
                    htmlTemplatesCode.addAll(generateHtmlTemplatesNested(child));
                    break;
                }
                case "container":
                    if (allowContainers && containersCode != null) {
                        containersCode.add(generateContainerLeaf(child));
                    } else {
                        // SCIPIO: 4.0.0: was folded into the parent, which lost its style
                        valueCode.add(helperScreenInclude(child));
                    }
                    break;
                case "section":
                    if (firstChildElement(child, "condition") == null && firstChildElement(child, "actions") == null
                            && firstChildElement(child, "fail-widgets") == null) {
                        Element branchElement = firstChildElement(child, "widgets");
                        if (branchElement != null) {
                            collectLeafChildren(branchElement, valueCode, containersCode, includeFormsCode,
                                    includeScreensCode, includeMenusCode, labelsCode, htmlTemplatesCode, dsiCode,
                                    allowContainers, order);
                        }
                    } else {
                        // SCIPIO: 4.0.0: was folded with both branches, so it rendered widgets and fail-widgets
                        valueCode.add(helperScreenInclude(child));
                    }
                    break;
                default: {
                    String code = generateUnifiedWidget(child, tag);
                    if (isNotEmpty(code)) {
                        valueCode.add(code);
                    } else if (hasChildElements(child)) {
                        // SCIPIO: 4.0.0: e.g. a screenlet with content was dropped (eventDetail)
                        valueCode.add(helperScreenInclude(child));
                    }
                }
            }
            if (order != null) {
                order.after();
            }
        }
    }

    protected String generateContainerInScreenlet(Element element) {
        String id = getAttr(element, "id");
        String style = getAttr(element, "style");

        List<String> attrs = new ArrayList<>();
        attrs.add(attrIfNotEmpty("id", id));
        attrs.add(attrIfNotEmpty("style", style));

        List<String> nestedContainersCode = new ArrayList<>();
        List<String> includeScreensCode = new ArrayList<>();
        List<String> includeFormsCode = new ArrayList<>();
        List<String> dsiCode = new ArrayList<>();
        List<String> htmlTemplatesCode = new ArrayList<>();
        List<String> labelsCode = new ArrayList<>();
        List<String> includeMenusCode = new ArrayList<>();
        List<String> widgetsCode = new ArrayList<>();
        List<String> sectionsCode = new ArrayList<>();

        ChildOrder order = new ChildOrder(dsiCode, nestedContainersCode, includeScreensCode, widgetsCode, includeFormsCode, htmlTemplatesCode, labelsCode, includeMenusCode, sectionsCode);
        for (Element child : childElementList(element)) {
            order.before();
            String tagName = child.getNodeName();
            switch (tagName) {
                case "container":
                    nestedContainersCode.add(generateContainerInScreenlet2(child));
                    break;
                case "decorator-section-include":
                    dsiCode.add(generateDecoratorSectionInclude(child));
                    break;
                case "include-screen":
                    includeScreensCode.add(generateIncludeScreenNested(child));
                    break;
                case "include-form":
                    includeFormsCode.add(generateIncludeFormNested(child));
                    break;
                // SCIPIO: 4.0.0: label/include-menu and generic widgets were silently dropped
                case "label":
                    labelsCode.add(generateLabelNested(child));
                    break;
                case "include-menu":
                    includeMenusCode.add(generateIncludeMenuNested(child));
                    break;
                // SCIPIO: 4.0.0: a section here was dropped without a warning
                case "section":
                    sectionsCode.add(generateSectionLeaf(child));
                    break;
                case "link":
                case "image":
                case "content":
                case "sub-content":
                case "horizontal-separator":
                case "include-grid":
                case "iterate-section":
                case "include-portal-page":
                case "include-tree": {
                    String genericWidget = generateUnifiedWidget(child, tagName);
                    if (isNotEmpty(genericWidget)) {
                        widgetsCode.add(genericWidget);
                    }
                    break;
                }
                case "platform-specific":
                    htmlTemplatesCode.addAll(generateHtmlTemplatesNested(child));
                    break;
            }
            order.after();
        }
        order.applyPositions();

        if (!labelsCode.isEmpty()) {
            attrs.add("labels = {" + String.join(", ", labelsCode) + "}");
        }
        if (!includeMenusCode.isEmpty()) {
            attrs.add("includeMenus = {" + String.join(", ", includeMenusCode) + "}");
        }
        if (!widgetsCode.isEmpty()) {
            attrs.add("widgets = {" + String.join(", ", widgetsCode) + "}");
        }
        if (!sectionsCode.isEmpty()) {
            attrs.add("sections = {" + String.join(", ", sectionsCode) + "}");
        }
        if (!nestedContainersCode.isEmpty()) {
            attrs.add("containers = {" + String.join(", ", nestedContainersCode) + "}");
        }
        if (!includeScreensCode.isEmpty()) {
            attrs.add("includeScreens = {" + String.join(", ", includeScreensCode) + "}");
        }
        if (!includeFormsCode.isEmpty()) {
            attrs.add("includeForms = {" + String.join(", ", includeFormsCode) + "}");
        }
        if (!htmlTemplatesCode.isEmpty()) {
            attrs.add("htmlTemplates = {" + String.join(", ", htmlTemplatesCode) + "}");
        }
        if (!dsiCode.isEmpty()) {
            attrs.add("decoratorSectionIncludes = {" + String.join(", ", dsiCode) + "}");
        }

        return "@ContainerInScreenlet(" + joinAttrs(attrs.toArray(new String[0])) + ")";
    }

    /**
     * Generates a @ContainerInScreenlet2 annotation (leaf level, no further nesting).
     * SCIPIO: 4.0.0: Added to support nested containers inside ScreenletNested.
     */
    protected String generateContainerInScreenlet2(Element element) {
        String id = getAttr(element, "id");
        String style = getAttr(element, "style");

        List<String> attrs = new ArrayList<>();
        attrs.add(attrIfNotEmpty("id", id));
        attrs.add(attrIfNotEmpty("style", style));

        List<String> includeScreensCode = new ArrayList<>();
        List<String> includeFormsCode = new ArrayList<>();
        List<String> dsiCode = new ArrayList<>();
        List<String> htmlTemplatesCode = new ArrayList<>();
        List<String> labelsCode = new ArrayList<>();
        List<String> includeMenusCode = new ArrayList<>();
        List<String> widgetsCode = new ArrayList<>();
        List<String> sectionsCode = new ArrayList<>();

        ChildOrder order = new ChildOrder(dsiCode, includeScreensCode, widgetsCode, includeFormsCode, htmlTemplatesCode, labelsCode, includeMenusCode, sectionsCode);
        for (Element child : childElementList(element)) {
            order.before();
            String tagName = child.getNodeName();
            switch (tagName) {
                case "decorator-section-include":
                    dsiCode.add(generateDecoratorSectionInclude(child));
                    break;
                case "include-screen":
                    includeScreensCode.add(generateIncludeScreenNested(child));
                    break;
                case "include-form":
                    includeFormsCode.add(generateIncludeFormNested(child));
                    break;
                // SCIPIO: 4.0.0: label/include-menu and generic widgets were silently dropped
                case "label":
                    labelsCode.add(generateLabelNested(child));
                    break;
                case "include-menu":
                    includeMenusCode.add(generateIncludeMenuNested(child));
                    break;
                // SCIPIO: 4.0.0: a section here was dropped without a warning
                case "section":
                    sectionsCode.add(generateSectionLeaf(child));
                    break;
                case "link":
                case "image":
                case "content":
                case "sub-content":
                case "horizontal-separator":
                case "include-grid":
                case "iterate-section":
                case "include-portal-page":
                case "include-tree": {
                    String genericWidget = generateUnifiedWidget(child, tagName);
                    if (isNotEmpty(genericWidget)) {
                        widgetsCode.add(genericWidget);
                    }
                    break;
                }
                case "platform-specific":
                    htmlTemplatesCode.addAll(generateHtmlTemplatesNested(child));
                    break;
                // NOTE: No nested containers at this level (leaf)
            }
            order.after();
        }
        order.applyPositions();

        if (!labelsCode.isEmpty()) {
            attrs.add("labels = {" + String.join(", ", labelsCode) + "}");
        }
        if (!includeMenusCode.isEmpty()) {
            attrs.add("includeMenus = {" + String.join(", ", includeMenusCode) + "}");
        }
        if (!widgetsCode.isEmpty()) {
            attrs.add("widgets = {" + String.join(", ", widgetsCode) + "}");
        }
        if (!sectionsCode.isEmpty()) {
            attrs.add("sections = {" + String.join(", ", sectionsCode) + "}");
        }
        if (!includeScreensCode.isEmpty()) {
            attrs.add("includeScreens = {" + String.join(", ", includeScreensCode) + "}");
        }
        if (!includeFormsCode.isEmpty()) {
            attrs.add("includeForms = {" + String.join(", ", includeFormsCode) + "}");
        }
        if (!htmlTemplatesCode.isEmpty()) {
            attrs.add("htmlTemplates = {" + String.join(", ", htmlTemplatesCode) + "}");
        }
        if (!dsiCode.isEmpty()) {
            attrs.add("decoratorSectionIncludes = {" + String.join(", ", dsiCode) + "}");
        }

        return "@ContainerInScreenlet2(" + joinAttrs(attrs.toArray(new String[0])) + ")";
    }

    protected String generateContainer(Element element, String screenName) {
        String id = getAttr(element, "id");
        String style = getAttr(element, "style");

        List<String> attrs = new ArrayList<>();
        attrs.add(attrIfNotEmpty("id", id));
        attrs.add(attrIfNotEmpty("style", style));

        return "@Container(" + joinAttrs(attrs.toArray(new String[0])) + ")";
    }

    protected String generateLabel(Element element) {
        String text = getAttr(element, "text");
        String style = getAttr(element, "style");
        String id = getAttr(element, "id");

        // Also check for text content
        if (isEmpty(text)) {
            text = element.getTextContent();
        }

        List<String> attrs = new ArrayList<>();
        attrs.add(attrIfNotEmpty("text", text));
        attrs.add(attrIfNotEmpty("style", style));
        attrs.add(attrIfNotEmpty("id", id));

        return "@Label(" + joinAttrs(attrs.toArray(new String[0])) + ")";
    }

    protected String generateLabelNested(Element element) {
        return generateLabel(element);
    }

    protected String generateContainerNested(Element element) {
        return generateContainerNested(element, 1); // Start at depth 1
    }

    /**
     * Generate a Container annotation with proper nesting hierarchy.
     *
     * @param element The container XML element
     * @param depth The nesting depth (1 = Container, 2 = Container2, 3 = Container3, 4 = Container4)
     * @return The generated @Container annotation code
     */
    /**
     * SCIPIO: 4.0.0: Generates @SectionNested{N} for a nested section (with condition/actions/widgets/fail-widgets kept)
     * at container depth N (Container -> SectionNested, Container2 -> SectionNested2, ...).
     */
    protected String generateSectionNestedAtDepth(Element sectionElement, int depth) {
        int d = Math.max(1, Math.min(depth, 4));
        List<String> attrs = new ArrayList<>();
        attrs.add(attrIfNotEmpty("name", getAttr(sectionElement, "name")));
        attrs.add(attrIfNotEmpty("contains", getAttr(sectionElement, "contains")));
        attrs.add(attrIfNotEmpty("id", getAttr(sectionElement, "id")));
        attrs.add(attrIfNotEmpty("style", getAttr(sectionElement, "style")));
        if ("true".equals(getAttr(sectionElement, "share-scope"))) attrs.add("shareScope = true");
        attrs.removeIf(a -> a == null || a.isEmpty());
        Element conditionElement = firstChildElement(sectionElement, "condition");
        if (conditionElement != null) {
            String conditionCode = generateConditionAnnotation(conditionElement);
            if (conditionCode != null) attrs.add("condition = " + conditionCode);
        }
        Element actionsElement = firstChildElement(sectionElement, "actions");
        if (actionsElement != null) {
            List<String> actionsCode = generateActionsNested(actionsElement);
            if (!actionsCode.isEmpty()) attrs.add("actions = @Actions(" + String.join(", ", actionsCode) + ")");
        }
        Element widgetsElement = firstChildElement(sectionElement, "widgets");
        if (widgetsElement != null) {
            String w = generateWidgetsForContainer(widgetsElement, d);
            if (w != null) attrs.add("widgets = " + w);
        }
        Element failWidgetsElement = firstChildElement(sectionElement, "fail-widgets");
        if (failWidgetsElement != null) {
            String w = generateWidgetsForContainer(failWidgetsElement, d);
            if (w != null) attrs.add("failWidgets = " + w);
        }
        return "@SectionNested" + (d > 1 ? String.valueOf(d) : "") + "(" + String.join(", ", attrs) + ")";
    }

    /** SCIPIO: 4.0.0: Generates @WidgetsForContainer{N} for the children of a widgets/fail-widgets element. */
    /**
     * SCIPIO: 4.0.0: Generates a @DecoratorScreenNested for a decorator-screen inside a nested section.
     *
     * <p>Its sections carry leaf-level widgets, which is what keeps the annotation types acyclic.</p>
     */
    protected String generateDecoratorScreenNested(Element element) {
        List<String> attrs = new ArrayList<>();
        attrs.add("name = " + toStringValue(getAttr(element, "name")));
        String location = getAttr(element, "location");
        if (isEmpty(location) && isNotEmpty(sourceLocation)) {
            location = sourceLocation;
        }
        attrs.add(attrIfNotEmpty("location", location));
        attrs.add(attrIfNotEmpty("fallbackName", getAttr(element, "fallback-name")));
        attrs.add(attrIfNotEmpty("fallbackLocation", getAttr(element, "fallback-location")));
        if ("true".equals(getAttr(element, "fallback-if-empty"))) {
            attrs.add("fallbackIfEmpty = true");
        }
        if ("true".equals(getAttr(element, "auto-decorator-section-include"))) {
            attrs.add("autoDecoratorSectionInclude = true");
        }
        List<String> sectionCodes = new ArrayList<>();
        for (Element section : childElementList(element, "decorator-section")) {
            List<String> sectionAttrs = new ArrayList<>();
            sectionAttrs.add("name = " + toStringValue(getAttr(section, "name")));
            sectionAttrs.add(attrIfNotEmpty("useWhen", getAttr(section, "use-when")));
            if ("true".equals(getAttr(section, "fallback-auto-include"))) {
                sectionAttrs.add("fallbackAutoInclude = true");
            }
            if ("true".equals(getAttr(section, "override-by-auto-include"))) {
                sectionAttrs.add("overrideByAutoInclude = true");
            }
            sectionAttrs.add(attrIfNotEmpty("contains", getAttr(section, "contains")));
            String widgetsCode = generateWidgetsForContainer(section, 4);
            if (isNotEmpty(widgetsCode)) {
                sectionAttrs.add("widgets = " + widgetsCode);
            }
            sectionCodes.add("@DecoratorSectionNested(" + joinAttrs(sectionAttrs.toArray(new String[0])) + ")");
        }
        if (!sectionCodes.isEmpty()) {
            attrs.add("sections = {" + String.join(", ", sectionCodes) + "}");
        }
        return "@DecoratorScreenNested(" + joinAttrs(attrs.toArray(new String[0])) + ")";
    }

    protected String generateWidgetsForContainer(Element widgetsElement, int depth) {
        List<String> valueCode = new ArrayList<>(), containersCode = new ArrayList<>(), sectionsCode = new ArrayList<>(), screenletsCode = new ArrayList<>();
        // SCIPIO: 4.0.0: a decorator-screen here was silently dropped; it is the decorator member now
        List<String> decoratorCode = new ArrayList<>();
        ChildOrder order = new ChildOrder(valueCode, decoratorCode, screenletsCode, containersCode, sectionsCode);
        collectWidgetsForContainer(widgetsElement, depth, valueCode, containersCode, sectionsCode, screenletsCode,
                decoratorCode, order);
        order.applyPositions();
        List<String> attrs = new ArrayList<>();
        if (!decoratorCode.isEmpty()) attrs.add("decorator = " + decoratorCode.get(0));
        if (!valueCode.isEmpty()) attrs.add("value = {" + String.join(", ", valueCode) + "}");
        if (!containersCode.isEmpty()) attrs.add("containers = {" + String.join(", ", containersCode) + "}");
        if (!sectionsCode.isEmpty()) attrs.add("sections = {" + String.join(", ", sectionsCode) + "}");
        if (!screenletsCode.isEmpty()) attrs.add("screenlets = {" + String.join(", ", screenletsCode) + "}");
        if (attrs.isEmpty()) return null;
        return "@WidgetsForContainer" + (depth > 1 ? String.valueOf(depth) : "") + "(" + String.join(", ", attrs) + ")";
    }

    protected void collectWidgetsForContainer(Element parent, int depth, List<String> valueCode, List<String> containersCode,
            List<String> sectionsCode, List<String> screenletsCode) {
        collectWidgetsForContainer(parent, depth, valueCode, containersCode, sectionsCode, screenletsCode, null, null);
    }

    /**
     * SCIPIO: 4.0.0: decoratorCode receives the one decorator-screen the parent may hold (null in a
     * platform-specific branch); order, when given, records the top-level children.
     */
    protected void collectWidgetsForContainer(Element parent, int depth, List<String> valueCode, List<String> containersCode,
            List<String> sectionsCode, List<String> screenletsCode, List<String> decoratorCode, ChildOrder order) {
        for (Element child : childElementList(parent)) {
            if (order != null) {
                order.before();
            }
            String tag = child.getNodeName();
            switch (tag) {
                case "decorator-screen":
                    if (depth < 4 && decoratorCode != null && decoratorCode.isEmpty()) {
                        decoratorCode.add(generateDecoratorScreenNested(child));
                    } else {
                        valueCode.add(helperScreenInclude(child));
                    }
                    break;
                case "section":
                    if (depth < 4) {
                        sectionsCode.add(generateSectionNestedAtDepth(child, depth + 1));
                    } else {
                        // SCIPIO: 4.0.0: the deepest level keeps the section as a @SectionLeaf; it used
                        // to be flattened, which lost its condition and merged its actions with its siblings'.
                        sectionsCode.add(generateSectionLeaf(child));
                    }
                    break;
                case "container":
                    containersCode.add(generateContainerLevel(child, Math.min(depth + 1, 4)));
                    break;
                case "screenlet":
                    screenletsCode.add(generateScreenletNested(child));
                    break;
                case "html-template": {
                    String htmlLocation = getAttr(child, "location");
                    String multi = getAttr(child, "multi-block");
                    // SCIPIO: 4.0.0: a template written inline (no location) was dropped (CommonSetupWizardDecorator)
                    String inline = child.getTextContent() != null ? child.getTextContent().trim() : "";
                    if (isNotEmpty(htmlLocation) || "true".equals(multi) || !inline.isEmpty()) {
                        valueCode.add("@Widget(type = WidgetType.HTML_TEMPLATE" + (isNotEmpty(htmlLocation) ? ", location = " + toStringValue(htmlLocation) : "")
                                + (isEmpty(htmlLocation) && !inline.isEmpty() ? ", content = " + toStringValue(inline) : "")
                                + ("true".equals(multi) ? ", multiBlock = true" : "") + ")");
                    }
                    break;
                }
                case "platform-specific":
                    for (Element ps : childElementList(child)) {
                        if ("html".equals(ps.getNodeName())) {
                            collectWidgetsForContainer(ps, depth, valueCode, containersCode, sectionsCode, screenletsCode);
                        } else {
                            // SCIPIO: 4.0.0: xsl-fo, text and xml branches keep their platform
                            valueCode.addAll(branchTemplateWidgets(ps));
                        }
                    }
                    break;
                default: {
                    String code = generateUnifiedWidget(child, tag);
                    if (isNotEmpty(code)) valueCode.add(code);
                    else if (hasChildElements(child)) valueCode.add(helperScreenInclude(child));
                    else System.err.println("WARN: unsupported widget '" + tag + "' inside nested section (dropped)");
                }
            }
            if (order != null) {
                order.after();
            }
        }
    }

    protected String generateContainerNested(Element element, int depth) {
        String id = getAttr(element, "id");
        String style = getAttr(element, "style");
        String autoUpdateInterval = getAttr(element, "auto-update-interval");
        String autoUpdateTarget = getAttr(element, "auto-update-target");

        // Validate depth (cap at 4 levels)
        if (depth > 4) {
            depth = 4;
        }

        List<String> attrs = new ArrayList<>();
        attrs.add(attrIfNotEmpty("id", id));
        attrs.add(attrIfNotEmpty("style", style));
        attrs.add(attrIfNotEmpty("autoUpdateInterval", autoUpdateInterval));
        attrs.add(attrIfNotEmpty("autoUpdateTarget", autoUpdateTarget));

        // Container supports: includeForms, includeScreens, labels, htmlTemplates, includeMenus, containers, screenlets
        List<String> includeFormsCode = new ArrayList<>();
        List<String> includeScreensCode = new ArrayList<>();
        List<String> labelsCode = new ArrayList<>();
        List<String> widgetsCode = new ArrayList<>();
        List<String> htmlTemplatesCode = new ArrayList<>();
        List<String> includeMenusCode = new ArrayList<>();
        List<String> containersCode = new ArrayList<>();
        List<String> nestedSectionsCode = new ArrayList<>(); // SCIPIO: 4.0.0: conditional nested sections
        List<String> screenletsCode = new ArrayList<>();
        List<String> decoratorSectionIncludesCode = new ArrayList<>();
        List<String> decoratorCode = new ArrayList<>();

        ChildOrder order = new ChildOrder(widgetsCode, decoratorCode, includeFormsCode, includeScreensCode, labelsCode, htmlTemplatesCode, includeMenusCode, containersCode, screenletsCode, nestedSectionsCode, decoratorSectionIncludesCode);
        for (Element child : childElementList(element)) {
            order.before();
            String tagName = child.getNodeName();
            switch (tagName) {
                case "include-form":
                    includeFormsCode.add(generateIncludeFormNested(child));
                    break;
                case "include-screen":
                    includeScreensCode.add(generateIncludeScreenNested(child));
                    break;
                // SCIPIO: 4.0.0: generic widgets were silently dropped (no container member could hold them)
                case "link":
                case "image":
                case "content":
                case "sub-content":
                case "horizontal-separator":
                case "include-grid":
                case "iterate-section":
                case "include-portal-page":
                case "include-tree": {
                    String genericWidget = generateUnifiedWidget(child, child.getNodeName());
                    if (isNotEmpty(genericWidget)) {
                        widgetsCode.add(genericWidget);
                    }
                    break;
                }
                case "label":
                    labelsCode.add(generateLabelNested(child));
                    break;
                case "platform-specific":
                    htmlTemplatesCode.addAll(generateHtmlTemplatesNested(child));
                    break;
                case "include-menu":
                    includeMenusCode.add(generateIncludeMenuNested(child));
                    break;
                case "container":
                    // Nested containers - generate appropriate level based on depth
                    if (depth < 4) {
                        containersCode.add(generateContainerLevel(child, depth + 1));
                    } else {
                        widgetsCode.add(helperScreenInclude(child));
                    }
                    break;
                case "decorator-section-include":
                    decoratorSectionIncludesCode.add(generateDecoratorSectionIncludeNested(child));
                    break;
                case "decorator-screen":
                    // SCIPIO: 4.0.0: was dropped and left an empty container (e.g. FindContacts)
                    decoratorCode.add(generateDecoratorScreenNested(child));
                    break;
                case "screenlet":
                    // SCIPIO: 4.0.0: Handle screenlets nested inside containers
                    screenletsCode.add(generateScreenletNested(child));
                    break;
                case "section":
                    // SCIPIO: 4.0.0: a name/contains carries render semantics too; flattening dropped it
                    // together with every decorator-section-include the section held.
                    if (firstChildElement(child, "condition") != null || firstChildElement(child, "actions") != null
                            || isNotEmpty(getAttr(child, "name")) || isNotEmpty(getAttr(child, "contains"))) {
                        nestedSectionsCode.add(generateSectionNestedAtDepth(child, depth));
                        break;
                    }
                    // Flatten sections - collect widgets into appropriate lists
                    // (containers found inside land one Container level deeper)
                    ChildOrder savedOrder = flattenOrder;
                    flattenOrder = order;
                    flattenSectionToDecoratorSection(child, includeFormsCode, includeScreensCode,
                            includeMenusCode, screenletsCode, labelsCode, containersCode, htmlTemplatesCode,
                            Math.min(depth + 1, 4), decoratorSectionIncludesCode);
                    flattenOrder = savedOrder;
                    break;
            }
            order.after();
        }
        order.applyPositions();

        // Build attribute list
        if (!includeFormsCode.isEmpty()) {
            attrs.add("includeForms = {" + String.join(", ", includeFormsCode) + "}");
        }
        if (!includeScreensCode.isEmpty()) {
            attrs.add("includeScreens = {" + String.join(", ", includeScreensCode) + "}");
        }
        if (!labelsCode.isEmpty()) {
            attrs.add("labels = {" + String.join(", ", labelsCode) + "}");
        }
        if (!widgetsCode.isEmpty()) {
            attrs.add("widgets = {" + String.join(", ", widgetsCode) + "}");
        }
        if (!htmlTemplatesCode.isEmpty()) {
            attrs.add("htmlTemplates = {" + String.join(", ", htmlTemplatesCode) + "}");
        }
        if (!includeMenusCode.isEmpty()) {
            attrs.add("includeMenus = {" + String.join(", ", includeMenusCode) + "}");
        }
        if (!nestedSectionsCode.isEmpty()) {
            attrs.add("sections = {" + String.join(", ", nestedSectionsCode) + "}");
        }
        if (!containersCode.isEmpty()) {
            attrs.add("containers = {" + String.join(", ", containersCode) + "}");
        }
        if (!screenletsCode.isEmpty()) {
            attrs.add("screenlets = {" + String.join(", ", screenletsCode) + "}");
        }

        if (!decoratorCode.isEmpty()) {
            attrs.add("decorator = " + decoratorCode.get(0));
        }
        if (!decoratorSectionIncludesCode.isEmpty()) {
            attrs.add("decoratorSectionIncludes = {" + String.join(", ", decoratorSectionIncludesCode) + "}");
        }

        return "@Container(" + joinAttrs(attrs.toArray(new String[0])) + ")";
    }

    /**
     * Generate container annotation at appropriate depth level.
     */
    protected String generateContainerLevel(Element element, int depth) {
        switch (depth) {
            case 1:
                return generateContainerNested(element, 1);
            case 2:
                return generateContainer2Nested(element, 2);
            case 3:
                return generateContainer3Nested(element, 3);
            case 4:
                return generateContainer4Nested(element, 4);
            default:
                // Fall back to deepest level for unsupported depth
                return generateContainer4Nested(element, 4);
        }
    }

    /**
     * Generate Container2 annotation for second-level nesting.
     */
    protected String generateContainer2Nested(Element element, int depth) {
        String id = getAttr(element, "id");
        String style = getAttr(element, "style");
        String autoUpdateInterval = getAttr(element, "auto-update-interval");
        String autoUpdateTarget = getAttr(element, "auto-update-target");

        List<String> attrs = new ArrayList<>();
        attrs.add(attrIfNotEmpty("id", id));
        attrs.add(attrIfNotEmpty("style", style));
        attrs.add(attrIfNotEmpty("autoUpdateInterval", autoUpdateInterval));
        attrs.add(attrIfNotEmpty("autoUpdateTarget", autoUpdateTarget));

        List<String> includeFormsCode = new ArrayList<>();
        List<String> includeScreensCode = new ArrayList<>();
        List<String> labelsCode = new ArrayList<>();
        List<String> widgetsCode = new ArrayList<>();
        List<String> htmlTemplatesCode = new ArrayList<>();
        List<String> includeMenusCode = new ArrayList<>();
        List<String> containersCode = new ArrayList<>();
        List<String> nestedSectionsCode = new ArrayList<>(); // SCIPIO: 4.0.0: conditional nested sections
        List<String> screenletsCode = new ArrayList<>();
        List<String> decoratorSectionIncludesCode = new ArrayList<>();

        ChildOrder order = new ChildOrder(widgetsCode, includeFormsCode, includeScreensCode, labelsCode, htmlTemplatesCode, includeMenusCode, containersCode, screenletsCode, nestedSectionsCode, decoratorSectionIncludesCode);
        for (Element child : childElementList(element)) {
            order.before();
            String tagName = child.getNodeName();
            switch (tagName) {
                case "include-form":
                    includeFormsCode.add(generateIncludeFormNested(child));
                    break;
                case "include-screen":
                    includeScreensCode.add(generateIncludeScreenNested(child));
                    break;
                // SCIPIO: 4.0.0: generic widgets were silently dropped (no container member could hold them)
                case "link":
                case "image":
                case "content":
                case "sub-content":
                case "horizontal-separator":
                case "include-grid":
                case "iterate-section":
                case "include-portal-page":
                case "include-tree": {
                    String genericWidget = generateUnifiedWidget(child, child.getNodeName());
                    if (isNotEmpty(genericWidget)) {
                        widgetsCode.add(genericWidget);
                    }
                    break;
                }
                case "label":
                    labelsCode.add(generateLabelNested(child));
                    break;
                case "platform-specific":
                    htmlTemplatesCode.addAll(generateHtmlTemplatesNested(child));
                    break;
                case "include-menu":
                    includeMenusCode.add(generateIncludeMenuNested(child));
                    break;
                case "container":
                    if (depth < 4) {
                        containersCode.add(generateContainerLevel(child, depth + 1));
                    } else {
                        widgetsCode.add(helperScreenInclude(child));
                    }
                    break;
                case "decorator-section-include":
                    decoratorSectionIncludesCode.add(generateDecoratorSectionIncludeNested(child));
                    break;
                case "screenlet":
                    // SCIPIO: 4.0.0: Handle screenlets nested inside containers
                    screenletsCode.add(generateScreenletNested(child));
                    break;
                case "section":
                    // SCIPIO: 4.0.0: a name/contains carries render semantics too; flattening dropped it
                    // together with every decorator-section-include the section held.
                    if (firstChildElement(child, "condition") != null || firstChildElement(child, "actions") != null
                            || isNotEmpty(getAttr(child, "name")) || isNotEmpty(getAttr(child, "contains"))) {
                        nestedSectionsCode.add(generateSectionNestedAtDepth(child, depth));
                        break;
                    }
                    ChildOrder savedOrder = flattenOrder;
                    flattenOrder = order;
                    flattenSectionToDecoratorSection(child, includeFormsCode, includeScreensCode,
                            includeMenusCode, screenletsCode, labelsCode, containersCode, htmlTemplatesCode,
                            Math.min(depth + 1, 4), decoratorSectionIncludesCode);
                    flattenOrder = savedOrder;
                    break;
            }
            order.after();
        }
        order.applyPositions();

        if (!includeFormsCode.isEmpty()) {
            attrs.add("includeForms = {" + String.join(", ", includeFormsCode) + "}");
        }
        if (!includeScreensCode.isEmpty()) {
            attrs.add("includeScreens = {" + String.join(", ", includeScreensCode) + "}");
        }
        if (!labelsCode.isEmpty()) {
            attrs.add("labels = {" + String.join(", ", labelsCode) + "}");
        }
        if (!widgetsCode.isEmpty()) {
            attrs.add("widgets = {" + String.join(", ", widgetsCode) + "}");
        }
        if (!htmlTemplatesCode.isEmpty()) {
            attrs.add("htmlTemplates = {" + String.join(", ", htmlTemplatesCode) + "}");
        }
        if (!includeMenusCode.isEmpty()) {
            attrs.add("includeMenus = {" + String.join(", ", includeMenusCode) + "}");
        }
        if (!nestedSectionsCode.isEmpty()) {
            attrs.add("sections = {" + String.join(", ", nestedSectionsCode) + "}");
        }
        if (!containersCode.isEmpty()) {
            attrs.add("containers = {" + String.join(", ", containersCode) + "}");
        }
        if (!screenletsCode.isEmpty()) {
            attrs.add("screenlets = {" + String.join(", ", screenletsCode) + "}");
        }

        if (!decoratorSectionIncludesCode.isEmpty()) {
            attrs.add("decoratorSectionIncludes = {" + String.join(", ", decoratorSectionIncludesCode) + "}");
        }

        return "@Container2(" + joinAttrs(attrs.toArray(new String[0])) + ")";
    }

    /**
     * Generate Container3 annotation for third-level nesting.
     */
    protected String generateContainer3Nested(Element element, int depth) {
        String id = getAttr(element, "id");
        String style = getAttr(element, "style");
        String autoUpdateInterval = getAttr(element, "auto-update-interval");
        String autoUpdateTarget = getAttr(element, "auto-update-target");

        List<String> attrs = new ArrayList<>();
        attrs.add(attrIfNotEmpty("id", id));
        attrs.add(attrIfNotEmpty("style", style));
        attrs.add(attrIfNotEmpty("autoUpdateInterval", autoUpdateInterval));
        attrs.add(attrIfNotEmpty("autoUpdateTarget", autoUpdateTarget));

        List<String> includeFormsCode = new ArrayList<>();
        List<String> includeScreensCode = new ArrayList<>();
        List<String> labelsCode = new ArrayList<>();
        List<String> widgetsCode = new ArrayList<>();
        List<String> htmlTemplatesCode = new ArrayList<>();
        List<String> includeMenusCode = new ArrayList<>();
        List<String> containersCode = new ArrayList<>();
        List<String> screenletsCode = new ArrayList<>();
        List<String> decoratorSectionIncludesCode = new ArrayList<>();

        ChildOrder order = new ChildOrder(widgetsCode, includeFormsCode, includeScreensCode, labelsCode, htmlTemplatesCode, includeMenusCode, containersCode, screenletsCode, decoratorSectionIncludesCode);
        for (Element child : childElementList(element)) {
            order.before();
            String tagName = child.getNodeName();
            switch (tagName) {
                case "include-form":
                    includeFormsCode.add(generateIncludeFormNested(child));
                    break;
                case "include-screen":
                    includeScreensCode.add(generateIncludeScreenNested(child));
                    break;
                // SCIPIO: 4.0.0: generic widgets were silently dropped (no container member could hold them)
                case "link":
                case "image":
                case "content":
                case "sub-content":
                case "horizontal-separator":
                case "include-grid":
                case "iterate-section":
                case "include-portal-page":
                case "include-tree": {
                    String genericWidget = generateUnifiedWidget(child, child.getNodeName());
                    if (isNotEmpty(genericWidget)) {
                        widgetsCode.add(genericWidget);
                    }
                    break;
                }
                case "label":
                    labelsCode.add(generateLabelNested(child));
                    break;
                case "platform-specific":
                    htmlTemplatesCode.addAll(generateHtmlTemplatesNested(child));
                    break;
                case "include-menu":
                    includeMenusCode.add(generateIncludeMenuNested(child));
                    break;
                case "container":
                    if (depth < 4) {
                        containersCode.add(generateContainerLevel(child, depth + 1));
                    } else {
                        widgetsCode.add(helperScreenInclude(child));
                    }
                    break;
                case "decorator-section-include":
                    decoratorSectionIncludesCode.add(generateDecoratorSectionIncludeNested(child));
                    break;
                case "screenlet":
                    // SCIPIO: 4.0.0: Handle screenlets nested inside containers
                    screenletsCode.add(generateScreenletNested(child));
                    break;
                case "section":
                    ChildOrder savedOrder = flattenOrder;
                    flattenOrder = order;
                    flattenSectionToDecoratorSection(child, includeFormsCode, includeScreensCode,
                            includeMenusCode, screenletsCode, labelsCode, containersCode, htmlTemplatesCode,
                            Math.min(depth + 1, 4), decoratorSectionIncludesCode);
                    flattenOrder = savedOrder;
                    break;
            }
            order.after();
        }
        order.applyPositions();

        if (!includeFormsCode.isEmpty()) {
            attrs.add("includeForms = {" + String.join(", ", includeFormsCode) + "}");
        }
        if (!includeScreensCode.isEmpty()) {
            attrs.add("includeScreens = {" + String.join(", ", includeScreensCode) + "}");
        }
        if (!labelsCode.isEmpty()) {
            attrs.add("labels = {" + String.join(", ", labelsCode) + "}");
        }
        if (!widgetsCode.isEmpty()) {
            attrs.add("widgets = {" + String.join(", ", widgetsCode) + "}");
        }
        if (!htmlTemplatesCode.isEmpty()) {
            attrs.add("htmlTemplates = {" + String.join(", ", htmlTemplatesCode) + "}");
        }
        if (!includeMenusCode.isEmpty()) {
            attrs.add("includeMenus = {" + String.join(", ", includeMenusCode) + "}");
        }
        if (!containersCode.isEmpty()) {
            attrs.add("containers = {" + String.join(", ", containersCode) + "}");
        }
        if (!screenletsCode.isEmpty()) {
            attrs.add("screenlets = {" + String.join(", ", screenletsCode) + "}");
        }

        if (!decoratorSectionIncludesCode.isEmpty()) {
            attrs.add("decoratorSectionIncludes = {" + String.join(", ", decoratorSectionIncludesCode) + "}");
        }

        return "@Container3(" + joinAttrs(attrs.toArray(new String[0])) + ")";
    }

    /**
     * Generate Container4 annotation for fourth-level nesting (leaf level).
     */
    protected String generateContainer4Nested(Element element, int depth) {
        String id = getAttr(element, "id");
        String style = getAttr(element, "style");
        String autoUpdateInterval = getAttr(element, "auto-update-interval");
        String autoUpdateTarget = getAttr(element, "auto-update-target");

        List<String> attrs = new ArrayList<>();
        attrs.add(attrIfNotEmpty("id", id));
        attrs.add(attrIfNotEmpty("style", style));
        attrs.add(attrIfNotEmpty("autoUpdateInterval", autoUpdateInterval));
        attrs.add(attrIfNotEmpty("autoUpdateTarget", autoUpdateTarget));

        List<String> includeFormsCode = new ArrayList<>();
        List<String> includeScreensCode = new ArrayList<>();
        List<String> labelsCode = new ArrayList<>();
        List<String> widgetsCode = new ArrayList<>();
        List<String> htmlTemplatesCode = new ArrayList<>();
        List<String> includeMenusCode = new ArrayList<>();
        List<String> screenletsCode = new ArrayList<>();
        List<String> decoratorSectionIncludesCode = new ArrayList<>();

        ChildOrder order = new ChildOrder(widgetsCode, includeFormsCode, includeScreensCode, labelsCode, htmlTemplatesCode, includeMenusCode, screenletsCode, decoratorSectionIncludesCode);
        for (Element child : childElementList(element)) {
            order.before();
            String tagName = child.getNodeName();
            switch (tagName) {
                case "include-form":
                    includeFormsCode.add(generateIncludeFormNested(child));
                    break;
                case "include-screen":
                    includeScreensCode.add(generateIncludeScreenNested(child));
                    break;
                // SCIPIO: 4.0.0: generic widgets were silently dropped (no container member could hold them)
                case "link":
                case "image":
                case "content":
                case "sub-content":
                case "horizontal-separator":
                case "include-grid":
                case "iterate-section":
                case "include-portal-page":
                case "include-tree": {
                    String genericWidget = generateUnifiedWidget(child, child.getNodeName());
                    if (isNotEmpty(genericWidget)) {
                        widgetsCode.add(genericWidget);
                    }
                    break;
                }
                case "label":
                    labelsCode.add(generateLabelNested(child));
                    break;
                case "platform-specific":
                    htmlTemplatesCode.addAll(generateHtmlTemplatesNested(child));
                    break;
                case "include-menu":
                    includeMenusCode.add(generateIncludeMenuNested(child));
                    break;
                case "container":
                    // SCIPIO: 4.0.0: was skipped (FindJob lost its search form)
                    widgetsCode.add(helperScreenInclude(child));
                    break;
                case "decorator-section-include":
                    decoratorSectionIncludesCode.add(generateDecoratorSectionIncludeNested(child));
                    break;
                case "screenlet":
                    // SCIPIO: 4.0.0: Handle screenlets nested inside containers
                    screenletsCode.add(generateScreenletNested(child));
                    break;
                case "section":
                    ChildOrder savedOrder = flattenOrder;
                    flattenOrder = order;
                    flattenSectionToDecoratorSection(child, includeFormsCode, includeScreensCode,
                            includeMenusCode, screenletsCode, labelsCode, null, htmlTemplatesCode,
                            Math.min(depth + 1, 4), decoratorSectionIncludesCode);
                    flattenOrder = savedOrder;
                    break;
            }
            order.after();
        }
        order.applyPositions();

        if (!includeFormsCode.isEmpty()) {
            attrs.add("includeForms = {" + String.join(", ", includeFormsCode) + "}");
        }
        if (!includeScreensCode.isEmpty()) {
            attrs.add("includeScreens = {" + String.join(", ", includeScreensCode) + "}");
        }
        if (!labelsCode.isEmpty()) {
            attrs.add("labels = {" + String.join(", ", labelsCode) + "}");
        }
        if (!widgetsCode.isEmpty()) {
            attrs.add("widgets = {" + String.join(", ", widgetsCode) + "}");
        }
        if (!htmlTemplatesCode.isEmpty()) {
            attrs.add("htmlTemplates = {" + String.join(", ", htmlTemplatesCode) + "}");
        }
        if (!includeMenusCode.isEmpty()) {
            attrs.add("includeMenus = {" + String.join(", ", includeMenusCode) + "}");
        }
        if (!screenletsCode.isEmpty()) {
            attrs.add("screenlets = {" + String.join(", ", screenletsCode) + "}");
        }

        if (!decoratorSectionIncludesCode.isEmpty()) {
            attrs.add("decoratorSectionIncludes = {" + String.join(", ", decoratorSectionIncludesCode) + "}");
        }

        return "@Container4(" + joinAttrs(attrs.toArray(new String[0])) + ")";
    }

    /**
     * SCIPIO: 4.0.0: One @HtmlTemplate per branch of a platform-specific element (html, xsl-fo, text,
     * xml), in document order. A branch other than html carries platform = "&lt;branch&gt;".
     */
    protected List<String> generateHtmlTemplatesNested(Element platformSpecificElement) {
        List<String> out = new ArrayList<>();
        for (Element branch : childElements(platformSpecificElement)) {
          for (Element htmlTemplate : childElements(branch)) {
            if (!"html-template".equals(htmlTemplate.getTagName())) {
                continue;
            }
            String platform = branch.getTagName();
            String location = getAttr(htmlTemplate, "location");
            String multiBlock = getAttr(htmlTemplate, "multi-block");
            String inlineContent = htmlTemplate.getTextContent();
            if (inlineContent != null) {
                inlineContent = inlineContent.trim();
            }
            List<String> attrs = new ArrayList<>();
            if (location == null || location.isEmpty()) {
                if (inlineContent == null || inlineContent.isEmpty()) {
                    continue;  // Skip empty templates
                }
                attrs.add("location = \"\"");
                attrs.add("content = " + toStringValue(inlineContent));
            } else {
                attrs.add(attrIfNotEmpty("location", location));
            }
            if (!"html".equals(platform)) {
                attrs.add("platform = \"" + platform + "\"");
            }
            if ("true".equals(multiBlock)) {
                attrs.add("multiBlock = true");
            }
            out.add("@HtmlTemplate(" + joinAttrs(attrs.toArray(new String[0])) + ")");
          }
        }
        return out;
    }

    /** @deprecated use {@link #generateHtmlTemplatesNested(Element)}; kept for callers that expect one template */
    @Deprecated
    protected String generateHtmlTemplateNested(Element platformSpecificElement) {
        List<String> all = generateHtmlTemplatesNested(platformSpecificElement);
        return all.isEmpty() ? null : all.get(0);
    }


    /** SCIPIO: 4.0.0: the child elements of an element, in document order. */
    protected List<Element> childElements(Element parent) {
        List<Element> out = new ArrayList<>();
        org.w3c.dom.NodeList nodes = parent.getChildNodes();
        for (int i = 0; i < nodes.getLength(); i++) {
            if (nodes.item(i) instanceof Element) {
                out.add((Element) nodes.item(i));
            }
        }
        return out;
    }


    /** SCIPIO: 4.0.0: the first child element of any name, or null. */
    protected Element firstChildElement(Element parent) {
        List<Element> all = childElements(parent);
        return all.isEmpty() ? null : all.get(0);
    }


    /** SCIPIO: 4.0.0: @Widget(type = HTML_TEMPLATE) for every html-template of one platform-specific branch. */
    protected List<String> branchTemplateWidgets(Element branch) {
        List<String> out = new ArrayList<>();
        for (Element tpl : childElements(branch)) {
            if (!"html-template".equals(tpl.getTagName())) {
                continue;
            }
            String loc = getAttr(tpl, "location");
            String multi = getAttr(tpl, "multi-block");
            // SCIPIO: 4.0.0: a template written inline (no location) was dropped, e.g. DeveloperDocIndex
            String inline = tpl.getTextContent() != null ? tpl.getTextContent().trim() : "";
            if (isEmpty(loc) && !"true".equals(multi) && inline.isEmpty()) {
                continue;
            }
            List<String> a = new ArrayList<>();
            a.add("type = WidgetType.HTML_TEMPLATE");
            a.add(attrIfNotEmpty("location", loc));
            if (isEmpty(loc) && !inline.isEmpty()) {
                a.add("content = " + toStringValue(inline));
            }
            if (!"html".equals(branch.getTagName())) {
                a.add("platform = \"" + branch.getTagName() + "\"");
            }
            if ("true".equals(multi)) {
                a.add("multiBlock = true");
            }
            out.add("@Widget(" + joinAttrs(a.toArray(new String[0])) + ")");
        }
        return out;
    }

    /** SCIPIO: 4.0.0: @Widget(type = HTML_TEMPLATE) for every template of every branch of a platform-specific element. */
    protected List<String> platformTemplateWidgets(Element platformSpecific) {
        List<String> out = new ArrayList<>();
        for (Element branch : childElements(platformSpecific)) {
            out.addAll(branchTemplateWidgets(branch));
        }
        return out;
    }

    protected String generateImageNested(Element element) {
        return generateImage(element);
    }

    protected String generateContentNested(Element element) {
        return generateContent(element);
    }

    protected String generateHorizontalSeparatorNested(Element element) {
        return generateHorizontalSeparator(element);
    }

    protected String generateSectionNested(Element element) {
        String name = getAttr(element, "name");
        String shareScopeStr = getAttr(element, "share-scope");

        List<String> attrs = new ArrayList<>();
        attrs.add(attrIfNotEmpty("name", name));
        if ("false".equals(shareScopeStr)) {
            attrs.add("shareScope = false");
        }

        // Handle condition
        Element conditionElement = firstChildElement(element, "condition");
        if (conditionElement != null) {
            String conditionCode = generateConditionAnnotation(conditionElement);
            if (conditionCode != null) {
                attrs.add("condition = " + conditionCode);
            }
        }

        // Handle actions
        Element actionsElement = firstChildElement(element, "actions");
        if (actionsElement != null) {
            List<String> actionsCode = generateActionsNested(actionsElement);
            if (!actionsCode.isEmpty()) {
                attrs.add("actions = @Actions({" + String.join(", ", actionsCode) + "})");
            }
        }

        // Handle widgets
        Element widgetsElement = firstChildElement(element, "widgets");
        if (widgetsElement != null) {
            String widgetsCode = generateWidgetsNested(widgetsElement);
            if (widgetsCode != null) {
                attrs.add("widgets = " + widgetsCode);
            }
        }

        // Handle fail-widgets
        Element failWidgetsElement = firstChildElement(element, "fail-widgets");
        if (failWidgetsElement != null) {
            String failWidgetsCode = generateFailWidgetsNested(failWidgetsElement);
            if (failWidgetsCode != null) {
                attrs.add("failWidgets = " + failWidgetsCode);
            }
        }

        return "@Section(" + joinAttrs(attrs.toArray(new String[0])) + ")";
    }

    /**
     * Generates actions attributes for @Actions annotation using unified Action format.
     * Returns a list containing a single attribute assignment: "value = {@Action(...), @Action(...), ...}".
     *
     * <p>SCIPIO: 4.0.0: Updated to use unified Action format that preserves document order.</p>
     */
    protected List<String> generateActionsNested(Element actionsElement) {
        // SCIPIO: 4.0.0: an <if> here was dropped (e.g. viewprofile lost its party lookup); the branch
        // generator emits it as @IfAction2 in ifs = {...} and stamps the order
        if (firstChildElement(actionsElement, "if") != null) {
            String branchCode = generateBranchActions(actionsElement, "");
            if (branchCode != null) {
                String inner = branchCode.substring("@Actions(".length(), branchCode.length() - 1);
                List<String> attrs = new ArrayList<>();
                if (isNotEmpty(inner)) {
                    attrs.add(inner);
                }
                return attrs;
            }
            System.err.println("WARN: nested <if> actions too deep for @IfAction2 (if blocks dropped)");
        }
        // Generate unified @Action annotations in document order (preserves execution order)
        List<String> unifiedActions = new ArrayList<>();

        for (Element child : childElementList(actionsElement)) {
            String tagName = child.getNodeName();
            String unifiedAction = generateUnifiedAction(child, tagName, "");
            if (unifiedAction != null) {
                unifiedActions.add(unifiedAction);
            }
        }

        // Build single value attribute with all actions in document order
        List<String> attrs = new ArrayList<>();
        if (!unifiedActions.isEmpty()) {
            attrs.add("value = {" + String.join(", ", unifiedActions) + "}");
        }
        return attrs;
    }

    /**
     * Generates a unified @Action annotation from an XML action element.
     * Preserves all attributes while using ActionType discriminator.
     *
     * @param element The action XML element
     * @param tagName The XML tag name
     * @param screenName The screen name (for script extraction)
     * @return The unified @Action annotation string, or null if unknown type
     */
    protected String generateUnifiedAction(Element element, String tagName, String screenName) {
        List<String> attrs = new ArrayList<>();

        switch (tagName) {
            case "set":
                attrs.add("type = ActionType.SET");
                attrs.add(attrIfNotEmpty("field", getAttr(element, "field")));
                attrs.add(attrIfNotEmpty("value", getAttr(element, "value")));
                attrs.add(attrIfNotEmpty("fromField", getAttr(element, "from-field")));
                attrs.add(attrIfNotEmpty("fromScope", getAttr(element, "from-scope")));
                attrs.add(attrIfNotEmpty("valueType", getAttr(element, "type")));
                attrs.add(attrIfNotEmpty("defaultValue", getAttr(element, "default-value")));
                if ("true".equals(getAttr(element, "global"))) {
                    attrs.add("global = true");
                }
                if ("false".equals(getAttr(element, "set-if-empty"))) {
                    attrs.add("setIfEmpty = false");
                }
                if ("false".equals(getAttr(element, "set-if-null"))) {
                    attrs.add("setIfNull = false");
                }
                break;

            case "clear-field":
                attrs.add("type = ActionType.CLEAR_FIELD");
                attrs.add(attrIfNotEmpty("field", getAttr(element, "field")));
                break;

            case "service":
                attrs.add("type = ActionType.SERVICE");
                attrs.add(attrIfNotEmpty("serviceName", getAttr(element, "service-name")));
                attrs.add(attrIfNotEmpty("resultMapName", getAttr(element, "result-map")));
                attrs.add(attrIfNotEmpty("resultMapList", getAttr(element, "result-map-list")));
                attrs.add(attrIfNotEmpty("resultMapField", getAttr(element, "result-map-field")));
                if ("false".equals(getAttr(element, "auto-field-map"))) {
                    attrs.add("autoFieldMap = false");
                }
                String fieldMaps = generateFieldMapsNested(element);
                if (fieldMaps != null) {
                    attrs.add("fieldMaps = " + fieldMaps);
                }
                break;

            case "entity-one":
                attrs.add("type = ActionType.ENTITY_ONE");
                attrs.add(attrIfNotEmpty("entityName", getAttr(element, "entity-name")));
                attrs.add(attrIfNotEmpty("valueField", getAttr(element, "value-field")));
                if ("false".equals(getAttr(element, "auto-field-map"))) {
                    attrs.add("autoFieldMap = false");
                }
                if ("true".equals(getAttr(element, "use-cache"))) {
                    attrs.add("useCache = true");
                }
                fieldMaps = generateFieldMapsNested(element);
                if (fieldMaps != null) {
                    attrs.add("fieldMaps = " + fieldMaps);
                }
                break;

            case "entity-and":
                attrs.add("type = ActionType.ENTITY_AND");
                attrs.add(attrIfNotEmpty("entityName", getAttr(element, "entity-name")));
                attrs.add(attrIfNotEmpty("list", getAttr(element, "list")));
                if ("true".equals(getAttr(element, "use-cache"))) {
                    attrs.add("useCache = true");
                }
                if ("true".equals(getAttr(element, "filter-by-date"))) {
                    attrs.add("filterByDate = true");
                }
                if ("true".equals(getAttr(element, "use-iterator"))) {
                    attrs.add("useIterator = true");
                }
                String resultSetType = getAttr(element, "result-set-type");
                if (isNotEmpty(resultSetType) && !"scroll".equals(resultSetType)) {
                    attrs.add(attrIfNotEmpty("resultSetType", resultSetType));
                }
                fieldMaps = generateFieldMapsNested(element);
                if (fieldMaps != null) {
                    attrs.add("fieldMaps = " + fieldMaps);
                }
                String orderBy = generateOrderByArray(element);
                if (orderBy != null) {
                    attrs.add("orderBy = " + orderBy);
                }
                String selectFields = generateSelectFieldsArray(element);
                if (selectFields != null) {
                    attrs.add("selectFields = " + selectFields);
                }
                break;

            case "entity-condition":
                attrs.add("type = ActionType.ENTITY_CONDITION");
                attrs.add(attrIfNotEmpty("entityName", getAttr(element, "entity-name")));
                attrs.add(attrIfNotEmpty("list", getAttr(element, "list")));
                if ("true".equals(getAttr(element, "use-cache"))) {
                    attrs.add("useCache = true");
                }
                if ("true".equals(getAttr(element, "filter-by-date"))) {
                    attrs.add("filterByDate = true");
                }
                if ("true".equals(getAttr(element, "distinct"))) {
                    attrs.add("distinct = true");
                }
                attrs.add(attrIfNotEmpty("delegatorName", getAttr(element, "delegator-name")));
                String conditions = generateConditionsArray(element);
                if (conditions != null) {
                    attrs.add("conditions = " + conditions);
                }
                orderBy = generateOrderByArray(element);
                if (orderBy != null) {
                    attrs.add("orderBy = " + orderBy);
                }
                selectFields = generateSelectFieldsArray(element);
                if (selectFields != null) {
                    attrs.add("selectFields = " + selectFields);
                }
                break;

            case "get-related-one":
                attrs.add("type = ActionType.GET_RELATED_ONE");
                attrs.add(attrIfNotEmpty("valueField", getAttr(element, "value-field")));
                attrs.add(attrIfNotEmpty("relationName", getAttr(element, "relation-name")));
                attrs.add(attrIfNotEmpty("toValueField", getAttr(element, "to-value-field")));
                if ("true".equals(getAttr(element, "use-cache"))) {
                    attrs.add("useCache = true");
                }
                break;

            case "get-related":
                attrs.add("type = ActionType.GET_RELATED");
                attrs.add(attrIfNotEmpty("valueField", getAttr(element, "value-field")));
                attrs.add(attrIfNotEmpty("relationName", getAttr(element, "relation-name")));
                attrs.add(attrIfNotEmpty("list", getAttr(element, "list")));
                attrs.add(attrIfNotEmpty("map", getAttr(element, "map")));
                attrs.add(attrIfNotEmpty("orderByList", getAttr(element, "order-by-list")));
                if ("true".equals(getAttr(element, "use-cache"))) {
                    attrs.add("useCache = true");
                }
                break;

            case "script":
                attrs.add("type = ActionType.SCRIPT");
                String location = getAttr(element, "location");
                String lang = getAttr(element, "lang", "groovy");
                // Check for inline script (CDATA)
                String code = element.getTextContent();
                if (isNotEmpty(code) && code.trim().length() > 0) {
                    // Extract inline script to external file
                    scriptCounter++;
                    location = extractScript(screenName, scriptCounter, lang, code.trim());
                }
                attrs.add(attrIfNotEmpty("location", location));
                if (!"groovy".equals(lang)) {
                    attrs.add(attrIfNotEmpty("lang", lang));
                }
                break;

            case "property-to-field":
                attrs.add("type = ActionType.PROPERTY_TO_FIELD");
                attrs.add(attrIfNotEmpty("field", getAttr(element, "field")));
                attrs.add(attrIfNotEmpty("resource", getAttr(element, "resource")));
                attrs.add(attrIfNotEmpty("property", getAttr(element, "property")));
                attrs.add(attrIfNotEmpty("defaultValue", getAttr(element, "default")));
                if ("true".equals(getAttr(element, "no-locale"))) {
                    attrs.add("noLocale = true");
                }
                attrs.add(attrIfNotEmpty("argListName", getAttr(element, "arg-list-name")));
                if ("true".equals(getAttr(element, "global"))) {
                    attrs.add("global = true");
                }
                break;

            case "property-map":
                attrs.add("type = ActionType.PROPERTY_MAP");
                attrs.add(attrIfNotEmpty("resource", getAttr(element, "resource")));
                attrs.add(attrIfNotEmpty("mapName", getAttr(element, "map-name")));
                if ("true".equals(getAttr(element, "global"))) {
                    attrs.add("global = true");
                }
                if ("true".equals(getAttr(element, "optional"))) {
                    attrs.add("optional = true");
                }
                break;

            case "include-screen-actions":
                attrs.add("type = ActionType.INCLUDE_SCREEN_ACTIONS");
                attrs.add(attrIfNotEmpty("name", getAttr(element, "name")));
                attrs.add(attrIfNotEmpty("location", getAttr(element, "location")));
                break;

            case "include-form-actions":
                attrs.add("type = ActionType.INCLUDE_FORM_ACTIONS");
                attrs.add(attrIfNotEmpty("name", getAttr(element, "name")));
                attrs.add(attrIfNotEmpty("location", getAttr(element, "location")));
                break;

            case "include-form-row-actions":
                attrs.add("type = ActionType.INCLUDE_FORM_ROW_ACTIONS");
                attrs.add(attrIfNotEmpty("name", getAttr(element, "name")));
                attrs.add(attrIfNotEmpty("location", getAttr(element, "location")));
                break;

            case "include-menu-actions":
                attrs.add("type = ActionType.INCLUDE_MENU_ACTIONS");
                attrs.add(attrIfNotEmpty("name", getAttr(element, "name")));
                attrs.add(attrIfNotEmpty("location", getAttr(element, "location")));
                break;

            case "include-tree-actions":
                attrs.add("type = ActionType.INCLUDE_TREE_ACTIONS");
                attrs.add(attrIfNotEmpty("name", getAttr(element, "name")));
                attrs.add(attrIfNotEmpty("location", getAttr(element, "location")));
                break;

            case "condition-to-field":
                attrs.add("type = ActionType.CONDITION_TO_FIELD");
                attrs.add(attrIfNotEmpty("field", getAttr(element, "field")));
                attrs.add(attrIfNotEmpty("valueType", getAttr(element, "type")));
                if ("true".equals(getAttr(element, "global"))) {
                    attrs.add("global = true");
                }
                attrs.add(attrIfNotEmpty("toScope", getAttr(element, "to-scope")));
                attrs.add(attrIfNotEmpty("onlyIfField", getAttr(element, "only-if-field")));
                // SCIPIO: 4.0.0: the nested condition is the value being stored; it was dropped
                if (!childElementList(element).isEmpty()) {
                    String conditionToFieldCode = generateConditionAnnotation(element);
                    if (isNotEmpty(conditionToFieldCode)) {
                        attrs.add("condition = " + conditionToFieldCode);
                    }
                }
                break;

            case "close-object":
                attrs.add("type = ActionType.CLOSE_OBJECT");
                attrs.add(attrIfNotEmpty("field", getAttr(element, "field")));
                break;

            case "throw-exception":
                attrs.add("type = ActionType.THROW_EXCEPTION");
                attrs.add(attrIfNotEmpty("field", getAttr(element, "field")));
                break;

            default:
                // Unknown action type - return null and let caller handle
                return null;
        }

        return "@Action(" + joinAttrs(attrs.toArray(new String[0])) + ")";
    }

    /**
     * Generates order-by array for entity actions.
     */
    protected String generateOrderByArray(Element element) {
        List<String> orderByFields = new ArrayList<>();
        for (Element child : childElementList(element)) {
            if ("order-by".equals(child.getNodeName())) {
                String fieldName = getAttr(child, "field-name");
                if (isNotEmpty(fieldName)) {
                    orderByFields.add("\"" + escapeString(fieldName) + "\"");
                }
            }
        }
        if (orderByFields.isEmpty()) {
            return null;
        }
        return "{" + String.join(", ", orderByFields) + "}";
    }

    /**
     * Generates select-fields array for entity actions.
     */
    protected String generateSelectFieldsArray(Element element) {
        List<String> selectFields = new ArrayList<>();
        for (Element child : childElementList(element)) {
            if ("select-field".equals(child.getNodeName())) {
                String fieldName = getAttr(child, "field-name");
                if (isNotEmpty(fieldName)) {
                    selectFields.add("\"" + escapeString(fieldName) + "\"");
                }
            }
        }
        if (selectFields.isEmpty()) {
            return null;
        }
        return "{" + String.join(", ", selectFields) + "}";
    }

    /**
     * Generates conditions array for entity-condition actions.
     */
    protected String generateConditionsArray(Element element) {
        List<String> conditions = new ArrayList<>();
        for (Element child : childElementList(element)) {
            if ("condition-list".equals(child.getNodeName())) {
                // Handle condition-list by extracting condition-expr elements
                for (Element condExpr : childElementList(child)) {
                    if ("condition-expr".equals(condExpr.getNodeName())) {
                        conditions.add(generateConditionExprNested(condExpr));
                    }
                }
            } else if ("condition-expr".equals(child.getNodeName())) {
                conditions.add(generateConditionExprNested(child));
            }
        }
        if (conditions.isEmpty()) {
            return null;
        }
        return "{" + String.join(", ", conditions) + "}";
    }

    /**
     * Generates a @ConditionExpr annotation for entity conditions.
     */
    protected String generateConditionExprNested(Element element) {
        List<String> attrs = new ArrayList<>();
        attrs.add(attrIfNotEmpty("fieldName", getAttr(element, "field-name")));
        attrs.add(attrIfNotEmpty("operator", getAttr(element, "operator")));
        attrs.add(attrIfNotEmpty("value", getAttr(element, "value")));
        attrs.add(attrIfNotEmpty("fromField", getAttr(element, "from-field")));
        attrs.add(attrIfNotEmpty("envName", getAttr(element, "env-name")));
        if ("true".equals(getAttr(element, "ignore-case"))) {
            attrs.add("ignoreCase = true");
        }
        if ("true".equals(getAttr(element, "ignore-if-null"))) {
            attrs.add("ignoreIfNull = true");
        }
        if ("true".equals(getAttr(element, "ignore-if-empty"))) {
            attrs.add("ignoreIfEmpty = true");
        }
        return "@ConditionExpr(" + joinAttrs(attrs.toArray(new String[0])) + ")";
    }

    /**
     * Generates field-map annotations array for unified Action format.
     * Returns a string like "{@FieldMap(...), @FieldMap(...)}" or null if no field-maps.
     */
    protected String generateFieldMapsNested(Element parentElement) {
        List<String> fieldMaps = new ArrayList<>();
        for (Element fm : childElementList(parentElement)) {
            if ("field-map".equals(fm.getNodeName())) {
                String fieldName = getAttr(fm, "field-name");
                String fromField = getAttr(fm, "from-field");
                String value = getAttr(fm, "value");
                List<String> fmAttrs = new ArrayList<>();
                if (isNotEmpty(fieldName)) fmAttrs.add(attrIfNotEmpty("fieldName", fieldName));
                if (isNotEmpty(fromField)) fmAttrs.add(attrIfNotEmpty("fromField", fromField));
                if (isNotEmpty(value)) fmAttrs.add(attrIfNotEmpty("value", value));
                fieldMaps.add("@FieldMap(" + joinAttrs(fmAttrs.toArray(new String[0])) + ")");
            }
        }
        if (fieldMaps.isEmpty()) {
            return null;
        }
        return "{" + String.join(", ", fieldMaps) + "}";
    }

    protected String generateWidgetsNested(Element widgetsElement) {
        // Generate unified @Widget annotations in document order (preserves render order)
        List<String> unifiedWidgets = new ArrayList<>();
        String decoratorScreenCode = null;

        // SCIPIO: 4.0.0: Collect complex widgets that can't be represented in unified format
        List<String> screenletsCode = new ArrayList<>();
        List<String> containersCode = new ArrayList<>();
        List<String> htmlTemplatesCode = new ArrayList<>();
        List<String> sectionsCode = new ArrayList<>();

        // SCIPIO: 4.0.0: the reader order of Widgets (addWidgetsContent)
        ChildOrder order = new ChildOrder(unifiedWidgets, sectionsCode, screenletsCode, containersCode, htmlTemplatesCode);
        for (Element child : childElementList(widgetsElement)) {
            order.before();
            String tagName = child.getNodeName();
            String unifiedWidget = generateUnifiedWidget(child, tagName);
            if (unifiedWidget != null) {
                unifiedWidgets.add(unifiedWidget);
            } else if ("decorator-screen".equals(tagName)) {
                // Decorator screen is handled separately
                decoratorScreenCode = generateDecoratorScreen(child);
            } else if ("section".equals(tagName)) {
                // A section carrying a condition or actions must keep them. Wrapping it in a
                // container would emit a stray div, which is fatal before the html element.
                if (firstChildElement(child, "condition") != null || firstChildElement(child, "actions") != null
                        || isNotEmpty(getAttr(child, "name")) || isNotEmpty(getAttr(child, "contains"))) {
                    sectionsCode.add(generateSectionNestedAtDepth(child, 1));
                } else {
                    // Flatten sections - extract their widgets in order
                    ChildOrder savedOrder = flattenOrder;
                    flattenOrder = order;
                    flattenSectionWidgetsExtended(child, unifiedWidgets, screenletsCode, containersCode, htmlTemplatesCode);
                    flattenOrder = savedOrder;
                }
            } else if ("screenlet".equals(tagName)) {
                // SCIPIO: 4.0.0: Handle screenlets that can't be unified (have nested content)
                String screenletCode = generateScreenlet(child, "");
                if (isNotEmpty(screenletCode)) {
                    screenletsCode.add(screenletCode);
                }
            } else if ("container".equals(tagName)) {
                // SCIPIO: 4.0.0: Handle containers that can't be unified (have nested content)
                String containerCode = generateContainerNested(child);
                if (isNotEmpty(containerCode)) {
                    containersCode.add(containerCode);
                }
            } else if ("platform-specific".equals(tagName)) {
                // SCIPIO: 4.0.0: Handle html-templates
                htmlTemplatesCode.addAll(generateHtmlTemplatesNested(child));
            }
            order.after();
        }
        order.applyPositions();

        List<String> attrs = new ArrayList<>();
        if (decoratorScreenCode != null) {
            attrs.add("decorator = " + decoratorScreenCode);
        }
        if (!unifiedWidgets.isEmpty()) {
            attrs.add("value = {" + String.join(", ", unifiedWidgets) + "}");
        }
        // SCIPIO: 4.0.0: Add complex widgets using legacy arrays
        if (!screenletsCode.isEmpty()) {
            attrs.add("screenlets = {" + String.join(", ", screenletsCode) + "}");
        }
        if (!containersCode.isEmpty()) {
            attrs.add("containers = {" + String.join(", ", containersCode) + "}");
        }
        if (!htmlTemplatesCode.isEmpty()) {
            attrs.add("htmlTemplates = {" + String.join(", ", htmlTemplatesCode) + "}");
        }
        // SCIPIO: 4.0.0: sections were collected but never emitted, so every nested section of a
        // widgets block (and everything it held) was dropped
        if (!sectionsCode.isEmpty()) {
            attrs.add("sections = {" + String.join(", ", sectionsCode) + "}");
        }

        if (attrs.isEmpty()) {
            return null;
        }

        return "@Widgets(" + joinAttrs(attrs.toArray(new String[0])) + ")";
    }

    /**
     * Flattens section widgets with extended support for complex widgets.
     * SCIPIO: 4.0.0: Added to properly handle screenlets and containers in sections.
     */
    protected void flattenSectionWidgetsExtended(Element sectionElement, List<String> unifiedWidgets,
                                                  List<String> screenletsCode, List<String> containersCode,
                                                  List<String> htmlTemplatesCode) {
        // SCIPIO: 4.0.0: a nested section WITH a condition/actions must keep them: wrap it in a synthetic container so
        // it becomes @Container(sections = {@SectionNested(condition = ..., actions = ..., widgets = ...)})
        if (containersCode != null && (firstChildElement(sectionElement, "condition") != null || firstChildElement(sectionElement, "actions") != null)) {
            Element wrapper = sectionElement.getOwnerDocument().createElement("container");
            wrapper.appendChild(sectionElement.cloneNode(true));
            String wrapperCode = generateContainerLevel(wrapper, 1);
            if (isNotEmpty(wrapperCode)) {
                containersCode.add(wrapperCode);
                return;
            }
        }
        Element widgetsEl = getFirstChild(sectionElement, "widgets");
        if (widgetsEl != null) {
            for (Element child : childElementList(widgetsEl)) {
                splitFlattenOrder();
                String tagName = child.getNodeName();
                String widget = generateUnifiedWidget(child, tagName);
                if (widget != null) {
                    unifiedWidgets.add(widget);
                } else if ("section".equals(tagName)) {
                    // Recursively flatten nested sections
                    flattenSectionWidgetsExtended(child, unifiedWidgets, screenletsCode, containersCode, htmlTemplatesCode);
                } else if ("screenlet".equals(tagName)) {
                    // Handle screenlets with nested content
                    String screenletCode = generateScreenlet(child, "");
                    if (isNotEmpty(screenletCode)) {
                        screenletsCode.add(screenletCode);
                    }
                } else if ("container".equals(tagName)) {
                    // Handle containers with nested content
                    String containerCode = generateContainerNested(child);
                    if (isNotEmpty(containerCode)) {
                        containersCode.add(containerCode);
                    }
                } else if ("platform-specific".equals(tagName)) {
                    String htmlTemplate = generateHtmlTemplateNested(child);
                    if (isNotEmpty(htmlTemplate)) {
                        htmlTemplatesCode.add(htmlTemplate);
                    }
                }
            }
        }
    }

    /**
     * Generates a unified @Widget annotation from an XML widget element.
     * Preserves all attributes while using WidgetType discriminator.
     *
     * @param element The widget XML element
     * @param tagName The XML tag name
     * @return The unified @Widget annotation string, or null if complex/unsupported type
     */
    protected String generateUnifiedWidget(Element element, String tagName) {
        List<String> attrs = new ArrayList<>();

        switch (tagName) {
            case "include-screen":
                attrs.add("type = WidgetType.INCLUDE_SCREEN");
                attrs.add(attrIfNotEmpty("name", getAttr(element, "name")));
                attrs.add(attrIfNotEmpty("location", getAttr(element, "location")));
                if ("true".equals(getAttr(element, "share-scope"))) {
                    attrs.add("shareScope = true");
                }
                break;

            case "include-form":
                attrs.add("type = WidgetType.INCLUDE_FORM");
                attrs.add(attrIfNotEmpty("name", getAttr(element, "name")));
                attrs.add(attrIfNotEmpty("location", getAttr(element, "location")));
                if ("true".equals(getAttr(element, "share-scope"))) {
                    attrs.add("shareScope = true");
                }
                break;

            case "include-menu":
                attrs.add("type = WidgetType.INCLUDE_MENU");
                attrs.add(attrIfNotEmpty("name", getAttr(element, "name")));
                attrs.add(attrIfNotEmpty("location", getAttr(element, "location")));
                break;

            case "include-grid":
                attrs.add("type = WidgetType.INCLUDE_GRID");
                attrs.add(attrIfNotEmpty("name", getAttr(element, "name")));
                attrs.add(attrIfNotEmpty("location", getAttr(element, "location")));
                break;

            case "include-tree":
                attrs.add("type = WidgetType.INCLUDE_TREE");
                attrs.add(attrIfNotEmpty("name", getAttr(element, "name")));
                attrs.add(attrIfNotEmpty("location", getAttr(element, "location")));
                break;

            case "label":
                attrs.add("type = WidgetType.LABEL");
                // SCIPIO: 4.0.0: label text may be element content, not the text attribute (was dropped)
                String labelText = getAttr(element, "text");
                if (isEmpty(labelText)) {
                    labelText = element.getTextContent();
                }
                attrs.add(attrIfNotEmpty("text", labelText));
                attrs.add(attrIfNotEmpty("style", getAttr(element, "style")));
                attrs.add(attrIfNotEmpty("id", getAttr(element, "id")));
                break;

            case "decorator-section-include":
                attrs.add("type = WidgetType.DECORATOR_SECTION_INCLUDE");
                attrs.add(attrIfNotEmpty("name", getAttr(element, "name")));
                break;

            case "horizontal-separator":
                attrs.add("type = WidgetType.HORIZONTAL_SEPARATOR");
                attrs.add(attrIfNotEmpty("style", getAttr(element, "style")));
                attrs.add(attrIfNotEmpty("id", getAttr(element, "id")));
                break;

            case "image":
                attrs.add("type = WidgetType.IMAGE");
                attrs.add(attrIfNotEmpty("src", getAttr(element, "src")));
                attrs.add(attrIfNotEmpty("alt", getAttr(element, "alt")));
                attrs.add(attrIfNotEmpty("style", getAttr(element, "style")));
                attrs.add(attrIfNotEmpty("id", getAttr(element, "id")));
                attrs.add(attrIfNotEmpty("title", getAttr(element, "title")));
                attrs.add(attrIfNotEmpty("width", getAttr(element, "width")));
                attrs.add(attrIfNotEmpty("height", getAttr(element, "height")));
                attrs.add(attrIfNotEmpty("border", getAttr(element, "border")));
                attrs.add(attrIfNotEmpty("urlMode", getAttr(element, "url-mode")));
                break;

            case "link":
                attrs.add("type = WidgetType.LINK");
                attrs.add(attrIfNotEmpty("text", getAttr(element, "text")));
                attrs.add(attrIfNotEmpty("style", getAttr(element, "style")));
                attrs.add(attrIfNotEmpty("id", getAttr(element, "id")));
                attrs.add(attrIfNotEmpty("target", getAttr(element, "target")));
                attrs.add(attrIfNotEmpty("targetWindow", getAttr(element, "target-window")));
                attrs.add(attrIfNotEmpty("linkType", getAttr(element, "link-type")));
                break;

            case "content":
                attrs.add("type = WidgetType.CONTENT");
                attrs.add(attrIfNotEmpty("contentId", getAttr(element, "content-id")));
                attrs.add(attrIfNotEmpty("dataResourceId", getDataResourceIdAttr(element)));
                attrs.add(attrIfNotEmpty("editRequest", getAttr(element, "edit-request")));
                attrs.add(attrIfNotEmpty("editContainerStyle", getAttr(element, "edit-container-style")));
                attrs.add(attrIfNotEmpty("enableEditValue", getAttr(element, "enable-edit-value")));
                attrs.add(attrIfNotEmpty("enableEditName", getAttr(element, "enable-edit-name")));
                if ("true".equals(getAttr(element, "xml-escape"))) {
                    attrs.add("xmlEscape = true");
                }
                break;

            case "sub-content":
                attrs.add("type = WidgetType.SUB_CONTENT");
                attrs.add(attrIfNotEmpty("contentId", getAttr(element, "content-id")));
                attrs.add(attrIfNotEmpty("mapKey", getAttr(element, "map-key")));
                attrs.add(attrIfNotEmpty("assocName", getAttr(element, "assoc-name")));
                attrs.add(attrIfNotEmpty("editRequest", getAttr(element, "edit-request")));
                attrs.add(attrIfNotEmpty("editContainerStyle", getAttr(element, "edit-container-style")));
                attrs.add(attrIfNotEmpty("enableEditName", getAttr(element, "enable-edit-name")));
                attrs.add(attrIfNotEmpty("enableEditValue", getAttr(element, "enable-edit-value")));
                break;

            case "platform-specific": {
                // SCIPIO: 4.0.0: every branch (html, xsl-fo, text, xml) and every template in it; several
                // widgets come back joined by ", " because the caller places the result in an array literal.
                List<String> platformWidgets = platformTemplateWidgets(element);
                if (platformWidgets.isEmpty()) {
                    return null;
                }
                return String.join(", ", platformWidgets);
            }

            case "container":
                // Containers with nested content use legacy annotation
                if (hasChildElements(element)) {
                    return null;
                }
                attrs.add("type = WidgetType.CONTAINER");
                attrs.add(attrIfNotEmpty("style", getAttr(element, "style")));
                attrs.add(attrIfNotEmpty("id", getAttr(element, "id")));
                attrs.add(attrIfNotEmpty("containerType", getAttr(element, "type")));
                attrs.add(attrIfNotEmpty("autoUpdateTargetId", getAttr(element, "auto-update-target-id")));
                String interval = getAttr(element, "auto-update-interval");
                if (isNotEmpty(interval)) {
                    attrs.add("autoUpdateInterval = " + interval);
                }
                attrs.add(attrIfNotEmpty("contains", getAttr(element, "contains")));
                break;

            case "screenlet":
                // Screenlets with nested content use legacy annotation
                return null;

            case "iterate-section": {
                attrs.add("type = WidgetType.ITERATE_SECTION");
                attrs.add(attrIfNotEmpty("list", getAttr(element, "list")));
                attrs.add(attrIfNotEmpty("entry", getAttr(element, "entry")));
                attrs.add(attrIfNotEmpty("key", getAttr(element, "key")));
                String viewSize = getAttr(element, "view-size");
                if (isNotEmpty(viewSize)) {
                    attrs.add("viewSize = " + viewSize);
                }
                if ("false".equals(getAttr(element, "paginate"))) {
                    attrs.add("paginate = false");
                }
                attrs.add(attrIfNotEmpty("paginateTarget", getAttr(element, "paginate-target")));
                // SCIPIO: 4.0.0: the section moves into a helper screen; this widget rendered nothing before
                Element iterateSection = firstChildElement(element, "section");
                if (iterateSection != null) {
                    String helperName = iterateHelperNames.get(element);
                    if (helperName == null) {
                        helperName = currentScreenName + "-iterate" + (++iterateCounter);
                        iterateHelperNames.put(element, helperName);
                        Element helper = element.getOwnerDocument().createElement("screen");
                        helper.setAttribute("name", helperName);
                        helper.appendChild(iterateSection.cloneNode(true));
                        pendingHelperScreens.add(helper);
                    }
                    attrs.add(attrIfNotEmpty("name", helperName));
                    attrs.add(attrIfNotEmpty("location", sourceLocation));
                }
                break;
            }
            case "include-portal-page":
                // SCIPIO: 4.0.0: was dropped, so every portal page screen was empty
                attrs.add("type = WidgetType.INCLUDE_PORTAL_PAGE");
                attrs.add(attrIfNotEmpty("id", getAttr(element, "id")));
                attrs.add(attrIfNotEmpty("confMode", getAttr(element, "conf-mode")));
                attrs.add(attrIfNotEmpty("usePrivate", getAttr(element, "use-private")));
                break;

            default:
                return null;
        }

        return "@Widget(" + joinAttrs(attrs.toArray(new String[0])) + ")";
    }

    /**
     * Flattens section widgets into the unified widgets list.
     */
    protected void flattenSectionWidgets(Element sectionElement, List<String> unifiedWidgets) {
        // Look for widgets in the section (check widgets, fail-widgets)
        Element widgetsEl = getFirstChild(sectionElement, "widgets");
        if (widgetsEl != null) {
            for (Element child : childElementList(widgetsEl)) {
                String tagName = child.getNodeName();
                String widget = generateUnifiedWidget(child, tagName);
                if (widget != null) {
                    unifiedWidgets.add(widget);
                } else if ("section".equals(tagName)) {
                    // Recursively flatten nested sections
                    flattenSectionWidgets(child, unifiedWidgets);
                }
            }
        }
    }

    /**
     * Checks if an element has child elements.
     */
    protected boolean hasChildElements(Element element) {
        for (Element child : childElementList(element)) {
            return true;
        }
        return false;
    }

    /**
     * Gets the first child element with the given name.
     */
    protected Element getFirstChild(Element parent, String name) {
        for (Element child : childElementList(parent)) {
            if (name.equals(child.getNodeName())) {
                return child;
            }
        }
        return null;
    }

    /**
     * Flattens a section element's widgets into the parent lists.
     * This is necessary because Java annotations cannot have cyclic references,
     * so Section cannot be nested inside Widgets/DecoratorSection.
     */
    protected void flattenSectionToDecoratorSection(Element sectionElement,
            List<String> includeFormsCode, List<String> includeScreensCode,
            List<String> includeMenusCode, List<String> screenletsCode,
            List<String> labelsCode, List<String> containersCode, List<String> htmlTemplatesCode) {
        flattenSectionToDecoratorSection(sectionElement, includeFormsCode, includeScreensCode,
                includeMenusCode, screenletsCode, labelsCode, containersCode, htmlTemplatesCode, 1, null);
    }

    /**
     * SCIPIO: 4.0.0: Kept for callers with no decorator-section-include list; those includes are lost.
     */
    protected void flattenSectionToDecoratorSection(Element sectionElement,
            List<String> includeFormsCode, List<String> includeScreensCode,
            List<String> includeMenusCode, List<String> screenletsCode,
            List<String> labelsCode, List<String> containersCode, List<String> htmlTemplatesCode,
            int containerDepth) {
        flattenSectionToDecoratorSection(sectionElement, includeFormsCode, includeScreensCode,
                includeMenusCode, screenletsCode, labelsCode, containersCode, htmlTemplatesCode,
                containerDepth, null);
    }

    /**
     * @param containerDepth the Container annotation level (1 = Container, 2 = Container2, ...)
     *        to emit for container children — callers inside a ContainerN pass N + 1 so the
     *        flattened containers land in the correct cycle-breaking type slot.
     */
    protected void flattenSectionToDecoratorSection(Element sectionElement,
            List<String> includeFormsCode, List<String> includeScreensCode,
            List<String> includeMenusCode, List<String> screenletsCode,
            List<String> labelsCode, List<String> containersCode, List<String> htmlTemplatesCode,
            int containerDepth, List<String> decoratorSectionIncludesCode) {

        // SCIPIO: 4.0.0: a nested section WITH a condition must not be flattened (its condition would be lost, e.g.
        // GlobalDecorator appheaderTemplate rendered when empty). Wrap it in a synthetic container so it becomes
        // @ContainerN(sections = {@SectionNested(condition = ..., widgets = ..., failWidgets = ...)}).
        if (containersCode != null && firstChildElement(sectionElement, "condition") != null) {
            Element wrapper = sectionElement.getOwnerDocument().createElement("container");
            wrapper.appendChild(sectionElement.cloneNode(true));
            String wrapperCode = generateContainerLevel(wrapper, containerDepth);
            if (isNotEmpty(wrapperCode)) {
                containersCode.add(wrapperCode);
                return;
            }
        }

        // Process widgets element
        Element widgetsElement = firstChildElement(sectionElement, "widgets");
        if (widgetsElement != null) {
            flattenWidgetsElement(widgetsElement, includeFormsCode, includeScreensCode,
                    includeMenusCode, screenletsCode, labelsCode, containersCode, htmlTemplatesCode, containerDepth,
                    decoratorSectionIncludesCode);
        }

        // Also process fail-widgets element
        Element failWidgetsElement = firstChildElement(sectionElement, "fail-widgets");
        if (failWidgetsElement != null) {
            flattenWidgetsElement(failWidgetsElement, includeFormsCode, includeScreensCode,
                    includeMenusCode, screenletsCode, labelsCode, containersCode, htmlTemplatesCode, containerDepth,
                    decoratorSectionIncludesCode);
        }
    }

    protected void flattenWidgetsElement(Element widgetsElement,
            List<String> includeFormsCode, List<String> includeScreensCode,
            List<String> includeMenusCode, List<String> screenletsCode,
            List<String> labelsCode, List<String> containersCode, List<String> htmlTemplatesCode,
            int containerDepth, List<String> decoratorSectionIncludesCode) {

        for (Element child : childElementList(widgetsElement)) {
            splitFlattenOrder();
            String tagName = child.getNodeName();
            switch (tagName) {
                // SCIPIO: 4.0.0: a flattened section lost every decorator-section-include it held
                case "decorator-section-include":
                    if (decoratorSectionIncludesCode != null) {
                        decoratorSectionIncludesCode.add(generateDecoratorSectionIncludeNested(child));
                    }
                    break;
                case "include-form":
                    if (includeFormsCode != null) includeFormsCode.add(generateIncludeFormNested(child));
                    break;
                case "include-screen":
                    if (includeScreensCode != null) includeScreensCode.add(generateIncludeScreenNested(child));
                    break;
                case "include-menu":
                    if (includeMenusCode != null) includeMenusCode.add(generateIncludeMenuNested(child));
                    break;
                case "screenlet":
                    if (screenletsCode != null) screenletsCode.add(generateScreenletNested(child));
                    break;
                case "label":
                    if (labelsCode != null) labelsCode.add(generateLabelNested(child));
                    break;
                case "container":
                    if (containersCode != null) containersCode.add(generateContainerLevel(child, containerDepth));
                    break;
                case "platform-specific":
                    if (htmlTemplatesCode != null) {
                        htmlTemplatesCode.addAll(generateHtmlTemplatesNested(child));
                    }
                    break;
                case "section":
                    // Recurse into nested sections
                    flattenSectionToDecoratorSection(child, includeFormsCode, includeScreensCode,
                            includeMenusCode, screenletsCode, labelsCode, containersCode, htmlTemplatesCode,
                            containerDepth, decoratorSectionIncludesCode);
                    break;
            }
        }
    }

    protected String generateFailWidgetsNested(Element failWidgetsElement) {
        // Re-use the same logic as widgets
        return generateWidgetsNested(failWidgetsElement);
    }

    /**
     * Generates a screen condition annotation from a condition element.
     * Uses the functionalConditions attribute with the new functional interface pattern.
     *
     * <p>Handles both simple conditions (if-true, if-empty, if-has-permission)
     * and compound conditions (and, or, xor, not). Compound conditions are flattened
     * where possible due to Java annotation cyclic reference limitations.</p>
     */
    protected String generateConditionAnnotation(Element conditionElement) {
        // SCIPIO: 4.0.0: Represents <or>/<xor> via OrCondition/XorCondition and a single top-level <not> via not = true
        // (previously collapsed to Always = silent semantic loss). Unrepresentable trees still fall back to Always for
        // sections; if-actions reject Always (see generateIfActionAnnotation).
        List<String> conditionCodes = new ArrayList<>();
        List<String> orCodes = new ArrayList<>();
        List<String> xorCodes = new ArrayList<>();
        boolean notWrapper = false;
        List<? extends Element> children = childElementList(conditionElement);
        if (children.size() == 1 && "not".equals(children.get(0).getNodeName())) {
            List<? extends Element> inner = childElementList(children.get(0));
            boolean representable = !inner.isEmpty();
            for (Element e : inner) {
                String t = e.getNodeName();
                if ("or".equals(t) || "xor".equals(t) || "not".equals(t)) {
                    representable = false;
                }
            }
            if (representable) {
                notWrapper = true;
                children = inner;
            }
        }
        for (Element child : children) {
            String tagName = child.getNodeName();
            if ("or".equals(tagName) || "xor".equals(tagName)) {
                String code = generateOrXorCondition(child);
                if (code != null) {
                    ("or".equals(tagName) ? orCodes : xorCodes).add(code);
                    continue;
                }
            }
            if ("and".equals(tagName)) {
                collectFunctionalConditions(child, conditionCodes);
                continue;
            }
            String code = generateFunctionalCondition(child);
            if (isNotEmpty(code)) {
                conditionCodes.add(code);
            }
        }
        if (conditionCodes.isEmpty() && orCodes.isEmpty() && xorCodes.isEmpty()) {
            return null;
        }
        List<String> attrs = new ArrayList<>();
        if (notWrapper) attrs.add("not = true");
        if (!conditionCodes.isEmpty()) attrs.add("functionalConditions = {" + String.join(", ", conditionCodes) + "}");
        if (!orCodes.isEmpty()) attrs.add("or = {" + String.join(", ", orCodes) + "}");
        if (!xorCodes.isEmpty()) attrs.add("xor = {" + String.join(", ", xorCodes) + "}");
        return "@com.ilscipio.scipio.widget.def.screen.Condition(" + String.join(", ", attrs) + ")";
    }

    /**
     * SCIPIO: 4.0.0: Builds an OrCondition/XorCondition from simple children (not(simple) is folded); null if unrepresentable.
     */
    protected String generateOrXorCondition(Element element) {
        String ann = "xor".equals(element.getNodeName()) ? "@XorCondition(" : "@OrCondition(";
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
                // SCIPIO: 4.0.0: not(if-true x) is not if-false x (an unset field is neither), and the arrays
                // take no value expression; both go to the tree form
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
                    ifCompare.add("@IfCompare(" + String.join(", ", a) + ")");
                    break;
                }
                case "if-has-permission": {
                    if (neg) return null;
                    String action = getAttr(c, "action");
                    ifHasPerm.add("@IfHasPermission(permission = " + toStringValue(getAttr(c, "permission"))
                            + (isNotEmpty(action) ? ", action = " + toStringValue(action) : "") + ")");
                    break;
                }
                default:
                    return null; // nested and/or/xor or other condition types: not representable in OrCondition
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
     * Collects functional condition codes, flattening And elements.
     */
    protected void collectFunctionalConditions(Element parentElement, List<String> conditionCodes) {
        for (Element child : childElementList(parentElement)) {
            String tagName = child.getNodeName();
            if ("and".equals(tagName)) {
                // Flatten And element - recurse into its children
                collectFunctionalConditions(child, conditionCodes);
            } else {
                // Other conditions - generate normally
                String code = generateFunctionalCondition(child);
                if (isNotEmpty(code)) {
                    conditionCodes.add(code);
                }
            }
        }
    }

    /**
     * Generates a functional @Condition annotation from an XML condition element.
     *
     * <p>Uses the new functional interface pattern with @Condition(type = X.class, params = {...}).</p>
     */
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
                // This case shouldn't be reached normally due to collectFunctionalConditions
                return null;
            case "or":
            case "xor": {
                // SCIPIO: 4.0.0: Or/Xor are represented through Condition.nested() (were always-true fallbacks)
                String composite = generateConditionTree(element, "or".equals(tagName) ? "Or" : "Xor", false);
                if (composite != null) {
                    return composite;
                }
                System.err.println("WARN: Screen condition '" + tagName + "' cannot be represented - using Always fallback");
                return "@Condition(type = Always.class)";
            }
            case "not":
                return generateNotCondition(element);

            // Simple conditions
            // SCIPIO: 4.0.0: value="${...}" was dropped, which left an empty field and broke the whole screen class
            case "if-true":
                return "@Condition(type = True.class, params = {" + toStringValue(fieldOrValue(element)) + "})";
            case "if-false":
                return "@Condition(type = False.class, params = {" + toStringValue(fieldOrValue(element)) + "})";
            case "if-empty":
                return "@Condition(type = Empty.class, params = {" + toStringValue(getAttr(element, "field")) + "})";
            case "if-not-empty":
                return "@Condition(type = NotEmpty.class, params = {" + toStringValue(getAttr(element, "field")) + "})";

            // Permission conditions
            case "if-has-permission":
                return generateHasPermissionCondition(element);
            case "if-service-permission":
                return generateServicePermissionCondition(element);
            case "if-entity-permission":
                return generateEntityPermissionCondition(element);

            // Compare conditions
            case "if-compare":
                return generateCompareCondition(element);
            case "if-compare-field":
                return generateCompareFieldCondition(element);
            case "if-regexp":
                return generateRegexpCondition(element);
            case "if-validate-method":
                return generateValidateMethodCondition(element);

            // Screen-specific conditions
            case "if-empty-section":
                return generateEmptySectionCondition(element);

            // Scipio extension conditions
            case "if-widget":
                return generateWidgetDefinedCondition(element);
            case "if-component":
                return generateComponentEnabledCondition(element);
            case "if-entity":
                return generateEntityDefinedCondition(element);
            case "if-service":
                return generateServiceDefinedCondition(element);

            default:
                return "@Condition(type = Empty.class, params = {\"/* TODO: unsupported condition " + tagName + " */\"})";
        }
    }

    /**
     * Generates a Not condition.
     * Special case: not + if-empty = NotEmpty condition.
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

        // Special case: not + if-not-empty = Empty
        if ("if-not-empty".equals(childTagName)) {
            String field = getAttr(child, "field");
            return "@Condition(type = Empty.class, params = {" + toStringValue(field) + "})";
        }

        // SCIPIO: 4.0.0: not + if-true is not False (and not + if-false is not True): an unset field is neither
        // true nor false. The functional @Condition has no not member, so the negation is a one-node tree.
        if ("if-true".equals(childTagName) || "if-false".equals(childTagName)) {
            return "@Condition(type = And.class, tree = {@ConditionNode(" + conditionNodeHeader(-1, true) + "type = "
                    + ("if-true".equals(childTagName) ? "True" : "False") + ".class, params = {"
                    + toStringValue(fieldOrValue(child)) + "})})";
        }

        // SCIPIO: 4.0.0: not + if-compare = if-compare with the negated operator
        if ("if-compare".equals(childTagName)) {
            String op = getAttr(child, "operator");
            String negOp = negateCompareOperator(isEmpty(op) ? "equals" : op);
            if (negOp != null) {
                return generateCompareCondition(child, negOp);
            }
        }
        // SCIPIO: 4.0.0: General case - generate the inner condition with not = true flag
        // This handles if-empty-section, if-has-permission, if-compare, and any other condition type
        String innerCondition = generateConditionAnnotation(child);
        if (innerCondition != null && innerCondition.startsWith("@Condition(")) {
            // Insert "not = true, " after "@Condition("
            return innerCondition.replace("@Condition(", "@Condition(not = true, ");
        }

        // SCIPIO: 4.0.0: De Morgan rewrite keeps not(or)/not(and) representable (were always-true fallbacks)
        if ("or".equals(childTagName) || "and".equals(childTagName)) {
            // De Morgan: not(or(a, b)) == and(not a, not b), and the mirror for and.
            String composite = generateConditionTree(child, "or".equals(childTagName) ? "And" : "Or", true);
            if (composite != null) {
                return composite;
            }
        }
        System.err.println("WARN: Screen condition 'not(" + childTagName + ")' cannot be represented in annotations - using Always fallback");
        return "@Condition(type = Always.class)";
    }

    protected String generateHasPermissionCondition(Element element) {
        String permission = getAttr(element, "permission");
        String action = getAttr(element, "action");

        List<String> params = new ArrayList<>();
        if (isNotEmpty(permission)) params.add(toStringValue(permission));
        if (isNotEmpty(action)) params.add(toStringValue(action));

        return "@Condition(type = HasPermission.class, params = {" + String.join(", ", params) + "})";
    }

    protected String generateServicePermissionCondition(Element element) {
        String serviceName = getAttr(element, "service-name");
        String mainAction = getAttr(element, "main-action");
        String resourceDescription = getAttr(element, "resource-description");
        String contextMap = getAttr(element, "context-map");

        List<String> params = new ArrayList<>();
        if (isNotEmpty(serviceName)) params.add(toStringValue(serviceName));
        if (isNotEmpty(mainAction)) params.add(toStringValue(mainAction));
        if (isNotEmpty(contextMap)) params.add(toStringValue(contextMap));
        if (isNotEmpty(resourceDescription)) params.add(toStringValue(resourceDescription));

        return "@Condition(type = ServicePermission.class, params = {" + String.join(", ", params) + "})";
    }

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

    protected String generateCompareCondition(Element element) {
        return generateCompareCondition(element, null);
    }

    protected String generateCompareCondition(Element element, String operatorOverride) {
        String field = getAttr(element, "field");
        String operator = (operatorOverride != null) ? operatorOverride : getAttr(element, "operator");
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

    protected String generateRegexpCondition(Element element) {
        String field = getAttr(element, "field");
        String expr = getAttr(element, "expr");

        List<String> params = new ArrayList<>();
        if (isNotEmpty(field)) params.add(toStringValue(field));
        if (isNotEmpty(expr)) params.add(toStringValue(expr));

        return "@Condition(type = Regexp.class, params = {" + String.join(", ", params) + "})";
    }

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

    protected String generateEmptySectionCondition(Element element) {
        String sectionName = getAttr(element, "section-name");
        return "@Condition(type = EmptySection.class, params = {" + toStringValue(sectionName) + "})";
    }

    protected String generateWidgetDefinedCondition(Element element) {
        String name = getAttr(element, "name");
        String location = getAttr(element, "location");
        String widgetType = getAttr(element, "type");

        List<String> params = new ArrayList<>();
        if (isNotEmpty(name)) params.add(toStringValue(name));
        if (isNotEmpty(location)) params.add(toStringValue(location));
        if (isNotEmpty(widgetType)) params.add(toStringValue(widgetType));

        return "@Condition(type = WidgetDefined.class, params = {" + String.join(", ", params) + "})";
    }

    protected String generateComponentEnabledCondition(Element element) {
        String componentName = getAttr(element, "component-name");
        return "@Condition(type = ComponentEnabled.class, params = {" + toStringValue(componentName) + "})";
    }

    protected String generateEntityDefinedCondition(Element element) {
        String entityName = getAttr(element, "entity-name");
        return "@Condition(type = EntityDefined.class, params = {" + toStringValue(entityName) + "})";
    }

    protected String generateServiceDefinedCondition(Element element) {
        String serviceName = getAttr(element, "service-name");
        return "@Condition(type = ServiceDefined.class, params = {" + toStringValue(serviceName) + "})";
    }

    protected List<String> generatePlatformSpecific(Element element, String screenName) {
        List<String> annotations = new ArrayList<>();

        Element htmlElement = firstChildElement(element, "html");
        if (htmlElement != null) {
            Element htmlTemplate = firstChildElement(htmlElement, "html-template");
            if (htmlTemplate != null) {
                String location = getAttr(htmlTemplate, "location");
                annotations.add("@HtmlTemplate(location = " + toStringValue(location) + ")");
            }
        }

        return annotations;
    }

    protected String generateSection(Element element, String screenName) {
        List<String> attrs = new ArrayList<>();

        // Optional attributes
        String name = getAttr(element, "name");
        if (isNotEmpty(name)) {
            attrs.add("name = \"" + escapeString(name) + "\"");
        }

        String shareScope = getAttr(element, "share-scope");
        if ("true".equals(shareScope)) {
            attrs.add("shareScope = true");
        }

        String id = getAttr(element, "id");
        if (isNotEmpty(id)) {
            attrs.add("id = \"" + escapeString(id) + "\"");
        }

        String style = getAttr(element, "style");
        if (isNotEmpty(style)) {
            attrs.add("style = \"" + escapeString(style) + "\"");
        }

        String contains = getAttr(element, "contains");
        if (isNotEmpty(contains)) {
            attrs.add("contains = \"" + escapeString(contains) + "\"");
        }

        // Process condition element
        Element conditionElement = firstChildElement(element, "condition");
        if (conditionElement != null) {
            String conditionCode = generateConditionAnnotation(conditionElement);
            if (isNotEmpty(conditionCode)) {
                attrs.add("condition = " + conditionCode);
            }
        }

        // Process actions element
        Element actionsElement = firstChildElement(element, "actions");
        if (actionsElement != null) {
            List<String> actionsCode = generateActionsNested(actionsElement);
            if (!actionsCode.isEmpty()) {
                attrs.add("actions = @Actions(" + String.join(", ", actionsCode) + ")");
            }
        }

        // Process widgets element
        Element widgetsElement = firstChildElement(element, "widgets");
        if (widgetsElement != null) {
            String widgetsCode = generateWidgetsNested(widgetsElement);
            if (isNotEmpty(widgetsCode)) {
                attrs.add("widgets = " + widgetsCode);
            }
        }

        // Process fail-widgets element
        Element failWidgetsElement = firstChildElement(element, "fail-widgets");
        if (failWidgetsElement != null) {
            String failWidgetsCode = generateWidgetsNested(failWidgetsElement);
            if (isNotEmpty(failWidgetsCode)) {
                attrs.add("failWidgets = " + failWidgetsCode);
            }
        }

        // SCIPIO: 4.0.0: Don't generate empty @Section() annotations
        if (attrs.isEmpty()) {
            return null;
        }
        return "@Section(" + String.join(", ", attrs) + ")";
    }

    protected String generateImage(Element element) {
        String src = getAttr(element, "src");
        String id = getAttr(element, "id");
        String style = getAttr(element, "style");
        String width = getAttr(element, "width");
        String height = getAttr(element, "height");
        String border = getAttr(element, "border");
        String alt = getAttr(element, "alt");
        String urlMode = getAttr(element, "url-mode");

        List<String> attrs = new ArrayList<>();
        attrs.add(attrIfNotEmpty("src", src));
        attrs.add(attrIfNotEmpty("id", id));
        attrs.add(attrIfNotEmpty("style", style));
        attrs.add(attrIfNotEmpty("width", width));
        attrs.add(attrIfNotEmpty("height", height));
        attrs.add(attrIfNotEmpty("border", border));
        attrs.add(attrIfNotEmpty("alt", alt));
        if (isNotEmpty(urlMode) && !"content".equals(urlMode)) {
            attrs.add(attrIfNotEmpty("urlMode", urlMode));
        }

        return "@Image(" + joinAttrs(attrs.toArray(new String[0])) + ")";
    }

    protected String generateHorizontalSeparator(Element element) {
        String id = getAttr(element, "id");
        String name = getAttr(element, "name");
        String style = getAttr(element, "style");

        List<String> attrs = new ArrayList<>();
        attrs.add(attrIfNotEmpty("id", id));
        attrs.add(attrIfNotEmpty("name", name));
        attrs.add(attrIfNotEmpty("style", style));

        return "@HorizontalSeparator(" + joinAttrs(attrs.toArray(new String[0])) + ")";
    }

    protected String generateContent(Element element) {
        String contentId = getAttr(element, "content-id");
        String dataResourceId = getDataResourceIdAttr(element);
        String editRequest = getAttr(element, "edit-request");
        String editContainerStyle = getAttr(element, "edit-container-style");
        String enableEditName = getAttr(element, "enable-edit-name");
        String xmlEscape = getAttr(element, "xml-escape");
        String width = getAttr(element, "width");
        String height = getAttr(element, "height");
        String border = getAttr(element, "border");

        List<String> attrs = new ArrayList<>();
        attrs.add(attrIfNotEmpty("contentId", contentId));
        attrs.add(attrIfNotEmpty("dataResourceId", dataResourceId));
        attrs.add(attrIfNotEmpty("editRequest", editRequest));
        if (isNotEmpty(editContainerStyle) && !"editWrapper".equals(editContainerStyle)) {
            attrs.add(attrIfNotEmpty("editContainerStyle", editContainerStyle));
        }
        if (isNotEmpty(enableEditName) && !"enableEdit".equals(enableEditName)) {
            attrs.add(attrIfNotEmpty("enableEditName", enableEditName));
        }
        if ("true".equals(xmlEscape)) {
            attrs.add("xmlEscape = true");
        }
        attrs.add(attrIfNotEmpty("width", width));
        attrs.add(attrIfNotEmpty("height", height));
        attrs.add(attrIfNotEmpty("border", border));

        return "@Content(" + joinAttrs(attrs.toArray(new String[0])) + ")";
    }

    protected String generateDecoratorSectionInclude(Element element) {
        String name = getAttr(element, "name");

        List<String> attrs = new ArrayList<>();
        attrs.add(attrIfNotEmpty("name", name));

        return "@DecoratorSectionInclude(" + joinAttrs(attrs.toArray(new String[0])) + ")";
    }

    /**
     * Converts a screen name to a valid Java interface name.
     */
    protected String toInterfaceName(String screenName) {
        // Replace invalid characters
        String result = screenName.replaceAll("[^a-zA-Z0-9_]", "_");
        // Ensure starts with letter
        if (result.length() > 0 && Character.isDigit(result.charAt(0))) {
            result = "_" + result;
        }
        return result;
    }

    // ===================== New Widget Type Generators =====================

    protected String generateIncludeGrid(Element element) {
        String name = getAttr(element, "name");
        String location = getAttr(element, "location");
        String shareScope = getAttr(element, "share-scope");

        List<String> attrs = new ArrayList<>();
        attrs.add(attrIfNotEmpty("name", name));
        attrs.add(attrIfNotEmpty("location", location));
        if ("true".equals(shareScope)) {
            attrs.add("shareScope = true");
        }

        return "@IncludeGrid(" + joinAttrs(attrs.toArray(new String[0])) + ")";
    }

    protected String generateIncludeTree(Element element) {
        String name = getAttr(element, "name");
        String location = getAttr(element, "location");
        String shareScope = getAttr(element, "share-scope");

        List<String> attrs = new ArrayList<>();
        attrs.add(attrIfNotEmpty("name", name));
        attrs.add(attrIfNotEmpty("location", location));
        if ("true".equals(shareScope)) {
            attrs.add("shareScope = true");
        }

        return "@IncludeTreeWidget(" + joinAttrs(attrs.toArray(new String[0])) + ")";
    }

    protected String generateIterateSection(Element element) {
        String entry = getAttr(element, "entry");
        String list = getAttr(element, "list");
        String key = getAttr(element, "key");
        String viewSize = getAttr(element, "view-size");
        String paginateTarget = getAttr(element, "paginate-target");
        String paginate = getAttr(element, "paginate");

        List<String> attrs = new ArrayList<>();
        attrs.add(attrIfNotEmpty("entry", entry));
        attrs.add(attrIfNotEmpty("list", list));
        attrs.add(attrIfNotEmpty("key", key));
        attrs.add(attrIfNotEmpty("viewSize", viewSize));
        attrs.add(attrIfNotEmpty("paginateTarget", paginateTarget));
        attrs.add(attrIfNotEmpty("paginate", paginate));

        return "@IterateSection(" + joinAttrs(attrs.toArray(new String[0])) + ")";
    }

    protected String generateSubContent(Element element) {
        String contentId = getAttr(element, "content-id");
        String mapKey = getAttr(element, "map-key");
        String editRequest = getAttr(element, "edit-request");
        String editContainerStyle = getAttr(element, "edit-container-style");
        String enableEditName = getAttr(element, "enable-edit-name");
        String xmlEscape = getAttr(element, "xml-escape");

        List<String> attrs = new ArrayList<>();
        attrs.add(attrIfNotEmpty("contentId", contentId));
        attrs.add(attrIfNotEmpty("mapKey", mapKey));
        attrs.add(attrIfNotEmpty("editRequest", editRequest));
        if (!"editWrapper".equals(editContainerStyle) && isNotEmpty(editContainerStyle)) {
            attrs.add(attrIfNotEmpty("editContainerStyle", editContainerStyle));
        }
        if (!"enableEdit".equals(enableEditName) && isNotEmpty(enableEditName)) {
            attrs.add(attrIfNotEmpty("enableEditName", enableEditName));
        }
        if ("true".equals(xmlEscape)) {
            attrs.add("xmlEscape = true");
        }

        return "@SubContent(" + joinAttrs(attrs.toArray(new String[0])) + ")";
    }

    protected String generateColumnContainer(Element element) {
        String id = getAttr(element, "id");
        String style = getAttr(element, "style");

        List<String> attrs = new ArrayList<>();
        attrs.add(attrIfNotEmpty("id", id));
        attrs.add(attrIfNotEmpty("style", style));

        // Handle column children
        List<Element> columns = childElementList(element, "column");
        if (!columns.isEmpty()) {
            List<String> columnsCode = new ArrayList<>();
            for (Element column : columns) {
                columnsCode.add(generateColumnNested(column));
            }
            attrs.add("columns = {" + String.join(", ", columnsCode) + "}");
        }

        return "@ColumnContainer(" + joinAttrs(attrs.toArray(new String[0])) + ")";
    }

    protected String generateColumnNested(Element element) {
        String id = getAttr(element, "id");
        String style = getAttr(element, "style");

        List<String> attrs = new ArrayList<>();
        attrs.add(attrIfNotEmpty("id", id));
        attrs.add(attrIfNotEmpty("style", style));

        // Collect nested widgets
        List<String> includeScreensCode = new ArrayList<>();
        List<String> includeFormsCode = new ArrayList<>();
        List<String> includeMenusCode = new ArrayList<>();
        List<String> labelsCode = new ArrayList<>();
        List<String> containersCode = new ArrayList<>();
        List<String> htmlTemplatesCode = new ArrayList<>();

        for (Element child : childElementList(element)) {
            String tagName = child.getNodeName();
            switch (tagName) {
                case "include-screen":
                    includeScreensCode.add(generateIncludeScreenNested(child));
                    break;
                case "include-form":
                    includeFormsCode.add(generateIncludeFormNested(child));
                    break;
                case "include-menu":
                    includeMenusCode.add(generateIncludeMenuNested(child));
                    break;
                case "label":
                    labelsCode.add(generateLabelNested(child));
                    break;
                case "container":
                    containersCode.add(generateContainerNested(child));
                    break;
                case "platform-specific":
                    htmlTemplatesCode.addAll(generateHtmlTemplatesNested(child));
                    break;
            }
        }

        if (!includeScreensCode.isEmpty()) {
            attrs.add("includeScreens = {" + String.join(", ", includeScreensCode) + "}");
        }
        if (!includeFormsCode.isEmpty()) {
            attrs.add("includeForms = {" + String.join(", ", includeFormsCode) + "}");
        }
        if (!includeMenusCode.isEmpty()) {
            attrs.add("includeMenus = {" + String.join(", ", includeMenusCode) + "}");
        }
        if (!labelsCode.isEmpty()) {
            attrs.add("labels = {" + String.join(", ", labelsCode) + "}");
        }
        if (!containersCode.isEmpty()) {
            attrs.add("containers = {" + String.join(", ", containersCode) + "}");
        }
        if (!htmlTemplatesCode.isEmpty()) {
            attrs.add("htmlTemplates = {" + String.join(", ", htmlTemplatesCode) + "}");
        }

        return "@Column(" + joinAttrs(attrs.toArray(new String[0])) + ")";
    }

    protected String generateScreenLink(Element element) {
        String text = getAttr(element, "text");
        String id = getAttr(element, "id");
        String style = getAttr(element, "style");
        String name = getAttr(element, "name");
        String title = getAttr(element, "title");
        String target = getAttr(element, "target");
        String targetWindow = getAttr(element, "target-window");
        String urlMode = getAttr(element, "url-mode");
        String linkType = getAttr(element, "link-type");
        String fullPath = getAttr(element, "full-path");
        String secure = getAttr(element, "secure");
        String encode = getAttr(element, "encode");
        String requestConfirmation = getAttr(element, "request-confirmation");
        String confirmationMessage = getAttr(element, "confirmation-message");
        String useWhen = getAttr(element, "use-when");

        List<String> attrs = new ArrayList<>();
        attrs.add(attrIfNotEmpty("text", text));
        attrs.add(attrIfNotEmpty("id", id));
        attrs.add(attrIfNotEmpty("style", style));
        attrs.add(attrIfNotEmpty("name", name));
        attrs.add(attrIfNotEmpty("title", title));
        attrs.add(attrIfNotEmpty("target", target));
        attrs.add(attrIfNotEmpty("targetWindow", targetWindow));
        if (!"intra-app".equals(urlMode) && isNotEmpty(urlMode)) {
            attrs.add(attrIfNotEmpty("urlMode", urlMode));
        }
        if (!"auto".equals(linkType) && isNotEmpty(linkType)) {
            attrs.add(attrIfNotEmpty("linkType", linkType));
        }
        attrs.add(attrIfNotEmpty("fullPath", fullPath));
        attrs.add(attrIfNotEmpty("secure", secure));
        attrs.add(attrIfNotEmpty("encode", encode));
        if ("true".equals(requestConfirmation)) {
            attrs.add("requestConfirmation = true");
        }
        attrs.add(attrIfNotEmpty("confirmationMessage", confirmationMessage));
        attrs.add(attrIfNotEmpty("useWhen", useWhen));

        // Handle parameters
        List<Element> parameters = childElementList(element, "parameter");
        if (!parameters.isEmpty()) {
            List<String> paramsCode = new ArrayList<>();
            for (Element param : parameters) {
                paramsCode.add(generateLinkParameter(param));
            }
            attrs.add("parameters = {" + String.join(", ", paramsCode) + "}");
        }

        return "@ScreenLink(" + joinAttrs(attrs.toArray(new String[0])) + ")";
    }

    protected String generateLinkParameter(Element element) {
        String paramName = getAttr(element, "param-name");
        String fromField = getAttr(element, "from-field");
        String value = getAttr(element, "value");

        List<String> attrs = new ArrayList<>();
        attrs.add(attrIfNotEmpty("paramName", paramName));
        attrs.add(attrIfNotEmpty("fromField", fromField));
        attrs.add(attrIfNotEmpty("value", value));

        return "@LinkParameter(" + joinAttrs(attrs.toArray(new String[0])) + ")";
    }

    protected String generateIncludePortalPage(Element element) {
        String id = getAttr(element, "id");
        String confMode = getAttr(element, "conf-mode");
        String usePrivate = getAttr(element, "use-private");

        List<String> attrs = new ArrayList<>();
        attrs.add(attrIfNotEmpty("id", id));
        if (!"false".equals(confMode) && isNotEmpty(confMode)) {
            attrs.add(attrIfNotEmpty("confMode", confMode));
        }
        if (!"true".equals(usePrivate) && isNotEmpty(usePrivate)) {
            attrs.add(attrIfNotEmpty("usePrivate", usePrivate));
        }

        return "@IncludePortalPage(" + joinAttrs(attrs.toArray(new String[0])) + ")";
    }
}
