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
import java.util.List;

import org.w3c.dom.Document;
import org.w3c.dom.Element;

/**
 * Converts Tree XML definitions to Java annotation source code.
 *
 * <p>SCIPIO: 4.0.0: Added for XML-to-Annotation conversion support.</p>
 */
public class TreeXmlToAnnotationConverter extends XmlToAnnotationConverter {

    private int scriptCounter = 0;

    public TreeXmlToAnnotationConverter(String packageName, String className, String componentName,
                                        File outputDir, File scriptOutputDir) {
        super(packageName, className, componentName, outputDir, scriptOutputDir);
    }

    @Override
    protected String getImports() {
        return "import com.ilscipio.scipio.widget.def.tree.*;" + NEWLINE +
               "import com.ilscipio.scipio.widget.def.screen.FieldMap;" + NEWLINE +
               "import com.ilscipio.scipio.widget.def.screen.SetAction;" + NEWLINE +
               "import com.ilscipio.scipio.widget.def.screen.ScriptAction;" + NEWLINE +
               "import com.ilscipio.scipio.widget.def.screen.EntityOneAction;" + NEWLINE +
               "import com.ilscipio.scipio.widget.def.screen.EntityAndAction;" + NEWLINE +
               "import com.ilscipio.scipio.widget.def.screen.EntityConditionAction;" + NEWLINE +
               "import com.ilscipio.scipio.widget.def.screen.ServiceAction;" + NEWLINE +
               "import com.ilscipio.scipio.widget.def.menu.UrlMode;" + NEWLINE +
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

        Element root = doc.getDocumentElement();
        List<Element> treeElements = childElementList(root, "tree");

        for (Element treeElement : treeElements) {
            sb.append(convertTree(treeElement));
            sb.append(NEWLINE);
        }

        sb.append(generateClassFooter());

        return sb.toString();
    }

    /**
     * Converts a single tree element to annotations.
     */
    protected String convertTree(Element treeElement) {
        StringBuilder sb = new StringBuilder();
        String treeName = getAttr(treeElement, "name");
        scriptCounter = 0;

        // Generate @Tree annotation
        sb.append(indent(1)).append(generateTreeAnnotation(treeElement, treeName));
        sb.append(NEWLINE);

        // Generate interface declaration
        // SCIPIO: 4.0.0: Disambiguate names that collide case-insensitively (Windows filesystem defect)
        sb.append(indent(1)).append("public interface ").append(resolveUniqueInterfaceName(toInterfaceName(treeName))).append(" {}");
        sb.append(NEWLINE);

        return sb.toString();
    }

    /**
     * Generates the @Tree annotation with all attributes.
     */
    protected String generateTreeAnnotation(Element treeElement, String treeName) {
        StringBuilder sb = new StringBuilder();
        sb.append("@Tree(").append(NEWLINE);

        List<String> attrs = new ArrayList<>();

        // Required attributes
        attrs.add(indent(2) + attrIfNotEmpty("name", treeName));

        // SCIPIO: 4.0.0: Add location attribute for backward compatibility with XML references
        if (isNotEmpty(sourceLocation)) {
            attrs.add(indent(2) + attrIfNotEmpty("location", sourceLocation));
        }

        String rootNodeName = getAttr(treeElement, "root-node-name");
        if (isNotEmpty(rootNodeName)) {
            attrs.add(indent(2) + attrIfNotEmpty("rootNodeName", rootNodeName));
        }

        // Default render style
        String defaultRenderStyle = getAttr(treeElement, "default-render-style");
        if (isNotEmpty(defaultRenderStyle) && !"simple".equals(defaultRenderStyle)) {
            String enumValue = defaultRenderStyle.replace("-", "_").toUpperCase();
            attrs.add(indent(2) + "defaultRenderStyle = RenderStyle." + enumValue);
        }

        // Other attributes
        addAttrIfNotEmpty(attrs, treeElement, "default-wrap-style", "defaultWrapStyle");
        addAttrIfNotEmpty(attrs, treeElement, "expand-collapse-request", "expandCollapseRequest");
        addAttrIfNotEmpty(attrs, treeElement, "trail-name", "trailName");
        addAttrIfNotEmpty(attrs, treeElement, "open-depth", "openDepth");
        addAttrIfNotEmpty(attrs, treeElement, "post-trail-open-depth", "postTrailOpenDepth");
        addAttrIfNotEmpty(attrs, treeElement, "entity-name", "entityName");

        // Boolean attributes
        addBoolAttr(attrs, treeElement, "force-child-check", "forceChildCheck", true);

        // Process nodes
        List<Element> nodeElements = childElementList(treeElement, "node");
        if (!nodeElements.isEmpty()) {
            StringBuilder nodesBuilder = new StringBuilder();
            nodesBuilder.append(indent(2)).append("nodes = {").append(NEWLINE);
            boolean first = true;
            for (Element node : nodeElements) {
                if (!first) nodesBuilder.append(",").append(NEWLINE);
                nodesBuilder.append(indent(3)).append(generateTreeNode(node, treeName));
                first = false;
            }
            nodesBuilder.append(NEWLINE).append(indent(2)).append("}");
            attrs.add(nodesBuilder.toString());
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
     * Generates @TreeNode annotation.
     */
    protected String generateTreeNode(Element nodeElement, String treeName) {
        StringBuilder sb = new StringBuilder();
        sb.append("@TreeNode(");

        List<String> attrs = new ArrayList<>();

        // Required name
        String nodeName = getAttr(nodeElement, "name");
        attrs.add(attrIfNotEmpty("name", nodeName));

        // Common attributes
        String wrapStyle = getAttr(nodeElement, "wrap-style");
        String renderStyle = getAttr(nodeElement, "render-style");
        String entryName = getAttr(nodeElement, "entry-name");
        String entityName = getAttr(nodeElement, "entity-name");
        String joinFieldName = getAttr(nodeElement, "join-field-name");

        if (isNotEmpty(wrapStyle)) attrs.add(attrIfNotEmpty("wrapStyle", wrapStyle));
        if (isNotEmpty(renderStyle) && !"simple".equals(renderStyle)) {
            String enumValue = renderStyle.replace("-", "_").toUpperCase();
            attrs.add("renderStyle = RenderStyle." + enumValue);
            attrs.add("useDefaultRenderStyle = false");
        }
        if (isNotEmpty(entryName)) attrs.add(attrIfNotEmpty("entryName", entryName));
        if (isNotEmpty(entityName)) attrs.add(attrIfNotEmpty("entityName", entityName));
        if (isNotEmpty(joinFieldName)) attrs.add(attrIfNotEmpty("joinFieldName", joinFieldName));

        // Process condition
        Element conditionElement = firstChildElement(nodeElement, "condition");
        if (conditionElement != null) {
            String conditionCode = generateTreeNodeCondition(conditionElement);
            if (isNotEmpty(conditionCode)) {
                attrs.add("condition = " + conditionCode);
            }
        }

        // Process actions
        Element actionsElement = firstChildElement(nodeElement, "actions");
        if (actionsElement != null) {
            String actionsCode = generateTreeActions(actionsElement, treeName + "_" + nodeName);
            if (isNotEmpty(actionsCode)) {
                attrs.add("actions = " + actionsCode);
            }
        }

        // Process entity-one
        Element entityOneElement = firstChildElement(nodeElement, "entity-one");
        if (entityOneElement != null) {
            String entityOneCode = generateTreeEntityOne(entityOneElement);
            if (isNotEmpty(entityOneCode)) {
                attrs.add("entityOne = " + entityOneCode);
            }
        }

        // Process service
        Element serviceElement = firstChildElement(nodeElement, "service");
        if (serviceElement != null) {
            String serviceCode = generateTreeService(serviceElement);
            if (isNotEmpty(serviceCode)) {
                attrs.add("service = " + serviceCode);
            }
        }

        // Process include-screen
        Element includeScreenElement = firstChildElement(nodeElement, "include-screen");
        if (includeScreenElement != null) {
            String includeScreenCode = generateTreeIncludeScreen(includeScreenElement);
            if (isNotEmpty(includeScreenCode)) {
                attrs.add("includeScreen = " + includeScreenCode);
            }
        }

        // Process label
        Element labelElement = firstChildElement(nodeElement, "label");
        if (labelElement != null) {
            String labelCode = generateTreeLabel(labelElement);
            if (isNotEmpty(labelCode)) {
                attrs.add("label = " + labelCode);
            }
        }

        // Process link
        Element linkElement = firstChildElement(nodeElement, "link");
        if (linkElement != null) {
            String linkCode = generateTreeLink(linkElement);
            if (isNotEmpty(linkCode)) {
                attrs.add("link = " + linkCode);
            }
        }

        // Process sub-nodes
        List<Element> subNodeElements = childElementList(nodeElement, "sub-node");
        if (!subNodeElements.isEmpty()) {
            StringBuilder subBuilder = new StringBuilder();
            subBuilder.append("subNodes = {");
            boolean first = true;
            for (Element subNode : subNodeElements) {
                if (!first) subBuilder.append(", ");
                subBuilder.append(generateSubNode(subNode, treeName));
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
     * Generates @TreeNodeCondition annotation with functional conditions.
     */
    protected String generateTreeNodeCondition(Element conditionElement) {
        List<String> conditionCodes = new ArrayList<>();
        collectFunctionalConditions(conditionElement, conditionCodes);

        if (conditionCodes.isEmpty()) {
            return "";
        }

        return "@TreeNodeCondition(conditions = {" + String.join(", ", conditionCodes) + "})";
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
            case "and":
                return null; // Handled by collectFunctionalConditions
            case "or":
            case "xor":
                // SCIPIO: Or/Xor cannot be directly represented - use Always as fallback
                String composite = generateConditionTree(element, "or".equals(tagName) ? "Or" : "Xor", false);
                if (composite != null) {
                    return composite;
                }
                System.err.println("WARN: Tree condition '" + tagName + "' cannot be represented - using Always fallback");
                return "@Condition(type = Always.class)";
            case "not":
                return generateNotCondition(element);
            case "if-empty":
                return "@Condition(type = Empty.class, params = {" + toStringValue(getAttr(element, "field")) + "})";
            case "if-not-empty":
                return "@Condition(type = NotEmpty.class, params = {" + toStringValue(getAttr(element, "field")) + "})";
            case "if-true":
                return "@Condition(type = True.class, params = {" + toStringValue(getAttr(element, "field")) + "})";
            case "if-false":
                return "@Condition(type = False.class, params = {" + toStringValue(getAttr(element, "field")) + "})";
            case "if-has-permission":
                return generateHasPermissionCondition(element);
            case "if-compare":
                return generateCompareCondition(element);
            case "if-service-permission":
                return generateServicePermissionCondition(element);
            default:
                // SCIPIO: Unknown condition - use Always as fallback
                System.err.println("WARN: Tree condition '" + tagName + "' not supported in annotations - using Always fallback");
                return "@Condition(type = Always.class)";
        }
    }

    protected String generateNotCondition(Element element) {
        Element child = firstChildElement(element, null);
        if (child == null) {
            System.err.println("WARN: Empty 'not' condition in tree - using Always fallback");
            return "@Condition(type = Always.class)";
        }

        String childTagName = child.getNodeName();

        if ("if-empty".equals(childTagName)) {
            return "@Condition(type = NotEmpty.class, params = {" + toStringValue(getAttr(child, "field")) + "})";
        }
        if ("if-not-empty".equals(childTagName)) {
            return "@Condition(type = Empty.class, params = {" + toStringValue(getAttr(child, "field")) + "})";
        }
        // SCIPIO: 4.0.0: not + if-true is not False (an unset field is neither true nor false); the negation is
        // a one-node tree because the functional @Condition has no not member
        if ("if-true".equals(childTagName) || "if-false".equals(childTagName)) {
            return "@Condition(type = And.class, tree = {@ConditionNode(not = true, type = "
                    + ("if-true".equals(childTagName) ? "True" : "False") + ".class, params = {"
                    + toStringValue(getAttr(child, "field")) + "})})";
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
        System.err.println("WARN: Tree condition 'not(" + childTagName + ")' cannot be represented in annotations - using Always fallback");
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

    protected String generateServicePermissionCondition(Element element) {
        String serviceName = getAttr(element, "service-name");
        String mainAction = getAttr(element, "main-action");

        List<String> params = new ArrayList<>();
        if (isNotEmpty(serviceName)) params.add(toStringValue(serviceName));
        if (isNotEmpty(mainAction)) params.add(toStringValue(mainAction));

        return "@Condition(type = ServicePermission.class, params = {" + String.join(", ", params) + "})";
    }

    /**
     * Generates @TreeActions annotation.
     */
    protected String generateTreeActions(Element actionsElement, String contextName) {
        List<String> setActions = new ArrayList<>();
        List<String> entityOneActions = new ArrayList<>();
        List<String> serviceActions = new ArrayList<>();
        List<String> scriptActions = new ArrayList<>();

        for (Element child : childElementList(actionsElement)) {
            String tagName = child.getNodeName();
            String annotation = generateActionAnnotation(child, tagName, contextName);
            if (isNotEmpty(annotation)) {
                switch (tagName) {
                    case "set":
                        setActions.add(annotation);
                        break;
                    case "entity-one":
                        entityOneActions.add(annotation);
                        break;
                    case "service":
                        serviceActions.add(annotation);
                        break;
                    case "script":
                        scriptActions.add(annotation);
                        break;
                }
            }
        }

        List<String> attrs = new ArrayList<>();
        if (!setActions.isEmpty()) {
            attrs.add("set = {" + String.join(", ", setActions) + "}");
        }
        if (!entityOneActions.isEmpty()) {
            attrs.add("entityOne = {" + String.join(", ", entityOneActions) + "}");
        }
        if (!serviceActions.isEmpty()) {
            attrs.add("service = {" + String.join(", ", serviceActions) + "}");
        }
        if (!scriptActions.isEmpty()) {
            attrs.add("script = {" + String.join(", ", scriptActions) + "}");
        }

        if (attrs.isEmpty()) {
            return "";
        }

        return "@TreeActions(" + String.join(", ", attrs) + ")";
    }

    /**
     * Generates a single action annotation.
     */
    protected String generateActionAnnotation(Element element, String tagName, String contextName) {
        switch (tagName) {
            case "set":
                return generateSetAction(element);
            case "script":
                return generateScriptAction(element, contextName);
            case "entity-one":
                return generateEntityOneAction(element);
            case "entity-and":
                return generateEntityAndAction(element);
            case "entity-condition":
                return generateEntityConditionAction(element);
            case "service":
                return generateServiceCallAction(element);
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

    protected String generateScriptAction(Element element, String contextName) {
        String location = getAttr(element, "location");
        String lang = getAttr(element, "lang", "groovy");

        // Check for inline script (CDATA)
        String code = element.getTextContent();
        if (isNotEmpty(code) && code.trim().length() > 0) {
            scriptCounter++;
            location = extractScript(contextName, scriptCounter, lang, code.trim());
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

    protected String generateEntityConditionAction(Element element) {
        String entityName = getAttr(element, "entity-name");
        String list = getAttr(element, "list");
        String useCache = getAttr(element, "use-cache");

        List<String> attrs = new ArrayList<>();
        if (isNotEmpty(entityName)) attrs.add(attrIfNotEmpty("entityName", entityName));
        if (isNotEmpty(list)) attrs.add(attrIfNotEmpty("list", list));
        if ("true".equalsIgnoreCase(useCache)) attrs.add("useCache = true");

        return "@EntityConditionAction(" + joinAttrs(attrs.toArray(new String[0])) + ")";
    }

    protected String generateServiceCallAction(Element element) {
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
     * Generates @TreeEntityOne annotation.
     */
    protected String generateTreeEntityOne(Element element) {
        String entityName = getAttr(element, "entity-name");
        String valueField = getAttr(element, "value-field");
        String useCache = getAttr(element, "use-cache");

        List<String> attrs = new ArrayList<>();
        if (isNotEmpty(entityName)) attrs.add(attrIfNotEmpty("entityName", entityName));
        if (isNotEmpty(valueField)) attrs.add(attrIfNotEmpty("valueField", valueField));
        if ("true".equalsIgnoreCase(useCache)) attrs.add("useCache = true");

        // Process field-map children
        List<String> fieldMaps = generateFieldMaps(element);
        if (!fieldMaps.isEmpty()) {
            attrs.add("fieldMaps = {" + String.join(", ", fieldMaps) + "}");
        }

        return "@TreeEntityOne(" + joinAttrs(attrs.toArray(new String[0])) + ")";
    }

    /**
     * Generates @TreeService annotation.
     */
    protected String generateTreeService(Element element) {
        String serviceName = getAttr(element, "service-name");
        String resultMapName = getAttr(element, "result-map-name");
        String resultMapList = getAttr(element, "result-map-list");

        List<String> attrs = new ArrayList<>();
        if (isNotEmpty(serviceName)) attrs.add(attrIfNotEmpty("serviceName", serviceName));
        if (isNotEmpty(resultMapName)) attrs.add(attrIfNotEmpty("resultMapName", resultMapName));
        if (isNotEmpty(resultMapList)) attrs.add(attrIfNotEmpty("resultMapList", resultMapList));

        // Process field-map children
        List<String> fieldMaps = generateFieldMaps(element);
        if (!fieldMaps.isEmpty()) {
            attrs.add("fieldMaps = {" + String.join(", ", fieldMaps) + "}");
        }

        return "@TreeService(" + joinAttrs(attrs.toArray(new String[0])) + ")";
    }

    /**
     * Generates field-map annotations.
     */
    protected List<String> generateFieldMaps(Element parentElement) {
        List<String> fieldMaps = new ArrayList<>();
        for (Element fm : childElementList(parentElement, "field-map")) {
            String fieldName = getAttr(fm, "field-name");
            String fromField = getAttr(fm, "from-field");
            String value = getAttr(fm, "value");
            List<String> fmAttrs = new ArrayList<>();
            if (isNotEmpty(fieldName)) fmAttrs.add(attrIfNotEmpty("fieldName", fieldName));
            if (isNotEmpty(fromField)) fmAttrs.add(attrIfNotEmpty("fromField", fromField));
            if (isNotEmpty(value)) fmAttrs.add(attrIfNotEmpty("value", value));
            fieldMaps.add("@FieldMap(" + joinAttrs(fmAttrs.toArray(new String[0])) + ")");
        }
        return fieldMaps;
    }

    /**
     * Generates @TreeIncludeScreen annotation.
     */
    protected String generateTreeIncludeScreen(Element element) {
        String name = getAttr(element, "name");
        String location = getAttr(element, "location");

        List<String> attrs = new ArrayList<>();
        if (isNotEmpty(name)) attrs.add(attrIfNotEmpty("name", name));
        if (isNotEmpty(location)) attrs.add(attrIfNotEmpty("location", location));

        return "@TreeIncludeScreen(" + joinAttrs(attrs.toArray(new String[0])) + ")";
    }

    /**
     * Generates @TreeLabel annotation.
     */
    protected String generateTreeLabel(Element element) {
        String text = getAttr(element, "text");
        String id = getAttr(element, "id");
        String style = getAttr(element, "style");

        // Check for text content if attribute is empty
        if (isEmpty(text)) {
            text = element.getTextContent();
        }

        List<String> attrs = new ArrayList<>();
        if (isNotEmpty(text)) attrs.add(attrIfNotEmpty("text", text));
        if (isNotEmpty(id)) attrs.add(attrIfNotEmpty("id", id));
        if (isNotEmpty(style)) attrs.add(attrIfNotEmpty("style", style));

        if (attrs.isEmpty()) {
            return "@TreeLabel";
        }
        return "@TreeLabel(" + joinAttrs(attrs.toArray(new String[0])) + ")";
    }

    /**
     * Generates @TreeLink annotation.
     */
    protected String generateTreeLink(Element element) {
        String target = getAttr(element, "target");
        String targetType = getAttr(element, "target-type");
        String targetWindow = getAttr(element, "target-window");
        String text = getAttr(element, "text");
        String id = getAttr(element, "id");
        String style = getAttr(element, "style");
        String name = getAttr(element, "name");
        String title = getAttr(element, "title");
        String urlMode = getAttr(element, "url-mode");
        String prefix = getAttr(element, "prefix");
        String fullPath = getAttr(element, "full-path");
        String secure = getAttr(element, "secure");
        String encode = getAttr(element, "encode");

        List<String> attrs = new ArrayList<>();
        if (isNotEmpty(target)) attrs.add(attrIfNotEmpty("target", target));
        if (isNotEmpty(targetType) && !"intra-app".equals(targetType)) attrs.add(attrIfNotEmpty("targetType", targetType));
        if (isNotEmpty(targetWindow)) attrs.add(attrIfNotEmpty("targetWindow", targetWindow));
        if (isNotEmpty(text)) attrs.add(attrIfNotEmpty("text", text));
        if (isNotEmpty(id)) attrs.add(attrIfNotEmpty("id", id));
        if (isNotEmpty(style)) attrs.add(attrIfNotEmpty("style", style));
        if (isNotEmpty(name)) attrs.add(attrIfNotEmpty("name", name));
        if (isNotEmpty(title)) attrs.add(attrIfNotEmpty("title", title));
        if (isNotEmpty(urlMode) && !"intra-app".equals(urlMode)) {
            String enumValue = urlMode.replace("-", "_").toUpperCase();
            attrs.add("urlMode = UrlMode." + enumValue);
        }
        if (isNotEmpty(prefix)) attrs.add(attrIfNotEmpty("prefix", prefix));
        if (isNotEmpty(fullPath)) attrs.add(attrIfNotEmpty("fullPath", fullPath));
        if (isNotEmpty(secure)) attrs.add(attrIfNotEmpty("secure", secure));
        if (isNotEmpty(encode)) attrs.add(attrIfNotEmpty("encode", encode));

        // Process parameters
        List<String> params = generateLinkParameters(element);
        if (!params.isEmpty()) {
            attrs.add("parameters = {" + String.join(", ", params) + "}");
        }

        // Process image
        Element imageElement = firstChildElement(element, "image");
        if (imageElement != null) {
            String imageCode = generateTreeImage(imageElement);
            if (isNotEmpty(imageCode)) {
                attrs.add("image = " + imageCode);
            }
        }

        if (attrs.isEmpty()) {
            return "@TreeLink";
        }
        return "@TreeLink(" + joinAttrs(attrs.toArray(new String[0])) + ")";
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
            if (isNotEmpty(paramName)) paramAttrs.add(attrIfNotEmpty("paramName", paramName));
            if (isNotEmpty(value)) paramAttrs.add(attrIfNotEmpty("value", value));
            if (isNotEmpty(fromField)) paramAttrs.add(attrIfNotEmpty("fromField", fromField));
            params.add("@TreeParameter(" + joinAttrs(paramAttrs.toArray(new String[0])) + ")");
        }
        return params;
    }

    /**
     * Generates @TreeImage annotation.
     */
    protected String generateTreeImage(Element element) {
        String src = getAttr(element, "src");
        String id = getAttr(element, "id");
        String style = getAttr(element, "style");
        String title = getAttr(element, "title");
        String width = getAttr(element, "width");
        String height = getAttr(element, "height");
        String urlMode = getAttr(element, "url-mode");
        String border = getAttr(element, "border");

        List<String> attrs = new ArrayList<>();
        if (isNotEmpty(src)) attrs.add(attrIfNotEmpty("src", src));
        if (isNotEmpty(id)) attrs.add(attrIfNotEmpty("id", id));
        if (isNotEmpty(style)) attrs.add(attrIfNotEmpty("style", style));
        if (isNotEmpty(title)) attrs.add(attrIfNotEmpty("title", title));
        if (isNotEmpty(width)) attrs.add(attrIfNotEmpty("width", width));
        if (isNotEmpty(height)) attrs.add(attrIfNotEmpty("height", height));
        if (isNotEmpty(urlMode) && !"intra-app".equals(urlMode)) {
            String enumValue = urlMode.replace("-", "_").toUpperCase();
            attrs.add("urlMode = UrlMode." + enumValue);
        }
        if (isNotEmpty(border)) attrs.add(attrIfNotEmpty("border", border));

        if (attrs.isEmpty()) {
            return "@TreeImage";
        }
        return "@TreeImage(" + joinAttrs(attrs.toArray(new String[0])) + ")";
    }

    /**
     * Generates @SubNode annotation.
     *
     * <p>SCIPIO: 4.0.0: sub-node's data source is a required choice of actions | entity-and | service |
     * entity-condition (widget-tree.xsd), followed by optional out-field-map children. All four data source
     * kinds and out-field-map are now handled - previously only actions was converted and the other three
     * were silently dropped.</p>
     */
    protected String generateSubNode(Element subNodeElement, String treeName) {
        String nodeName = getAttr(subNodeElement, "node-name");

        List<String> attrs = new ArrayList<>();
        if (isNotEmpty(nodeName)) attrs.add(attrIfNotEmpty("nodeName", nodeName));

        // Process actions
        Element actionsElement = firstChildElement(subNodeElement, "actions");
        if (actionsElement != null) {
            String actionsCode = generateTreeActions(actionsElement, treeName + "_subnode");
            if (isNotEmpty(actionsCode)) {
                attrs.add("actions = " + actionsCode);
            }
        }

        // Process entity-and
        Element entityAndElement = firstChildElement(subNodeElement, "entity-and");
        if (entityAndElement != null) {
            String entityAndCode = generateSubNodeEntityAnd(entityAndElement);
            if (isNotEmpty(entityAndCode)) {
                attrs.add("entityAnd = " + entityAndCode);
            }
        }

        // Process service
        Element serviceElement = firstChildElement(subNodeElement, "service");
        if (serviceElement != null) {
            String serviceCode = generateTreeService(serviceElement);
            if (isNotEmpty(serviceCode)) {
                attrs.add("service = " + serviceCode);
            }
        }

        // Process entity-condition
        Element entityConditionElement = firstChildElement(subNodeElement, "entity-condition");
        if (entityConditionElement != null) {
            String entityConditionCode = generateSubNodeEntityCondition(entityConditionElement);
            if (isNotEmpty(entityConditionCode)) {
                attrs.add("entityCondition = " + entityConditionCode);
            }
        }

        // Process out-field-map children
        List<Element> outFieldMapElements = childElementList(subNodeElement, "out-field-map");
        if (!outFieldMapElements.isEmpty()) {
            List<String> outFieldMaps = new ArrayList<>();
            for (Element outFieldMapElement : outFieldMapElements) {
                outFieldMaps.add(generateOutFieldMap(outFieldMapElement));
            }
            attrs.add("outFieldMaps = {" + String.join(", ", outFieldMaps) + "}");
        }

        return "@SubNode(" + joinAttrs(attrs.toArray(new String[0])) + ")";
    }

    /**
     * Generates an @OutFieldMap annotation from an out-field-map element.
     */
    protected String generateOutFieldMap(Element element) {
        String fieldName = getAttr(element, "field-name");
        String toFieldName = getAttr(element, "to-field-name");

        List<String> attrs = new ArrayList<>();
        if (isNotEmpty(fieldName)) attrs.add(attrIfNotEmpty("fieldName", fieldName));
        if (isNotEmpty(toFieldName)) attrs.add(attrIfNotEmpty("toFieldName", toFieldName));

        return "@OutFieldMap(" + joinAttrs(attrs.toArray(new String[0])) + ")";
    }

    /**
     * Generates an @EntityAnd annotation from a sub-node's entity-and element.
     */
    protected String generateSubNodeEntityAnd(Element element) {
        String entityName = getAttr(element, "entity-name");
        String list = getAttr(element, "list");
        String useCache = getAttr(element, "use-cache");
        String filterByDate = getAttr(element, "filter-by-date");
        String resultSetType = getAttr(element, "result-set-type");

        List<String> attrs = new ArrayList<>();
        if (isNotEmpty(entityName)) attrs.add(attrIfNotEmpty("entityName", entityName));
        if (isNotEmpty(list)) attrs.add(attrIfNotEmpty("list", list));
        if ("true".equalsIgnoreCase(useCache)) attrs.add("useCache = true");
        if (isNotEmpty(filterByDate) && !"false".equals(filterByDate)) attrs.add(attrIfNotEmpty("filterByDate", filterByDate));
        if (isNotEmpty(resultSetType) && !"scroll".equals(resultSetType)) attrs.add(attrIfNotEmpty("resultSetType", resultSetType));

        // field-map children (SCIPIO: 4.0.0: previously silently dropped)
        List<String> fieldMaps = generateFieldMaps(element);
        if (!fieldMaps.isEmpty()) {
            attrs.add("fieldMaps = {" + String.join(", ", fieldMaps) + "}");
        }

        // select-field children
        List<String> selectFields = generateFieldNameList(element, "select-field");
        if (!selectFields.isEmpty()) {
            attrs.add("selectFields = {" + String.join(", ", selectFields) + "}");
        }

        // order-by children (SCIPIO: 4.0.0: previously silently dropped)
        List<String> orderBy = generateFieldNameList(element, "order-by");
        if (!orderBy.isEmpty()) {
            attrs.add("orderBy = {" + String.join(", ", orderBy) + "}");
        }

        return "@EntityAnd(" + joinAttrs(attrs.toArray(new String[0])) + ")";
    }

    /**
     * Generates an @EntityCondition annotation from a sub-node's entity-condition element.
     *
     * <p>SCIPIO: 4.0.0: The XML has a required top-level (condition-expr | condition-list) choice. A bare
     * condition-expr (no wrapping list) becomes a single-element {@code conditions()} array. A condition-list
     * becomes {@code combine()}/{@code conditions()} (direct condition-expr children) plus
     * {@code conditionLists()} (direct nested condition-list children, e.g. an "or" group inside an "and"
     * group). {@link TreeConditionList} only supports one further level of condition-expr children (matches
     * observed real data); a condition-list nested deeper than that emits a TODO marker and a converter
     * warning instead of silently dropping the extra conditions.</p>
     */
    protected String generateSubNodeEntityCondition(Element element) {
        String entityName = getAttr(element, "entity-name");
        String list = getAttr(element, "list");
        String useCache = getAttr(element, "use-cache");
        String filterByDate = getAttr(element, "filter-by-date");
        String distinct = getAttr(element, "distinct");
        String delegatorName = getAttr(element, "delegator-name");
        String resultSetType = getAttr(element, "result-set-type");

        List<String> attrs = new ArrayList<>();
        if (isNotEmpty(entityName)) attrs.add(attrIfNotEmpty("entityName", entityName));
        if (isNotEmpty(list)) attrs.add(attrIfNotEmpty("list", list));
        if ("true".equalsIgnoreCase(useCache)) attrs.add("useCache = true");
        if (isNotEmpty(filterByDate) && !"false".equals(filterByDate)) attrs.add(attrIfNotEmpty("filterByDate", filterByDate));
        if ("true".equalsIgnoreCase(distinct)) attrs.add("distinct = true");
        if (isNotEmpty(delegatorName)) attrs.add(attrIfNotEmpty("delegatorName", delegatorName));
        if (isNotEmpty(resultSetType) && !"scroll".equals(resultSetType)) attrs.add(attrIfNotEmpty("resultSetType", resultSetType));

        // condition-expr | condition-list (required choice per widget-tree.xsd)
        Element conditionListElement = firstChildElement(element, "condition-list");
        Element conditionExprElement = firstChildElement(element, "condition-expr");
        if (conditionListElement != null) {
            String combine = getAttr(conditionListElement, "combine", "and");
            if (isNotEmpty(combine) && !"and".equals(combine)) {
                attrs.add(attrIfNotEmpty("combine", combine));
            }
            List<String> conditions = new ArrayList<>();
            List<String> conditionLists = new ArrayList<>();
            for (Element child : childElementList(conditionListElement)) {
                String tagName = child.getNodeName();
                if ("condition-expr".equals(tagName)) {
                    conditions.add(generateTreeConditionExpr(child));
                } else if ("condition-list".equals(tagName)) {
                    conditionLists.add(generateSubNodeConditionList(child));
                } else {
                    System.err.println("WARN: Tree entity-condition condition-list child '" + tagName
                            + "' not supported in annotations - skipped");
                }
            }
            if (!conditions.isEmpty()) {
                attrs.add("conditions = {" + String.join(", ", conditions) + "}");
            }
            if (!conditionLists.isEmpty()) {
                attrs.add("conditionLists = {" + String.join(", ", conditionLists) + "}");
            }
        } else if (conditionExprElement != null) {
            // Bare condition-expr with no wrapping condition-list
            attrs.add("conditions = {" + generateTreeConditionExpr(conditionExprElement) + "}");
        }

        // select-field children
        List<String> selectFields = generateFieldNameList(element, "select-field");
        if (!selectFields.isEmpty()) {
            attrs.add("selectFields = {" + String.join(", ", selectFields) + "}");
        }

        // order-by children
        List<String> orderBy = generateFieldNameList(element, "order-by");
        if (!orderBy.isEmpty()) {
            attrs.add("orderBy = {" + String.join(", ", orderBy) + "}");
        }

        return "@EntityCondition(" + joinAttrs(attrs.toArray(new String[0])) + ")";
    }

    /**
     * Generates a nested @TreeConditionList from a condition-list element that is itself a direct child of
     * the top-level condition-list. Only condition-expr children are supported at this depth; a further
     * nested condition-list here would exceed the 2-level nesting {@link TreeConditionList} can express.
     */
    protected String generateSubNodeConditionList(Element conditionListElement) {
        String combine = getAttr(conditionListElement, "combine", "and");

        List<String> attrs = new ArrayList<>();
        if (isNotEmpty(combine) && !"and".equals(combine)) {
            attrs.add(attrIfNotEmpty("combine", combine));
        }

        List<String> conditions = new ArrayList<>();
        boolean deeperNestingFound = false;
        for (Element child : childElementList(conditionListElement)) {
            String tagName = child.getNodeName();
            if ("condition-expr".equals(tagName)) {
                conditions.add(generateTreeConditionExpr(child));
            } else if ("condition-list".equals(tagName)) {
                deeperNestingFound = true;
            } else {
                System.err.println("WARN: Tree condition-list child '" + tagName + "' not supported in annotations - skipped");
            }
        }
        if (!conditions.isEmpty()) {
            attrs.add("conditions = {" + String.join(", ", conditions) + "}");
        }

        String result = "@TreeConditionList(" + joinAttrs(attrs.toArray(new String[0])) + ")";
        if (deeperNestingFound) {
            // SCIPIO: 4.0.0: TreeConditionList only supports one level of condition-expr children; a
            // condition-list nested deeper than 2 levels total is not representable in annotations and
            // requires manual conversion. Do not drop silently: warn and mark in the generated code.
            System.err.println("WARN: Tree condition-list nests deeper than 2 levels - deeper nesting cannot be "
                    + "represented in @TreeConditionList and was omitted; manual review required");
            result = "/* TODO: condition-list nests deeper than 2 levels - manual conversion required */ " + result;
        }
        return result;
    }

    /**
     * Generates a @TreeConditionExpr annotation from a condition-expr element.
     */
    protected String generateTreeConditionExpr(Element element) {
        String fieldName = getAttr(element, "field-name");
        String operator = getAttr(element, "operator");
        String value = getAttr(element, "value");
        String fromField = getAttr(element, "from-field");
        String ignoreIfNull = getAttr(element, "ignore-if-null");
        String ignoreIfEmpty = getAttr(element, "ignore-if-empty");
        String ignoreCase = getAttr(element, "ignore-case");

        List<String> attrs = new ArrayList<>();
        if (isNotEmpty(fieldName)) attrs.add(attrIfNotEmpty("fieldName", fieldName));
        if (isNotEmpty(operator) && !"equals".equals(operator)) attrs.add(attrIfNotEmpty("operator", operator));
        if (isNotEmpty(value)) attrs.add(attrIfNotEmpty("value", value));
        if (isNotEmpty(fromField)) attrs.add(attrIfNotEmpty("fromField", fromField));
        if ("true".equalsIgnoreCase(ignoreIfNull)) attrs.add("ignoreIfNull = true");
        if ("true".equalsIgnoreCase(ignoreIfEmpty)) attrs.add("ignoreIfEmpty = true");
        if ("true".equalsIgnoreCase(ignoreCase)) attrs.add("ignoreCase = true");

        return "@TreeConditionExpr(" + joinAttrs(attrs.toArray(new String[0])) + ")";
    }

    /**
     * Generates a list of quoted field-name string literals from child elements with the given tag name
     * (used for select-field and order-by, both of which are simple field-name-only elements).
     */
    protected List<String> generateFieldNameList(Element parentElement, String tagName) {
        List<String> result = new ArrayList<>();
        for (Element child : childElementList(parentElement, tagName)) {
            String fieldName = getAttr(child, "field-name");
            if (isNotEmpty(fieldName)) {
                result.add(toStringValue(fieldName));
            }
        }
        return result;
    }

    /**
     * Converts a tree name to a valid Java interface name.
     * SCIPIO: 4.0.0: If the tree name matches the outer class name, append underscore to avoid duplicate definition.
     */
    protected String toInterfaceName(String treeName) {
        String result = treeName.replaceAll("[^a-zA-Z0-9_]", "_");
        if (result.length() > 0 && Character.isDigit(result.charAt(0))) {
            result = "_" + result;
        }
        // Avoid duplicate definition when tree name matches outer class name
        if (result.equals(className)) {
            result = result + "_";
        }
        return result;
    }
}
