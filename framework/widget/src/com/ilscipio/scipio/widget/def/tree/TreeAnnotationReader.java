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
package com.ilscipio.scipio.widget.def.tree;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilValidate;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

import com.ilscipio.scipio.widget.def.screen.EntityOneAction;
import com.ilscipio.scipio.widget.def.screen.FieldMap;
import com.ilscipio.scipio.widget.def.screen.ScriptAction;
import com.ilscipio.scipio.widget.def.screen.ServiceAction;
import com.ilscipio.scipio.widget.def.screen.SetAction;

/**
 * Reads tree annotations and converts them to XML DOM for ModelTree construction.
 *
 * <p>This reader follows the synthetic XML generation pattern used by FormAnnotationReader
 * and MenuAnnotationReader, where annotations are converted to DOM elements that the
 * existing ModelTree constructor can process.</p>
 *
 * <p>SCIPIO: 4.0.0: Added for tree annotations support.</p>
 */
public class TreeAnnotationReader {

    private static final String MODULE = TreeAnnotationReader.class.getName();

    // SCIPIO: 4.0.0: Reused per-thread DocumentBuilder. Previously buildTreeDocument() called
    // DocumentBuilderFactory.newInstance().newDocumentBuilder() PER CALL, and each instantiation
    // re-scans the Xerces classpath resources. Mirrors the identical fix in FormAnnotationReader.
    private static final ThreadLocal<DocumentBuilder> THREAD_LOCAL_DOCUMENT_BUILDER = ThreadLocal.withInitial(() -> {
        try {
            return DocumentBuilderFactory.newInstance().newDocumentBuilder();
        } catch (ParserConfigurationException e) {
            throw new IllegalStateException("Error creating shared DocumentBuilder for tree annotation reading", e);
        }
    });

    /**
     * Reads a Tree annotation and creates a synthetic XML document.
     *
     * @param treeClass The class containing the @Tree annotation
     * @param treeName The name of the tree to read
     * @return A DOM Document representing the tree definition
     */
    public Document readTreeDocument(Class<?> treeClass, String treeName) {
        Tree treeDef = findTreeAnnotation(treeClass, treeName);
        if (treeDef == null) {
            Debug.logError("Tree annotation not found: " + treeName + " in class " + treeClass.getName(), MODULE);
            return null;
        }
        return buildTreeDocument(treeDef);
    }

    /**
     * Finds a Tree annotation by name in the given class.
     */
    protected Tree findTreeAnnotation(Class<?> treeClass, String treeName) {
        // Check for single @Tree annotation
        Tree singleTree = treeClass.getAnnotation(Tree.class);
        if (singleTree != null && treeName.equals(singleTree.name())) {
            return singleTree;
        }

        // Check for @TreeList (multiple trees)
        TreeList treeList = treeClass.getAnnotation(TreeList.class);
        if (treeList != null) {
            for (Tree tree : treeList.value()) {
                if (treeName.equals(tree.name())) {
                    return tree;
                }
            }
        }

        // Check inner interfaces/classes
        for (Class<?> innerClass : treeClass.getDeclaredClasses()) {
            singleTree = innerClass.getAnnotation(Tree.class);
            if (singleTree != null && treeName.equals(singleTree.name())) {
                return singleTree;
            }
        }

        return null;
    }

    /**
     * Builds a DOM Document from a Tree annotation.
     */
    protected Document buildTreeDocument(Tree treeDef) {
        try {
            // SCIPIO: 4.0.0: Reuse the per-thread DocumentBuilder instead of creating a new
            // DocumentBuilderFactory/DocumentBuilder for every single call (see field javadoc above).
            DocumentBuilder builder = THREAD_LOCAL_DOCUMENT_BUILDER.get();
            Document doc = builder.newDocument();

            // Root element: <trees>
            Element treesElement = doc.createElement("trees");
            doc.appendChild(treesElement);

            // <tree> element
            Element treeElement = doc.createElement("tree");
            treeElement.setAttribute("name", treeDef.name());
            treeElement.setAttribute("root-node-name", treeDef.rootNodeName());

            // Render style
            if (treeDef.defaultRenderStyle() != RenderStyle.SIMPLE) {
                treeElement.setAttribute("default-render-style", treeDef.defaultRenderStyle().getXmlValue());
            }

            setAttrIfNotEmpty(treeElement, "default-wrap-style", treeDef.defaultWrapStyle());
            setAttrIfNotEmpty(treeElement, "expand-collapse-request", treeDef.expandCollapseRequest());
            setAttrIfNotEmpty(treeElement, "trail-name", treeDef.trailName());

            if (!"0".equals(treeDef.openDepth())) {
                treeElement.setAttribute("open-depth", treeDef.openDepth());
            }
            if (!"0".equals(treeDef.postTrailOpenDepth())) {
                treeElement.setAttribute("post-trail-open-depth", treeDef.postTrailOpenDepth());
            }

            setAttrIfNotEmpty(treeElement, "entity-name", treeDef.entityName());

            if (!treeDef.forceChildCheck()) {
                treeElement.setAttribute("force-child-check", "false");
            }

            // Add nodes
            for (TreeNode nodeDef : treeDef.nodes()) {
                addNodeElement(doc, treeElement, nodeDef);
            }

            treesElement.appendChild(treeElement);
            return doc;

        } catch (RuntimeException e) { // SCIPIO: 4.0.0: was catch (ParserConfigurationException) - that
            // checked exception can no longer occur here now that DocumentBuilder creation happens in
            // THREAD_LOCAL_DOCUMENT_BUILDER's initializer (which wraps it as IllegalStateException)
            Debug.logError(e, "Error creating tree document", MODULE);
            return null;
        }
    }

    /**
     * Adds a node element to the tree.
     */
    protected void addNodeElement(Document doc, Element parentElement, TreeNode nodeDef) {
        Element nodeElement = doc.createElement("node");
        nodeElement.setAttribute("name", nodeDef.name());

        setAttrIfNotEmpty(nodeElement, "wrap-style", nodeDef.wrapStyle());

        if (!nodeDef.useDefaultRenderStyle()) {
            nodeElement.setAttribute("render-style", nodeDef.renderStyle().getXmlValue());
        }

        setAttrIfNotEmpty(nodeElement, "entry-name", nodeDef.entryName());
        setAttrIfNotEmpty(nodeElement, "entity-name", nodeDef.entityName());
        setAttrIfNotEmpty(nodeElement, "join-field-name", nodeDef.joinFieldName());

        // Condition
        if (!nodeDef.condition().UNSET()) {
            addConditionElement(doc, nodeElement, nodeDef.condition());
        }

        // Actions (mutually exclusive with entity-one and service)
        if (!nodeDef.actions().UNSET()) {
            Element actionsElement = doc.createElement("actions");
            addActionsContent(doc, actionsElement, nodeDef.actions());
            if (actionsElement.hasChildNodes()) {
                nodeElement.appendChild(actionsElement);
            }
        } else if (!nodeDef.entityOne().UNSET()) {
            addEntityOneElement(doc, nodeElement, nodeDef.entityOne());
        } else if (!nodeDef.service().UNSET()) {
            addServiceElement(doc, nodeElement, nodeDef.service());
        }

        // Content (mutually exclusive: include-screen, label, or link)
        if (!nodeDef.includeScreen().UNSET()) {
            addIncludeScreenElement(doc, nodeElement, nodeDef.includeScreen());
        } else if (!nodeDef.label().UNSET()) {
            addLabelElement(doc, nodeElement, nodeDef.label());
        } else if (!nodeDef.link().UNSET()) {
            addLinkElement(doc, nodeElement, nodeDef.link());
        }

        // Sub-nodes
        for (SubNode subNodeDef : nodeDef.subNodes()) {
            addSubNodeElement(doc, nodeElement, subNodeDef);
        }

        parentElement.appendChild(nodeElement);
    }

    /**
     * Adds a condition element.
     */
    protected void addConditionElement(Document doc, Element parentElement, TreeNodeCondition condition) {
        Element conditionElement = doc.createElement("condition");
        boolean hasCondition = false;

        // Check for functional conditions first (takes precedence)
        com.ilscipio.scipio.widget.def.condition.Condition[] funcConditions = condition.conditions();
        if (funcConditions != null && funcConditions.length > 0) {
            if (funcConditions.length == 1) {
                Element condElem = buildFunctionalConditionElement(doc, funcConditions[0]);
                if (condElem != null) {
                    conditionElement.appendChild(condElem);
                    hasCondition = true;
                }
            } else {
                // Multiple conditions - wrap in <and>
                Element andElement = doc.createElement("and");
                for (com.ilscipio.scipio.widget.def.condition.Condition funcCond : funcConditions) {
                    Element condElem = buildFunctionalConditionElement(doc, funcCond);
                    if (condElem != null) {
                        andElement.appendChild(condElem);
                    }
                }
                if (andElement.hasChildNodes()) {
                    conditionElement.appendChild(andElement);
                    hasCondition = true;
                }
            }
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

        if (conditionElement.hasChildNodes()) {
            parentElement.appendChild(conditionElement);
        }
    }

    /**
     * Builds an XML element from a functional condition annotation.
     */
    protected Element buildFunctionalConditionElement(Document doc, com.ilscipio.scipio.widget.def.condition.Condition funcCond) {
        String typeName = funcCond.type().getSimpleName();
        // SCIPIO: 4.0.0: composite conditions were unsupported here, so or/xor/not silently became always-true
        if (isCompositeConditionType(typeName) && funcCond.tree().length > 0) {
            return buildConditionTreeElement(doc, typeName, funcCond.tree());
        }
        switch (typeName) {
            case "Always":
                return null;
            case "And":
            case "Or":
            case "Xor": {
                Element compositeElem = doc.createElement(typeName.toLowerCase());
                for (com.ilscipio.scipio.widget.def.condition.NestedCondition nested : funcCond.nested()) {
                    Element child = buildNestedConditionElement(doc, nested);
                    if (child != null) {
                        compositeElem.appendChild(child);
                    }
                }
                return compositeElem.hasChildNodes() ? compositeElem : null;
            }
            case "Not": {
                com.ilscipio.scipio.widget.def.condition.NestedCondition[] nestedConds = funcCond.nested();
                if (nestedConds.length == 1) {
                    Element child = buildNestedConditionElement(doc, nestedConds[0]);
                    if (child == null) {
                        return null;
                    }
                    if ("not".equals(child.getNodeName())) {
                        return child;
                    }
                    Element notElem = doc.createElement("not");
                    notElem.appendChild(child);
                    return notElem;
                }
                return null;
            }
        }
        return buildSimpleConditionElement(doc, typeName, funcCond.params());
    }

    /**
     * SCIPIO: 4.0.0: Builds a composite condition element from the flat Condition.tree() form,
     * which carries any depth (the NestedCondition chain stopped at a fixed one).
     */
    protected Element buildConditionTreeElement(Document doc, String typeName,
            com.ilscipio.scipio.widget.def.condition.ConditionNode[] nodes) {
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
    protected void appendConditionTreeChildren(Document doc, Element parent,
            com.ilscipio.scipio.widget.def.condition.ConditionNode[] nodes, int parentIndex) {
        for (int i = 0; i < nodes.length; i++) {
            com.ilscipio.scipio.widget.def.condition.ConditionNode node = nodes[i];
            if (node.parent() != parentIndex) {
                continue;
            }
            String nodeType = node.type().getSimpleName();
            Element elem;
            if (isCompositeConditionType(nodeType)) {
                elem = doc.createElement(nodeType.toLowerCase());
                appendConditionTreeChildren(doc, elem, nodes, i);
                if (!elem.hasChildNodes()) {
                    continue;
                }
            } else {
                elem = buildSimpleConditionElement(doc, nodeType, node.params());
                if (elem == null) {
                    continue;
                }
            }
            if (node.not()) {
                Element notElem = doc.createElement("not");
                notElem.appendChild(elem);
                elem = notElem;
            }
            parent.appendChild(elem);
        }
    }

    /** SCIPIO: 4.0.0: True for the condition types that hold members rather than parameters. */
    protected boolean isCompositeConditionType(String typeName) {
        return "And".equals(typeName) || "Or".equals(typeName)
                || "Xor".equals(typeName) || "Not".equals(typeName);
    }

    /**
     * SCIPIO: 4.0.0: Builds the XML element of a nested (one level deep) condition, negated when requested.
     */
    protected Element buildNestedConditionElement(Document doc, com.ilscipio.scipio.widget.def.condition.NestedCondition nested) {
        Element elem = buildCompositeOrSimpleElement(doc, nested.type().getSimpleName(), nested.params(), nested.nested());
        if (elem == null || !nested.not()) {
            return elem;
        }
        Element notElem = doc.createElement("not");
        notElem.appendChild(elem);
        return notElem;
    }

    /**
     * SCIPIO: 4.0.0: Builds a condition element that may itself be a composite of second-level conditions.
     */
    protected Element buildCompositeOrSimpleElement(Document doc, String typeName, String[] params,
            com.ilscipio.scipio.widget.def.condition.NestedCondition2[] members) {
        switch (typeName) {
            case "Always":
                return null;
            case "And":
            case "Or":
            case "Xor": {
                Element compositeElem = doc.createElement(typeName.toLowerCase());
                for (com.ilscipio.scipio.widget.def.condition.NestedCondition2 member : members) {
                    Element child = buildNested2ConditionElement(doc, member);
                    if (child != null) {
                        compositeElem.appendChild(child);
                    }
                }
                return compositeElem.hasChildNodes() ? compositeElem : null;
            }
            case "Not": {
                if (members.length == 1) {
                    Element child = buildNested2ConditionElement(doc, members[0]);
                    if (child == null) {
                        return null;
                    }
                    if ("not".equals(child.getNodeName())) {
                        return child;
                    }
                    Element notElem = doc.createElement("not");
                    notElem.appendChild(child);
                    return notElem;
                }
                return null;
            }
        }
        return buildSimpleConditionElement(doc, typeName, params);
    }

    /**
     * SCIPIO: 4.0.0: Builds the XML element of a second-level nested condition, negated when requested.
     */
    protected Element buildNested2ConditionElement(Document doc,
            com.ilscipio.scipio.widget.def.condition.NestedCondition2 nested) {
        Element elem = buildSimpleConditionElement(doc, nested.type().getSimpleName(), nested.params());
        if (elem == null || !nested.not()) {
            return elem;
        }
        Element notElem = doc.createElement("not");
        notElem.appendChild(elem);
        return notElem;
    }

    /**
     * SCIPIO: 4.0.0: Builds the XML element of a non-composite condition type.
     */
    protected Element buildSimpleConditionElement(Document doc, String typeName, String[] params) {
        switch (typeName) {
            case "Empty":
                if (params.length > 0) {
                    Element elem = doc.createElement("if-empty");
                    elem.setAttribute("field", params[0]);
                    return elem;
                }
                break;
            case "NotEmpty":
                if (params.length > 0) {
                    Element notElem = doc.createElement("not");
                    Element ifEmptyElem = doc.createElement("if-empty");
                    ifEmptyElem.setAttribute("field", params[0]);
                    notElem.appendChild(ifEmptyElem);
                    return notElem;
                }
                break;
            case "True":
                if (params.length > 0) {
                    Element elem = doc.createElement("if-true");
                    elem.setAttribute("field", params[0]);
                    return elem;
                }
                break;
            case "False":
                if (params.length > 0) {
                    // SCIPIO: 4.0.0: if-false, not a negated if-true: an unset field is neither, so not(if-false) holds
                    Element elem = doc.createElement("if-false");
                    elem.setAttribute("field", params[0]);
                    return elem;
                }
                break;
            case "HasPermission":
                Element permElem = doc.createElement("if-has-permission");
                if (params.length > 0) permElem.setAttribute("permission", params[0]);
                if (params.length > 1) permElem.setAttribute("action", params[1]);
                return permElem;
            case "ServicePermission":
                Element servPermElem = doc.createElement("if-service-permission");
                if (params.length > 0) servPermElem.setAttribute("service-name", params[0]);
                if (params.length > 1) servPermElem.setAttribute("main-action", params[1]);
                return servPermElem;
            case "Compare":
                Element compElem = doc.createElement("if-compare");
                if (params.length > 0) compElem.setAttribute("field", params[0]);
                if (params.length > 1) compElem.setAttribute("operator", params[1]);
                if (params.length > 2) compElem.setAttribute("value", params[2]);
                // SCIPIO: Always set type - default to String if not provided
                if (params.length > 3 && UtilValidate.isNotEmpty(params[3])) {
                    compElem.setAttribute("type", params[3]);
                } else {
                    compElem.setAttribute("type", "String");
                }
                return compElem;
        }
        return null;
    }

    /**
     * Adds actions content to an actions element.
     */
    protected void addActionsContent(Document doc, Element actionsElement, TreeActions actions) {
        // Set actions
        for (SetAction setAction : actions.set()) {
            addSetActionElement(doc, actionsElement, setAction);
        }

        // Entity-one actions
        for (EntityOneAction entityOne : actions.entityOne()) {
            addEntityOneActionElement(doc, actionsElement, entityOne);
        }

        // Service actions
        for (ServiceAction service : actions.service()) {
            addServiceActionElement(doc, actionsElement, service);
        }

        // Script actions
        for (ScriptAction script : actions.script()) {
            addScriptActionElement(doc, actionsElement, script);
        }
    }

    /**
     * Adds a set action element.
     */
    protected void addSetActionElement(Document doc, Element parentElement, SetAction action) {
        Element setElement = doc.createElement("set");
        setElement.setAttribute("field", action.field());
        setAttrIfNotEmpty(setElement, "from-field", action.fromField());
        setAttrIfNotEmpty(setElement, "value", action.value());
        setAttrIfNotEmpty(setElement, "default-value", action.defaultValue());
        setAttrIfNotEmpty(setElement, "type", action.type());
        if (action.global()) {
            setElement.setAttribute("global", "true");
        }
        parentElement.appendChild(setElement);
    }

    /**
     * Adds an entity-one action element (in actions).
     */
    protected void addEntityOneActionElement(Document doc, Element parentElement, EntityOneAction entityOne) {
        Element elem = doc.createElement("entity-one");
        elem.setAttribute("entity-name", entityOne.entityName());
        setAttrIfNotEmpty(elem, "value-field", entityOne.valueField());
        if (entityOne.useCache()) {
            elem.setAttribute("use-cache", "true");
        }
        if (!entityOne.autoFieldMap()) {
            elem.setAttribute("auto-field-map", "false");
        }

        for (FieldMap fm : entityOne.fieldMaps()) {
            Element fmElem = doc.createElement("field-map");
            fmElem.setAttribute("field-name", fm.fieldName());
            setAttrIfNotEmpty(fmElem, "from-field", fm.fromField());
            setAttrIfNotEmpty(fmElem, "value", fm.value());
            elem.appendChild(fmElem);
        }

        // Note: EntityOneAction (screen package) doesn't have selectFields() method

        parentElement.appendChild(elem);
    }

    /**
     * Adds a service action element (in actions).
     */
    protected void addServiceActionElement(Document doc, Element parentElement, ServiceAction service) {
        Element elem = doc.createElement("service");
        elem.setAttribute("service-name", service.serviceName());
        setAttrIfNotEmpty(elem, "result-map-name", service.resultMapName());
        if (!service.autoFieldMap()) {
            elem.setAttribute("auto-field-map", "false");
        }

        for (FieldMap fm : service.fieldMaps()) {
            Element fmElem = doc.createElement("field-map");
            fmElem.setAttribute("field-name", fm.fieldName());
            setAttrIfNotEmpty(fmElem, "from-field", fm.fromField());
            setAttrIfNotEmpty(fmElem, "value", fm.value());
            elem.appendChild(fmElem);
        }

        parentElement.appendChild(elem);
    }

    /**
     * Adds a script action element.
     */
    protected void addScriptActionElement(Document doc, Element parentElement, ScriptAction script) {
        Element elem = doc.createElement("script");
        setAttrIfNotEmpty(elem, "location", script.location());
        setAttrIfNotEmpty(elem, "lang", script.lang());
        if (UtilValidate.isNotEmpty(script.script())) {
            elem.setAttribute("script", script.script());
        }
        parentElement.appendChild(elem);
    }

    /**
     * Adds an entity-one element (node-level).
     */
    protected void addEntityOneElement(Document doc, Element parentElement, TreeEntityOne entityOne) {
        Element elem = doc.createElement("entity-one");
        elem.setAttribute("entity-name", entityOne.entityName());
        setAttrIfNotEmpty(elem, "value-field", entityOne.valueField());
        if (entityOne.useCache()) {
            elem.setAttribute("use-cache", "true");
        }
        if (!entityOne.autoFieldMap()) {
            elem.setAttribute("auto-field-map", "false");
        }

        for (FieldMap fm : entityOne.fieldMaps()) {
            Element fmElem = doc.createElement("field-map");
            fmElem.setAttribute("field-name", fm.fieldName());
            setAttrIfNotEmpty(fmElem, "from-field", fm.fromField());
            setAttrIfNotEmpty(fmElem, "value", fm.value());
            elem.appendChild(fmElem);
        }

        for (String selectField : entityOne.selectFields()) {
            Element sfElem = doc.createElement("select-field");
            sfElem.setAttribute("field-name", selectField);
            elem.appendChild(sfElem);
        }

        parentElement.appendChild(elem);
    }

    /**
     * Adds a service element (node-level).
     */
    protected void addServiceElement(Document doc, Element parentElement, TreeService service) {
        Element elem = doc.createElement("service");
        elem.setAttribute("service-name", service.serviceName());
        setAttrIfNotEmpty(elem, "result-map", service.resultMap());
        if (!"true".equals(service.autoFieldMap())) {
            elem.setAttribute("auto-field-map", service.autoFieldMap());
        }
        setAttrIfNotEmpty(elem, "result-map-list", service.resultMapList());
        setAttrIfNotEmpty(elem, "result-map-value", service.resultMapValue());
        setAttrIfNotEmpty(elem, "value", service.value());

        for (FieldMap fm : service.fieldMaps()) {
            Element fmElem = doc.createElement("field-map");
            fmElem.setAttribute("field-name", fm.fieldName());
            setAttrIfNotEmpty(fmElem, "from-field", fm.fromField());
            setAttrIfNotEmpty(fmElem, "value", fm.value());
            elem.appendChild(fmElem);
        }

        parentElement.appendChild(elem);
    }

    /**
     * Adds an include-screen element.
     */
    protected void addIncludeScreenElement(Document doc, Element parentElement, TreeIncludeScreen includeScreen) {
        Element elem = doc.createElement("include-screen");
        elem.setAttribute("name", includeScreen.name());
        elem.setAttribute("location", includeScreen.location());
        if (includeScreen.shareScope()) {
            elem.setAttribute("share-scope", "true");
        }
        parentElement.appendChild(elem);
    }

    /**
     * Adds a label element.
     */
    protected void addLabelElement(Document doc, Element parentElement, TreeLabel label) {
        Element elem = doc.createElement("label");
        setAttrIfNotEmpty(elem, "text", label.text());
        setAttrIfNotEmpty(elem, "id", label.id());
        setAttrIfNotEmpty(elem, "style", label.style());
        parentElement.appendChild(elem);
    }

    /**
     * Adds a link element.
     */
    protected void addLinkElement(Document doc, Element parentElement, TreeLink link) {
        Element elem = doc.createElement("link");
        setAttrIfNotEmpty(elem, "text", link.text());
        setAttrIfNotEmpty(elem, "id", link.id());
        setAttrIfNotEmpty(elem, "style", link.style());
        setAttrIfNotEmpty(elem, "name", link.name());
        setAttrIfNotEmpty(elem, "title", link.title());
        setAttrIfNotEmpty(elem, "target", link.target());
        setAttrIfNotEmpty(elem, "target-window", link.targetWindow());
        setAttrIfNotEmpty(elem, "prefix", link.prefix());

        if (!"auto".equals(link.linkType())) {
            elem.setAttribute("link-type", link.linkType());
        }
        if (link.urlMode() != com.ilscipio.scipio.widget.def.menu.UrlMode.INTRA_APP) {
            elem.setAttribute("url-mode", link.urlMode().getXmlValue());
        }

        setAttrIfNotEmpty(elem, "full-path", link.fullPath());
        setAttrIfNotEmpty(elem, "secure", link.secure());
        setAttrIfNotEmpty(elem, "encode", link.encode());

        if (link.requestConfirmation()) {
            elem.setAttribute("request-confirmation", "true");
        }
        setAttrIfNotEmpty(elem, "confirmation-message", link.confirmationMessage());
        setAttrIfNotEmpty(elem, "use-when", link.useWhen());

        // Parameters
        for (TreeParameter param : link.parameters()) {
            Element paramElem = doc.createElement("parameter");
            paramElem.setAttribute("param-name", param.paramName());
            setAttrIfNotEmpty(paramElem, "from-field", param.fromField());
            setAttrIfNotEmpty(paramElem, "value", param.value());
            elem.appendChild(paramElem);
        }

        // Image
        if (!link.image().UNSET()) {
            addImageElement(doc, elem, link.image());
        }

        parentElement.appendChild(elem);
    }

    /**
     * Adds an image element.
     */
    protected void addImageElement(Document doc, Element parentElement, TreeImage image) {
        Element elem = doc.createElement("image");
        setAttrIfNotEmpty(elem, "src", image.src());
        setAttrIfNotEmpty(elem, "id", image.id());
        setAttrIfNotEmpty(elem, "style", image.style());
        setAttrIfNotEmpty(elem, "width", image.width());
        setAttrIfNotEmpty(elem, "height", image.height());
        setAttrIfNotEmpty(elem, "border", image.border());
        setAttrIfNotEmpty(elem, "alt", image.alt());
        setAttrIfNotEmpty(elem, "title", image.title());
        if (!"content".equals(image.urlMode())) {
            elem.setAttribute("url-mode", image.urlMode());
        }
        parentElement.appendChild(elem);
    }

    /**
     * Adds a sub-node element.
     */
    protected void addSubNodeElement(Document doc, Element parentElement, SubNode subNode) {
        Element elem = doc.createElement("sub-node");
        elem.setAttribute("node-name", subNode.nodeName());

        // Data source (mutually exclusive: actions, entity-and, service, entity-condition)
        if (!subNode.actions().UNSET()) {
            Element actionsElement = doc.createElement("actions");
            addActionsContent(doc, actionsElement, subNode.actions());
            if (actionsElement.hasChildNodes()) {
                elem.appendChild(actionsElement);
            }
        } else if (!subNode.entityAnd().UNSET()) {
            addEntityAndElement(doc, elem, subNode.entityAnd());
        } else if (!subNode.service().UNSET()) {
            addServiceElement(doc, elem, subNode.service());
        } else if (!subNode.entityCondition().UNSET()) {
            addEntityConditionElement(doc, elem, subNode.entityCondition());
        }

        // Out field maps
        for (OutFieldMap outFieldMap : subNode.outFieldMaps()) {
            Element ofmElem = doc.createElement("out-field-map");
            ofmElem.setAttribute("field-name", outFieldMap.fieldName());
            setAttrIfNotEmpty(ofmElem, "to-field-name", outFieldMap.toFieldName());
            elem.appendChild(ofmElem);
        }

        parentElement.appendChild(elem);
    }

    /**
     * Adds an entity-and element.
     */
    protected void addEntityAndElement(Document doc, Element parentElement, EntityAnd entityAnd) {
        Element elem = doc.createElement("entity-and");
        elem.setAttribute("entity-name", entityAnd.entityName());
        setAttrIfNotEmpty(elem, "list", entityAnd.list());
        if (entityAnd.useCache()) {
            elem.setAttribute("use-cache", "true");
        }
        if (!"false".equals(entityAnd.filterByDate())) {
            elem.setAttribute("filter-by-date", entityAnd.filterByDate());
        }
        if (!"scroll".equals(entityAnd.resultSetType())) {
            elem.setAttribute("result-set-type", entityAnd.resultSetType());
        }

        for (FieldMap fm : entityAnd.fieldMaps()) {
            Element fmElem = doc.createElement("field-map");
            fmElem.setAttribute("field-name", fm.fieldName());
            setAttrIfNotEmpty(fmElem, "from-field", fm.fromField());
            setAttrIfNotEmpty(fmElem, "value", fm.value());
            elem.appendChild(fmElem);
        }

        for (String selectField : entityAnd.selectFields()) {
            Element sfElem = doc.createElement("select-field");
            sfElem.setAttribute("field-name", selectField);
            elem.appendChild(sfElem);
        }

        for (String orderBy : entityAnd.orderBy()) {
            Element obElem = doc.createElement("order-by");
            obElem.setAttribute("field-name", orderBy);
            elem.appendChild(obElem);
        }

        parentElement.appendChild(elem);
    }

    /**
     * Adds an entity-condition element.
     */
    protected void addEntityConditionElement(Document doc, Element parentElement, EntityCondition entityCondition) {
        Element elem = doc.createElement("entity-condition");
        elem.setAttribute("entity-name", entityCondition.entityName());
        if (entityCondition.useCache()) {
            elem.setAttribute("use-cache", "true");
        }
        if (!"false".equals(entityCondition.filterByDate())) {
            elem.setAttribute("filter-by-date", entityCondition.filterByDate());
        }
        if (entityCondition.distinct()) {
            elem.setAttribute("distinct", "true");
        }
        setAttrIfNotEmpty(elem, "delegator-name", entityCondition.delegatorName());
        setAttrIfNotEmpty(elem, "list", entityCondition.list());
        if (!"scroll".equals(entityCondition.resultSetType())) {
            elem.setAttribute("result-set-type", entityCondition.resultSetType());
        }

        // Structured condition support (equivalent to widget-tree.xsd condition-list/condition-expr)
        if (entityCondition.conditions().length > 0 || entityCondition.conditionLists().length > 0) {
            Element conditionListElem = doc.createElement("condition-list");
            if (!"and".equals(entityCondition.combine())) {
                conditionListElem.setAttribute("combine", entityCondition.combine());
            }
            for (TreeConditionExpr conditionExpr : entityCondition.conditions()) {
                conditionListElem.appendChild(addConditionExprElement(doc, conditionExpr));
            }
            for (TreeConditionList conditionList : entityCondition.conditionLists()) {
                conditionListElem.appendChild(addConditionListElement(doc, conditionList));
            }
            elem.appendChild(conditionListElem);
        } else if (UtilValidate.isNotEmpty(entityCondition.conditionExpr())) {
            // SCIPIO: Legacy fallback - deprecated flat conditionExpr() form
            Element condExpr = doc.createElement("condition-expr");
            condExpr.setAttribute("field-name", entityCondition.conditionExpr());
            condExpr.setAttribute("operator", "equals");
            condExpr.setAttribute("value", "true");
            elem.appendChild(condExpr);
        }

        for (String selectField : entityCondition.selectFields()) {
            Element sfElem = doc.createElement("select-field");
            sfElem.setAttribute("field-name", selectField);
            elem.appendChild(sfElem);
        }

        for (String orderBy : entityCondition.orderBy()) {
            Element obElem = doc.createElement("order-by");
            obElem.setAttribute("field-name", orderBy);
            elem.appendChild(obElem);
        }

        parentElement.appendChild(elem);
    }

    /**
     * Builds a condition-expr element from a {@link TreeConditionExpr} annotation.
     */
    protected Element addConditionExprElement(Document doc, TreeConditionExpr conditionExpr) {
        Element elem = doc.createElement("condition-expr");
        elem.setAttribute("field-name", conditionExpr.fieldName());
        if (!"equals".equals(conditionExpr.operator())) {
            elem.setAttribute("operator", conditionExpr.operator());
        }
        setAttrIfNotEmpty(elem, "from-field", conditionExpr.fromField());
        setAttrIfNotEmpty(elem, "value", conditionExpr.value());
        if (conditionExpr.ignoreIfNull()) {
            elem.setAttribute("ignore-if-null", "true");
        }
        if (conditionExpr.ignoreIfEmpty()) {
            elem.setAttribute("ignore-if-empty", "true");
        }
        if (conditionExpr.ignoreCase()) {
            elem.setAttribute("ignore-case", "true");
        }
        return elem;
    }

    /**
     * Builds a nested condition-list element from a {@link TreeConditionList} annotation.
     */
    protected Element addConditionListElement(Document doc, TreeConditionList conditionList) {
        Element elem = doc.createElement("condition-list");
        if (!"and".equals(conditionList.combine())) {
            elem.setAttribute("combine", conditionList.combine());
        }
        for (TreeConditionExpr conditionExpr : conditionList.conditions()) {
            elem.appendChild(addConditionExprElement(doc, conditionExpr));
        }
        return elem;
    }

    /**
     * Sets an attribute if the value is not empty.
     */
    protected void setAttrIfNotEmpty(Element element, String attrName, String value) {
        if (UtilValidate.isNotEmpty(value)) {
            element.setAttribute(attrName, value);
        }
    }
}
