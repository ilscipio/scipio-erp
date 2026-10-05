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
package com.ilscipio.scipio.service.converter;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.util.*;

/**
 * Converts service-eca XML (secas.xml) to Java annotation-based SECA definitions.
 *
 * <p>Generates @Seca annotated classes from service ECA XML definitions.</p>
 *
 * <p>SCIPIO: 4.0.0: Added for XML-to-Annotation conversion support.</p>
 */
public class SecaXmlToAnnotationConverter {

    protected static final String INDENT = "    ";
    protected static final String NEWLINE = System.lineSeparator();

    protected final String packageName;
    protected final String className;
    protected final String componentName;
    protected final File outputDir;

    public SecaXmlToAnnotationConverter(String packageName, String className, String componentName, File outputDir) {
        this.packageName = packageName;
        this.className = className;
        this.componentName = componentName;
        this.outputDir = outputDir;
    }

    /**
     * Converts the service-eca XML document to Java annotation source code.
     */
    public String convert(Document doc) {
        StringBuilder sb = new StringBuilder();
        sb.append(generateClassHeader());

        Element root = doc.getDocumentElement();

        // Generate Seca annotations from eca elements
        List<Element> ecas = childElementList(root, "eca");
        int ecaIndex = 1;
        for (Element eca : ecas) {
            String ecaCode = generateSeca(eca, ecaIndex++);
            if (isNotEmpty(ecaCode)) {
                sb.append(NEWLINE);
                sb.append(ecaCode);
            }
        }

        sb.append(NEWLINE);
        sb.append(generateClassFooter());
        return sb.toString();
    }

    // ========================================================================
    // SECA Generation
    // ========================================================================

    /**
     * Generates @Seca annotation for an eca element.
     */
    protected String generateSeca(Element eca, int ecaIndex) {
        String service = getAttr(eca, "service");
        String event = getAttr(eca, "event");
        String runOnFailure = getAttr(eca, "run-on-failure");
        String runOnError = getAttr(eca, "run-on-error");
        String enabled = getAttr(eca, "enabled");

        if (isEmpty(service) || isEmpty(event)) return "";

        // Collect conditions, sets, and actions
        List<Element> conditions = new ArrayList<>();
        List<Element> sets = new ArrayList<>();
        List<Element> actions = new ArrayList<>();

        NodeList children = eca.getChildNodes();
        for (int i = 0; i < children.getLength(); i++) {
            Node child = children.item(i);
            if (child.getNodeType() == Node.ELEMENT_NODE) {
                Element elem = (Element) child;
                String tagName = elem.getTagName();
                if ("set".equals(tagName)) {
                    sets.add(elem);
                } else if ("action".equals(tagName)) {
                    actions.add(elem);
                } else if (isConditionElement(tagName)) {
                    conditions.add(elem);
                }
            }
        }

        StringBuilder sb = new StringBuilder();
        String interfaceName = toInterfaceName(service) + event.replace("-", "") + "Seca" + ecaIndex;

        // Generate description comment
        sb.append(INDENT).append("/**").append(NEWLINE);
        sb.append(INDENT).append(" * SECA for service ").append(service).append(" on event ").append(event).append(".").append(NEWLINE);
        sb.append(INDENT).append(" */").append(NEWLINE);

        // @Seca annotation
        sb.append(INDENT).append("@Seca(").append(NEWLINE);
        sb.append(INDENT).append(INDENT).append("service = ").append(toStringValue(service));
        sb.append(",").append(NEWLINE);
        sb.append(INDENT).append(INDENT).append("event = ").append(toStringValue(event));

        if ("true".equals(runOnFailure)) {
            sb.append(",").append(NEWLINE);
            sb.append(INDENT).append(INDENT).append("runOnFailure = \"true\"");
        }
        if ("true".equals(runOnError)) {
            sb.append(",").append(NEWLINE);
            sb.append(INDENT).append(INDENT).append("runOnError = \"true\"");
        }
        if ("false".equals(enabled)) {
            sb.append(",").append(NEWLINE);
            sb.append(INDENT).append(INDENT).append("enabled = \"false\"");
        }

        // Generate condition expression from condition elements
        if (!conditions.isEmpty()) {
            String conditionExpr = generateConditionExpression(conditions);
            if (isNotEmpty(conditionExpr)) {
                sb.append(",").append(NEWLINE);
                sb.append(INDENT).append(INDENT).append("condition = ").append(toStringValue(conditionExpr));
            }
        }

        // Assignments (set elements)
        if (!sets.isEmpty()) {
            sb.append(",").append(NEWLINE);
            sb.append(INDENT).append(INDENT).append("assignments = {").append(NEWLINE);
            boolean first = true;
            for (Element set : sets) {
                if (!first) sb.append(",").append(NEWLINE);
                sb.append(generateSecaSetNested(set, 3));
                first = false;
            }
            sb.append(NEWLINE).append(INDENT).append(INDENT).append("}");
        }

        // Actions
        if (!actions.isEmpty()) {
            sb.append(",").append(NEWLINE);
            sb.append(INDENT).append(INDENT).append("actions = {").append(NEWLINE);
            boolean first = true;
            for (Element action : actions) {
                if (!first) sb.append(",").append(NEWLINE);
                sb.append(generateSecaActionNested(action, 3));
                first = false;
            }
            sb.append(NEWLINE).append(INDENT).append(INDENT).append("}");
        }

        sb.append(NEWLINE);
        sb.append(INDENT).append(")").append(NEWLINE);
        sb.append(INDENT).append("public interface ").append(interfaceName).append(" {}").append(NEWLINE);

        return sb.toString();
    }

    /**
     * Generates a condition expression from condition elements.
     * Converts XML conditions to flexible expression syntax.
     */
    protected String generateConditionExpression(List<Element> conditions) {
        if (conditions.isEmpty()) return "";

        List<String> parts = new ArrayList<>();
        for (Element condition : conditions) {
            String expr = generateSingleConditionExpression(condition);
            if (isNotEmpty(expr)) {
                parts.add(expr);
            }
        }

        if (parts.isEmpty()) return "";
        if (parts.size() == 1) return parts.get(0);

        // Multiple conditions default to AND
        return String.join(" && ", parts);
    }

    protected String generateSingleConditionExpression(Element condition) {
        String tagName = condition.getTagName();

        switch (tagName) {
            case "condition":
                return generateSimpleConditionExpression(condition);
            case "condition-field":
                return generateConditionFieldExpression(condition);
            case "condition-service":
                return generateConditionServiceExpression(condition);
            case "and":
                return generateLogicalConditionExpression(condition, "&&");
            case "or":
                return generateLogicalConditionExpression(condition, "||");
            case "not":
                return generateNotConditionExpression(condition);
            case "xor":
                return generateXorConditionExpression(condition);
            default:
                return "";
        }
    }

    protected String generateSimpleConditionExpression(Element condition) {
        String field = getAttr(condition, "field");
        if (isEmpty(field)) {
            field = getAttr(condition, "field-name");
        }
        String mapName = getAttr(condition, "map-name");
        String operator = getAttr(condition, "operator");
        String value = getAttr(condition, "value");

        if (isEmpty(field) || isEmpty(operator)) return "";

        String fieldRef = isNotEmpty(mapName) ? mapName + "." + field : field;

        switch (operator) {
            case "equals":
                return fieldRef + " == " + (isNotEmpty(value) ? "'" + value + "'" : "null");
            case "not-equals":
                return fieldRef + " != " + (isNotEmpty(value) ? "'" + value + "'" : "null");
            case "is-empty":
                return "empty(" + fieldRef + ")";
            case "is-not-empty":
                return "!empty(" + fieldRef + ")";
            case "less":
                return fieldRef + " < " + value;
            case "greater":
                return fieldRef + " > " + value;
            case "less-equals":
                return fieldRef + " <= " + value;
            case "greater-equals":
                return fieldRef + " >= " + value;
            case "contains":
                return fieldRef + ".contains('" + value + "')";
            default:
                return "";
        }
    }

    protected String generateConditionFieldExpression(Element condition) {
        String fieldName = getAttr(condition, "field-name");
        String mapName = getAttr(condition, "map-name");
        String operator = getAttr(condition, "operator");
        String toFieldName = getAttr(condition, "to-field-name");
        String toMapName = getAttr(condition, "to-map-name");

        if (isEmpty(fieldName) || isEmpty(operator)) return "";

        String fieldRef = isNotEmpty(mapName) ? mapName + "." + fieldName : fieldName;
        String toFieldRef = isNotEmpty(toFieldName)
                ? (isNotEmpty(toMapName) ? toMapName + "." + toFieldName : toFieldName)
                : fieldName;

        switch (operator) {
            case "equals":
                return fieldRef + " == " + toFieldRef;
            case "not-equals":
                return fieldRef + " != " + toFieldRef;
            case "less":
                return fieldRef + " < " + toFieldRef;
            case "greater":
                return fieldRef + " > " + toFieldRef;
            case "less-equals":
                return fieldRef + " <= " + toFieldRef;
            case "greater-equals":
                return fieldRef + " >= " + toFieldRef;
            case "contains":
                return fieldRef + ".contains(" + toFieldRef + ")";
            default:
                return "";
        }
    }

    protected String generateConditionServiceExpression(Element condition) {
        String serviceName = getAttr(condition, "service-name");
        if (isEmpty(serviceName)) return "";
        return "service:" + serviceName;
    }

    protected String generateLogicalConditionExpression(Element condition, String operator) {
        List<Element> children = childElementList(condition, null);
        List<String> parts = new ArrayList<>();
        for (Element child : children) {
            if (isConditionElement(child.getTagName())) {
                String expr = generateSingleConditionExpression(child);
                if (isNotEmpty(expr)) {
                    parts.add(expr);
                }
            }
        }
        if (parts.isEmpty()) return "";
        if (parts.size() == 1) return parts.get(0);
        return "(" + String.join(" " + operator + " ", parts) + ")";
    }

    protected String generateNotConditionExpression(Element condition) {
        List<Element> children = childElementList(condition, null);
        for (Element child : children) {
            if (isConditionElement(child.getTagName())) {
                String expr = generateSingleConditionExpression(child);
                if (isNotEmpty(expr)) {
                    return "!(" + expr + ")";
                }
            }
        }
        return "";
    }

    protected String generateXorConditionExpression(Element condition) {
        List<Element> children = childElementList(condition, null);
        List<String> parts = new ArrayList<>();
        for (Element child : children) {
            if (isConditionElement(child.getTagName())) {
                String expr = generateSingleConditionExpression(child);
                if (isNotEmpty(expr)) {
                    parts.add(expr);
                }
            }
        }
        if (parts.isEmpty()) return "";
        if (parts.size() == 1) return parts.get(0);
        // XOR is simulated - only one should be true
        return "xor(" + String.join(", ", parts) + ")";
    }

    protected boolean isConditionElement(String tagName) {
        return "condition".equals(tagName) ||
               "condition-field".equals(tagName) ||
               "condition-service".equals(tagName) ||
               "condition-property".equals(tagName) ||
               "condition-property-field".equals(tagName) ||
               "and".equals(tagName) ||
               "or".equals(tagName) ||
               "not".equals(tagName) ||
               "xor".equals(tagName);
    }

    protected String generateSecaSetNested(Element set, int indentLevel) {
        String ind = indent(indentLevel);
        String fieldName = getAttr(set, "field-name");
        String envName = getAttr(set, "env-name");
        String value = getAttr(set, "value");
        String format = getAttr(set, "format");

        StringBuilder sb = new StringBuilder();
        sb.append(ind).append("@SecaSet(fieldName = ").append(toStringValue(fieldName));

        if (isNotEmpty(envName)) {
            sb.append(", envName = ").append(toStringValue(envName));
        }
        if (isNotEmpty(value)) {
            sb.append(", value = ").append(toStringValue(value));
        }
        if (isNotEmpty(format)) {
            sb.append(", format = ").append(toStringValue(format));
        }

        sb.append(")");
        return sb.toString();
    }

    protected String generateSecaActionNested(Element action, int indentLevel) {
        String ind = indent(indentLevel);
        String service = getAttr(action, "service");
        String mode = getAttr(action, "mode");
        String runAsUser = getAttr(action, "run-as-user");
        String resultMapName = getAttr(action, "result-map-name");
        String newTransaction = getAttr(action, "new-transaction");
        String resultToContext = getAttr(action, "result-to-context");
        String resultToResult = getAttr(action, "result-to-result");
        String ignoreFailure = getAttr(action, "ignore-failure");
        String ignoreError = getAttr(action, "ignore-error");
        String persist = getAttr(action, "persist");
        String priority = getAttr(action, "priority");
        String jobPool = getAttr(action, "job-pool");

        StringBuilder sb = new StringBuilder();
        sb.append(ind).append("@SecaAction(").append(NEWLINE);
        sb.append(ind).append(INDENT).append("service = ").append(toStringValue(service));

        if (isNotEmpty(mode)) {
            sb.append(",").append(NEWLINE);
            sb.append(ind).append(INDENT).append("mode = ").append(toStringValue(mode));
        }
        if (isNotEmpty(runAsUser)) {
            sb.append(",").append(NEWLINE);
            sb.append(ind).append(INDENT).append("runAsUser = ").append(toStringValue(runAsUser));
        }
        if (isNotEmpty(resultMapName)) {
            sb.append(",").append(NEWLINE);
            sb.append(ind).append(INDENT).append("resultMapName = ").append(toStringValue(resultMapName));
        }
        if ("true".equals(newTransaction)) {
            sb.append(",").append(NEWLINE);
            sb.append(ind).append(INDENT).append("newTransaction = \"true\"");
        }
        if ("false".equals(resultToContext)) {
            sb.append(",").append(NEWLINE);
            sb.append(ind).append(INDENT).append("resultToContext = \"false\"");
        }
        if ("true".equals(resultToResult)) {
            sb.append(",").append(NEWLINE);
            sb.append(ind).append(INDENT).append("resultToResult = \"true\"");
        }
        if ("false".equals(ignoreFailure)) {
            sb.append(",").append(NEWLINE);
            sb.append(ind).append(INDENT).append("ignoreFailure = \"false\"");
        }
        if ("false".equals(ignoreError)) {
            sb.append(",").append(NEWLINE);
            sb.append(ind).append(INDENT).append("ignoreError = \"false\"");
        }
        if ("true".equals(persist)) {
            sb.append(",").append(NEWLINE);
            sb.append(ind).append(INDENT).append("persist = \"true\"");
        }
        if (isNotEmpty(priority)) {
            sb.append(",").append(NEWLINE);
            sb.append(ind).append(INDENT).append("priority = ").append(toStringValue(priority));
        }
        if (isNotEmpty(jobPool)) {
            sb.append(",").append(NEWLINE);
            sb.append(ind).append(INDENT).append("jobPool = ").append(toStringValue(jobPool));
        }

        sb.append(NEWLINE).append(ind).append(")");
        return sb.toString();
    }

    // ========================================================================
    // File Output Methods
    // ========================================================================

    /**
     * Writes the generated source to a file.
     */
    public void writeToFile(String source) throws IOException {
        File packageDir = new File(outputDir, packageName.replace('.', File.separatorChar));
        packageDir.mkdirs();

        File javaFile = new File(packageDir, className + ".java");
        try (FileWriter writer = new FileWriter(javaFile)) {
            writer.write(source);
        }
    }

    /**
     * Gets the output Java file path.
     */
    public File getOutputFile() {
        File packageDir = new File(outputDir, packageName.replace('.', File.separatorChar));
        return new File(packageDir, className + ".java");
    }

    // ========================================================================
    // Code Generation Utilities
    // ========================================================================

    protected String generateClassHeader() {
        StringBuilder sb = new StringBuilder();

        // License header
        sb.append("/*").append(NEWLINE);
        sb.append(" * Licensed to the Apache Software Foundation (ASF) under one").append(NEWLINE);
        sb.append(" * or more contributor license agreements.  See the NOTICE file").append(NEWLINE);
        sb.append(" * distributed with this work for additional information").append(NEWLINE);
        sb.append(" * regarding copyright ownership.  The ASF licenses this file").append(NEWLINE);
        sb.append(" * to you under the Apache License, Version 2.0 (the").append(NEWLINE);
        sb.append(" * \"License\"); you may not use this file except in compliance").append(NEWLINE);
        sb.append(" * with the License.  You may obtain a copy of the License at").append(NEWLINE);
        sb.append(" *").append(NEWLINE);
        sb.append(" * http://www.apache.org/licenses/LICENSE-2.0").append(NEWLINE);
        sb.append(" *").append(NEWLINE);
        sb.append(" * Unless required by applicable law or agreed to in writing,").append(NEWLINE);
        sb.append(" * software distributed under the License is distributed on an").append(NEWLINE);
        sb.append(" * \"AS IS\" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY").append(NEWLINE);
        sb.append(" * KIND, either express or implied.  See the License for the").append(NEWLINE);
        sb.append(" * specific language governing permissions and limitations").append(NEWLINE);
        sb.append(" * under the License.").append(NEWLINE);
        sb.append(" */").append(NEWLINE);

        // Package
        sb.append("package ").append(packageName).append(";").append(NEWLINE);
        sb.append(NEWLINE);

        // Imports
        sb.append("import com.ilscipio.scipio.service.def.seca.*;").append(NEWLINE);
        sb.append(NEWLINE);

        // Class javadoc
        sb.append("/**").append(NEWLINE);
        sb.append(" * Auto-generated annotation-based service ECA definitions.").append(NEWLINE);
        sb.append(" *").append(NEWLINE);
        sb.append(" * <p>Generated from secas.xml by convertXmlToAnnotation Gradle task.</p>").append(NEWLINE);
        sb.append(" *").append(NEWLINE);
        sb.append(" * <p>SCIPIO: 4.0.0: Auto-generated.</p>").append(NEWLINE);
        sb.append(" */").append(NEWLINE);

        // Class declaration
        sb.append("public class ").append(className).append(" {").append(NEWLINE);

        return sb.toString();
    }

    protected String generateClassFooter() {
        return "}" + NEWLINE;
    }

    protected String indent(int level) {
        StringBuilder sb = new StringBuilder();
        for (int i = 0; i < level; i++) {
            sb.append(INDENT);
        }
        return sb.toString();
    }

    protected String toStringValue(String value) {
        if (value == null) return "\"\"";
        return "\"" + escapeString(value) + "\"";
    }

    protected String escapeString(String s) {
        if (s == null) return null;
        return s.replace("\\", "\\\\")
                .replace("\"", "\\\"")
                .replace("\n", "\\n")
                .replace("\r", "\\r")
                .replace("\t", "\\t");
    }

    protected String toInterfaceName(String name) {
        if (isEmpty(name)) return "Unknown";
        StringBuilder sb = new StringBuilder();
        boolean capitalizeNext = true;
        for (char c : name.toCharArray()) {
            if (c == '-' || c == '_' || c == '.') {
                capitalizeNext = true;
            } else if (Character.isLetterOrDigit(c)) {
                if (capitalizeNext) {
                    sb.append(Character.toUpperCase(c));
                    capitalizeNext = false;
                } else {
                    sb.append(c);
                }
            }
        }
        String result = sb.toString();
        if (result.length() > 0 && Character.isDigit(result.charAt(0))) {
            result = "_" + result;
        }
        return result;
    }

    protected String getAttr(Element element, String attrName) {
        return element != null ? element.getAttribute(attrName) : "";
    }

    // ========================================================================
    // DOM Utilities
    // ========================================================================

    protected List<Element> childElementList(Element parent, String tagName) {
        List<Element> result = new ArrayList<>();
        if (parent == null) return result;
        NodeList children = parent.getChildNodes();
        for (int i = 0; i < children.getLength(); i++) {
            Node child = children.item(i);
            if (child.getNodeType() == Node.ELEMENT_NODE) {
                Element element = (Element) child;
                if (tagName == null || tagName.equals(element.getTagName())) {
                    result.add(element);
                }
            }
        }
        return result;
    }

    protected static boolean isEmpty(String s) {
        return s == null || s.isEmpty();
    }

    protected static boolean isNotEmpty(String s) {
        return s != null && !s.isEmpty();
    }
}
