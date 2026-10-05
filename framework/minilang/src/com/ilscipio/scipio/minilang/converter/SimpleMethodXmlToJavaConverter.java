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
package com.ilscipio.scipio.minilang.converter;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

/**
 * Converts Simple Method XML files to Java event classes.
 *
 * <p>Each simple-method becomes a static method in the generated class.
 * The conversion aims to produce readable, maintainable Java code that
 * mirrors the original simple-method logic.</p>
 *
 * <p>SCIPIO: 4.0.0: Added for Simple Method XML-to-Java conversion support.</p>
 */
public class SimpleMethodXmlToJavaConverter {

    private static final String INDENT = "    ";
    private static final String NEWLINE = System.lineSeparator();

    private final String packageName;
    private final String className;
    private final String componentName;
    private final File outputDir;

    /**
     * Source location URL (e.g., "component://setup/script/com/ilscipio/scipio/setup/SetupEvents.xml")
     */
    private String sourceLocation;

    // Track variable names to avoid collisions
    private Set<String> declaredVars = new HashSet<>();

    // Track declared variable types for casting
    private Map<String, String> declaredVarTypes = new HashMap<>();

    // Track variables that must be typed as GenericValue (used with store/create/remove)
    private Set<String> genericValueVars = new HashSet<>();

    // Track iterate entry vars that were hoisted because they're used outside their loop
    private Set<String> hoistedIterateEntryVars = new HashSet<>();

    // Track vars hoisted at method level (vs block level) - these persist throughout the method
    private Set<String> methodLevelVars = new HashSet<>();

    // Track variables assigned from scripts - these must be typed as Object
    // (script results are Object and may differ from explicit type attributes)
    private Set<String> scriptAssignedVars = new HashSet<>();

    // Track imports needed
    private Set<String> imports = new HashSet<>();

    // Current indentation level
    private int indentLevel = 2;

    // Groovy script counter for extracted scripts
    private int groovyScriptCounter = 0;

    // Map of xml-resource to class name mappings
    private Map<String, String> xmlResourceToClass = new HashMap<>();

    // Map of variable name renamings (original -> renamed) for conflict resolution
    // Used when entry variable in iterate loop conflicts with existing variable
    private Map<String, String> varRenameMap = new HashMap<>();

    /**
     * Creates a new converter.
     *
     * @param packageName Java package for generated class
     * @param className Java class name for generated class
     * @param componentName Component name (e.g., "setup")
     * @param outputDir Directory for generated Java file
     */
    public SimpleMethodXmlToJavaConverter(String packageName, String className, String componentName, File outputDir) {
        this.packageName = packageName;
        this.className = className;
        this.componentName = componentName;
        this.outputDir = outputDir;
        initXmlResourceMappings();
    }

    /**
     * Initializes known XML resource to class mappings.
     */
    private void initXmlResourceMappings() {
        // Add known mappings - these can be extended
        xmlResourceToClass.put("component://commonext/script/org/ofbiz/setup/SetupEvents.xml",
                "com.ilscipio.scipio.commonext.event.CommonextSetupEvents");
        xmlResourceToClass.put("component://party/script/org/ofbiz/party/user/UserEvents.xml",
                "com.ilscipio.scipio.party.event.UserEvents");
    }

    /**
     * Sets the source location URL.
     */
    public void setSourceLocation(String sourceLocation) {
        this.sourceLocation = sourceLocation;
    }

    /**
     * Gets the source location URL.
     */
    public String getSourceLocation() {
        return sourceLocation;
    }

    /**
     * Converts the XML document to Java source code.
     *
     * @param doc XML document to convert
     * @return Generated Java source code
     */
    public String convert(Document doc) {
        StringBuilder sb = new StringBuilder();

        // Reset state
        imports.clear();

        // Add standard imports
        addStandardImports();

        // Process all simple-methods first to collect imports
        Element root = doc.getDocumentElement();
        List<Element> methodElements = childElementList(root, "simple-method");

        StringBuilder methodsBuilder = new StringBuilder();
        for (Element methodElement : methodElements) {
            methodsBuilder.append(convertMethod(methodElement));
            methodsBuilder.append(NEWLINE);
        }

        // Build final class
        sb.append(generateClassHeader());
        sb.append(NEWLINE);
        sb.append(methodsBuilder);
        sb.append(generateClassFooter());

        return sb.toString();
    }

    /**
     * Adds standard imports needed for most conversions.
     */
    private void addStandardImports() {
        imports.add("javax.servlet.http.HttpServletRequest");
        imports.add("javax.servlet.http.HttpServletResponse");
        imports.add("java.util.Map");
        imports.add("java.util.HashMap");
        imports.add("java.util.List");
        imports.add("java.util.LinkedList");
        imports.add("java.util.Locale");
        imports.add("org.ofbiz.base.util.Debug");
        imports.add("org.ofbiz.base.util.UtilHttp");
        imports.add("org.ofbiz.base.util.UtilMisc");
        imports.add("org.ofbiz.base.util.UtilProperties");
        imports.add("org.ofbiz.base.util.UtilValidate");
        imports.add("org.ofbiz.entity.Delegator");
        imports.add("org.ofbiz.entity.GenericValue");
        imports.add("org.ofbiz.service.LocalDispatcher");
    }

    /**
     * Generates the class header with package, imports, and class declaration.
     */
    private String generateClassHeader() {
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

        // Imports (sorted)
        List<String> sortedImports = new ArrayList<>(imports);
        sortedImports.sort(String::compareTo);
        for (String imp : sortedImports) {
            sb.append("import ").append(imp).append(";").append(NEWLINE);
        }
        sb.append(NEWLINE);

        // Class javadoc
        sb.append("/**").append(NEWLINE);
        sb.append(" * Auto-generated event class from Simple Method XML.").append(NEWLINE);
        if (sourceLocation != null) {
            sb.append(" *").append(NEWLINE);
            sb.append(" * <p>Generated from: ").append(sourceLocation).append("</p>").append(NEWLINE);
        }
        sb.append(" *").append(NEWLINE);
        sb.append(" * <p>SCIPIO: 4.0.0: Auto-generated.</p>").append(NEWLINE);
        sb.append(" */").append(NEWLINE);

        // Class declaration
        sb.append("public class ").append(className).append(" {").append(NEWLINE);
        sb.append(NEWLINE);

        // Module constant for Debug
        sb.append(INDENT).append("private static final String MODULE = ").append(className).append(".class.getName();").append(NEWLINE);

        return sb.toString();
    }

    /**
     * Generates the class footer.
     */
    private String generateClassFooter() {
        return "}" + NEWLINE;
    }

    /**
     * Converts a single simple-method to a Java method.
     */
    private String convertMethod(Element methodElement) {
        StringBuilder sb = new StringBuilder();

        // Reset per-method state
        declaredVars.clear();
        declaredVarTypes.clear();
        genericValueVars.clear();
        hoistedIterateEntryVars.clear();
        methodLevelVars.clear();
        scriptAssignedVars.clear();
        groovyScriptCounter = 0;
        indentLevel = 2;

        // Pre-scan for variables that must be GenericValue (used with store/create/remove)
        collectGenericValueVars(methodElement, genericValueVars);

        // Pre-scan for variables assigned from scripts (must be typed as Object)
        collectScriptAssignedVars(methodElement, scriptAssignedVars);

        String methodName = getAttr(methodElement, "method-name");
        String shortDescription = getAttr(methodElement, "short-description");
        boolean loginRequired = parseBoolean(getAttr(methodElement, "login-required"), true);

        // Method javadoc
        sb.append(NEWLINE);
        sb.append(INDENT).append("/**").append(NEWLINE);
        if (isNotEmpty(shortDescription)) {
            sb.append(INDENT).append(" * ").append(escapeJavadoc(shortDescription)).append(NEWLINE);
        }
        sb.append(INDENT).append(" *").append(NEWLINE);
        sb.append(INDENT).append(" * @param request The HTTP request").append(NEWLINE);
        sb.append(INDENT).append(" * @param response The HTTP response").append(NEWLINE);
        sb.append(INDENT).append(" * @return Event result (\"success\", \"error\", etc.)").append(NEWLINE);
        sb.append(INDENT).append(" */").append(NEWLINE);

        // Method signature
        sb.append(INDENT).append("public static String ").append(toJavaMethodName(methodName));
        sb.append("(HttpServletRequest request, HttpServletResponse response) {").append(NEWLINE);

        // Standard method preamble
        sb.append(indent()).append("Delegator delegator = (Delegator) request.getAttribute(\"delegator\");").append(NEWLINE);
        sb.append(indent()).append("LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute(\"dispatcher\");").append(NEWLINE);
        sb.append(indent()).append("Locale locale = UtilHttp.getLocale(request);").append(NEWLINE);
        sb.append(indent()).append("GenericValue userLogin = (GenericValue) request.getSession().getAttribute(\"userLogin\");").append(NEWLINE);
        sb.append(indent()).append("Map<String, Object> context = UtilHttp.getCombinedMap(request);").append(NEWLINE);
        sb.append(NEWLINE);

        // Mark standard vars as declared with their types
        declaredVars.add("delegator");
        declaredVarTypes.put("delegator", "Delegator");
        declaredVars.add("dispatcher");
        declaredVarTypes.put("dispatcher", "LocalDispatcher");
        declaredVars.add("locale");
        declaredVarTypes.put("locale", "Locale");
        declaredVars.add("userLogin");
        declaredVarTypes.put("userLogin", "GenericValue");
        declaredVars.add("context");
        declaredVarTypes.put("context", "Map<String, Object>");
        declaredVars.add("request");
        declaredVarTypes.put("request", "HttpServletRequest");
        declaredVars.add("response");
        declaredVarTypes.put("response", "HttpServletResponse");

        // Check if method uses field-to-result - if so, initialize result map
        if (hasDescendant(methodElement, "field-to-result")) {
            sb.append(indent()).append("Map<String, Object> result = new HashMap<>();").append(NEWLINE);
            sb.append(NEWLINE);
            declaredVars.add("result");
            declaredVarTypes.put("result", "Map<String, Object>");
            imports.add("java.util.Map");
            imports.add("java.util.HashMap");
        }

        // Check if method uses add-error - if so, initialize error_list
        if (hasDescendant(methodElement, "add-error")) {
            sb.append(indent()).append("List<String> error_list = new LinkedList<>();").append(NEWLINE);
            sb.append(NEWLINE);
            declaredVars.add("error_list");
            declaredVarTypes.put("error_list", "List<String>");
            imports.add("java.util.LinkedList");
        }

        // Check if method calls getGlAccountTypeDefaultInline - if so, pre-declare lookedUpValue
        // This is needed because the inline method sets this variable and it's used after the call
        if (hasDescendantWithAttr(methodElement, "call-simple-method", "method-name", "getGlAccountTypeDefaultInline")) {
            if (!declaredVars.contains("lookedUpValue")) {
                sb.append(indent()).append("GenericValue lookedUpValue = null;").append(NEWLINE);
                declaredVars.add("lookedUpValue");
                declaredVarTypes.put("lookedUpValue", "GenericValue");
            }
        }

        // Pre-scan for variables that are set in multiple sequential if blocks
        // These need to be hoisted before the if blocks
        sb.append(hoistSequentialIfVariables(methodElement));

        // Process method body
        sb.append(convertMethodBody(methodElement));

        // Default return if no explicit return found
        sb.append(NEWLINE);
        sb.append(indent()).append("return \"success\";").append(NEWLINE);

        sb.append(INDENT).append("}").append(NEWLINE);

        return sb.toString();
    }

    /**
     * Converts the body of a simple-method.
     */
    private String convertMethodBody(Element methodElement) {
        StringBuilder sb = new StringBuilder();

        for (Element child : childElementList(methodElement)) {
            String converted = convertElement(child);
            if (isNotEmpty(converted)) {
                sb.append(converted);
            }
        }

        return sb.toString();
    }

    /**
     * Converts a single XML element to Java code.
     */
    private String convertElement(Element element) {
        String tagName = element.getNodeName();

        switch (tagName) {
            case "set":
                return convertSet(element);
            case "call-service":
                return convertCallService(element);
            case "call-simple-method":
                return convertCallSimpleMethod(element);
            case "check-errors":
                return convertCheckErrors(element);
            case "property-to-field":
                return convertPropertyToField(element);
            case "if-compare":
                return convertIfCompare(element);
            case "if-compare-field":
                return convertIfCompareField(element);
            case "if-empty":
                return convertIfEmpty(element);
            case "if-not-empty":
                return convertIfNotEmpty(element);
            case "if":
                return convertIf(element);
            case "entity-one":
                return convertEntityOne(element);
            case "entity-and":
                return convertEntityAnd(element);
            case "add-error":
                return convertAddError(element);
            case "log":
                return convertLog(element);
            case "script":
                return convertScript(element);
            case "iterate":
                return convertIterate(element);
            case "set-service-fields":
                return convertSetServiceFields(element);
            case "session-to-field":
                return convertSessionToField(element);
            case "request-to-field":
                return convertRequestToField(element);
            case "field-to-request":
                return convertFieldToRequest(element);
            case "field-to-session":
                return convertFieldToSession(element);
            case "call-class-method":
                return convertCallClassMethod(element);
            case "now-timestamp":
                return convertNowTimestamp(element);
            case "store-list":
                return convertStoreList(element);
            case "filter-list-by-date":
                return convertFilterListByDate(element);
            case "map-to-map":
                return convertMapToMap(element);
            case "call-map-processor":
                return convertCallMapProcessor(element);
            case "return":
                return convertReturn(element);
            case "entity-condition":
                return convertEntityCondition(element);
            case "make-value":
                return convertMakeValue(element);
            case "clone-value":
                return convertCloneValue(element);
            case "store-value":
                return convertStoreValue(element);
            case "create-value":
                return convertCreateValue(element);
            case "remove-value":
                return convertRemoveValue(element);
            case "find-by-primary-key":
                return convertFindByPrimaryKey(element);
            case "set-pk-fields":
                return convertSetPkFields(element);
            case "set-nonpk-fields":
                return convertSetNonPkFields(element);
            case "get-related":
                return convertGetRelated(element);
            case "get-related-one":
                return convertGetRelatedOne(element);
            case "clear-field":
                return convertClearField(element);
            case "first-from-list":
                return convertFirstFromList(element);
            case "sequenced-id":
                return convertSequencedId(element);
            case "string-to-list":
                return convertStringToList(element);
            case "entity-count":
                return convertEntityCount(element);
            case "field-to-result":
                return convertFieldToResult(element);
            case "while":
                return convertWhile(element);
            case "calculate":
                return convertCalculate(element);
            case "iterate-map":
                return convertIterateMap(element);
            case "assert":
                return convertAssert(element);
            case "field-to-list":
                return convertFieldToList(element);
            case "make-next-seq-id":
                return convertMakeNextSeqId(element);
            case "to-string":
                return convertToString(element);
            case "call-object-method":
                return convertCallObjectMethod(element);
            case "not":
                // Handled by parent assert element
                return "";
            case "else":
                // Handled by parent if-* elements
                return "";
            case "then":
                // Handled by parent if element
                return "";
            case "condition":
                // Handled by parent if element
                return "";
            default:
                // Generate TODO comment for unsupported operations
                return indent() + "// TODO: Convert <" + tagName + "> element" + NEWLINE;
        }
    }

    /**
     * Converts a <set> element.
     *
     * <p>Handles the property map pattern: {@code <set field="x" value="" set-if-null="true"/>}
     * which creates a new HashMap in simple-methods.</p>
     */
    private String convertSet(Element element) {
        StringBuilder sb = new StringBuilder();

        String field = getAttr(element, "field");
        String value = getAttr(element, "value");
        String fromField = getAttr(element, "from-field");
        String type = getAttr(element, "type");
        boolean setIfNull = parseBoolean(getAttr(element, "set-if-null"), false);

        // Handle special request/session attributes
        if ("_event_message_".equals(field) || "_EVENT_MESSAGE_".equals(field)) {
            String valueExpr = isNotEmpty(fromField) ? buildFieldAccess(fromField) : convertValueLiteral(value, type);
            sb.append(indent()).append("request.setAttribute(\"_EVENT_MESSAGE_\", ").append(valueExpr).append(");").append(NEWLINE);
            return sb.toString();
        }
        if ("_error_message_".equals(field) || "_ERROR_MESSAGE_".equals(field)) {
            String valueExpr = isNotEmpty(fromField) ? buildFieldAccess(fromField) : convertValueLiteral(value, type);
            sb.append(indent()).append("request.setAttribute(\"_ERROR_MESSAGE_\", ").append(valueExpr).append(");").append(NEWLINE);
            return sb.toString();
        }

        String javaField = toJavaFieldName(field);
        String containerField = getContainerField(field);
        String accessField = getAccessField(field);

        // Check for property map pattern: value="" with set-if-null="true" (with or without type="NewMap")
        // This is used in simple-methods to create a new Map variable
        // But for simple variables that were already set to strings, this sets to null
        boolean isNewMapPattern = "NewMap".equals(type);
        if (!isNewMapPattern && setIfNull && "".equals(value)) {
            // For container.key patterns, create new HashMap entry
            // For simple variables, only create HashMap if explicitly typed as Map
            if (containerField != null) {
                isNewMapPattern = true;
            } else {
                // Simple variable - check if it's known to be a Map type
                String existingType = declaredVarTypes.get(javaField);
                if (existingType != null && existingType.contains("Map")) {
                    isNewMapPattern = true;
                }
                // Otherwise treat as null assignment
            }
        }

        if (isNotEmpty(fromField)) {
            // Check for nullfield pattern - mini-lang idiom to set to null (case-insensitive)
            if ("nullfield".equalsIgnoreCase(fromField)) {
                // Treat as null assignment
                if (containerField != null) {
                    String containerType = declaredVarTypes.get(containerField);
                    boolean needsCast = containerType == null ||
                                        (!"GenericValue".equals(containerType) && !"Map<String, Object>".equals(containerType));
                    String putTarget = needsCast ? "((Map<String, Object>) " + containerField + ")" : containerField;
                    sb.append(indent()).append(putTarget).append(".put(\"").append(accessField).append("\", null);").append(NEWLINE);
                } else {
                    if (!declaredVars.contains(javaField)) {
                        String typeDecl = getTypeForField(type, field);
                        sb.append(indent()).append(typeDecl).append(" ").append(javaField).append(" = null;").append(NEWLINE);
                        declaredVars.add(javaField);
                        declaredVarTypes.put(javaField, typeDecl);
                    } else {
                        sb.append(indent()).append(javaField).append(" = null;").append(NEWLINE);
                    }
                }
                return sb.toString();
            }

            // set field from another field
            String fromExpr = buildFieldAccess(fromField);
            if (containerField != null) {
                // Check if this is a list append pattern (empty brackets like "list[]")
                boolean isListAppend = field.endsWith("[]") && isEmpty(accessField);

                if (isListAppend) {
                    // List append pattern: list.add(value)
                    String containerType = declaredVarTypes.get(containerField);
                    if (!declaredVars.contains(containerField)) {
                        sb.append(indent()).append("List<Object> ").append(containerField)
                                .append(" = new LinkedList<>();").append(NEWLINE);
                        declaredVars.add(containerField);
                        declaredVarTypes.put(containerField, "List<Object>");
                        imports.add("java.util.List");
                        imports.add("java.util.LinkedList");
                    }
                    containerType = declaredVarTypes.get(containerField);
                    boolean needsCast = containerType == null ||
                                        (!"List<Object>".equals(containerType) &&
                                         !containerType.startsWith("List"));
                    String addTarget = needsCast ? "((List<Object>) " + containerField + ")" : containerField;
                    sb.append(indent()).append(addTarget).append(".add(").append(fromExpr).append(");").append(NEWLINE);
                    return sb.toString();
                }

                // Setting into a map/entity: container.put(key, value)
                // First ensure the container exists
                String containerType = declaredVarTypes.get(containerField);
                if (!declaredVars.contains(containerField)) {
                    sb.append(indent()).append("Map<String, Object> ").append(containerField)
                            .append(" = new HashMap<>();").append(NEWLINE);
                    declaredVars.add(containerField);
                    declaredVarTypes.put(containerField, "Map<String, Object>");
                    imports.add("java.util.Map");
                    imports.add("java.util.HashMap");
                } else if (containerType == null) {
                    // Variable exists as Object type (from clear-field), reassign to HashMap
                    sb.append(indent()).append(containerField).append(" = new HashMap<>();").append(NEWLINE);
                    // DON'T update declaredVarTypes - the Java declaration is still Object
                    // so subsequent operations need to cast
                    imports.add("java.util.HashMap");
                }
                // else: container is already Map, GenericValue, or other type - just use .put()

                // Check if this is a dynamic map access (key contains a field reference or FlexibleString)
                boolean isDynamicAccess = field.contains("[") || accessField.contains("${");
                String keyExpr;
                if (isDynamicAccess) {
                    // accessField is a field expression or FlexibleString like "${varName}" or "${invoice.partyIdFrom}"
                    if (accessField.startsWith("${") && accessField.endsWith("}")) {
                        // Extract variable/field expression from ${...}
                        String innerExpr = accessField.substring(2, accessField.length() - 1);
                        // If it contains a dot, it's a field access expression like invoice.partyIdFrom
                        if (innerExpr.contains(".")) {
                            keyExpr = "(String) " + buildFieldAccess(innerExpr);
                        } else {
                            String javaVar = toJavaFieldName(innerExpr);
                            // Check if the variable is declared as Object - if so, cast to String
                            String varType = declaredVarTypes.get(javaVar);
                            if ("Object".equals(varType) || !declaredVars.contains(javaVar)) {
                                keyExpr = "(String) " + buildFieldAccess(innerExpr);
                            } else {
                                keyExpr = javaVar;
                            }
                        }
                    } else {
                        // Dynamic access like map[field.subfield] - need String cast for the key
                        keyExpr = "(String) " + buildFieldAccess(accessField);
                    }
                } else {
                    // accessField is a simple string key
                    keyExpr = "\"" + accessField + "\"";
                }

                // Re-fetch containerType after potential update above
                containerType = declaredVarTypes.get(containerField);
                // Determine if we need to cast - cast if not GenericValue or Map
                boolean needsCast = !"GenericValue".equals(containerType) &&
                                    !"Map<String, Object>".equals(containerType);
                String putTarget = needsCast ? "((Map<String, Object>) " + containerField + ")" : containerField;

                // Use .put() - works for both Map and GenericValue
                if ("NewMap".equals(type)) {
                    sb.append(indent()).append(putTarget).append(".put(").append(keyExpr)
                            .append(", new HashMap<String, Object>());").append(NEWLINE);
                } else {
                    sb.append(indent()).append(putTarget).append(".put(").append(keyExpr)
                            .append(", ").append(fromExpr).append(");").append(NEWLINE);
                }
            } else {
                // Direct variable assignment
                String typeDecl = getTypeForField(type, field);

                // Override type to Object if variable is assigned from a script at any point
                // Script results are Object, so using a more specific type would cause type errors
                if (scriptAssignedVars.contains(javaField) && !"Object".equals(typeDecl)) {
                    typeDecl = "Object";
                }

                // Override type to GenericValue if variable is used with store/create/remove
                if (genericValueVars.contains(javaField) && "Object".equals(typeDecl)) {
                    typeDecl = "GenericValue";
                }

                boolean needsConversion = !"Object".equals(typeDecl);

                // Determine source type from fromField
                String sourceType = null;
                if (fromField != null && !fromField.contains(".")) {
                    sourceType = declaredVarTypes.get(toJavaFieldName(fromField));
                }

                if (!declaredVars.contains(javaField)) {
                    sb.append(indent()).append(typeDecl).append(" ").append(javaField).append(" = ");
                    declaredVars.add(javaField);
                    if (!"Object".equals(typeDecl)) {
                        declaredVarTypes.put(javaField, typeDecl);
                    }
                    if (needsConversion) {
                        sb.append(convertToType(fromExpr, typeDecl, sourceType));
                    } else {
                        sb.append(fromExpr);
                    }
                    sb.append(";").append(NEWLINE);
                } else {
                    sb.append(indent()).append(javaField).append(" = ");
                    // Get target type from declared variable or specified type
                    String knownType = declaredVarTypes.get(javaField);
                    String targetType = knownType != null ? knownType : typeDecl;
                    // Override type to GenericValue if variable is used with store/create/remove
                    if (genericValueVars.contains(javaField) && ("Object".equals(targetType) || targetType == null)) {
                        targetType = "GenericValue";
                    }
                    if (targetType != null && !"Object".equals(targetType)) {
                        sb.append(convertToType(fromExpr, targetType, sourceType));
                    } else {
                        sb.append(fromExpr);
                    }
                    sb.append(";").append(NEWLINE);
                }
            }
        } else if (isNewMapPattern) {
            // Property map pattern: create new HashMap
            if (containerField != null) {
                String containerType = declaredVarTypes.get(containerField);
                if (!declaredVars.contains(containerField)) {
                    sb.append(indent()).append("Map<String, Object> ").append(containerField)
                            .append(" = new HashMap<>();").append(NEWLINE);
                    declaredVars.add(containerField);
                    declaredVarTypes.put(containerField, "Map<String, Object>");
                    imports.add("java.util.Map");
                    imports.add("java.util.HashMap");
                } else if (containerType == null) {
                    // Object type - reassign to HashMap
                    sb.append(indent()).append(containerField).append(" = new HashMap<>();").append(NEWLINE);
                    // DON'T update declaredVarTypes - the Java declaration is still Object
                    imports.add("java.util.HashMap");
                }
                // Check if indexed map access
                boolean isIndexedAccess = field.contains("[");
                String keyExpr = isIndexedAccess ? "(String) " + buildFieldAccess(accessField) : "\"" + accessField + "\"";
                // Re-fetch containerType after potential update and determine if cast is needed
                containerType = declaredVarTypes.get(containerField);
                boolean needsCast = !"GenericValue".equals(containerType) &&
                                    !"Map<String, Object>".equals(containerType);
                String putTarget = needsCast ? "((Map<String, Object>) " + containerField + ")" : containerField;
                sb.append(indent()).append(putTarget).append(".put(").append(keyExpr)
                        .append(", new HashMap<String, Object>());").append(NEWLINE);
            } else {
                if (!declaredVars.contains(javaField)) {
                    sb.append(indent()).append("Map<String, Object> ").append(javaField)
                            .append(" = new HashMap<>();").append(NEWLINE);
                    declaredVars.add(javaField);
                    declaredVarTypes.put(javaField, "Map<String, Object>");
                } else {
                    sb.append(indent()).append(javaField).append(" = new HashMap<>();").append(NEWLINE);
                    // Don't update declaredVarTypes - keep the original declared type
                    // This ensures we cast properly if the variable was declared as Object
                }
            }
        } else if (isNotEmpty(value)) {
            // set field to literal value
            String javaValue = convertValueLiteral(value, type);
            if (containerField != null) {
                String containerType = declaredVarTypes.get(containerField);
                // Check if this is a list append pattern (empty brackets like "list[]")
                boolean isListAppend = field.endsWith("[]") && isEmpty(accessField);

                if (isListAppend) {
                    // List append pattern: list.add(value)
                    if (!declaredVars.contains(containerField)) {
                        sb.append(indent()).append("List<Object> ").append(containerField)
                                .append(" = new LinkedList<>();").append(NEWLINE);
                        declaredVars.add(containerField);
                        declaredVarTypes.put(containerField, "List<Object>");
                        imports.add("java.util.List");
                        imports.add("java.util.LinkedList");
                    }
                    containerType = declaredVarTypes.get(containerField);
                    boolean needsCast = containerType == null ||
                                        (!"List<Object>".equals(containerType) &&
                                         !containerType.startsWith("List"));
                    String addTarget = needsCast ? "((List<Object>) " + containerField + ")" : containerField;
                    sb.append(indent()).append(addTarget).append(".add(").append(javaValue).append(");").append(NEWLINE);
                } else {
                    // Map put pattern
                    if (!declaredVars.contains(containerField)) {
                        sb.append(indent()).append("Map<String, Object> ").append(containerField)
                                .append(" = new HashMap<>();").append(NEWLINE);
                        declaredVars.add(containerField);
                        declaredVarTypes.put(containerField, "Map<String, Object>");
                        imports.add("java.util.Map");
                        imports.add("java.util.HashMap");
                    } else if (containerType == null) {
                        // Object type - reassign to HashMap
                        sb.append(indent()).append(containerField).append(" = new HashMap<>();").append(NEWLINE);
                        declaredVarTypes.put(containerField, "Map<String, Object>");
                        imports.add("java.util.HashMap");
                    }
                    // Check if indexed map access
                    boolean isIndexedAccess = field.contains("[");
                    String keyExpr = isIndexedAccess ? "(String) " + buildFieldAccess(accessField) : "\"" + accessField + "\"";
                    // Re-fetch containerType after potential update and determine if cast is needed
                    containerType = declaredVarTypes.get(containerField);
                    boolean needsCast = !"GenericValue".equals(containerType) &&
                                        !"Map<String, Object>".equals(containerType);
                    String putTarget = needsCast ? "((Map<String, Object>) " + containerField + ")" : containerField;
                    sb.append(indent()).append(putTarget).append(".put(").append(keyExpr)
                            .append(", ").append(javaValue).append(");").append(NEWLINE);
                }
            } else {
                String typeDecl = getTypeForField(type, field);
                // Override type to Object if variable is assigned from a script at any point
                if (scriptAssignedVars.contains(javaField) && !"Object".equals(typeDecl)) {
                    typeDecl = "Object";
                }
                if (!declaredVars.contains(javaField)) {
                    sb.append(indent()).append(typeDecl).append(" ").append(javaField)
                            .append(" = ").append(javaValue).append(";").append(NEWLINE);
                    declaredVars.add(javaField);
                } else {
                    sb.append(indent()).append(javaField).append(" = ").append(javaValue).append(";").append(NEWLINE);
                }
            }
        } else if (isEmpty(value) && isEmpty(fromField)) {
            // Empty set without set-if-null - could be setting to null or empty string
            // In simple-methods this typically means set to null
            if (containerField != null) {
                String containerType = declaredVarTypes.get(containerField);
                if (!declaredVars.contains(containerField)) {
                    sb.append(indent()).append("Map<String, Object> ").append(containerField)
                            .append(" = new HashMap<>();").append(NEWLINE);
                    declaredVars.add(containerField);
                    declaredVarTypes.put(containerField, "Map<String, Object>");
                    imports.add("java.util.Map");
                    imports.add("java.util.HashMap");
                } else if (containerType == null) {
                    // Object type - reassign to HashMap
                    sb.append(indent()).append(containerField).append(" = new HashMap<>();").append(NEWLINE);
                    declaredVarTypes.put(containerField, "Map<String, Object>");
                    imports.add("java.util.HashMap");
                }
                // Check if this is an indexed map access (key contains a field reference)
                boolean isIndexedAccess = field.contains("[");
                String keyExpr;
                if (isIndexedAccess) {
                    // Dynamic access - cast to String for map key
                    keyExpr = "(String) " + buildFieldAccess(accessField);
                } else {
                    keyExpr = "\"" + accessField + "\"";
                }
                // Re-fetch containerType after potential update and determine if cast is needed
                containerType = declaredVarTypes.get(containerField);
                boolean needsCast = !"GenericValue".equals(containerType) &&
                                    !"Map<String, Object>".equals(containerType);
                String putTarget = needsCast ? "((Map<String, Object>) " + containerField + ")" : containerField;
                sb.append(indent()).append(putTarget).append(".put(").append(keyExpr)
                        .append(", null);").append(NEWLINE);
            } else {
                if (!declaredVars.contains(javaField)) {
                    sb.append(indent()).append("Object ").append(javaField).append(" = null;").append(NEWLINE);
                    declaredVars.add(javaField);
                } else {
                    sb.append(indent()).append(javaField).append(" = null;").append(NEWLINE);
                }
            }
        }

        return sb.toString();
    }

    /**
     * Converts a <call-service> element.
     */
    private String convertCallService(Element element) {
        StringBuilder sb = new StringBuilder();
        imports.add("org.ofbiz.service.ServiceUtil");

        String serviceName = getAttr(element, "service-name");
        String inMapName = getAttr(element, "in-map-name");

        // Get result-to-field mappings
        List<Element> resultToFields = childElementList(element, "result-to-field");
        // Get results-to-map mapping (stores entire result map to a named variable)
        Element resultsToMap = firstChildElement(element, "results-to-map");

        String resultVar = "serviceResult" + (declaredVars.contains("serviceResult") ? System.nanoTime() % 1000 : "");

        // Pre-declare result variables BEFORE the try block so they're accessible after
        // Handle results-to-map: declares a Map variable to hold service results
        String resultsMapVar = null;
        if (resultsToMap != null) {
            resultsMapVar = getAttr(resultsToMap, "map-name");
            if (isNotEmpty(resultsMapVar)) {
                String javaMapVar = toJavaFieldName(resultsMapVar);
                if (!declaredVars.contains(javaMapVar)) {
                    sb.append(indent()).append("Map<String, Object> ").append(javaMapVar).append(" = null;").append(NEWLINE);
                    declaredVars.add(javaMapVar);
                    declaredVarTypes.put(javaMapVar, "Map<String, Object>");
                    imports.add("java.util.Map");
                }
            }
        }

        // Pre-declare result-to-field variables before try block
        for (Element rtf : resultToFields) {
            String field = getAttr(rtf, "field");
            String resultName = getAttr(rtf, "result-name");
            // If field is not specified, default to result-name
            if (isEmpty(field)) {
                field = resultName;
            }
            String containerField = getContainerField(field);
            if (containerField == null) {
                String javaField = toJavaFieldName(field);
                if (!declaredVars.contains(javaField)) {
                    sb.append(indent()).append("Object ").append(javaField).append(" = null;").append(NEWLINE);
                    declaredVars.add(javaField);
                }
            } else {
                // Pre-declare container Map before try block
                if (!declaredVars.contains(containerField)) {
                    sb.append(indent()).append("Map<String, Object> ").append(containerField)
                            .append(" = new HashMap<>();").append(NEWLINE);
                    declaredVars.add(containerField);
                    declaredVarTypes.put(containerField, "Map<String, Object>");
                    imports.add("java.util.Map");
                    imports.add("java.util.HashMap");
                }
            }
        }

        sb.append(indent()).append("try {").append(NEWLINE);
        indentLevel++;

        // Use in-map-name if provided, cast to Map if needed for type safety
        String mapExpr;
        if (isNotEmpty(inMapName)) {
            String inMapType = declaredVarTypes.get(toJavaFieldName(inMapName));
            if (inMapType == null || "Object".equals(inMapType)) {
                // Cast to Map if type is unknown or Object
                mapExpr = "(Map<String, Object>) " + toJavaFieldName(inMapName);
            } else {
                mapExpr = toJavaFieldName(inMapName);
            }
        } else {
            mapExpr = "UtilMisc.toMap(\"userLogin\", userLogin, \"locale\", locale)";
        }
        sb.append(indent()).append("Map<String, Object> ").append(resultVar)
                .append(" = dispatcher.runSync(\"").append(serviceName).append("\", ").append(mapExpr).append(");").append(NEWLINE);

        // Check for errors
        sb.append(indent()).append("if (ServiceUtil.isError(").append(resultVar).append(")) {").append(NEWLINE);
        indentLevel++;
        sb.append(indent()).append("String errMsg = ServiceUtil.getErrorMessage(").append(resultVar).append(");").append(NEWLINE);
        sb.append(indent()).append("request.setAttribute(\"_ERROR_MESSAGE_\", errMsg);").append(NEWLINE);
        sb.append(indent()).append("return \"error\";").append(NEWLINE);
        indentLevel--;
        sb.append(indent()).append("}").append(NEWLINE);

        // Handle results-to-map: assign entire result map to variable
        if (resultsToMap != null && isNotEmpty(resultsMapVar)) {
            String javaMapVar = toJavaFieldName(resultsMapVar);
            sb.append(indent()).append(javaMapVar).append(" = ").append(resultVar).append(";").append(NEWLINE);
        }

        // Process result-to-field mappings
        for (Element rtf : resultToFields) {
            String resultName = getAttr(rtf, "result-name");
            String field = getAttr(rtf, "field");
            // If field is not specified, default to result-name
            if (isEmpty(field)) {
                field = resultName;
            }
            String javaField = toJavaFieldName(field);
            String containerField = getContainerField(field);
            String accessField = getAccessField(field);

            if (containerField != null) {
                // Ensure container is declared - create as empty Map if not
                if (!declaredVars.contains(containerField)) {
                    sb.append(indent()).append("Map<String, Object> ").append(containerField)
                            .append(" = new HashMap<>();").append(NEWLINE);
                    declaredVars.add(containerField);
                    declaredVarTypes.put(containerField, "Map<String, Object>");
                    imports.add("java.util.Map");
                    imports.add("java.util.HashMap");
                }
                // Check if container needs casting
                String containerType = declaredVarTypes.get(containerField);
                boolean needsCast = containerType == null ||
                                    (!"GenericValue".equals(containerType) && !"Map<String, Object>".equals(containerType));
                String putTarget = needsCast ? "((Map<String, Object>) " + containerField + ")" : containerField;
                sb.append(indent()).append(putTarget).append(".put(\"").append(accessField)
                        .append("\", ").append(resultVar).append(".get(\"").append(resultName).append("\"));").append(NEWLINE);
            } else {
                // Variable already declared before try, just assign with appropriate cast
                String varType = declaredVarTypes.get(javaField);
                String getExpr = resultVar + ".get(\"" + resultName + "\")";
                if (varType != null && !"Object".equals(varType)) {
                    // Cast to declared type
                    getExpr = "(" + varType + ") " + getExpr;
                }
                sb.append(indent()).append(javaField).append(" = ").append(getExpr).append(";").append(NEWLINE);
            }
        }

        indentLevel--;
        sb.append(indent()).append("} catch (Exception e) {").append(NEWLINE);
        indentLevel++;
        sb.append(indent()).append("Debug.logError(e, \"Error calling ").append(serviceName).append(": \" + e.getMessage(), MODULE);").append(NEWLINE);
        sb.append(indent()).append("request.setAttribute(\"_ERROR_MESSAGE_\", e.getMessage());").append(NEWLINE);
        sb.append(indent()).append("return \"error\";").append(NEWLINE);
        indentLevel--;
        sb.append(indent()).append("}").append(NEWLINE);

        return sb.toString();
    }

    /**
     * Converts a <call-simple-method> element.
     */
    private String convertCallSimpleMethod(Element element) {
        StringBuilder sb = new StringBuilder();

        String methodName = getAttr(element, "method-name");
        String xmlResource = getAttr(element, "xml-resource");

        // Special handling for known inline methods that set variables
        // getGlArithmeticSettingsInline sets: ledgerDecimals, roundingMode
        if ("getGlArithmeticSettingsInline".equals(methodName)) {
            sb.append(indent()).append("// getGlArithmeticSettingsInline: Load GL arithmetic settings").append(NEWLINE);
            if (!declaredVars.contains("ledgerDecimals")) {
                sb.append(indent()).append("Object ledgerDecimals = UtilProperties.getPropertyValue(\"arithmetic\", \"ledger.decimals\", \"4\");").append(NEWLINE);
                declaredVars.add("ledgerDecimals");
                declaredVarTypes.put("ledgerDecimals", "Object");
                imports.add("org.ofbiz.base.util.UtilProperties");
            }
            if (!declaredVars.contains("roundingMode")) {
                sb.append(indent()).append("Object roundingMode = UtilProperties.getPropertyValue(\"arithmetic\", \"ledger.rounding\", \"HalfUp\");").append(NEWLINE);
                declaredVars.add("roundingMode");
                declaredVarTypes.put("roundingMode", "Object");
            }
            sb.append(indent()).append("Debug.logInfo(\"Got settings from arithmetic.properties: ledgerDecimals=\" + ledgerDecimals + \", roundingMode=\" + roundingMode, MODULE);").append(NEWLINE);
            return sb.toString();
        }

        // getArithmeticSettingsInline sets: roundingDecimals, roundingMode (for finaccount)
        if ("getArithmeticSettingsInline".equals(methodName)) {
            sb.append(indent()).append("// getArithmeticSettingsInline: Load FinAccount arithmetic settings").append(NEWLINE);
            if (!declaredVars.contains("roundingDecimals")) {
                sb.append(indent()).append("Object roundingDecimals = UtilProperties.getPropertyValue(\"arithmetic\", \"finaccount.decimals\", \"2\");").append(NEWLINE);
                declaredVars.add("roundingDecimals");
                declaredVarTypes.put("roundingDecimals", "Object");
                imports.add("org.ofbiz.base.util.UtilProperties");
            }
            if (!declaredVars.contains("roundingMode")) {
                sb.append(indent()).append("Object roundingMode = UtilProperties.getPropertyValue(\"arithmetic\", \"finaccount.roundingSimpleMethod\", \"HalfUp\");").append(NEWLINE);
                declaredVars.add("roundingMode");
                declaredVarTypes.put("roundingMode", "Object");
            }
            sb.append(indent()).append("Debug.logVerbose(\"Got settings from arithmetic.properties: roundingDecimals=\" + roundingDecimals + \", roundingMode=\" + roundingMode, MODULE);").append(NEWLINE);
            return sb.toString();
        }

        // getGlAccountTypeDefaultInline sets: lookedUpValue (GenericValue)
        // The variable is hoisted at method level by convertMethod()
        if ("getGlAccountTypeDefaultInline".equals(methodName)) {
            sb.append(indent()).append("// getGlAccountTypeDefaultInline: Look up GlAccountTypeDefault").append(NEWLINE);
            imports.add("org.ofbiz.entity.util.EntityQuery");
            sb.append(indent()).append("try {").append(NEWLINE);
            indentLevel++;
            sb.append(indent()).append("lookedUpValue = EntityQuery.use(delegator)").append(NEWLINE);
            sb.append(indent()).append("        .from(\"GlAccountTypeDefault\")").append(NEWLINE);
            sb.append(indent()).append("        .where(UtilMisc.toMap(\"organizationPartyId\", context.get(\"organizationPartyId\"), \"glAccountTypeId\", context.get(\"glAccountTypeId\")))").append(NEWLINE);
            sb.append(indent()).append("        .cache()").append(NEWLINE);
            sb.append(indent()).append("        .queryOne();").append(NEWLINE);
            indentLevel--;
            sb.append(indent()).append("} catch (Exception e) {").append(NEWLINE);
            indentLevel++;
            sb.append(indent()).append("Debug.logError(e, \"Error querying GlAccountTypeDefault: \" + e.getMessage(), MODULE);").append(NEWLINE);
            indentLevel--;
            sb.append(indent()).append("}").append(NEWLINE);
            return sb.toString();
        }

        // For methods ending in "Inline", call them but don't check return (they're utility methods)
        if (methodName.endsWith("Inline") && isEmpty(xmlResource)) {
            sb.append(indent()).append(toJavaMethodName(methodName)).append("(request, response);").append(NEWLINE);
            return sb.toString();
        }

        // Determine result variable and whether to declare it
        // Note: If we're inside a nested block (indentLevel > 2), we always declare fresh
        // because Java has block scope and variables declared in one if-block aren't visible in sibling blocks
        String resultVar;
        boolean needsDeclaration;
        boolean inNestedBlock = indentLevel > 2;

        if (declaredVars.contains("result") && !inNestedBlock) {
            String resultType = declaredVarTypes.get("result");
            if ("Map<String, Object>".equals(resultType)) {
                // result is declared as Map - use different variable for inline call
                resultVar = "inlineResult";
                needsDeclaration = !declaredVars.contains(resultVar);
                if (needsDeclaration) {
                    declaredVars.add(resultVar);
                    declaredVarTypes.put(resultVar, "String");
                }
            } else {
                // result is declared as String - reuse it
                resultVar = "result";
                needsDeclaration = false;
            }
        } else {
            // result not declared yet OR we're in a nested block - declare it locally
            resultVar = "result";
            needsDeclaration = true;
            if (!inNestedBlock) {
                declaredVars.add(resultVar);
                declaredVarTypes.put(resultVar, "String");
            }
        }

        String resultDecl = needsDeclaration ? "String " + resultVar + " = " : resultVar + " = ";

        if (isEmpty(xmlResource)) {
            // Local method call
            sb.append(indent()).append(resultDecl).append(toJavaMethodName(methodName))
                    .append("(request, response);").append(NEWLINE);
            sb.append(indent()).append("if (!\"success\".equals(").append(resultVar).append(")) {").append(NEWLINE);
            indentLevel++;
            sb.append(indent()).append("return ").append(resultVar).append(";").append(NEWLINE);
            indentLevel--;
            sb.append(indent()).append("}").append(NEWLINE);
        } else {
            // External simple method call
            String targetClass = xmlResourceToClass.get(xmlResource);
            if (targetClass != null) {
                imports.add(targetClass);
                String simpleClassName = targetClass.substring(targetClass.lastIndexOf('.') + 1);
                sb.append(indent()).append(resultDecl).append(simpleClassName).append(".")
                        .append(toJavaMethodName(methodName)).append("(request, response);").append(NEWLINE);
                sb.append(indent()).append("if (!\"success\".equals(").append(resultVar).append(")) {").append(NEWLINE);
                indentLevel++;
                sb.append(indent()).append("return ").append(resultVar).append(";").append(NEWLINE);
                indentLevel--;
                sb.append(indent()).append("}").append(NEWLINE);
            } else {
                // Unknown resource - generate TODO
                sb.append(indent()).append("// TODO: Call simple-method \"").append(methodName)
                        .append("\" from \"").append(xmlResource).append("\"").append(NEWLINE);
                sb.append(indent()).append("// Original: call-simple-method method-name=\"").append(methodName)
                        .append("\" xml-resource=\"").append(xmlResource).append("\"").append(NEWLINE);
            }
        }

        return sb.toString();
    }

    /**
     * Converts a <check-errors> element.
     */
    private String convertCheckErrors(Element element) {
        StringBuilder sb = new StringBuilder();

        sb.append(indent()).append("if (UtilValidate.isNotEmpty(request.getAttribute(\"_ERROR_MESSAGE_\")) ||").append(NEWLINE);
        sb.append(indent()).append("        UtilValidate.isNotEmpty(request.getAttribute(\"_ERROR_MESSAGE_LIST_\"))) {").append(NEWLINE);
        indentLevel++;
        sb.append(indent()).append("return \"error\";").append(NEWLINE);
        indentLevel--;
        sb.append(indent()).append("}").append(NEWLINE);

        return sb.toString();
    }

    /**
     * Converts a <property-to-field> element.
     */
    private String convertPropertyToField(Element element) {
        StringBuilder sb = new StringBuilder();

        String field = getAttr(element, "field");
        String resource = getAttr(element, "resource");
        String property = getAttr(element, "property");

        String javaField = toJavaFieldName(field);

        if (!declaredVars.contains(javaField)) {
            sb.append(indent()).append("String ").append(javaField).append(" = UtilProperties.getMessage(\"")
                    .append(resource).append("\", \"").append(property).append("\", locale);").append(NEWLINE);
            declaredVars.add(javaField);
        } else {
            sb.append(indent()).append(javaField).append(" = UtilProperties.getMessage(\"")
                    .append(resource).append("\", \"").append(property).append("\", locale);").append(NEWLINE);
        }

        return sb.toString();
    }

    /**
     * Converts an <if-compare> element.
     */
    private String convertIfCompare(Element element) {
        StringBuilder sb = new StringBuilder();

        String field = getAttr(element, "field");
        String operator = getAttr(element, "operator");
        String value = getAttr(element, "value");
        String type = getAttr(element, "type");

        // Pre-scan both branches for variables that need to be hoisted
        Element elseElement = firstChildElement(element, "else");
        sb.append(hoistSharedVariables(element, elseElement));

        String condition;

        // Check if field is a complex EL expression (contains @or, @and, ==)
        // Example: ${payment.statusId == 'PMNT_SENT' @or payment.statusId == 'PMNT_RECEIVED'}
        if (field != null && field.startsWith("${") && field.endsWith("}") &&
            (field.contains("@or") || field.contains("@and") || field.contains("=="))) {
            // Parse the EL expression into Java boolean expression
            String elExpr = field.substring(2, field.length() - 1);
            String javaCondition = convertElBooleanExpression(elExpr);
            // Compare result with expected value
            if ("true".equals(value)) {
                condition = javaCondition;
            } else {
                condition = "!(" + javaCondition + ")";
            }
        } else {
            // Ensure container variables are declared before being accessed in the condition
            sb.append(ensureContainerDeclared(field));
            String fieldExpr = buildFieldAccess(field);
            condition = buildCompareCondition(fieldExpr, operator, value, type);
        }

        sb.append(indent()).append("if (").append(condition).append(") {").append(NEWLINE);
        indentLevel++;

        // Process direct children (not else)
        for (Element child : childElementList(element)) {
            if (!"else".equals(child.getNodeName())) {
                sb.append(convertElement(child));
            }
        }

        indentLevel--;

        // Handle else
        if (elseElement != null) {
            sb.append(indent()).append("} else {").append(NEWLINE);
            indentLevel++;
            for (Element child : childElementList(elseElement)) {
                sb.append(convertElement(child));
            }
            indentLevel--;
        }

        sb.append(indent()).append("}").append(NEWLINE);

        return sb.toString();
    }

    /**
     * Converts an <if-compare-field> element.
     */
    private String convertIfCompareField(Element element) {
        StringBuilder sb = new StringBuilder();

        String field = getAttr(element, "field");
        String operator = getAttr(element, "operator");
        String toField = getAttr(element, "to-field");

        // Pre-scan both branches for variables that need to be hoisted
        Element elseElement = firstChildElement(element, "else");
        sb.append(hoistSharedVariables(element, elseElement));

        // Ensure container variables are declared before being accessed in the condition
        sb.append(ensureContainerDeclared(field));
        sb.append(ensureContainerDeclared(toField));

        String fieldExpr = buildFieldAccess(field);
        String toFieldExpr = buildFieldAccess(toField);
        String condition = buildFieldCompareCondition(fieldExpr, operator, toFieldExpr);

        sb.append(indent()).append("if (").append(condition).append(") {").append(NEWLINE);
        indentLevel++;

        for (Element child : childElementList(element)) {
            if (!"else".equals(child.getNodeName())) {
                sb.append(convertElement(child));
            }
        }

        indentLevel--;

        if (elseElement != null) {
            sb.append(indent()).append("} else {").append(NEWLINE);
            indentLevel++;
            for (Element child : childElementList(elseElement)) {
                sb.append(convertElement(child));
            }
            indentLevel--;
        }

        sb.append(indent()).append("}").append(NEWLINE);

        return sb.toString();
    }

    /**
     * Converts an <if-empty> element.
     */
    private String convertIfEmpty(Element element) {
        StringBuilder sb = new StringBuilder();

        String field = getAttr(element, "field");

        // Pre-scan both branches for variables that need to be hoisted
        Element elseElement = firstChildElement(element, "else");
        sb.append(hoistSharedVariables(element, elseElement));

        // Ensure container variables are declared before being accessed in the condition
        sb.append(ensureContainerDeclared(field));

        String fieldExpr = buildFieldAccess(field);
        sb.append(indent()).append("if (UtilValidate.isEmpty(").append(fieldExpr).append(")) {").append(NEWLINE);
        indentLevel++;

        for (Element child : childElementList(element)) {
            if (!"else".equals(child.getNodeName())) {
                sb.append(convertElement(child));
            }
        }

        indentLevel--;

        if (elseElement != null) {
            sb.append(indent()).append("} else {").append(NEWLINE);
            indentLevel++;
            for (Element child : childElementList(elseElement)) {
                sb.append(convertElement(child));
            }
            indentLevel--;
        }

        sb.append(indent()).append("}").append(NEWLINE);

        return sb.toString();
    }

    /**
     * Converts an <if-not-empty> element.
     */
    private String convertIfNotEmpty(Element element) {
        StringBuilder sb = new StringBuilder();

        String field = getAttr(element, "field");

        // Pre-scan both branches for variables that need to be hoisted
        Element elseElement = firstChildElement(element, "else");
        sb.append(hoistSharedVariables(element, elseElement));

        // Ensure container variables are declared before being accessed in the condition
        sb.append(ensureContainerDeclared(field));

        String fieldExpr = buildFieldAccess(field);
        sb.append(indent()).append("if (UtilValidate.isNotEmpty(").append(fieldExpr).append(")) {").append(NEWLINE);
        indentLevel++;

        for (Element child : childElementList(element)) {
            if (!"else".equals(child.getNodeName())) {
                sb.append(convertElement(child));
            }
        }

        indentLevel--;

        if (elseElement != null) {
            sb.append(indent()).append("} else {").append(NEWLINE);
            indentLevel++;
            for (Element child : childElementList(elseElement)) {
                sb.append(convertElement(child));
            }
            indentLevel--;
        }

        sb.append(indent()).append("}").append(NEWLINE);

        return sb.toString();
    }

    /**
     * Pre-scans both branches of an if-else and hoists variable declarations
     * for variables that are set in either branch (needed because they may be used after the if-else).
     */
    private String hoistSharedVariables(Element ifElement, Element elseElement) {
        StringBuilder sb = new StringBuilder();
        if (elseElement == null) {
            return sb.toString();
        }

        // Find all variable declarations in if branch (excluding nested else)
        Map<String, String> ifBranchTypes = new HashMap<>();
        Set<String> ifBranchVars = new HashSet<>();
        for (Element child : childElementList(ifElement)) {
            if (!"else".equals(child.getNodeName())) {
                collectSetVariables(child, ifBranchVars, ifBranchTypes);
            }
        }

        // Find all variable declarations in else branch
        Map<String, String> elseBranchTypes = new HashMap<>();
        Set<String> elseBranchVars = new HashSet<>();
        for (Element child : childElementList(elseElement)) {
            collectSetVariables(child, elseBranchVars, elseBranchTypes);
        }

        // Collect all variables from both branches and their types
        Map<String, String> allVars = new LinkedHashMap<>();
        for (String varName : ifBranchVars) {
            allVars.put(varName, ifBranchTypes.getOrDefault(varName, "Object"));
        }
        for (String varName : elseBranchVars) {
            if (!allVars.containsKey(varName)) {
                allVars.put(varName, elseBranchTypes.getOrDefault(varName, "Object"));
            }
        }

        // Pre-declare all variables from either branch that are not yet declared
        for (Map.Entry<String, String> entry : allVars.entrySet()) {
            String varName = entry.getKey();
            if (!declaredVars.contains(varName)) {
                String type = entry.getValue();
                String javaType = getTypeForField(type, varName);
                sb.append(indent()).append(javaType).append(" ").append(varName).append(" = null;").append(NEWLINE);
                declaredVars.add(varName);
                declaredVarTypes.put(varName, javaType);
            }
        }

        return sb.toString();
    }

    /**
     * Pre-scans the method body for variables that are defined inside scoped blocks
     * (if, iterate, while, etc.). In mini-lang, all variables are method-scoped,
     * so we need to hoist them to be visible throughout the method in Java.
     */
    private String hoistSequentialIfVariables(Element methodElement) {
        StringBuilder sb = new StringBuilder();

        // genericValueVars is populated at method start in convertMethod()
        // and contains all variables that must be typed as GenericValue

        List<Element> children = childElementList(methodElement);

        // Collect variables from ALL scoped blocks in the method
        // Scoped blocks: if-*, if, iterate, while, iterate-map
        Map<String, String> varsToHoist = new LinkedHashMap<>();

        // Collect iterate entry variable names that are ONLY used as iterate entries
        // (not also used by first-from-list or other elements)
        Set<String> pureIterateEntryVars = new HashSet<>();
        Set<String> firstFromListEntryVars = new HashSet<>();
        collectPureIterateEntryVars(methodElement, pureIterateEntryVars, firstFromListEntryVars);
        // Remove any iterate entry vars that are also used by first-from-list
        pureIterateEntryVars.removeAll(firstFromListEntryVars);
        // Remove any iterate entry vars that are used OUTSIDE their iterate block
        Set<String> iterateVarsUsedOutside = findIterateEntryVarsUsedOutside(methodElement);
        pureIterateEntryVars.removeAll(iterateVarsUsedOutside);

        for (Element child : children) {
            String nodeName = child.getNodeName();
            // Check for any element that creates a new scope in Java
            if (nodeName.startsWith("if-") || "if".equals(nodeName) ||
                "iterate".equals(nodeName) || "while".equals(nodeName) ||
                "iterate-map".equals(nodeName)) {

                Map<String, String> vars = new HashMap<>();
                Set<String> varSet = new HashSet<>();
                collectSetVariables(child, varSet, vars);

                // Store the variable names and types
                for (String varName : varSet) {
                    // Skip pure iterate entry variables - they're declared by for-each loops
                    // But DON'T skip if also used by first-from-list
                    if (pureIterateEntryVars.contains(varName)) {
                        continue;
                    }
                    // Keep the first type we see for each variable, but prefer more specific types
                    String newType = vars.getOrDefault(varName, "Object");
                    // Force GenericValue type for variables used with store/create/remove
                    if (genericValueVars.contains(varName)) {
                        newType = "GenericValue";
                    }
                    if (!varsToHoist.containsKey(varName)) {
                        varsToHoist.put(varName, newType);
                    } else {
                        // Prefer List<GenericValue> over List<Object>
                        String existingType = varsToHoist.get(varName);
                        if ("List<Object>".equals(existingType) && "List<GenericValue>".equals(newType)) {
                            varsToHoist.put(varName, newType);
                        }
                        // Prefer GenericValue over Object
                        if ("Object".equals(existingType) && "GenericValue".equals(newType)) {
                            varsToHoist.put(varName, newType);
                        }
                    }
                }
            }
        }

        // Hoist ALL variables defined in scoped blocks
        for (Map.Entry<String, String> entry : varsToHoist.entrySet()) {
            String varName = entry.getKey();
            if (!declaredVars.contains(varName)) {
                String type = entry.getValue();
                String javaType = getTypeForField(type, varName);
                sb.append(indent()).append(javaType).append(" ").append(varName).append(" = null;").append(NEWLINE);
                declaredVars.add(varName);
                declaredVarTypes.put(varName, javaType);
                methodLevelVars.add(varName);  // Track as method-level var
                // Track if this was an iterate entry var hoisted for external use
                if (iterateVarsUsedOutside.contains(varName)) {
                    hoistedIterateEntryVars.add(varName);
                }
            }
        }

        return sb.toString();
    }

    /**
     * Recursively collects iterate entry variable names and first-from-list entry names.
     * This helps distinguish variables that are ONLY iterate entries (declared by for-each)
     * from those that are also used by first-from-list (need to be hoisted).
     */
    private void collectPureIterateEntryVars(Element element, Set<String> iterateEntryVars, Set<String> firstFromListVars) {
        String nodeName = element.getNodeName();
        if ("iterate".equals(nodeName)) {
            String entry = getAttr(element, "entry");
            if (entry != null) {
                iterateEntryVars.add(toJavaFieldName(entry));
            }
        } else if ("iterate-map".equals(nodeName)) {
            String key = getAttr(element, "key");
            String value = getAttr(element, "value");
            if (key != null) iterateEntryVars.add(toJavaFieldName(key));
            if (value != null) iterateEntryVars.add(toJavaFieldName(value));
        } else if ("first-from-list".equals(nodeName)) {
            String entry = getAttr(element, "entry");
            if (entry != null) {
                firstFromListVars.add(toJavaFieldName(entry));
            }
        }
        // Recurse into children
        for (Element child : childElementList(element)) {
            collectPureIterateEntryVars(child, iterateEntryVars, firstFromListVars);
        }
    }

    /**
     * Finds iterate entry vars that are used OUTSIDE their iterate block.
     * These must be hoisted to method level.
     */
    private Set<String> findIterateEntryVarsUsedOutside(Element methodElement) {
        Set<String> varsUsedOutside = new HashSet<>();
        findIterateEntryVarsUsedOutsideRecursive(methodElement, varsUsedOutside);
        return varsUsedOutside;
    }

    private void findIterateEntryVarsUsedOutsideRecursive(Element element, Set<String> varsUsedOutside) {
        List<Element> children = childElementList(element);
        for (int i = 0; i < children.size(); i++) {
            Element child = children.get(i);
            String nodeName = child.getNodeName();

            if ("iterate".equals(nodeName)) {
                String entry = getAttr(child, "entry");
                if (entry != null) {
                    String javaEntry = toJavaFieldName(entry);
                    // Check all siblings AFTER this iterate for references to the entry variable
                    for (int j = i + 1; j < children.size(); j++) {
                        if (elementUsesVariable(children.get(j), javaEntry)) {
                            varsUsedOutside.add(javaEntry);
                            break;
                        }
                    }
                }
            }

            // Recurse into this child to check nested iterates
            findIterateEntryVarsUsedOutsideRecursive(child, varsUsedOutside);
        }
    }

    /**
     * Checks if an element or any of its descendants uses the given variable name.
     */
    private boolean elementUsesVariable(Element element, String varName) {
        // Check common attributes that reference variables
        String[] attrNames = {"field", "from-field", "value-field", "list", "map", "entry", "to-field", "from"};
        for (String attrName : attrNames) {
            String attrValue = getAttr(element, attrName);
            if (attrValue != null) {
                // Check if the attribute references this variable
                String baseVar = attrValue.split("\\.")[0].split("\\[")[0];
                if (toJavaFieldName(baseVar).equals(varName)) {
                    return true;
                }
            }
        }

        // Check in value attribute for ${...} expressions
        String value = getAttr(element, "value");
        if (value != null && value.contains("${" + varName + "}")) {
            return true;
        }

        // Recurse into children
        for (Element child : childElementList(element)) {
            if (elementUsesVariable(child, varName)) {
                return true;
            }
        }
        return false;
    }

    /**
     * Recursively collects variables that must be typed as GenericValue.
     * This includes variables that:
     * - Are created by make-value, entity-one, find-by-primary-key
     * - Are used by store-value, create-value, remove-value
     */
    private void collectGenericValueVars(Element element, Set<String> genericValueVars) {
        String nodeName = element.getNodeName();
        // Variables created as GenericValue
        if ("make-value".equals(nodeName) || "entity-one".equals(nodeName) ||
            "find-by-primary-key".equals(nodeName)) {
            String valueField = getAttr(element, "value-field");
            if (valueField != null && !valueField.contains(".")) {
                genericValueVars.add(toJavaFieldName(valueField));
            }
        }
        // Variables USED with entity operations - these must be GenericValue
        else if ("store-value".equals(nodeName) || "create-value".equals(nodeName) ||
                 "remove-value".equals(nodeName)) {
            String valueField = getAttr(element, "value-field");
            if (valueField != null && !valueField.contains(".")) {
                genericValueVars.add(toJavaFieldName(valueField));
            }
        }
        // Clone value creates GenericValue
        else if ("clone-value".equals(nodeName)) {
            String newValueField = getAttr(element, "new-value-field");
            if (newValueField != null && !newValueField.contains(".")) {
                genericValueVars.add(toJavaFieldName(newValueField));
            }
        }
        // Recurse into children
        for (Element child : childElementList(element)) {
            collectGenericValueVars(child, genericValueVars);
        }
    }

    /**
     * Recursively collects variable names that are assigned from script values or converted via to-string.
     * These variables must be typed as Object since they hold values of different types.
     */
    private void collectScriptAssignedVars(Element element, Set<String> scriptVars) {
        String nodeName = element.getNodeName();

        if ("set".equals(nodeName)) {
            String field = getAttr(element, "field");
            String value = getAttr(element, "value");
            // Check if value is a script
            if (field != null && !field.contains(".") && !field.contains("[") && value != null) {
                boolean isScriptValue = value.startsWith("script:") || value.startsWith("groovy:") || value.startsWith("bsh:");
                // Also check for ${groovy:...} or ${script:...} patterns
                if (!isScriptValue && value.contains("${")) {
                    isScriptValue = value.contains("${groovy:") || value.contains("${script:") || value.contains("${bsh:");
                }
                if (isScriptValue) {
                    scriptVars.add(toJavaFieldName(field));
                }
            }
        } else if ("to-string".equals(nodeName)) {
            // to-string converts a field in-place to String type
            // The variable must be Object to hold both the original type and String
            String field = getAttr(element, "field");
            if (field != null && !field.contains(".")) {
                scriptVars.add(toJavaFieldName(field));
            }
        }
        // Recurse into children
        for (Element child : childElementList(element)) {
            collectScriptAssignedVars(child, scriptVars);
        }
    }

    /**
     * Recursively collects variable names from elements that declare variables.
     */
    private void collectSetVariables(Element element, Set<String> vars, Map<String, String> types) {
        String nodeName = element.getNodeName();

        if ("set".equals(nodeName)) {
            String field = getAttr(element, "field");
            String type = getAttr(element, "type");
            String value = getAttr(element, "value");
            // Collect simple variable names (not map.key patterns)
            if (field != null && !field.contains(".") && !field.contains("[")) {
                String javaField = toJavaFieldName(field);
                vars.add(javaField);
                // Check if value is a script - script results are Object type
                boolean isScriptValue = value != null && (value.startsWith("script:") || value.startsWith("groovy:") || value.startsWith("bsh:"));
                if ("NewMap".equals(type)) {
                    types.put(javaField, "NewMap");
                } else if ("NewList".equals(type)) {
                    types.put(javaField, "NewList");
                } else if (isScriptValue) {
                    // Script values return Object - if variable already has a different type, downgrade to Object
                    String existingType = types.get(javaField);
                    if (existingType != null && !"Object".equals(existingType)) {
                        types.put(javaField, "Object");
                    } else if (existingType == null) {
                        types.put(javaField, "Object");
                    }
                } else if (isNotEmpty(type)) {
                    // Only set specific type if not already set to Object (script downgrade)
                    String existingType = types.get(javaField);
                    if (!"Object".equals(existingType)) {
                        types.put(javaField, type);
                    }
                }
            } else if (field != null && field.endsWith("[]")) {
                // List append pattern: list[] means append to list
                String listName = field.substring(0, field.length() - 2);
                if (!listName.contains(".")) {
                    String javaField = toJavaFieldName(listName);
                    if (!vars.contains(javaField)) {
                        vars.add(javaField);
                        types.put(javaField, "List<Object>");
                    }
                }
            } else if (field != null && field.contains(".")) {
                // Also collect container variables from map.key patterns (e.g., "someMap.keyName")
                String containerField = getContainerField(field);
                if (containerField != null) {
                    String javaContainer = toJavaFieldName(containerField);
                    if (!vars.contains(javaContainer)) {
                        vars.add(javaContainer);
                        types.put(javaContainer, "NewMap");
                    }
                    // Don't override GenericValue type with NewMap - GenericValue also supports .put()
                    // If the container is already known to be GenericValue, keep that type
                }
            }
        } else if ("set-service-fields".equals(nodeName)) {
            String toMap = getAttr(element, "to-map");
            if (toMap != null && !toMap.contains(".")) {
                String javaField = toJavaFieldName(toMap);
                vars.add(javaField);
                types.put(javaField, "NewMap");
            }
        } else if ("now-timestamp".equals(nodeName)) {
            String field = getAttr(element, "field");
            if (field != null && !field.contains(".")) {
                String javaField = toJavaFieldName(field);
                vars.add(javaField);
                types.put(javaField, "Timestamp");
            }
        } else if ("now".equals(nodeName)) {
            String field = getAttr(element, "field");
            if (field != null && !field.contains(".")) {
                String javaField = toJavaFieldName(field);
                vars.add(javaField);
                types.put(javaField, "Timestamp");
            }
        } else if ("entity-one".equals(nodeName) || "find-by-primary-key".equals(nodeName)) {
            String field = getAttr(element, "value-field");
            if (field != null && !field.contains(".")) {
                String javaField = toJavaFieldName(field);
                vars.add(javaField);
                types.put(javaField, "GenericValue");
            }
        } else if ("entity-and".equals(nodeName) || "entity-condition".equals(nodeName) || "get-related".equals(nodeName)) {
            String field = getAttr(element, "list");
            if (field != null && !field.contains(".")) {
                String javaField = toJavaFieldName(field);
                vars.add(javaField);
                types.put(javaField, "List<GenericValue>");
            }
        } else if ("get-related-one".equals(nodeName)) {
            String field = getAttr(element, "to-value-field");
            if (field != null && !field.contains(".")) {
                String javaField = toJavaFieldName(field);
                vars.add(javaField);
                types.put(javaField, "GenericValue");
            }
        } else if ("make-value".equals(nodeName)) {
            String field = getAttr(element, "value-field");
            if (field != null && !field.contains(".")) {
                String javaField = toJavaFieldName(field);
                vars.add(javaField);
                types.put(javaField, "GenericValue");
            }
        } else if ("call-service".equals(nodeName)) {
            // Check for result-to-field declarations
            for (Element resultToField : childElementList(element, "result-to-field")) {
                String field = getAttr(resultToField, "field");
                if (isEmpty(field)) field = getAttr(resultToField, "result-name");
                if (field != null && !field.contains(".")) {
                    String javaField = toJavaFieldName(field);
                    vars.add(javaField);
                    // result-to-field types are generally Object
                }
            }
        } else if ("sequenced-id".equals(nodeName)) {
            String field = getAttr(element, "field");
            if (field != null && !field.contains(".")) {
                String javaField = toJavaFieldName(field);
                vars.add(javaField);
                types.put(javaField, "String");
            }
        } else if ("first-from-list".equals(nodeName)) {
            String entry = getAttr(element, "entry");
            if (entry != null && !entry.contains(".")) {
                String javaField = toJavaFieldName(entry);
                vars.add(javaField);
                types.put(javaField, "GenericValue");
            }
        } else if ("string-to-list".equals(nodeName)) {
            String list = getAttr(element, "list");
            if (list != null && !list.contains(".")) {
                String javaField = toJavaFieldName(list);
                vars.add(javaField);
                types.put(javaField, "List<String>");
            }
        } else if ("entity-count".equals(nodeName)) {
            String countField = getAttr(element, "count-field");
            if (countField != null && !countField.contains(".")) {
                String javaField = toJavaFieldName(countField);
                vars.add(javaField);
                types.put(javaField, "Long");
            }
        } else if ("calculate".equals(nodeName)) {
            String field = getAttr(element, "field");
            if (field != null && !field.contains(".")) {
                String javaField = toJavaFieldName(field);
                vars.add(javaField);
                String type = getAttr(element, "type");
                types.put(javaField, "BigDecimal".equals(type) ? "BigDecimal" : "Object");
            }
        } else if ("session-to-field".equals(nodeName) || "request-to-field".equals(nodeName)) {
            String field = getAttr(element, "field");
            if (field != null && !field.contains(".")) {
                String javaField = toJavaFieldName(field);
                vars.add(javaField);
                // Session/request values are generally Object
            }
        } else if ("filter-list-by-date".equals(nodeName)) {
            String toList = getAttr(element, "to-list");
            if (toList != null && !toList.contains(".")) {
                String javaField = toJavaFieldName(toList);
                vars.add(javaField);
                types.put(javaField, "List<GenericValue>");
            }
        } else if ("map-to-map".equals(nodeName)) {
            String toMap = getAttr(element, "to-map");
            if (toMap != null && !toMap.contains(".")) {
                String javaField = toJavaFieldName(toMap);
                vars.add(javaField);
                types.put(javaField, "Map<String, Object>");
            }
        } else if ("field-to-list".equals(nodeName)) {
            String list = getAttr(element, "list");
            if (list != null) {
                // Convert dotted paths to underscore-joined names (toJavaFieldName handles this)
                // E.g., createAcctgTransAndEntriesMap.acctgTransEntries -> createAcctgTransAndEntriesMap_acctgTransEntries
                String javaField = toJavaFieldName(list);
                vars.add(javaField);
                // Only set List<Object> if no more specific list type exists
                String existingType = types.get(javaField);
                if (existingType == null || (!existingType.startsWith("List<") || "List<Object>".equals(existingType))) {
                    types.put(javaField, "List<Object>");
                }
                // If List<GenericValue> or List<String> already set, keep that more specific type
            }
        } else if ("clear-field".equals(nodeName)) {
            String field = getAttr(element, "field");
            if (field != null && !field.contains(".")) {
                String javaField = toJavaFieldName(field);
                vars.add(javaField);
                // clear-field sets to null, type unknown
            }
        } else if ("property-to-field".equals(nodeName)) {
            String field = getAttr(element, "field");
            if (field != null && !field.contains(".")) {
                String javaField = toJavaFieldName(field);
                vars.add(javaField);
                types.put(javaField, "String");
            }
        } else if ("call-class-method".equals(nodeName) || "call-object-method".equals(nodeName)) {
            String retField = getAttr(element, "ret-field");
            if (retField != null && !retField.contains(".")) {
                String javaField = toJavaFieldName(retField);
                vars.add(javaField);
                // ret-field type is typically Object unless we can infer it
            }
        } else if ("clone-value".equals(nodeName)) {
            String newValueField = getAttr(element, "new-value-field");
            if (newValueField != null && !newValueField.contains(".")) {
                String javaField = toJavaFieldName(newValueField);
                vars.add(javaField);
                types.put(javaField, "GenericValue");
            }
        } else if ("call-service".equals(nodeName)) {
            // Check for result-to-field declarations
            for (Element resultToField : childElementList(element, "result-to-field")) {
                String field = getAttr(resultToField, "field");
                if (isEmpty(field)) field = getAttr(resultToField, "result-name");
                if (field != null) {
                    if (!field.contains(".")) {
                        String javaField = toJavaFieldName(field);
                        vars.add(javaField);
                        // result-to-field types are generally Object
                    } else {
                        // For dotted paths like "payment.paymentId", collect the container
                        String containerField = getContainerField(field);
                        if (containerField != null && !vars.contains(containerField)) {
                            vars.add(containerField);
                            types.put(containerField, "NewMap");
                        }
                    }
                }
            }
        } else if ("iterate".equals(nodeName)) {
            // Collect iterate entry variable - needed if it's used outside the loop
            String entry = getAttr(element, "entry");
            String list = getAttr(element, "list");
            if (entry != null && !entry.contains(".")) {
                String javaField = toJavaFieldName(entry);
                vars.add(javaField);
                // Determine entry type based on list name heuristics
                String listVar = list != null ? toJavaFieldName(list.split("\\.")[0]) : null;
                String listType = listVar != null ? types.get(listVar) : null;
                String entryType = "GenericValue";  // Default
                if (listType != null && listType.startsWith("List<")) {
                    // Extract element type from List<ElementType>
                    int startIdx = listType.indexOf('<') + 1;
                    int endIdx = listType.lastIndexOf('>');
                    if (startIdx > 0 && endIdx > startIdx) {
                        entryType = listType.substring(startIdx, endIdx);
                    }
                } else if (list != null && (list.contains("error") || list.contains("Error"))) {
                    // Error lists typically contain strings
                    entryType = "String";
                }
                types.put(javaField, entryType);
            }
        }

        // Recurse into children
        for (Element child : childElementList(element)) {
            collectSetVariables(child, vars, types);
        }
    }

    /**
     * Converts an <if> element with condition/then/else structure.
     */
    private String convertIf(Element element) {
        StringBuilder sb = new StringBuilder();

        Element conditionElement = firstChildElement(element, "condition");
        Element thenElement = firstChildElement(element, "then");
        Element elseElement = firstChildElement(element, "else");

        if (conditionElement != null) {
            // Pre-scan condition to ensure all container variables are declared
            sb.append(ensureConditionContainersDeclared(conditionElement));

            // Hoist variable declarations from both branches before the if statement
            sb.append(hoistIfElseVariables(thenElement, elseElement));

            String condition = convertCondition(conditionElement);
            sb.append(indent()).append("if (").append(condition).append(") {").append(NEWLINE);
            indentLevel++;

            if (thenElement != null) {
                for (Element child : childElementList(thenElement)) {
                    sb.append(convertElement(child));
                }
            }

            indentLevel--;

            if (elseElement != null) {
                sb.append(indent()).append("} else {").append(NEWLINE);
                indentLevel++;
                for (Element child : childElementList(elseElement)) {
                    sb.append(convertElement(child));
                }
                indentLevel--;
            }

            sb.append(indent()).append("}").append(NEWLINE);
        }

        return sb.toString();
    }

    /**
     * Converts a condition element to a Java boolean expression.
     */
    private String convertCondition(Element conditionElement) {
        Element child = firstChildElement(conditionElement);
        if (child == null) return "true";

        return convertConditionElement(child);
    }

    /**
     * Converts a single condition element.
     */
    private String convertConditionElement(Element element) {
        String tagName = element.getNodeName();

        switch (tagName) {
            case "and":
                return convertAndCondition(element);
            case "or":
                return convertOrCondition(element);
            case "not":
                return convertNotCondition(element);
            case "if-compare":
                return convertIfCompareCondition(element);
            case "if-compare-field":
                return convertIfCompareFieldCondition(element);
            case "if-empty":
                return "UtilValidate.isEmpty(" + buildFieldAccess(getAttr(element, "field")) + ")";
            case "if-not-empty":
                return "UtilValidate.isNotEmpty(" + buildFieldAccess(getAttr(element, "field")) + ")";
            default:
                return "true /* TODO: " + tagName + " */";
        }
    }

    private String convertAndCondition(Element element) {
        List<String> conditions = new ArrayList<>();
        for (Element child : childElementList(element)) {
            conditions.add(convertConditionElement(child));
        }
        return "(" + String.join(" && ", conditions) + ")";
    }

    private String convertOrCondition(Element element) {
        List<String> conditions = new ArrayList<>();
        for (Element child : childElementList(element)) {
            conditions.add(convertConditionElement(child));
        }
        return "(" + String.join(" || ", conditions) + ")";
    }

    private String convertNotCondition(Element element) {
        Element child = firstChildElement(element);
        if (child != null) {
            return "!(" + convertConditionElement(child) + ")";
        }
        return "true";
    }

    private String convertIfCompareCondition(Element element) {
        String field = getAttr(element, "field");
        String operator = getAttr(element, "operator");
        String value = getAttr(element, "value");
        String type = getAttr(element, "type");
        return buildCompareCondition(buildFieldAccess(field), operator, value, type);
    }

    private String convertIfCompareFieldCondition(Element element) {
        String field = getAttr(element, "field");
        String operator = getAttr(element, "operator");
        String toField = getAttr(element, "to-field");
        return buildFieldCompareCondition(buildFieldAccess(field), operator, buildFieldAccess(toField));
    }

    /**
     * Converts an <entity-one> element.
     */
    private String convertEntityOne(Element element) {
        StringBuilder sb = new StringBuilder();
        imports.add("org.ofbiz.entity.util.EntityQuery");

        String entityName = getAttr(element, "entity-name");
        String valueField = getAttr(element, "value-field");
        boolean useCache = parseBoolean(getAttr(element, "use-cache"), false);

        String javaField = toJavaFieldName(valueField);

        // Build field-map conditions
        List<Element> fieldMaps = childElementList(element, "field-map");
        StringBuilder whereBuilder = new StringBuilder();
        whereBuilder.append("UtilMisc.toMap(");
        boolean first = true;
        for (Element fm : fieldMaps) {
            if (!first) whereBuilder.append(", ");
            String fieldName = getAttr(fm, "field-name");
            String fromField = getAttr(fm, "from-field");
            String value = getAttr(fm, "value");
            whereBuilder.append("\"").append(fieldName).append("\", ");
            if (isNotEmpty(fromField)) {
                whereBuilder.append(buildFieldAccess(fromField));
            } else if (isNotEmpty(value)) {
                whereBuilder.append("\"").append(escapeString(value)).append("\"");
            } else {
                // When only field-name is specified, use it as both key and variable name
                whereBuilder.append(toJavaFieldName(fieldName));
            }
            first = false;
        }
        whereBuilder.append(")");

        // Declare variable before try block to ensure proper scope
        if (!declaredVars.contains(javaField)) {
            sb.append(indent()).append("GenericValue ").append(javaField).append(" = null;").append(NEWLINE);
            declaredVars.add(javaField);
        }
        // Always track type as GenericValue to prevent HashMap reassignment
        declaredVarTypes.put(javaField, "GenericValue");

        sb.append(indent()).append("try {").append(NEWLINE);
        indentLevel++;

        sb.append(indent()).append(javaField).append(" = EntityQuery.use(delegator)").append(NEWLINE);
        sb.append(indent()).append("        .from(\"").append(entityName).append("\")").append(NEWLINE);
        sb.append(indent()).append("        .where(").append(whereBuilder).append(")").append(NEWLINE);
        if (useCache) {
            sb.append(indent()).append("        .cache()").append(NEWLINE);
        }
        sb.append(indent()).append("        .queryOne();").append(NEWLINE);

        indentLevel--;
        sb.append(indent()).append("} catch (Exception e) {").append(NEWLINE);
        indentLevel++;
        sb.append(indent()).append("Debug.logError(e, \"Error querying ").append(entityName).append(": \" + e.getMessage(), MODULE);").append(NEWLINE);
        sb.append(indent()).append("request.setAttribute(\"_ERROR_MESSAGE_\", e.getMessage());").append(NEWLINE);
        sb.append(indent()).append("return \"error\";").append(NEWLINE);
        indentLevel--;
        sb.append(indent()).append("}").append(NEWLINE);

        return sb.toString();
    }

    /**
     * Converts an <entity-and> element.
     */
    private String convertEntityAnd(Element element) {
        StringBuilder sb = new StringBuilder();
        imports.add("org.ofbiz.entity.util.EntityQuery");

        String entityName = getAttr(element, "entity-name");
        String list = getAttr(element, "list");
        boolean filterByDate = parseBoolean(getAttr(element, "filter-by-date"), false);

        String javaField = toJavaFieldName(list);

        // Build field-map conditions
        List<Element> fieldMaps = childElementList(element, "field-map");
        StringBuilder whereBuilder = new StringBuilder();
        whereBuilder.append("UtilMisc.toMap(");
        boolean first = true;
        for (Element fm : fieldMaps) {
            if (!first) whereBuilder.append(", ");
            String fieldName = getAttr(fm, "field-name");
            String fromField = getAttr(fm, "from-field");
            String value = getAttr(fm, "value");
            whereBuilder.append("\"").append(fieldName).append("\", ");
            if (isNotEmpty(fromField)) {
                whereBuilder.append(buildFieldAccess(fromField));
            } else if (isNotEmpty(value)) {
                whereBuilder.append("\"").append(escapeString(value)).append("\"");
            } else {
                // When only field-name is specified, use it as both key and variable name
                whereBuilder.append(toJavaFieldName(fieldName));
            }
            first = false;
        }
        whereBuilder.append(")");

        // Declare variable before try block to ensure proper scope
        if (!declaredVars.contains(javaField)) {
            sb.append(indent()).append("List<GenericValue> ").append(javaField).append(" = null;").append(NEWLINE);
            declaredVars.add(javaField);
        }
        // Track type as List<GenericValue>
        declaredVarTypes.put(javaField, "List<GenericValue>");

        sb.append(indent()).append("try {").append(NEWLINE);
        indentLevel++;

        sb.append(indent()).append(javaField).append(" = EntityQuery.use(delegator)").append(NEWLINE);
        sb.append(indent()).append("        .from(\"").append(entityName).append("\")").append(NEWLINE);
        sb.append(indent()).append("        .where(").append(whereBuilder).append(")").append(NEWLINE);
        if (filterByDate) {
            sb.append(indent()).append("        .filterByDate()").append(NEWLINE);
        }
        sb.append(indent()).append("        .queryList();").append(NEWLINE);

        indentLevel--;
        sb.append(indent()).append("} catch (Exception e) {").append(NEWLINE);
        indentLevel++;
        sb.append(indent()).append("Debug.logError(e, \"Error querying ").append(entityName).append(": \" + e.getMessage(), MODULE);").append(NEWLINE);
        sb.append(indent()).append("request.setAttribute(\"_ERROR_MESSAGE_\", e.getMessage());").append(NEWLINE);
        sb.append(indent()).append("return \"error\";").append(NEWLINE);
        indentLevel--;
        sb.append(indent()).append("}").append(NEWLINE);

        return sb.toString();
    }

    /**
     * Converts an <add-error> element.
     */
    private String convertAddError(Element element) {
        StringBuilder sb = new StringBuilder();

        Element failProperty = firstChildElement(element, "fail-property");
        Element failMessage = firstChildElement(element, "fail-message");

        // In minilang, add-error adds to an error_list which is checked by check-errors
        // or directly via if-not-empty field="error_list"
        if (failProperty != null) {
            String resource = getAttr(failProperty, "resource");
            String property = getAttr(failProperty, "property");
            sb.append(indent()).append("{").append(NEWLINE);
            indentLevel++;
            sb.append(indent()).append("String errorMsg = UtilProperties.getMessage(\"")
                    .append(resource).append("\", \"").append(property).append("\", locale);").append(NEWLINE);
            sb.append(indent()).append("error_list.add(errorMsg);").append(NEWLINE);
            sb.append(indent()).append("request.setAttribute(\"_ERROR_MESSAGE_\", errorMsg);").append(NEWLINE);
            indentLevel--;
            sb.append(indent()).append("}").append(NEWLINE);
        } else if (failMessage != null) {
            String message = getAttr(failMessage, "message");
            String escapedMsg = escapeString(message);
            sb.append(indent()).append("error_list.add(\"").append(escapedMsg).append("\");").append(NEWLINE);
            sb.append(indent()).append("request.setAttribute(\"_ERROR_MESSAGE_\", \"")
                    .append(escapedMsg).append("\");").append(NEWLINE);
        }

        return sb.toString();
    }

    /**
     * Converts a <log> element.
     */
    private String convertLog(Element element) {
        StringBuilder sb = new StringBuilder();

        String level = getAttr(element, "level");
        String message = getAttr(element, "message");

        String logLevel = "Info";  // SCIPIO: 4.0.0: Default capitalized for Debug.logInfo()
        switch (level.toLowerCase()) {
            case "error":
                logLevel = "Error";
                break;
            case "warning":
                logLevel = "Warning";
                break;
            case "info":
                logLevel = "Info";
                break;
            case "verbose":
            case "debug":
                logLevel = "Verbose";
                break;
        }

        // Convert ${field} references to concatenated strings
        String javaMessage = convertMessageToJava(message);

        // Ensure the message is a String - wrap in String.valueOf if it's a single field access
        if (javaMessage.startsWith("((") || javaMessage.startsWith("context.get(")) {
            javaMessage = "String.valueOf(" + javaMessage + ")";
        }

        sb.append(indent()).append("Debug.log").append(logLevel).append("(")
                .append(javaMessage).append(", MODULE);").append(NEWLINE);

        return sb.toString();
    }

    /**
     * Converts a <script> element.
     */
    private String convertScript(Element element) {
        StringBuilder sb = new StringBuilder();

        String lang = getAttr(element, "lang");
        String location = getAttr(element, "location");
        String code = element.getTextContent().trim();

        if ("groovy".equalsIgnoreCase(lang) || isEmpty(lang)) {
            // For scripts with location that may create bindings, declare variables before try block
            if (!isEmpty(location)) {
                sb.append(indent()).append("Map<String, Object> scriptContext = new HashMap<>();").append(NEWLINE);
                declaredVars.add("scriptContext");
                declaredVarTypes.put("scriptContext", "Map<String, Object>");
            }

            sb.append(indent()).append("try {").append(NEWLINE);
            indentLevel++;

            if (isEmpty(location)) {
                // For inline scripts, declare context inside try
                sb.append(indent()).append("Map<String, Object> scriptContext = new HashMap<>();").append(NEWLINE);
            }

            sb.append(indent()).append("scriptContext.put(\"delegator\", delegator);").append(NEWLINE);
            sb.append(indent()).append("scriptContext.put(\"dispatcher\", dispatcher);").append(NEWLINE);
            sb.append(indent()).append("scriptContext.put(\"locale\", locale);").append(NEWLINE);
            sb.append(indent()).append("scriptContext.put(\"userLogin\", userLogin);").append(NEWLINE);
            sb.append(indent()).append("scriptContext.put(\"context\", context);").append(NEWLINE);
            sb.append(indent()).append("scriptContext.put(\"parameters\", context);").append(NEWLINE);
            // Add request/response since we're always generating event methods
            sb.append(indent()).append("scriptContext.put(\"request\", request);").append(NEWLINE);
            sb.append(indent()).append("scriptContext.put(\"response\", response);").append(NEWLINE);

            if (!isEmpty(location)) {
                // Execute external Groovy script at location
                imports.add("org.ofbiz.base.util.ScriptUtil");
                sb.append(indent()).append("ScriptUtil.executeScript(\"").append(location).append("\", null, scriptContext);").append(NEWLINE);
            } else if (!isEmpty(code)) {
                // Execute inline Groovy script
                imports.add("org.ofbiz.base.util.GroovyUtil");
                String escapedCode = escapeString(code);
                sb.append(indent()).append("Object scriptResult = GroovyUtil.eval(\"").append(escapedCode).append("\", scriptContext);").append(NEWLINE);
            }

            indentLevel--;
            sb.append(indent()).append("} catch (Exception e) {").append(NEWLINE);
            indentLevel++;
            sb.append(indent()).append("Debug.logError(e, \"Error executing Groovy script: \" + e.getMessage(), MODULE);").append(NEWLINE);
            indentLevel--;
            sb.append(indent()).append("}").append(NEWLINE);

            // For scripts with location, extract common bindings after try/catch
            if (!isEmpty(location)) {
                sb.append(indent()).append("// Extract script bindings into local scope").append(NEWLINE);
                sb.append(indent()).append("Object multiPartMap = scriptContext.get(\"multiPartMap\");").append(NEWLINE);
                declaredVars.add("multiPartMap");
                declaredVarTypes.put("multiPartMap", "Object");
            }
        }

        return sb.toString();
    }

    /**
     * Converts an <iterate> element.
     */
    private String convertIterate(Element element) {
        StringBuilder sb = new StringBuilder();

        String list = getAttr(element, "list");
        String entry = getAttr(element, "entry");

        // Check for singular/plural naming fallback
        // Simple-methods sometimes use plural list name when singular was defined, or vice versa
        String listVar = toJavaFieldName(list.split("\\.")[0]);
        if (!declaredVars.contains(listVar)) {
            // Try singular (remove trailing 's')
            if (listVar.endsWith("s") && declaredVars.contains(listVar.substring(0, listVar.length() - 1))) {
                list = list.replace(listVar, listVar.substring(0, listVar.length() - 1));
                listVar = listVar.substring(0, listVar.length() - 1);
            }
            // Try plural (add trailing 's')
            else if (declaredVars.contains(listVar + "s")) {
                list = list.replace(listVar, listVar + "s");
                listVar = listVar + "s";
            }
        }

        String listExpr = buildFieldAccess(list);
        String originalEntryVar = toJavaFieldName(entry);
        String entryVar = originalEntryVar;
        boolean wasHoisted = false;  // Entry var was hoisted because it's used outside the loop
        boolean wasRenamed = false;

        // Check if entry variable is already declared at method level
        // When declared at method level, use a temp var in for-each and assign to the existing var
        if (methodLevelVars.contains(entryVar)) {
            // Entry var exists at method level - use a temp var for iteration
            wasHoisted = true;
        } else if (declaredVars.contains(entryVar)) {
            // Entry var name conflicts with a block-scoped variable - rename it
            entryVar = entryVar + "Entry";
            wasRenamed = true;
            varRenameMap.put(originalEntryVar, entryVar);
        }

        // Check if we need to cast the list expression and determine entry type
        String listType = declaredVarTypes.get(listVar);
        String iterableExpr;
        String entryType = "GenericValue"; // Default for most entity lists

        if (listType != null && listType.startsWith("List<")) {
            // Extract element type from List<ElementType>
            int startIdx = listType.indexOf('<') + 1;
            int endIdx = listType.lastIndexOf('>');
            if (startIdx > 0 && endIdx > startIdx) {
                entryType = listType.substring(startIdx, endIdx);
            }
            iterableExpr = listExpr;
        } else if ("List<String>".equals(listType) || listVar.endsWith("List") && listVar.contains("error")) {
            // Common pattern: error lists contain strings
            entryType = "String";
            iterableExpr = "(List<String>) " + listExpr;
        } else if ("Object".equals(listType) || listType == null) {
            // Unknown list type (e.g., from iterate-map values) - use Object to be safe
            entryType = "Object";
            iterableExpr = "(List<Object>) " + listExpr;
        } else {
            // Cast to List<GenericValue> for iteration
            iterableExpr = "(List<GenericValue>) " + listExpr;
        }

        sb.append(indent()).append("if (").append(listExpr).append(" != null) {").append(NEWLINE);
        indentLevel++;

        if (wasHoisted) {
            // Entry var was hoisted - use a temp var in the for-each and assign inside
            String tempVar = entryVar + "_iter";
            sb.append(indent()).append("for (").append(entryType).append(" ").append(tempVar).append(" : ").append(iterableExpr).append(") {").append(NEWLINE);
            indentLevel++;
            // Assign temp var to the hoisted entry var
            // Cast if hoisted type differs from loop entry type
            String hoistedType = declaredVarTypes.get(entryVar);
            if (hoistedType != null && !hoistedType.equals(entryType)) {
                sb.append(indent()).append(entryVar).append(" = (").append(hoistedType).append(") ").append(tempVar).append(";").append(NEWLINE);
            } else {
                sb.append(indent()).append(entryVar).append(" = ").append(tempVar).append(";").append(NEWLINE);
            }
        } else {
            sb.append(indent()).append("for (").append(entryType).append(" ").append(entryVar).append(" : ").append(iterableExpr).append(") {").append(NEWLINE);
            indentLevel++;
            declaredVars.add(entryVar);
            declaredVarTypes.put(entryVar, entryType);
        }

        for (Element child : childElementList(element)) {
            sb.append(convertElement(child));
        }

        indentLevel--;
        sb.append(indent()).append("}").append(NEWLINE);
        indentLevel--;
        sb.append(indent()).append("}").append(NEWLINE);

        // Clear the renaming after the loop exits
        if (wasRenamed) {
            varRenameMap.remove(originalEntryVar);
        }

        return sb.toString();
    }

    /**
     * Converts a <set-service-fields> element.
     */
    private String convertSetServiceFields(Element element) {
        StringBuilder sb = new StringBuilder();

        String serviceName = getAttr(element, "service-name");
        String map = getAttr(element, "map");
        String toMap = getAttr(element, "to-map");

        String mapExpr = buildFieldAccess(map);
        String toMapVar = toJavaFieldName(toMap);

        // Ensure target map is declared as Map<String, Object>
        String existingType = declaredVarTypes.get(toMapVar);
        if (!declaredVars.contains(toMapVar)) {
            sb.append(indent()).append("Map<String, Object> ").append(toMapVar).append(" = new HashMap<>();").append(NEWLINE);
            declaredVars.add(toMapVar);
            declaredVarTypes.put(toMapVar, "Map<String, Object>");
        } else if (existingType != null && ("Object".equals(existingType) || !existingType.contains("Map"))) {
            // Variable exists but was declared as Object - assign new HashMap
            // DON'T update declaredVarTypes - the variable is still declared as Object
            sb.append(indent()).append(toMapVar).append(" = new HashMap<>();").append(NEWLINE);
        }

        sb.append(indent()).append("// set-service-fields from \"").append(map).append("\" to \"").append(toMap)
                .append("\" for service \"").append(serviceName).append("\"").append(NEWLINE);

        // If the variable is declared as Object, we need to cast it for putAll
        existingType = declaredVarTypes.get(toMapVar);
        String putTarget = (existingType == null || "Object".equals(existingType) || !existingType.contains("Map"))
                ? "((Map<String, Object>) " + toMapVar + ")"
                : toMapVar;
        sb.append(indent()).append(putTarget).append(".putAll(UtilMisc.toMap(").append(mapExpr).append("));").append(NEWLINE);

        return sb.toString();
    }

    /**
     * Converts a <session-to-field> element.
     */
    private String convertSessionToField(Element element) {
        StringBuilder sb = new StringBuilder();

        String field = getAttr(element, "field");
        String sessionName = getAttr(element, "session-name");
        if (isEmpty(sessionName)) sessionName = field;

        String javaField = toJavaFieldName(field);

        if (!declaredVars.contains(javaField)) {
            sb.append(indent()).append("Object ").append(javaField)
                    .append(" = request.getSession().getAttribute(\"").append(sessionName).append("\");").append(NEWLINE);
            declaredVars.add(javaField);
        } else {
            sb.append(indent()).append(javaField)
                    .append(" = request.getSession().getAttribute(\"").append(sessionName).append("\");").append(NEWLINE);
        }

        return sb.toString();
    }

    /**
     * Converts a <request-to-field> element.
     */
    private String convertRequestToField(Element element) {
        StringBuilder sb = new StringBuilder();

        String field = getAttr(element, "field");
        String requestName = getAttr(element, "request-name");
        if (isEmpty(requestName)) requestName = field;

        String javaField = toJavaFieldName(field);

        if (!declaredVars.contains(javaField)) {
            sb.append(indent()).append("Object ").append(javaField)
                    .append(" = request.getAttribute(\"").append(requestName).append("\");").append(NEWLINE);
            declaredVars.add(javaField);
        } else {
            sb.append(indent()).append(javaField)
                    .append(" = request.getAttribute(\"").append(requestName).append("\");").append(NEWLINE);
        }

        return sb.toString();
    }

    /**
     * Converts a <field-to-request> element.
     */
    private String convertFieldToRequest(Element element) {
        StringBuilder sb = new StringBuilder();

        String field = getAttr(element, "field");
        String requestName = getAttr(element, "request-name");
        if (isEmpty(requestName)) requestName = field;

        String fieldExpr = buildFieldAccess(field);

        sb.append(indent()).append("request.setAttribute(\"").append(requestName).append("\", ").append(fieldExpr).append(");").append(NEWLINE);

        return sb.toString();
    }

    /**
     * Converts a <field-to-session> element.
     */
    private String convertFieldToSession(Element element) {
        StringBuilder sb = new StringBuilder();

        String field = getAttr(element, "field");
        String sessionName = getAttr(element, "session-name");
        if (isEmpty(sessionName)) sessionName = field;

        String fieldExpr = buildFieldAccess(field);

        sb.append(indent()).append("request.getSession().setAttribute(\"").append(sessionName).append("\", ").append(fieldExpr).append(");").append(NEWLINE);

        return sb.toString();
    }

    /**
     * Converts a <call-class-method> element.
     * Calls static method on a class with return value.
     */
    private String convertCallClassMethod(Element element) {
        StringBuilder sb = new StringBuilder();

        String className = getAttr(element, "class-name");
        String methodName = getAttr(element, "method-name");
        String retField = getAttr(element, "ret-field");

        // Get method arguments from child <field> and <string> elements
        List<String> args = new ArrayList<>();
        for (Element fieldEl : childElementList(element)) {
            String nodeName = fieldEl.getNodeName();
            if ("field".equals(nodeName)) {
                String fieldName = getAttr(fieldEl, "field");
                String type = getAttr(fieldEl, "type");
                if (isNotEmpty(fieldName)) {
                    String expr = buildFieldAccess(fieldName);
                    // Apply type cast if specified or inferred
                    if (isNotEmpty(type)) {
                        String javaType = getTypeForField(type, null);
                        if (!"Object".equals(javaType)) {
                            expr = "(" + javaType + ") " + expr;
                        }
                    } else {
                        // Infer String type for common ID/name field suffixes when no type specified
                        String leafField = fieldName.contains(".") ? fieldName.substring(fieldName.lastIndexOf('.') + 1) : fieldName;
                        if (leafField.endsWith("Id") || leafField.endsWith("Name") ||
                            leafField.endsWith("Code") || leafField.endsWith("Type") ||
                            leafField.endsWith("Date") || leafField.endsWith("date")) {
                            expr = "(String) " + expr;
                        }
                    }
                    args.add(expr);
                }
            } else if ("string".equals(nodeName)) {
                String value = getAttr(fieldEl, "value");
                if (isNotEmpty(value)) {
                    args.add("\"" + escapeString(value) + "\"");
                } else {
                    args.add("\"\"");
                }
            }
        }

        String argsStr = String.join(", ", args);

        // Extract simple class name for method call
        String simpleClassName = className;
        if (className.contains(".")) {
            simpleClassName = className.substring(className.lastIndexOf('.') + 1);
            imports.add(className);
        }

        // Wrap in try-catch since the called method may throw checked exceptions
        sb.append(indent()).append("try {").append(NEWLINE);
        indentLevel++;

        if (isNotEmpty(retField)) {
            String javaField = toJavaFieldName(retField);
            // Check if retField contains a dot (setting into a map)
            if (retField.contains(".")) {
                String[] parts = retField.split("\\.", 2);
                String container = toJavaFieldName(parts[0]);
                String key = parts[1];
                sb.append(indent()).append("((Map<String, Object>) ").append(container).append(").put(\"").append(key)
                        .append("\", ").append(simpleClassName).append(".").append(methodName).append("(").append(argsStr).append("));").append(NEWLINE);
            } else {
                if (!declaredVars.contains(javaField)) {
                    // Pre-declare variable before try block
                    indentLevel--;
                    sb.insert(sb.lastIndexOf("try {") - indent().length(), indent() + "Object " + javaField + " = null;" + NEWLINE);
                    indentLevel++;
                    declaredVars.add(javaField);
                    declaredVarTypes.put(javaField, "Object");
                }
                sb.append(indent()).append(javaField).append(" = ")
                        .append(simpleClassName).append(".").append(methodName).append("(").append(argsStr).append(");").append(NEWLINE);
            }
        } else {
            sb.append(indent()).append(simpleClassName).append(".").append(methodName).append("(").append(argsStr).append(");").append(NEWLINE);
        }

        indentLevel--;
        sb.append(indent()).append("} catch (Exception e) {").append(NEWLINE);
        indentLevel++;
        sb.append(indent()).append("Debug.logError(e, \"Error calling ").append(simpleClassName).append(".").append(methodName).append(": \" + e.getMessage(), MODULE);").append(NEWLINE);
        sb.append(indent()).append("request.setAttribute(\"_ERROR_MESSAGE_\", e.getMessage());").append(NEWLINE);
        sb.append(indent()).append("return \"error\";").append(NEWLINE);
        indentLevel--;
        sb.append(indent()).append("}").append(NEWLINE);

        return sb.toString();
    }

    /**
     * Converts a <now-timestamp> element.
     */
    private String convertNowTimestamp(Element element) {
        StringBuilder sb = new StringBuilder();
        imports.add("java.sql.Timestamp");

        String field = getAttr(element, "field");
        String javaField = toJavaFieldName(field);

        // Check if variable will be converted to a different type (e.g., via to-string)
        String varType = scriptAssignedVars.contains(javaField) ? "Object" : "Timestamp";

        if (!declaredVars.contains(javaField)) {
            sb.append(indent()).append(varType).append(" ").append(javaField)
                    .append(" = new Timestamp(System.currentTimeMillis());").append(NEWLINE);
            declaredVars.add(javaField);
            declaredVarTypes.put(javaField, varType);
        } else {
            sb.append(indent()).append(javaField)
                    .append(" = new Timestamp(System.currentTimeMillis());").append(NEWLINE);
        }

        return sb.toString();
    }

    /**
     * Converts a <store-list> element.
     */
    private String convertStoreList(Element element) {
        StringBuilder sb = new StringBuilder();

        String list = getAttr(element, "list");
        String listExpr = buildFieldAccess(list);

        sb.append(indent()).append("try {").append(NEWLINE);
        indentLevel++;
        sb.append(indent()).append("delegator.storeAll((List<GenericValue>) ").append(listExpr).append(");").append(NEWLINE);
        indentLevel--;
        sb.append(indent()).append("} catch (Exception e) {").append(NEWLINE);
        indentLevel++;
        sb.append(indent()).append("Debug.logError(e, \"Error storing list: \" + e.getMessage(), MODULE);").append(NEWLINE);
        sb.append(indent()).append("request.setAttribute(\"_ERROR_MESSAGE_\", e.getMessage());").append(NEWLINE);
        sb.append(indent()).append("return \"error\";").append(NEWLINE);
        indentLevel--;
        sb.append(indent()).append("}").append(NEWLINE);

        return sb.toString();
    }

    /**
     * Converts a <filter-list-by-date> element.
     */
    private String convertFilterListByDate(Element element) {
        StringBuilder sb = new StringBuilder();
        imports.add("org.ofbiz.entity.util.EntityUtil");

        String list = getAttr(element, "list");
        String toList = getAttr(element, "to-list");
        String validDate = getAttr(element, "valid-date");

        String listExpr = buildFieldAccess(list);
        String validDateExpr = isNotEmpty(validDate) ? buildFieldAccess(validDate) : "null";
        String toListVar = toJavaFieldName(toList);

        if (!declaredVars.contains(toListVar)) {
            sb.append(indent()).append("List<GenericValue> ").append(toListVar)
                    .append(" = EntityUtil.filterByDate((List<GenericValue>) ").append(listExpr);
            if (isNotEmpty(validDate)) {
                sb.append(", ").append(validDateExpr);
            }
            sb.append(");").append(NEWLINE);
            declaredVars.add(toListVar);
        } else {
            sb.append(indent()).append(toListVar)
                    .append(" = EntityUtil.filterByDate((List<GenericValue>) ").append(listExpr);
            if (isNotEmpty(validDate)) {
                sb.append(", ").append(validDateExpr);
            }
            sb.append(");").append(NEWLINE);
        }

        return sb.toString();
    }

    /**
     * Converts a <map-to-map> element.
     */
    private String convertMapToMap(Element element) {
        StringBuilder sb = new StringBuilder();

        String map = getAttr(element, "map");
        String toMap = getAttr(element, "to-map");

        String mapExpr = buildFieldAccess(map);
        String toMapVar = toJavaFieldName(toMap);

        if (!declaredVars.contains(toMapVar)) {
            sb.append(indent()).append("Map<String, Object> ").append(toMapVar)
                    .append(" = new HashMap<>((Map<String, Object>) ").append(mapExpr).append(");").append(NEWLINE);
            declaredVars.add(toMapVar);
        } else {
            sb.append(indent()).append(toMapVar)
                    .append(".putAll((Map<String, Object>) ").append(mapExpr).append(");").append(NEWLINE);
        }

        return sb.toString();
    }

    /**
     * Converts a <call-map-processor> element.
     */
    private String convertCallMapProcessor(Element element) {
        StringBuilder sb = new StringBuilder();

        String inMapName = getAttr(element, "in-map-name");
        String outMapName = getAttr(element, "out-map-name");

        sb.append(indent()).append("// TODO: Convert call-map-processor (in-map: ").append(inMapName)
                .append(", out-map: ").append(outMapName).append(")").append(NEWLINE);

        Element simpleMapProcessor = firstChildElement(element, "simple-map-processor");
        if (simpleMapProcessor != null) {
            String processorName = getAttr(simpleMapProcessor, "name");
            sb.append(indent()).append("// simple-map-processor name: ").append(processorName).append(NEWLINE);

            String outMapVar = toJavaFieldName(outMapName);
            if (!declaredVars.contains(outMapVar)) {
                sb.append(indent()).append("Map<String, Object> ").append(outMapVar).append(" = new HashMap<>();").append(NEWLINE);
                declaredVars.add(outMapVar);
            }
        }

        return sb.toString();
    }

    /**
     * Converts a <return> element.
     */
    private String convertReturn(Element element) {
        StringBuilder sb = new StringBuilder();

        String responseCode = getAttr(element, "response-code");
        if (isEmpty(responseCode)) responseCode = "success";

        sb.append(indent()).append("return \"").append(responseCode).append("\";").append(NEWLINE);

        return sb.toString();
    }

    /**
     * Converts an <entity-condition> element.
     */
    private String convertEntityCondition(Element element) {
        StringBuilder sb = new StringBuilder();
        imports.add("org.ofbiz.entity.util.EntityQuery");
        imports.add("org.ofbiz.entity.condition.EntityCondition");

        String entityName = getAttr(element, "entity-name");
        String list = getAttr(element, "list");
        boolean useCache = parseBoolean(getAttr(element, "use-cache"), false);
        boolean filterByDate = parseBoolean(getAttr(element, "filter-by-date"), false);
        boolean distinct = parseBoolean(getAttr(element, "distinct"), false);

        String javaField = toJavaFieldName(list);

        // Declare variable before try block to ensure proper scope
        if (!declaredVars.contains(javaField)) {
            sb.append(indent()).append("List<GenericValue> ").append(javaField).append(" = null;").append(NEWLINE);
            declaredVars.add(javaField);
        }
        // Track type as List<GenericValue>
        declaredVarTypes.put(javaField, "List<GenericValue>");

        sb.append(indent()).append("try {").append(NEWLINE);
        indentLevel++;

        sb.append(indent()).append(javaField).append(" = EntityQuery.use(delegator)").append(NEWLINE);
        sb.append(indent()).append("        .from(\"").append(entityName).append("\")").append(NEWLINE);
        if (distinct) {
            sb.append(indent()).append("        .distinct()").append(NEWLINE);
        }
        if (useCache) {
            sb.append(indent()).append("        .cache()").append(NEWLINE);
        }
        if (filterByDate) {
            sb.append(indent()).append("        .filterByDate()").append(NEWLINE);
        }
        sb.append(indent()).append("        .queryList();").append(NEWLINE);

        indentLevel--;
        sb.append(indent()).append("} catch (Exception e) {").append(NEWLINE);
        indentLevel++;
        sb.append(indent()).append("Debug.logError(e, \"Error querying ").append(entityName).append(": \" + e.getMessage(), MODULE);").append(NEWLINE);
        sb.append(indent()).append("request.setAttribute(\"_ERROR_MESSAGE_\", e.getMessage());").append(NEWLINE);
        sb.append(indent()).append("return \"error\";").append(NEWLINE);
        indentLevel--;
        sb.append(indent()).append("}").append(NEWLINE);

        return sb.toString();
    }

    /**
     * Converts a <make-value> element.
     */
    private String convertMakeValue(Element element) {
        StringBuilder sb = new StringBuilder();

        String entityName = getAttr(element, "entity-name");
        String valueField = getAttr(element, "value-field");

        String javaField = toJavaFieldName(valueField);

        if (!declaredVars.contains(javaField)) {
            sb.append(indent()).append("GenericValue ").append(javaField)
                    .append(" = delegator.makeValue(\"").append(entityName).append("\");").append(NEWLINE);
            declaredVars.add(javaField);
            declaredVarTypes.put(javaField, "GenericValue");
        } else {
            sb.append(indent()).append(javaField)
                    .append(" = delegator.makeValue(\"").append(entityName).append("\");").append(NEWLINE);
            // Update type since we're reassigning
            declaredVarTypes.put(javaField, "GenericValue");
        }

        return sb.toString();
    }

    /**
     * Converts a <clone-value> element.
     * Creates a copy of a GenericValue.
     */
    private String convertCloneValue(Element element) {
        StringBuilder sb = new StringBuilder();

        String valueField = getAttr(element, "value-field");
        String newValueField = getAttr(element, "new-value-field");

        String sourceField = buildFieldAccess(valueField);
        String targetField = toJavaFieldName(newValueField);

        // Clone creates a copy of the GenericValue
        String cloneExpr = "GenericValue.create((GenericValue) " + sourceField + ")";

        if (!declaredVars.contains(targetField)) {
            sb.append(indent()).append("GenericValue ").append(targetField)
                    .append(" = ").append(cloneExpr).append(";").append(NEWLINE);
            declaredVars.add(targetField);
            declaredVarTypes.put(targetField, "GenericValue");
        } else {
            sb.append(indent()).append(targetField)
                    .append(" = ").append(cloneExpr).append(";").append(NEWLINE);
            declaredVarTypes.put(targetField, "GenericValue");
        }

        return sb.toString();
    }

    /**
     * Converts a <store-value> element.
     */
    private String convertStoreValue(Element element) {
        StringBuilder sb = new StringBuilder();

        String valueField = getAttr(element, "value-field");
        String javaField = toJavaFieldName(valueField);
        String fieldType = declaredVarTypes.get(javaField);

        // Cast to GenericValue if the field is declared as Object
        String storeExpr;
        if (fieldType == null || "Object".equals(fieldType)) {
            storeExpr = "(GenericValue) " + javaField;
        } else {
            storeExpr = javaField;
        }

        sb.append(indent()).append("try {").append(NEWLINE);
        indentLevel++;
        sb.append(indent()).append("delegator.store(").append(storeExpr).append(");").append(NEWLINE);
        indentLevel--;
        sb.append(indent()).append("} catch (Exception e) {").append(NEWLINE);
        indentLevel++;
        sb.append(indent()).append("Debug.logError(e, \"Error storing value: \" + e.getMessage(), MODULE);").append(NEWLINE);
        sb.append(indent()).append("request.setAttribute(\"_ERROR_MESSAGE_\", e.getMessage());").append(NEWLINE);
        sb.append(indent()).append("return \"error\";").append(NEWLINE);
        indentLevel--;
        sb.append(indent()).append("}").append(NEWLINE);

        return sb.toString();
    }

    /**
     * Converts a <create-value> element.
     */
    private String convertCreateValue(Element element) {
        StringBuilder sb = new StringBuilder();

        String valueField = getAttr(element, "value-field");
        String javaField = toJavaFieldName(valueField);
        String fieldType = declaredVarTypes.get(javaField);

        // Cast to GenericValue if the field is declared as Object
        String createExpr;
        if (fieldType == null || "Object".equals(fieldType)) {
            createExpr = "(GenericValue) " + javaField;
        } else {
            createExpr = javaField;
        }

        sb.append(indent()).append("try {").append(NEWLINE);
        indentLevel++;
        sb.append(indent()).append("delegator.create(").append(createExpr).append(");").append(NEWLINE);
        indentLevel--;
        sb.append(indent()).append("} catch (Exception e) {").append(NEWLINE);
        indentLevel++;
        sb.append(indent()).append("Debug.logError(e, \"Error creating value: \" + e.getMessage(), MODULE);").append(NEWLINE);
        sb.append(indent()).append("request.setAttribute(\"_ERROR_MESSAGE_\", e.getMessage());").append(NEWLINE);
        sb.append(indent()).append("return \"error\";").append(NEWLINE);
        indentLevel--;
        sb.append(indent()).append("}").append(NEWLINE);

        return sb.toString();
    }

    /**
     * Converts a <remove-value> element.
     */
    private String convertRemoveValue(Element element) {
        StringBuilder sb = new StringBuilder();

        String valueField = getAttr(element, "value-field");
        String javaField = toJavaFieldName(valueField);
        String fieldType = declaredVarTypes.get(javaField);

        // Cast to GenericValue if the field is declared as Object
        String removeExpr;
        if (fieldType == null || "Object".equals(fieldType)) {
            removeExpr = "(GenericValue) " + javaField;
        } else {
            removeExpr = javaField;
        }

        sb.append(indent()).append("try {").append(NEWLINE);
        indentLevel++;
        sb.append(indent()).append("delegator.removeValue(").append(removeExpr).append(");").append(NEWLINE);
        indentLevel--;
        sb.append(indent()).append("} catch (Exception e) {").append(NEWLINE);
        indentLevel++;
        sb.append(indent()).append("Debug.logError(e, \"Error removing value: \" + e.getMessage(), MODULE);").append(NEWLINE);
        sb.append(indent()).append("request.setAttribute(\"_ERROR_MESSAGE_\", e.getMessage());").append(NEWLINE);
        sb.append(indent()).append("return \"error\";").append(NEWLINE);
        indentLevel--;
        sb.append(indent()).append("}").append(NEWLINE);

        return sb.toString();
    }

    /**
     * Converts a <find-by-primary-key> element.
     */
    private String convertFindByPrimaryKey(Element element) {
        StringBuilder sb = new StringBuilder();
        imports.add("org.ofbiz.entity.util.EntityQuery");

        String entityName = getAttr(element, "entity-name");
        String valueField = getAttr(element, "value-field");
        String map = getAttr(element, "map");
        boolean useCache = parseBoolean(getAttr(element, "use-cache"), false);

        String javaField = toJavaFieldName(valueField);
        String mapExpr = isNotEmpty(map) ? buildFieldAccess(map) : "UtilMisc.toMap()";

        // Declare variable before try block to ensure proper scope
        if (!declaredVars.contains(javaField)) {
            sb.append(indent()).append("GenericValue ").append(javaField).append(" = null;").append(NEWLINE);
            declaredVars.add(javaField);
        }
        // Track type as GenericValue
        declaredVarTypes.put(javaField, "GenericValue");

        sb.append(indent()).append("try {").append(NEWLINE);
        indentLevel++;

        sb.append(indent()).append(javaField).append(" = EntityQuery.use(delegator)").append(NEWLINE);
        sb.append(indent()).append("        .from(\"").append(entityName).append("\")").append(NEWLINE);
        sb.append(indent()).append("        .where(").append(mapExpr).append(")").append(NEWLINE);
        if (useCache) {
            sb.append(indent()).append("        .cache()").append(NEWLINE);
        }
        sb.append(indent()).append("        .queryOne();").append(NEWLINE);

        indentLevel--;
        sb.append(indent()).append("} catch (Exception e) {").append(NEWLINE);
        indentLevel++;
        sb.append(indent()).append("Debug.logError(e, \"Error finding by primary key ").append(entityName).append(": \" + e.getMessage(), MODULE);").append(NEWLINE);
        sb.append(indent()).append("request.setAttribute(\"_ERROR_MESSAGE_\", e.getMessage());").append(NEWLINE);
        sb.append(indent()).append("return \"error\";").append(NEWLINE);
        indentLevel--;
        sb.append(indent()).append("}").append(NEWLINE);

        return sb.toString();
    }

    /**
     * Converts a <set-pk-fields> element.
     */
    private String convertSetPkFields(Element element) {
        StringBuilder sb = new StringBuilder();

        String valueField = getAttr(element, "value-field");
        String map = getAttr(element, "map");

        String javaField = toJavaFieldName(valueField);
        String fieldType = declaredVarTypes.get(javaField);
        String mapExpr = buildFieldAccess(map);

        // Cast to GenericValue if the field is declared as Object
        String valueExpr;
        if (fieldType == null || "Object".equals(fieldType)) {
            valueExpr = "((GenericValue) " + javaField + ")";
        } else {
            valueExpr = javaField;
        }

        sb.append(indent()).append(valueExpr).append(".setPKFields((Map<String, Object>) ").append(mapExpr).append(");").append(NEWLINE);

        return sb.toString();
    }

    /**
     * Converts a <set-nonpk-fields> element.
     */
    private String convertSetNonPkFields(Element element) {
        StringBuilder sb = new StringBuilder();

        String valueField = getAttr(element, "value-field");
        String map = getAttr(element, "map");

        String javaField = toJavaFieldName(valueField);
        String fieldType = declaredVarTypes.get(javaField);
        String mapExpr = buildFieldAccess(map);

        // Cast to GenericValue if the field is declared as Object
        String valueExpr;
        if (fieldType == null || "Object".equals(fieldType)) {
            valueExpr = "((GenericValue) " + javaField + ")";
        } else {
            valueExpr = javaField;
        }

        sb.append(indent()).append(valueExpr).append(".setNonPKFields((Map<String, Object>) ").append(mapExpr).append(");").append(NEWLINE);

        return sb.toString();
    }

    /**
     * Converts a <get-related> element.
     */
    private String convertGetRelated(Element element) {
        StringBuilder sb = new StringBuilder();

        String relationName = getAttr(element, "relation-name");
        String valueField = getAttr(element, "value-field");
        String list = getAttr(element, "list");
        boolean useCache = parseBoolean(getAttr(element, "use-cache"), false);

        String valueExpr = buildFieldAccess(valueField);
        String javaField = toJavaFieldName(list);

        // Check if the source needs to be cast to GenericValue
        String valueVar = toJavaFieldName(valueField.split("\\.")[0]);
        String valueType = declaredVarTypes.get(valueVar);
        boolean needsCast = valueType == null || "Object".equals(valueType);
        String castValueExpr = needsCast ? "((GenericValue) " + valueExpr + ")" : valueExpr;

        // Declare variable before try block to ensure proper scope
        if (!declaredVars.contains(javaField)) {
            sb.append(indent()).append("List<GenericValue> ").append(javaField).append(" = null;").append(NEWLINE);
            declaredVars.add(javaField);
        }
        // Track type as List<GenericValue>
        declaredVarTypes.put(javaField, "List<GenericValue>");

        sb.append(indent()).append("try {").append(NEWLINE);
        indentLevel++;

        sb.append(indent()).append(javaField).append(" = ")
                .append(castValueExpr).append(".getRelated(\"").append(relationName).append("\", null, null, ")
                .append(useCache).append(");").append(NEWLINE);

        indentLevel--;
        sb.append(indent()).append("} catch (Exception e) {").append(NEWLINE);
        indentLevel++;
        sb.append(indent()).append("Debug.logError(e, \"Error getting related ").append(relationName).append(": \" + e.getMessage(), MODULE);").append(NEWLINE);
        sb.append(indent()).append("request.setAttribute(\"_ERROR_MESSAGE_\", e.getMessage());").append(NEWLINE);
        sb.append(indent()).append("return \"error\";").append(NEWLINE);
        indentLevel--;
        sb.append(indent()).append("}").append(NEWLINE);

        return sb.toString();
    }

    /**
     * Converts a <get-related-one> element.
     */
    private String convertGetRelatedOne(Element element) {
        StringBuilder sb = new StringBuilder();

        String relationName = getAttr(element, "relation-name");
        String valueField = getAttr(element, "value-field");
        String toValueField = getAttr(element, "to-value-field");
        boolean useCache = parseBoolean(getAttr(element, "use-cache"), false);

        String valueExpr = buildFieldAccess(valueField);
        String javaField = toJavaFieldName(toValueField);

        // Check if the source needs to be cast to GenericValue
        String valueVar = toJavaFieldName(valueField.split("\\.")[0]);
        String valueType = declaredVarTypes.get(valueVar);
        boolean needsCast = valueType == null || "Object".equals(valueType);
        String castValueExpr = needsCast ? "((GenericValue) " + valueExpr + ")" : valueExpr;

        // Declare variable before try block to ensure proper scope
        if (!declaredVars.contains(javaField)) {
            sb.append(indent()).append("GenericValue ").append(javaField).append(" = null;").append(NEWLINE);
            declaredVars.add(javaField);
        }
        // Track type as GenericValue
        declaredVarTypes.put(javaField, "GenericValue");

        sb.append(indent()).append("try {").append(NEWLINE);
        indentLevel++;

        sb.append(indent()).append(javaField).append(" = ")
                .append(castValueExpr).append(".getRelatedOne(\"").append(relationName).append("\", ")
                .append(useCache).append(");").append(NEWLINE);

        indentLevel--;
        sb.append(indent()).append("} catch (Exception e) {").append(NEWLINE);
        indentLevel++;
        sb.append(indent()).append("Debug.logError(e, \"Error getting related one ").append(relationName).append(": \" + e.getMessage(), MODULE);").append(NEWLINE);
        sb.append(indent()).append("request.setAttribute(\"_ERROR_MESSAGE_\", e.getMessage());").append(NEWLINE);
        sb.append(indent()).append("return \"error\";").append(NEWLINE);
        indentLevel--;
        sb.append(indent()).append("}").append(NEWLINE);

        return sb.toString();
    }

    /**
     * Converts a <clear-field> element.
     */
    private String convertClearField(Element element) {
        StringBuilder sb = new StringBuilder();

        String field = getAttr(element, "field");
        String javaField = toJavaFieldName(field);
        String containerField = getContainerField(field);
        String accessField = getAccessField(field);

        if (containerField != null) {
            // Ensure the container map exists
            if (!declaredVars.contains(containerField)) {
                sb.append(indent()).append("Map<String, Object> ").append(containerField)
                        .append(" = new HashMap<>();").append(NEWLINE);
                declaredVars.add(containerField);
                imports.add("java.util.Map");
                imports.add("java.util.HashMap");
            }
            sb.append(indent()).append(containerField).append(".remove(\"").append(accessField).append("\");").append(NEWLINE);
        } else {
            // Declare the variable if not yet declared, then assign null
            if (!declaredVars.contains(javaField)) {
                sb.append(indent()).append("Object ").append(javaField).append(" = null;").append(NEWLINE);
                declaredVars.add(javaField);
                // Track that this variable was declared as Object
                declaredVarTypes.put(javaField, "Object");
            } else {
                sb.append(indent()).append(javaField).append(" = null;").append(NEWLINE);
            }
        }

        return sb.toString();
    }

    /**
     * Converts a <first-from-list> element.
     */
    private String convertFirstFromList(Element element) {
        StringBuilder sb = new StringBuilder();
        imports.add("org.ofbiz.entity.util.EntityUtil");

        String list = getAttr(element, "list");
        String entry = getAttr(element, "entry");

        String listExpr = buildFieldAccess(list);
        String javaField = toJavaFieldName(entry);

        if (!declaredVars.contains(javaField)) {
            sb.append(indent()).append("GenericValue ").append(javaField)
                    .append(" = EntityUtil.getFirst((List<GenericValue>) ").append(listExpr).append(");").append(NEWLINE);
            declaredVars.add(javaField);
            declaredVarTypes.put(javaField, "GenericValue");
        } else {
            sb.append(indent()).append(javaField)
                    .append(" = EntityUtil.getFirst((List<GenericValue>) ").append(listExpr).append(");").append(NEWLINE);
            // Update type to GenericValue since we're assigning from EntityUtil.getFirst
            declaredVarTypes.put(javaField, "GenericValue");
        }

        return sb.toString();
    }

    /**
     * Converts a <sequenced-id> element.
     */
    private String convertSequencedId(Element element) {
        StringBuilder sb = new StringBuilder();

        String seqName = getAttr(element, "sequence-name");
        String field = getAttr(element, "field");

        // Check if field contains a dot - means we're setting a field on an object (GenericValue/Map)
        if (field.contains(".")) {
            int dotIdx = field.indexOf('.');
            String containerField = field.substring(0, dotIdx);
            String subField = field.substring(dotIdx + 1);
            String javaContainerField = toJavaFieldName(containerField);

            // Generate: container.put("subField", delegator.getNextSeqId("SequenceName"));
            sb.append(indent()).append("((GenericValue) ").append(javaContainerField).append(").put(\"")
                    .append(subField).append("\", delegator.getNextSeqId(\"").append(seqName).append("\"));").append(NEWLINE);
        } else {
            String javaField = toJavaFieldName(field);

            // Generate: String fieldName = delegator.getNextSeqId("SequenceName");
            if (!declaredVars.contains(javaField)) {
                sb.append(indent()).append("String ").append(javaField)
                        .append(" = delegator.getNextSeqId(\"").append(seqName).append("\");").append(NEWLINE);
                declaredVars.add(javaField);
                declaredVarTypes.put(javaField, "String");
            } else {
                sb.append(indent()).append(javaField)
                        .append(" = delegator.getNextSeqId(\"").append(seqName).append("\");").append(NEWLINE);
            }
        }

        return sb.toString();
    }

    // ========================================================================
    // Helper Methods
    // ========================================================================

    private String indent() {
        StringBuilder sb = new StringBuilder();
        for (int i = 0; i < indentLevel; i++) {
            sb.append(INDENT);
        }
        return sb.toString();
    }

    private String buildFieldAccess(String field) {
        if (field == null) return "null";

        // Handle FlexibleString expressions like "container.${varName}" or "${varName}"
        if (field.contains("${")) {
            // Pattern for "container.${varName}" - dynamic map key access
            Pattern flexPattern = Pattern.compile("(\\w+)\\.\\$\\{(\\w+)\\}(.*)");
            Matcher flexMatcher = flexPattern.matcher(field);
            if (flexMatcher.matches()) {
                String container = flexMatcher.group(1);
                String varName = flexMatcher.group(2);
                String rest = flexMatcher.group(3);
                String javaVarName = toJavaFieldName(varName);
                String containerType = declaredVarTypes.get(toJavaFieldName(container));
                String containerExpr = toJavaFieldName(container);

                // Check if the key variable is Object-typed - if so, cast to String for map access
                String varType = declaredVarTypes.get(javaVarName);
                String keyExpr = "Object".equals(varType) ? "(String) " + javaVarName : javaVarName;

                String access;
                if ("GenericValue".equals(containerType)) {
                    access = containerExpr + ".get(" + keyExpr + ")";
                } else {
                    access = "((Map<String, Object>) " + containerExpr + ").get(" + keyExpr + ")";
                }

                if (isNotEmpty(rest) && rest.startsWith(".")) {
                    access = "((Map<String, Object>) " + access + ").get(\"" + rest.substring(1) + "\")";
                }
                return access;
            }

            // Pattern for standalone "${varName}"
            Pattern standalonePattern = Pattern.compile("\\$\\{(\\w+)\\}");
            Matcher standaloneMatcher = standalonePattern.matcher(field);
            if (standaloneMatcher.matches()) {
                return toJavaFieldName(standaloneMatcher.group(1));
            }
        }

        // Handle special fields
        if ("parameters".equals(field)) {
            return "context";
        }

        // Handle _NA_ constant - special mini-lang value meaning literal "_NA_" string
        if ("_NA_".equals(field)) {
            return "\"_NA_\"";
        }

        // Handle nullField/nullfield pattern - mini-lang idiom for null value
        if ("nullfield".equalsIgnoreCase(field)) {
            return "null";
        }

        // Handle array/map access like "purposes[0]" or "map[key.field]"
        // Check for brackets BEFORE dots because the key inside brackets may contain dots
        if (field.contains("[")) {
            // First try numeric array access
            Pattern numericPattern = Pattern.compile("(\\w+)\\[(\\d+)\\](.*)");
            Matcher numericMatcher = numericPattern.matcher(field);
            if (numericMatcher.matches()) {
                String listName = numericMatcher.group(1);
                String index = numericMatcher.group(2);
                String rest = numericMatcher.group(3);
                String access = "((List<?>) " + toJavaFieldName(listName) + ").get(" + index + ")";
                if (isNotEmpty(rest) && rest.startsWith(".")) {
                    access = "((GenericValue) " + access + ").get(\"" + rest.substring(1) + "\")";
                }
                return access;
            }

            // Handle dynamic map access like "map[key.field]"
            Pattern mapPattern = Pattern.compile("(\\w+)\\[([^\\]]+)\\](.*)");
            Matcher mapMatcher = mapPattern.matcher(field);
            if (mapMatcher.matches()) {
                String mapName = mapMatcher.group(1);
                String keyExpr = mapMatcher.group(2);
                String rest = mapMatcher.group(3);
                // Convert the key expression to Java
                String keyAccess = buildFieldAccess(keyExpr);
                String access = "((Map<String, Object>) " + toJavaFieldName(mapName) + ").get(" + keyAccess + ")";
                if (isNotEmpty(rest) && rest.startsWith(".")) {
                    access = "((Map<String, Object>) " + access + ").get(\"" + rest.substring(1) + "\")";
                }
                return access;
            }
        }

        // Handle dotted paths like "entity.field" or "map.key"
        if (field.contains(".")) {
            String[] parts = field.split("\\.", 2);
            String container = parts[0];
            String key = parts[1];

            // Check for resource bundle patterns like "AccountingUiLabels.PropertyKey"
            if (container.endsWith("UiLabels") || container.endsWith("ErrorLabels") ||
                container.endsWith("Msgs") || container.endsWith("Messages") ||
                container.endsWith("Labels")) {
                // Convert to UtilProperties.getMessage call
                imports.add("org.ofbiz.base.util.UtilProperties");
                return "UtilProperties.getMessage(\"" + container + "\", \"" + key + "\", locale)";
            }

            // Handle nested dots
            if (key.contains(".")) {
                return "((Map<String, Object>) " + buildFieldAccess(container) + ".get(\"" + key.split("\\.")[0] + "\")).get(\"" + key.substring(key.indexOf('.') + 1) + "\")";
            }

            if ("parameters".equals(container)) {
                return "context.get(\"" + key + "\")";
            }

            // Check if container is declared - if not, try context
            String containerVar = toJavaFieldName(container);
            if (!declaredVars.contains(containerVar)) {
                return "((Map<String, Object>) context.get(\"" + container + "\")).get(\"" + key + "\")";
            }

            return "((Map<String, Object>) " + containerVar + ").get(\"" + key + "\")";
        }

        String javaField = toJavaFieldName(field);

        // If the variable is not declared, it might be a parameter - try context
        if (!declaredVars.contains(javaField)) {
            // Common parameter names that should come from context
            return "context.get(\"" + field + "\")";
        }

        return javaField;
    }

    private String getContainerField(String field) {
        if (field == null) return null;

        // Handle indexed map access like "map[key.field]" - the map name is the container
        if (field.contains("[")) {
            int bracketIdx = field.indexOf('[');
            return toJavaFieldName(field.substring(0, bracketIdx));
        }

        if (field.contains(".")) {
            String[] parts = field.split("\\.", 2);
            return toJavaFieldName(parts[0]);
        }
        return null;
    }

    private String getAccessField(String field) {
        if (field == null) return field;

        // Handle indexed map access like "map[key.field]" or "list[]" - return the key expression
        if (field.contains("[")) {
            int startIdx = field.indexOf('[') + 1;
            int endIdx = field.lastIndexOf(']');
            // For empty brackets [], endIdx == startIdx, which is valid (returns "")
            if (endIdx >= startIdx) {
                return field.substring(startIdx, endIdx);
            }
        }

        if (field.contains(".")) {
            String[] parts = field.split("\\.", 2);
            return parts[1];
        }
        return field;
    }

    /**
     * Ensures that container variables used in field expressions are declared before use.
     * This handles cases where a map is first accessed in a condition before being set.
     *
     * @param field The field expression (e.g., "map[key.field]" or "container.field")
     * @return Code to declare the container if needed, or empty string
     */
    private String ensureContainerDeclared(String field) {
        StringBuilder sb = new StringBuilder();
        String containerField = getContainerField(field);

        if (containerField != null && !declaredVars.contains(containerField) &&
                !"context".equals(containerField) && !"delegator".equals(containerField) &&
                !"dispatcher".equals(containerField) && !"userLogin".equals(containerField) &&
                !"locale".equals(containerField)) {
            // Determine type based on access pattern
            if (field.contains("[")) {
                // Indexed access - declare as Map
                sb.append(indent()).append("Map<String, Object> ").append(containerField)
                        .append(" = new HashMap<>();").append(NEWLINE);
                declaredVars.add(containerField);
                declaredVarTypes.put(containerField, "Map<String, Object>");
            } else if (field.contains(".")) {
                // Dot access - declare as Object (could be Map or GenericValue)
                sb.append(indent()).append("Object ").append(containerField)
                        .append(" = null;").append(NEWLINE);
                declaredVars.add(containerField);
                declaredVarTypes.put(containerField, "Object");
            }
        }
        return sb.toString();
    }

    /**
     * Pre-scans a condition element and ensures all container variables used in fields are declared.
     * This recursively handles nested and/or/not conditions.
     */
    private String ensureConditionContainersDeclared(Element conditionElement) {
        StringBuilder sb = new StringBuilder();
        collectAndDeclareConditionContainers(conditionElement, sb);
        return sb.toString();
    }

    private void collectAndDeclareConditionContainers(Element element, StringBuilder sb) {
        String tagName = element.getNodeName();

        switch (tagName) {
            case "condition":
            case "and":
            case "or":
            case "not":
                // Recursively process child elements
                for (Element child : childElementList(element)) {
                    collectAndDeclareConditionContainers(child, sb);
                }
                break;
            case "if-compare":
            case "if-empty":
            case "if-not-empty":
                String field = getAttr(element, "field");
                if (isNotEmpty(field)) {
                    sb.append(ensureContainerDeclared(field));
                }
                break;
            case "if-compare-field":
                String field1 = getAttr(element, "field");
                String field2 = getAttr(element, "to-field");
                if (isNotEmpty(field1)) {
                    sb.append(ensureContainerDeclared(field1));
                }
                if (isNotEmpty(field2)) {
                    sb.append(ensureContainerDeclared(field2));
                }
                break;
        }
    }

    private String toJavaFieldName(String field) {
        if (field == null) return "unknown";

        // Handle special names
        if ("parameters".equals(field)) return "context";

        // Remove array indices
        field = field.replaceAll("\\[\\d+\\]", "");

        // Remove dots
        field = field.replace(".", "_");

        // Convert to valid Java identifier
        field = field.replaceAll("[^a-zA-Z0-9_]", "_");
        if (!field.isEmpty() && Character.isDigit(field.charAt(0))) {
            field = "_" + field;
        }

        // Handle empty field names
        if (field.isEmpty()) {
            field = "emptyField";
        }

        // Check if this variable was renamed due to a conflict in an iterate loop
        String renamed = varRenameMap.get(field);
        if (renamed != null) {
            return renamed;
        }

        return field;
    }

    private String toJavaMethodName(String methodName) {
        if (methodName == null) return "unknownMethod";

        // Convert kebab-case to camelCase
        StringBuilder sb = new StringBuilder();
        boolean capitalizeNext = false;
        for (char c : methodName.toCharArray()) {
            if (c == '-' || c == '_') {
                capitalizeNext = true;
            } else {
                if (capitalizeNext) {
                    sb.append(Character.toUpperCase(c));
                    capitalizeNext = false;
                } else {
                    sb.append(c);
                }
            }
        }
        return sb.toString();
    }

    private String getTypeForField(String type, String field) {
        if ("Boolean".equals(type) || "java.lang.Boolean".equals(type)) return "Boolean";
        if ("Integer".equals(type) || "java.lang.Integer".equals(type)) return "Integer";
        if ("Long".equals(type) || "java.lang.Long".equals(type)) return "Long";
        if ("Double".equals(type) || "java.lang.Double".equals(type)) return "Double";
        if ("Number".equals(type) || "java.lang.Number".equals(type)) return "Number";
        if ("String".equals(type) || "java.lang.String".equals(type)) return "String";
        if ("BigDecimal".equals(type) || "java.math.BigDecimal".equals(type)) {
            imports.add("java.math.BigDecimal");
            return "BigDecimal";
        }
        if ("Timestamp".equals(type) || "java.sql.Timestamp".equals(type)) {
            imports.add("java.sql.Timestamp");
            return "Timestamp";
        }
        if ("ByteBuffer".equals(type) || "java.nio.ByteBuffer".equals(type)) {
            imports.add("java.nio.ByteBuffer");
            return "ByteBuffer";
        }
        if ("NewMap".equals(type)) return "Map<String, Object>";
        if ("NewList".equals(type)) return "List<Object>";
        if ("GenericValue".equals(type)) return "GenericValue";
        if ("List<GenericValue>".equals(type)) return "List<GenericValue>";
        if ("List<Object>".equals(type)) return "List<Object>";
        if (type != null && type.startsWith("List<")) return type;
        if (type != null && type.startsWith("Map<")) return type;
        return "Object";
    }

    /**
     * Generates code for converting a value expression to a target type.
     * Unlike simple casts, this handles conversions that require constructors
     * (e.g., Integer to BigDecimal, Long to Timestamp).
     *
     * @param valueExpr The Java expression for the value
     * @param targetType The target type to convert to
     * @param sourceType The known source type (may be null)
     * @return Java expression with proper type conversion
     */
    private String convertToType(String valueExpr, String targetType, String sourceType) {
        if (targetType == null || "Object".equals(targetType)) {
            return valueExpr;
        }

        // Same type - no conversion needed
        if (targetType.equals(sourceType)) {
            return valueExpr;
        }

        // BigDecimal conversions - can't cast from Integer/Long/Double
        if ("BigDecimal".equals(targetType)) {
            imports.add("java.math.BigDecimal");
            if ("Integer".equals(sourceType) || "Long".equals(sourceType) || "Double".equals(sourceType)) {
                return "BigDecimal.valueOf(" + valueExpr + ")";
            }
            if ("BigDecimal".equals(sourceType)) {
                return valueExpr;
            }
            // For Object/unknown source with known value from .get(), just cast
            if (valueExpr.contains(".get(")) {
                return "(BigDecimal) " + valueExpr;
            }
            // For simple variables with unknown type, use safe valueOf
            return "BigDecimal.valueOf(((Number) " + valueExpr + ").doubleValue())";
        }

        // Timestamp conversions - can't cast from Long
        if ("Timestamp".equals(targetType)) {
            imports.add("java.sql.Timestamp");
            if ("Long".equals(sourceType)) {
                // Long 0L typically means "null/empty" timestamp, not epoch
                // Return null for 0L or create Timestamp otherwise
                return "(" + valueExpr + " == 0L ? null : new Timestamp(" + valueExpr + "))";
            }
            if ("Timestamp".equals(sourceType)) {
                return valueExpr;
            }
            // For Object/unknown source from .get(), just cast
            return "(Timestamp) " + valueExpr;
        }

        // String conversions
        if ("String".equals(targetType)) {
            if ("String".equals(sourceType)) {
                return valueExpr;
            }
            if (valueExpr.contains(".get(")) {
                return "(String) " + valueExpr;
            }
            return "String.valueOf(" + valueExpr + ")";
        }

        // GenericValue - just cast
        if ("GenericValue".equals(targetType)) {
            if ("GenericValue".equals(sourceType)) {
                return valueExpr;
            }
            return "(GenericValue) " + valueExpr;
        }

        // Default: simple cast
        return "(" + targetType + ") " + valueExpr;
    }

    private String convertValueLiteral(String value, String type) {
        if (value == null) return "null";

        // Handle ${} expressions
        if (value.contains("${")) {
            // For Long/Integer types with simple arithmetic, use native Java operators
            if (("Long".equals(type) || "Integer".equals(type)) && isSimpleArithmetic(value)) {
                String inner = value.substring(2, value.length() - 1); // strip ${ and }
                return convertSimpleArithmetic(inner, type);
            }

            String expr = convertMessageToJava(value);
            // If there's a specific type expected and the expression returns Object (from Groovy/map access),
            // add a cast to the expected type
            if (isNotEmpty(type) && !"Object".equals(type) && !"String".equals(type)) {
                String castType = getTypeForField(type, null);
                if (!"Object".equals(castType)) {
                    return "(" + castType + ") " + expr;
                }
            }
            return expr;
        }

        if ("Boolean".equals(type)) {
            return "true".equalsIgnoreCase(value) ? "Boolean.TRUE" : "Boolean.FALSE";
        }
        if ("Integer".equals(type)) {
            return value;
        }
        if ("Long".equals(type)) {
            return value + "L";
        }
        if ("Double".equals(type)) {
            return value + "D";
        }
        if ("BigDecimal".equals(type)) {
            imports.add("java.math.BigDecimal");
            // Handle special cases like 0, 1, 10
            if ("0".equals(value)) {
                return "BigDecimal.ZERO";
            } else if ("1".equals(value)) {
                return "BigDecimal.ONE";
            } else if ("10".equals(value)) {
                return "BigDecimal.TEN";
            }
            return "new BigDecimal(\"" + escapeString(value) + "\")";
        }
        if ("Timestamp".equals(type)) {
            imports.add("java.sql.Timestamp");
            return "Timestamp.valueOf(\"" + escapeString(value) + "\")";
        }

        return "\"" + escapeString(value) + "\"";
    }

    private String convertMessageToJava(String message) {
        if (message == null) return "\"\"";

        // Convert ${field} to concatenated string
        Pattern pattern = Pattern.compile("\\$\\{([^}]+)\\}");
        Matcher matcher = pattern.matcher(message);

        StringBuilder result = new StringBuilder();
        int lastEnd = 0;
        boolean hasExpressions = false;

        while (matcher.find()) {
            hasExpressions = true;
            String before = message.substring(lastEnd, matcher.start());
            if (!before.isEmpty()) {
                if (result.length() > 0) result.append(" + ");
                result.append("\"").append(escapeString(before)).append("\"");
            }

            String field = matcher.group(1);
            if (result.length() > 0) result.append(" + ");

            // Check if this is a groovy/bsh expression
            if (field.startsWith("groovy:") || field.startsWith("bsh:")) {
                // Execute groovy expression at runtime using GroovyUtil
                String script = field.substring(field.indexOf(':') + 1).trim();
                imports.add("org.ofbiz.base.util.GroovyUtil");
                // Build inline Groovy execution
                result.append("GroovyUtil.eval(\"").append(escapeString(script)).append("\", UtilMisc.toMap(")
                        .append("\"delegator\", delegator, \"dispatcher\", dispatcher, \"locale\", locale, ")
                        .append("\"userLogin\", userLogin, \"context\", context, \"parameters\", context))");
                imports.add("org.ofbiz.base.util.UtilMisc");
            } else if (field.startsWith("util:size(") && field.endsWith(")")) {
                // Handle util:size(listName) - gets the size of a list, cast to long for type compatibility
                String listName = field.substring("util:size(".length(), field.length() - 1);
                String listExpr = buildFieldAccess(listName);
                result.append("(long) (").append(listExpr).append(" != null ? ((java.util.List<?>) ").append(listExpr).append(").size() : 0)");
            } else if (field.startsWith("str:toString(") && field.endsWith(")")) {
                // Handle str:toString(fieldName) - convert to String.valueOf()
                String innerField = field.substring("str:toString(".length(), field.length() - 1);
                // Inner field might be another function call like date:year()
                String innerExpr = convertUelFunction(innerField);
                result.append("String.valueOf(").append(innerExpr).append(")");
            } else if (field.startsWith("date:year(") && field.endsWith(")")) {
                // Handle date:year(date, timezone, locale) - extract year from date
                result.append(convertUelFunction(field));
            } else if (field.contains(" - ") || field.contains(" + ") || field.contains(" * ") || field.contains(" / ")) {
                // Handle arithmetic expressions like "a.field - b.field" or "a.amount + b.amount"
                result.append(convertArithmeticExpression(field));
            } else {
                result.append(buildFieldAccess(field));
            }

            lastEnd = matcher.end();
        }

        if (hasExpressions) {
            String after = message.substring(lastEnd);
            if (!after.isEmpty()) {
                result.append(" + \"").append(escapeString(after)).append("\"");
            }
            return result.toString();
        } else {
            return "\"" + escapeString(message) + "\"";
        }
    }

    /**
     * Converts an EL boolean expression to Java.
     * Handles expressions like: payment.statusId == 'PMNT_SENT' @or payment.statusId == 'PMNT_RECEIVED'
     */
    private String convertElBooleanExpression(String elExpr) {
        if (elExpr == null || elExpr.isEmpty()) {
            return "true";
        }

        // Replace @or with || and @and with &&
        String javaExpr = elExpr.replace(" @or ", " || ").replace(" @and ", " && ");

        // Split by || and && to process each comparison
        // Use a pattern that preserves the operators
        String[] parts = javaExpr.split("(?=\\s\\|\\||\\s&&)|(?<=\\s\\|\\||\\s&&)");

        StringBuilder result = new StringBuilder();
        for (String part : parts) {
            part = part.trim();
            if (part.isEmpty()) continue;

            if ("||".equals(part) || "&&".equals(part)) {
                result.append(" ").append(part).append(" ");
            } else {
                // Convert comparison like "payment.statusId == 'PMNT_SENT'"
                result.append(convertElComparison(part));
            }
        }

        return result.toString();
    }

    /**
     * Converts a single EL comparison to Java.
     * Example: payment.statusId == 'PMNT_SENT' -> "PMNT_SENT".equals(payment.get("statusId"))
     */
    private String convertElComparison(String comparison) {
        comparison = comparison.trim();

        // Match pattern: field.property == 'value' or field.property != 'value'
        Pattern eqPattern = Pattern.compile("([\\w.]+)\\s*(==|!=)\\s*'([^']*)'");
        Matcher eqMatcher = eqPattern.matcher(comparison);
        if (eqMatcher.matches()) {
            String fieldPath = eqMatcher.group(1);
            String op = eqMatcher.group(2);
            String value = eqMatcher.group(3);

            String fieldExpr = buildFieldAccess(fieldPath);
            if ("==".equals(op)) {
                return "\"" + value + "\".equals(" + fieldExpr + ")";
            } else {
                return "!\"" + value + "\".equals(" + fieldExpr + ")";
            }
        }

        // Match pattern: field.property == otherField.property
        Pattern fieldPattern = Pattern.compile("([\\w.]+)\\s*(==|!=)\\s*([\\w.]+)");
        Matcher fieldMatcher = fieldPattern.matcher(comparison);
        if (fieldMatcher.matches()) {
            String leftPath = fieldMatcher.group(1);
            String op = fieldMatcher.group(2);
            String rightPath = fieldMatcher.group(3);

            String leftExpr = buildFieldAccess(leftPath);
            String rightExpr = buildFieldAccess(rightPath);
            if ("==".equals(op)) {
                return "java.util.Objects.equals(" + leftExpr + ", " + rightExpr + ")";
            } else {
                return "!java.util.Objects.equals(" + leftExpr + ", " + rightExpr + ")";
            }
        }

        // Fallback: treat as field access for boolean field
        return buildFieldAccess(comparison) + " != null && (Boolean) " + buildFieldAccess(comparison);
    }

    /**
     * Converts a UEL function expression to Java.
     * Handles function calls like date:year(), util:size(), str:toString(), etc.
     */
    private String convertUelFunction(String field) {
        if (field == null || field.isEmpty()) {
            return "null";
        }

        // Handle date:year(date, timezone, locale)
        if (field.startsWith("date:year(") && field.endsWith(")")) {
            String argsStr = field.substring("date:year(".length(), field.length() - 1);
            int firstComma = argsStr.indexOf(',');
            String dateField = (firstComma > 0) ? argsStr.substring(0, firstComma).trim() : argsStr.trim();
            String dateExpr = buildFieldAccess(dateField);
            imports.add("org.ofbiz.base.util.UtilDateTime");
            return "UtilDateTime.getYear((java.sql.Timestamp) " + dateExpr + ", java.util.TimeZone.getDefault(), locale)";
        }

        // Handle util:size(list)
        if (field.startsWith("util:size(") && field.endsWith(")")) {
            String listName = field.substring("util:size(".length(), field.length() - 1);
            String listExpr = buildFieldAccess(listName);
            return "(long) (" + listExpr + " != null ? ((java.util.List<?>) " + listExpr + ").size() : 0)";
        }

        // Handle str:toString(expr) - nested function calls
        if (field.startsWith("str:toString(") && field.endsWith(")")) {
            String innerField = field.substring("str:toString(".length(), field.length() - 1);
            String innerExpr = convertUelFunction(innerField);
            return "String.valueOf(" + innerExpr + ")";
        }

        // Handle util:defaultTimeZone() and util:defaultLocale()
        if ("util:defaultTimeZone()".equals(field)) {
            return "java.util.TimeZone.getDefault()";
        }
        if ("util:defaultLocale()".equals(field)) {
            return "java.util.Locale.getDefault()";
        }

        // No function, just a field access
        return buildFieldAccess(field);
    }

    /**
     * Converts an arithmetic expression like "a.field - b.field" to Java BigDecimal operations.
     */
    private String convertArithmeticExpression(String expr) {
        imports.add("java.math.BigDecimal");

        // Split by operators while preserving order
        // Handle subtraction: a.field - b.field
        if (expr.contains(" - ")) {
            String[] parts = expr.split(" - ", 2);
            if (parts.length == 2) {
                String left = convertArithmeticOperand(parts[0].trim());
                String right = convertArithmeticOperand(parts[1].trim());
                return "(" + left + ").subtract(" + right + ")";
            }
        }

        // Handle addition: a.field + b.field
        if (expr.contains(" + ")) {
            String[] parts = expr.split(" \\+ ", 2);
            if (parts.length == 2) {
                String left = convertArithmeticOperand(parts[0].trim());
                String right = convertArithmeticOperand(parts[1].trim());
                return "(" + left + ").add(" + right + ")";
            }
        }

        // Handle multiplication: a.field * b.field
        if (expr.contains(" * ")) {
            String[] parts = expr.split(" \\* ", 2);
            if (parts.length == 2) {
                String left = convertArithmeticOperand(parts[0].trim());
                String right = convertArithmeticOperand(parts[1].trim());
                return "(" + left + ").multiply(" + right + ")";
            }
        }

        // Handle division: a.field / b.field
        if (expr.contains(" / ")) {
            String[] parts = expr.split(" / ", 2);
            if (parts.length == 2) {
                String left = convertArithmeticOperand(parts[0].trim());
                String right = convertArithmeticOperand(parts[1].trim());
                return "(" + left + ").divide(" + right + ", java.math.RoundingMode.HALF_UP)";
            }
        }

        // Fallback - treat as single operand
        return convertArithmeticOperand(expr);
    }

    /**
     * Converts an operand in an arithmetic expression to a BigDecimal accessor.
     */
    private String convertArithmeticOperand(String operand) {
        // Strip surrounding parentheses
        String stripped = operand.trim();
        while (stripped.startsWith("(") && stripped.endsWith(")")) {
            stripped = stripped.substring(1, stripped.length() - 1).trim();
        }

        // Check if it's a numeric literal
        if (stripped.matches("-?\\d+(\\.\\d+)?")) {
            return "new BigDecimal(\"" + stripped + "\")";
        }
        // Otherwise treat as a field access and cast to BigDecimal
        return "(BigDecimal) " + buildFieldAccess(stripped);
    }

    /**
     * Checks if the value is a simple arithmetic expression that can use native Java operators.
     * e.g., ${var + 1} or ${a + b}
     */
    private boolean isSimpleArithmetic(String value) {
        if (value == null || !value.startsWith("${") || !value.endsWith("}")) return false;
        String inner = value.substring(2, value.length() - 1);
        // Check for simple arithmetic patterns (no nested ${}, no groovy:, no complex expressions)
        if (inner.contains("${") || inner.contains("groovy:") || inner.contains("bsh:")) return false;
        // Must have an operator
        return inner.contains(" + ") || inner.contains(" - ") || inner.contains(" * ") || inner.contains(" / ");
    }

    /**
     * Converts simple arithmetic for Long/Integer types using native Java operators.
     */
    private String convertSimpleArithmetic(String expr, String type) {
        StringBuilder result = new StringBuilder();

        // Split by operators and reconstruct
        String[] parts;
        String op;
        if (expr.contains(" + ")) {
            parts = expr.split(" \\+ ", 2);
            op = " + ";
        } else if (expr.contains(" - ")) {
            parts = expr.split(" - ", 2);
            op = " - ";
        } else if (expr.contains(" * ")) {
            parts = expr.split(" \\* ", 2);
            op = " * ";
        } else if (expr.contains(" / ")) {
            parts = expr.split(" / ", 2);
            op = " / ";
        } else {
            // No operator, just convert the field
            return toJavaFieldName(expr.trim());
        }

        if (parts.length == 2) {
            String left = convertSimpleArithmeticOperand(parts[0].trim(), type);
            String right = convertSimpleArithmeticOperand(parts[1].trim(), type);
            result.append(left).append(op).append(right);
        }

        return result.toString();
    }

    /**
     * Converts an operand for simple arithmetic (Long/Integer types).
     * Returns an expression that is guaranteed to be a primitive type for arithmetic.
     */
    private String convertSimpleArithmeticOperand(String operand, String type) {
        // Check if it's a numeric literal
        if (operand.matches("-?\\d+")) {
            return "Long".equals(type) ? operand + "L" : operand;
        }
        // Determine the value accessor method based on type
        String valueMethod = "Long".equals(type) ? "longValue()" : "intValue()";
        // Check if it's a simple field name
        if (operand.matches("[a-zA-Z_][a-zA-Z0-9_]*")) {
            String javaField = toJavaFieldName(operand);
            // Check if the variable is declared with a known numeric type
            String varType = declaredVarTypes.get(javaField);
            if ("Long".equals(varType) || "long".equals(varType) ||
                "Integer".equals(varType) || "int".equals(varType)) {
                return javaField;
            }
            // Cast Object to Number for arithmetic
            if (!declaredVars.contains(javaField)) {
                return "((Number) context.get(\"" + operand + "\"))." + valueMethod;
            }
            return "((Number) " + javaField + ")." + valueMethod;
        }
        // Otherwise treat as a field access - need to cast to Number for arithmetic
        String fieldAccess = buildFieldAccess(operand);
        return "((Number) " + fieldAccess + ")." + valueMethod;
    }

    private String buildCompareCondition(String fieldExpr, String operator, String value, String type) {
        String valueExpr = convertValueLiteral(value, type);

        switch (operator) {
            case "equals":
                if ("Boolean".equals(type)) {
                    return valueExpr + ".equals(" + fieldExpr + ")";
                }
                return "\"" + escapeString(value) + "\".equals(" + fieldExpr + ")";
            case "not-equals":
                if ("Boolean".equals(type)) {
                    return "!" + valueExpr + ".equals(" + fieldExpr + ")";
                }
                return "!\"" + escapeString(value) + "\".equals(" + fieldExpr + ")";
            case "greater":
                return "((Comparable) " + fieldExpr + ").compareTo(" + valueExpr + ") > 0";
            case "greater-equals":
                return "((Comparable) " + fieldExpr + ").compareTo(" + valueExpr + ") >= 0";
            case "less":
                return "((Comparable) " + fieldExpr + ").compareTo(" + valueExpr + ") < 0";
            case "less-equals":
                return "((Comparable) " + fieldExpr + ").compareTo(" + valueExpr + ") <= 0";
            default:
                return fieldExpr + " != null /* TODO: operator " + operator + " */";
        }
    }

    private String buildFieldCompareCondition(String fieldExpr, String operator, String toFieldExpr) {
        switch (operator) {
            case "equals":
                return "java.util.Objects.equals(" + fieldExpr + ", " + toFieldExpr + ")";
            case "not-equals":
                return "!java.util.Objects.equals(" + fieldExpr + ", " + toFieldExpr + ")";
            default:
                return fieldExpr + " != null /* TODO: field compare operator " + operator + " */";
        }
    }

    private String escapeString(String s) {
        if (s == null) return "";
        return s.replace("\\", "\\\\")
                .replace("\"", "\\\"")
                .replace("\n", "\\n")
                .replace("\r", "\\r")
                .replace("\t", "\\t");
    }

    private String escapeJavadoc(String s) {
        if (s == null) return "";
        return s.replace("*/", "* /")
                .replace("<", "&lt;")
                .replace(">", "&gt;");
    }

    /**
     * Collects variable declarations that would be made by elements in a branch.
     * Returns a map of variable name to type.
     */
    private Map<String, String> collectBranchVariables(Element branch) {
        Map<String, String> variables = new LinkedHashMap<>();
        if (branch == null) return variables;
        for (Element child : childElementList(branch)) {
            collectElementVariables(child, variables);
        }
        return variables;
    }

    /**
     * Recursively collects variable declarations from an element.
     */
    private void collectElementVariables(Element element, Map<String, String> variables) {
        String tagName = element.getNodeName();

        switch (tagName) {
            case "make-value":
            case "entity-one":
            case "find-by-primary-key":
            case "get-related-one": {
                String field = getAttr(element, "value-field");
                if (isEmpty(field)) field = getAttr(element, "value-name");
                if (isNotEmpty(field)) {
                    String javaField = toJavaFieldName(field);
                    if (!declaredVars.contains(javaField)) {
                        variables.put(javaField, "GenericValue");
                    }
                }
                break;
            }
            case "entity-condition":
            case "find-by-and":
            case "get-related": {
                String list = getAttr(element, "list");
                if (isNotEmpty(list)) {
                    String javaField = toJavaFieldName(list);
                    if (!declaredVars.contains(javaField)) {
                        variables.put(javaField, "List<GenericValue>");
                    }
                }
                break;
            }
            case "set":
            case "set-service-fields":
            case "make-in-map": {
                String field = getAttr(element, "field");
                if (isEmpty(field)) field = getAttr(element, "to-map");
                if (isNotEmpty(field)) {
                    String javaField = toJavaFieldName(field);
                    if (!declaredVars.contains(javaField)) {
                        String type = getAttr(element, "type");
                        if ("NewMap".equals(type) || "set-service-fields".equals(tagName) || "make-in-map".equals(tagName)) {
                            variables.put(javaField, "Map<String, Object>");
                        } else if ("NewList".equals(type)) {
                            variables.put(javaField, "List<Object>");
                        } else if (isNotEmpty(type)) {
                            variables.put(javaField, getTypeForField(type, field));
                        } else {
                            variables.put(javaField, "Object");
                        }
                    }
                }
                break;
            }
            case "call-service": {
                // Check for result-to-field declarations
                for (Element resultToField : childElementList(element, "result-to-field")) {
                    String field = getAttr(resultToField, "field");
                    if (isEmpty(field)) field = getAttr(resultToField, "result-name");
                    if (isNotEmpty(field)) {
                        String javaField = toJavaFieldName(field);
                        if (!declaredVars.contains(javaField)) {
                            variables.put(javaField, "Object");
                        }
                    }
                }
                break;
            }
            case "call-class-method":
            case "call-object-method": {
                // Check for ret-field declarations
                String retField = getAttr(element, "ret-field");
                if (isNotEmpty(retField) && !retField.contains(".")) {
                    String javaField = toJavaFieldName(retField);
                    if (!declaredVars.contains(javaField)) {
                        variables.put(javaField, "Object");
                    }
                }
                break;
            }
            case "first-from-list": {
                // first-from-list declares an entry variable
                String entry = getAttr(element, "entry");
                if (isNotEmpty(entry) && !entry.contains(".")) {
                    String javaField = toJavaFieldName(entry);
                    if (!declaredVars.contains(javaField)) {
                        variables.put(javaField, "GenericValue");
                    }
                }
                break;
            }
            case "if": {
                // Recursively collect from nested if-then-else
                Element thenElement = firstChildElement(element, "then");
                Element elseElement = firstChildElement(element, "else");
                if (thenElement != null) {
                    for (Element child : childElementList(thenElement)) {
                        collectElementVariables(child, variables);
                    }
                }
                if (elseElement != null) {
                    for (Element child : childElementList(elseElement)) {
                        collectElementVariables(child, variables);
                    }
                }
                break;
            }
            case "if-empty":
            case "if-not-empty":
            case "if-compare":
            case "if-compare-field": {
                // Collect from nested if body
                for (Element child : childElementList(element)) {
                    if (!"else".equals(child.getNodeName())) {
                        collectElementVariables(child, variables);
                    }
                }
                Element elseElement = firstChildElement(element, "else");
                if (elseElement != null) {
                    for (Element child : childElementList(elseElement)) {
                        collectElementVariables(child, variables);
                    }
                }
                break;
            }
            case "iterate":
            case "iterate-map": {
                // Collect from loop body
                for (Element child : childElementList(element)) {
                    collectElementVariables(child, variables);
                }
                break;
            }
            default:
                // For unknown elements, still scan children
                for (Element child : childElementList(element)) {
                    collectElementVariables(child, variables);
                }
                break;
        }
    }

    /**
     * Pre-declares variables that are used in an if-else block.
     * Returns the declaration code to insert before the if statement.
     */
    private String hoistIfElseVariables(Element thenElement, Element elseElement) {
        StringBuilder sb = new StringBuilder();

        // Collect variables from both branches
        Map<String, String> thenVars = collectBranchVariables(thenElement);
        Map<String, String> elseVars = collectBranchVariables(elseElement);

        // Create union of all variables
        Map<String, String> allVars = new LinkedHashMap<>();
        allVars.putAll(thenVars);
        for (Map.Entry<String, String> entry : elseVars.entrySet()) {
            if (!allVars.containsKey(entry.getKey())) {
                allVars.put(entry.getKey(), entry.getValue());
            }
        }

        // Pre-declare all variables that appear in either branch
        for (Map.Entry<String, String> entry : allVars.entrySet()) {
            String varName = entry.getKey();
            String varType = entry.getValue();

            if (!declaredVars.contains(varName)) {
                sb.append(indent()).append(varType).append(" ").append(varName).append(" = null;").append(NEWLINE);
                declaredVars.add(varName);
                declaredVarTypes.put(varName, varType);
            }
        }

        return sb.toString();
    }

    /**
     * Pre-declares variables that are set inside an iterate loop.
     * This ensures variables set inside the loop are accessible after the loop.
     * Note: Entry variables are also hoisted here; convertIterate will rename them to avoid conflicts.
     */
    private String hoistIterateVariables(Element iterateElement) {
        StringBuilder sb = new StringBuilder();

        // Collect variables from all children of the iterate element
        Map<String, String> loopVars = new LinkedHashMap<>();
        for (Element child : childElementList(iterateElement)) {
            collectElementVariables(child, loopVars);
        }

        // Pre-declare all variables that appear in the loop body
        for (Map.Entry<String, String> entry : loopVars.entrySet()) {
            String varName = entry.getKey();
            String varType = entry.getValue();

            if (!declaredVars.contains(varName)) {
                sb.append(indent()).append(varType).append(" ").append(varName).append(" = null;").append(NEWLINE);
                declaredVars.add(varName);
                declaredVarTypes.put(varName, varType);
            }
        }

        return sb.toString();
    }

    private boolean parseBoolean(String value, boolean defaultValue) {
        if (isEmpty(value)) return defaultValue;
        return "true".equalsIgnoreCase(value) || "Y".equalsIgnoreCase(value);
    }

    // ========================================================================
    // DOM Utilities
    // ========================================================================

    private List<Element> childElementList(Element parent, String tagName) {
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

    private List<Element> childElementList(Element parent) {
        return childElementList(parent, null);
    }

    private Element firstChildElement(Element parent, String tagName) {
        if (parent == null) return null;

        NodeList children = parent.getChildNodes();
        for (int i = 0; i < children.getLength(); i++) {
            Node child = children.item(i);
            if (child.getNodeType() == Node.ELEMENT_NODE) {
                Element element = (Element) child;
                if (tagName == null || tagName.equals(element.getTagName())) {
                    return element;
                }
            }
        }
        return null;
    }

    private Element firstChildElement(Element parent) {
        return firstChildElement(parent, null);
    }

    /**
     * Checks if an element has a descendant with the given tag name.
     */
    private boolean hasDescendant(Element parent, String tagName) {
        if (parent == null) return false;
        NodeList descendants = parent.getElementsByTagName(tagName);
        return descendants.getLength() > 0;
    }

    /**
     * Checks if an element has a descendant with the given tag name and attribute value.
     */
    private boolean hasDescendantWithAttr(Element parent, String tagName, String attrName, String attrValue) {
        if (parent == null) return false;
        NodeList descendants = parent.getElementsByTagName(tagName);
        for (int i = 0; i < descendants.getLength(); i++) {
            Element descendant = (Element) descendants.item(i);
            if (attrValue.equals(descendant.getAttribute(attrName))) {
                return true;
            }
        }
        return false;
    }

    private String getAttr(Element element, String attrName) {
        return element.getAttribute(attrName);
    }

    private static boolean isEmpty(String s) {
        return s == null || s.isEmpty();
    }

    private static boolean isNotEmpty(String s) {
        return s != null && !s.isEmpty();
    }

    /**
     * Converts a <string-to-list> element.
     * <p>Appends a string value to a list. Creates the list if it doesn't exist.</p>
     */
    private String convertStringToList(Element element) {
        StringBuilder sb = new StringBuilder();
        String string = getAttr(element, "string");
        String list = getAttr(element, "list");
        String javaList = toJavaFieldName(list);

        // Create list if not declared
        if (!declaredVars.contains(javaList)) {
            sb.append(indent()).append("List<String> ").append(javaList).append(" = new ArrayList<>();").append(NEWLINE);
            declaredVars.add(javaList);
            declaredVarTypes.put(javaList, "List<String>");
            imports.add("java.util.List");
            imports.add("java.util.ArrayList");
        }

        // Add the string to the list
        sb.append(indent()).append(javaList).append(".add(\"").append(escapeString(string)).append("\");").append(NEWLINE);
        return sb.toString();
    }

    /**
     * Converts an <entity-count> element.
     * <p>Counts entities matching conditions using EntityQuery.queryCount().</p>
     */
    private String convertEntityCount(Element element) {
        StringBuilder sb = new StringBuilder();
        String countField = getAttr(element, "count-field");
        String entityName = getAttr(element, "entity-name");
        String javaField = toJavaFieldName(countField);

        // Build EntityQuery
        sb.append(indent());
        if (!declaredVars.contains(javaField)) {
            sb.append("Long ");
            declaredVars.add(javaField);
            declaredVarTypes.put(javaField, "Long");
        }
        sb.append(javaField).append(" = null;").append(NEWLINE);

        sb.append(indent()).append("try {").append(NEWLINE);
        indentLevel++;
        sb.append(indent()).append(javaField).append(" = EntityQuery.use(delegator)").append(NEWLINE);
        sb.append(indent()).append("        .from(\"").append(entityName).append("\")").append(NEWLINE);

        // Handle condition-expr children
        List<Element> conditionExprs = childElementList(element, "condition-expr");
        if (!conditionExprs.isEmpty()) {
            sb.append(indent()).append("        .where(");
            if (conditionExprs.size() == 1) {
                Element condExpr = conditionExprs.get(0);
                String fieldName = getAttr(condExpr, "field-name");
                String value = getAttr(condExpr, "value");
                String fromField = getAttr(condExpr, "from-field");
                if (isNotEmpty(fromField)) {
                    sb.append("\"").append(fieldName).append("\", ").append(buildFieldAccess(fromField));
                } else {
                    sb.append("\"").append(fieldName).append("\", \"").append(escapeString(value)).append("\"");
                }
            } else {
                // Multiple conditions - use UtilMisc.toMap
                sb.append("UtilMisc.toMap(");
                boolean first = true;
                for (Element condExpr : conditionExprs) {
                    if (!first) sb.append(", ");
                    first = false;
                    String fieldName = getAttr(condExpr, "field-name");
                    String value = getAttr(condExpr, "value");
                    String fromField = getAttr(condExpr, "from-field");
                    sb.append("\"").append(fieldName).append("\", ");
                    if (isNotEmpty(fromField)) {
                        sb.append(buildFieldAccess(fromField));
                    } else {
                        sb.append("\"").append(escapeString(value)).append("\"");
                    }
                }
                sb.append(")");
                imports.add("org.ofbiz.base.util.UtilMisc");
            }
            sb.append(")").append(NEWLINE);
        }

        sb.append(indent()).append("        .queryCount();").append(NEWLINE);
        indentLevel--;
        sb.append(indent()).append("} catch (Exception e) {").append(NEWLINE);
        indentLevel++;
        sb.append(indent()).append("Debug.logError(e, \"Error counting ").append(entityName).append(": \" + e.getMessage(), MODULE);").append(NEWLINE);
        indentLevel--;
        sb.append(indent()).append("}").append(NEWLINE);

        imports.add("org.ofbiz.entity.util.EntityQuery");
        return sb.toString();
    }

    /**
     * Converts a <field-to-result> element.
     * <p>Puts a field value into the result map.</p>
     */
    private String convertFieldToResult(Element element) {
        StringBuilder sb = new StringBuilder();
        String field = getAttr(element, "field");
        String resultName = getAttr(element, "result-name");
        if (resultName == null || resultName.isEmpty()) {
            resultName = field;
        }

        String fieldExpr = buildFieldAccess(field);
        sb.append(indent()).append("result.put(\"").append(resultName).append("\", ").append(fieldExpr).append(");").append(NEWLINE);
        return sb.toString();
    }

    /**
     * Converts a <while> element.
     * <p>Generates a Java while loop with the given condition.</p>
     */
    private String convertWhile(Element element) {
        StringBuilder sb = new StringBuilder();

        // Get condition and then elements
        Element conditionElement = firstChildElement(element, "condition");
        Element thenElement = firstChildElement(element, "then");

        if (conditionElement == null || thenElement == null) {
            return indent() + "// TODO: Convert <while> element (missing condition or then)" + NEWLINE;
        }

        // Build the condition expression
        String condition = buildConditionExpression(conditionElement);

        sb.append(indent()).append("while (").append(condition).append(") {").append(NEWLINE);
        indentLevel++;

        // Process then body
        for (Element child : childElementList(thenElement)) {
            sb.append(convertElement(child));
        }

        indentLevel--;
        sb.append(indent()).append("}").append(NEWLINE);

        return sb.toString();
    }

    /**
     * Builds a Java condition expression from a condition element.
     */
    private String buildConditionExpression(Element conditionElement) {
        // Get first child of condition
        Element child = firstChildElement(conditionElement);
        if (child == null) {
            return "true";
        }
        return buildConditionFromElement(child);
    }

    /**
     * Recursively builds condition expression from element.
     */
    private String buildConditionFromElement(Element element) {
        String nodeName = element.getNodeName();

        switch (nodeName) {
            case "and": {
                List<Element> children = childElementList(element);
                StringBuilder sb = new StringBuilder("(");
                boolean first = true;
                for (Element child : children) {
                    if (!first) sb.append(" && ");
                    first = false;
                    sb.append(buildConditionFromElement(child));
                }
                sb.append(")");
                return sb.toString();
            }
            case "or": {
                List<Element> children = childElementList(element);
                StringBuilder sb = new StringBuilder("(");
                boolean first = true;
                for (Element child : children) {
                    if (!first) sb.append(" || ");
                    first = false;
                    sb.append(buildConditionFromElement(child));
                }
                sb.append(")");
                return sb.toString();
            }
            case "not": {
                Element child = firstChildElement(element);
                if (child != null) {
                    return "!(" + buildConditionFromElement(child) + ")";
                }
                return "true";
            }
            case "if-empty": {
                String field = getAttr(element, "field");
                String fieldExpr = buildFieldAccess(field);
                imports.add("org.ofbiz.base.util.UtilValidate");
                return "UtilValidate.isEmpty(" + fieldExpr + ")";
            }
            case "if-not-empty": {
                String field = getAttr(element, "field");
                String fieldExpr = buildFieldAccess(field);
                imports.add("org.ofbiz.base.util.UtilValidate");
                return "UtilValidate.isNotEmpty(" + fieldExpr + ")";
            }
            case "if-compare": {
                String field = getAttr(element, "field");
                String operator = getAttr(element, "operator");
                String value = getAttr(element, "value");
                String type = getAttr(element, "type");
                String fieldExpr = buildFieldAccess(field);
                return buildCompareExpression(fieldExpr, operator, value, type);
            }
            case "if-compare-field": {
                String field = getAttr(element, "field");
                String toField = getAttr(element, "to-field");
                String operator = getAttr(element, "operator");
                String type = getAttr(element, "type");
                String fieldExpr = buildFieldAccess(field);
                String toFieldExpr = buildFieldAccess(toField);
                return buildCompareFieldExpression(fieldExpr, operator, toFieldExpr, type);
            }
            default:
                return "true /* TODO: " + nodeName + " */";
        }
    }

    /**
     * Builds a compare expression for if-compare conditions.
     */
    private String buildCompareExpression(String fieldExpr, String operator, String value, String type) {
        String valueLiteral = convertValueLiteral(value, type);

        switch (operator) {
            case "equals":
                if ("true".equals(value) || "false".equals(value)) {
                    return fieldExpr + " == " + value;
                }
                return "\"" + escapeString(value) + "\".equals(" + fieldExpr + ")";
            case "not-equals":
                return "!\"" + escapeString(value) + "\".equals(" + fieldExpr + ")";
            case "greater":
                return fieldExpr + " > " + valueLiteral;
            case "greater-equals":
                return fieldExpr + " >= " + valueLiteral;
            case "less":
                return fieldExpr + " < " + valueLiteral;
            case "less-equals":
                return fieldExpr + " <= " + valueLiteral;
            default:
                return fieldExpr + " /* " + operator + " " + value + " */";
        }
    }

    /**
     * Builds a compare-field expression.
     */
    private String buildCompareFieldExpression(String fieldExpr, String operator, String toFieldExpr, String type) {
        switch (operator) {
            case "equals":
                return "java.util.Objects.equals(" + fieldExpr + ", " + toFieldExpr + ")";
            case "not-equals":
                return "!java.util.Objects.equals(" + fieldExpr + ", " + toFieldExpr + ")";
            case "greater":
                return "((Comparable)" + fieldExpr + ").compareTo(" + toFieldExpr + ") > 0";
            case "greater-equals":
                return "((Comparable)" + fieldExpr + ").compareTo(" + toFieldExpr + ") >= 0";
            case "less":
                return "((Comparable)" + fieldExpr + ").compareTo(" + toFieldExpr + ") < 0";
            case "less-equals":
                return "((Comparable)" + fieldExpr + ").compareTo(" + toFieldExpr + ") <= 0";
            default:
                return fieldExpr + " /* " + operator + " " + toFieldExpr + " */";
        }
    }

    /**
     * Converts a <calculate> element.
     * <p>Performs arithmetic calculations with nested calcop operations.</p>
     */
    private String convertCalculate(Element element) {
        StringBuilder sb = new StringBuilder();
        String field = getAttr(element, "field");
        String type = getAttr(element, "type");
        String decimalScale = getAttr(element, "decimal-scale");
        String roundingMode = getAttr(element, "rounding-mode");

        String javaField = toJavaFieldName(field);
        String containerField = getContainerField(field);
        String accessField = getAccessField(field);

        // Process nested calcop or number
        Element calcop = firstChildElement(element, "calcop");
        Element numberElement = firstChildElement(element, "number");

        String calcExpr;
        if (calcop != null) {
            calcExpr = buildCalcExpression(calcop);
        } else if (numberElement != null) {
            // Simple number initialization like <calculate field="x" type="BigDecimal"><number value="0"/></calculate>
            String value = getAttr(numberElement, "value");
            if ("BigDecimal".equals(type)) {
                calcExpr = "new BigDecimal(\"" + value + "\")";
                imports.add("java.math.BigDecimal");
            } else {
                calcExpr = value;
            }
        } else {
            return indent() + "// TODO: Convert <calculate> element (no calcop or number)" + NEWLINE;
        }

        // Handle BigDecimal with scale and rounding
        if ("BigDecimal".equals(type) && isNotEmpty(decimalScale)) {
            imports.add("java.math.BigDecimal");
            imports.add("java.math.RoundingMode");

            // Convert FlexibleString expressions for decimalScale
            String scaleExpr = convertFlexibleStringToJava(decimalScale);
            // Convert FlexibleString expressions for roundingMode
            String roundingModeJava = convertRoundingModeFlexible(roundingMode);

            if (containerField != null) {
                // Setting into a container (map or entity)
                String containerType = declaredVarTypes.get(containerField);
                if ("GenericValue".equals(containerType)) {
                    sb.append(indent()).append(containerField).append(".set(\"").append(accessField)
                            .append("\", (").append(calcExpr).append(").setScale(")
                            .append(scaleExpr).append(", ").append(roundingModeJava).append("));").append(NEWLINE);
                } else {
                    sb.append(indent()).append("((Map<String, Object>) ").append(containerField).append(").put(\"").append(accessField)
                            .append("\", (").append(calcExpr).append(").setScale(")
                            .append(scaleExpr).append(", ").append(roundingModeJava).append("));").append(NEWLINE);
                }
            } else {
                // Setting to a variable
                if (!declaredVars.contains(javaField)) {
                    sb.append(indent()).append("BigDecimal ").append(javaField).append(" = (").append(calcExpr)
                            .append(").setScale(").append(scaleExpr).append(", ").append(roundingModeJava).append(");").append(NEWLINE);
                    declaredVars.add(javaField);
                    declaredVarTypes.put(javaField, "BigDecimal");
                } else {
                    sb.append(indent()).append(javaField).append(" = (").append(calcExpr)
                            .append(").setScale(").append(scaleExpr).append(", ").append(roundingModeJava).append(");").append(NEWLINE);
                }
            }
        } else {
            // Simple calculation without BigDecimal precision
            if (containerField != null) {
                String containerType = declaredVarTypes.get(containerField);
                if ("GenericValue".equals(containerType)) {
                    sb.append(indent()).append(containerField).append(".set(\"").append(accessField)
                            .append("\", ").append(calcExpr).append(");").append(NEWLINE);
                } else {
                    sb.append(indent()).append("((Map<String, Object>) ").append(containerField).append(").put(\"").append(accessField)
                            .append("\", ").append(calcExpr).append(");").append(NEWLINE);
                }
            } else {
                if (!declaredVars.contains(javaField)) {
                    String javaType = "BigDecimal".equals(type) ? "BigDecimal" : "Object";
                    sb.append(indent()).append(javaType).append(" ").append(javaField).append(" = ").append(calcExpr).append(";").append(NEWLINE);
                    declaredVars.add(javaField);
                    declaredVarTypes.put(javaField, javaType);
                    if ("BigDecimal".equals(type)) {
                        imports.add("java.math.BigDecimal");
                    }
                } else {
                    sb.append(indent()).append(javaField).append(" = ").append(calcExpr).append(";").append(NEWLINE);
                }
            }
        }

        return sb.toString();
    }

    /**
     * Builds a calculation expression from a calcop element.
     */
    private String buildCalcExpression(Element calcop) {
        String operator = getAttr(calcop, "operator");
        String field = getAttr(calcop, "field");

        imports.add("java.math.BigDecimal");

        // If this is a get operation, just return the field
        if ("get".equals(operator)) {
            String fieldExpr = buildFieldAccess(field);
            // Ensure it's a BigDecimal
            return "new BigDecimal(" + fieldExpr + ".toString())";
        }

        // Otherwise, process nested calcops
        List<Element> nestedCalcops = childElementList(calcop, "calcop");
        if (nestedCalcops.isEmpty()) {
            // No nested ops, might be a simple field get
            if (isNotEmpty(field)) {
                String fieldExpr = buildFieldAccess(field);
                return "new BigDecimal(" + fieldExpr + ".toString())";
            }
            return "BigDecimal.ZERO";
        }

        // Build expressions for nested calcops
        List<String> operands = new ArrayList<>();
        for (Element nested : nestedCalcops) {
            operands.add(buildCalcExpression(nested));
        }

        // Apply the operator
        switch (operator) {
            case "add":
                if (operands.size() == 2) {
                    return "(" + operands.get(0) + ").add(" + operands.get(1) + ")";
                }
                return operands.stream().reduce((a, b) -> "(" + a + ").add(" + b + ")").orElse("BigDecimal.ZERO");
            case "subtract":
                if (operands.size() == 2) {
                    return "(" + operands.get(0) + ").subtract(" + operands.get(1) + ")";
                }
                return operands.get(0);
            case "multiply":
                if (operands.size() == 2) {
                    return "(" + operands.get(0) + ").multiply(" + operands.get(1) + ")";
                }
                return operands.stream().reduce((a, b) -> "(" + a + ").multiply(" + b + ")").orElse("BigDecimal.ONE");
            case "divide":
                if (operands.size() == 2) {
                    return "(" + operands.get(0) + ").divide(" + operands.get(1) + ", java.math.RoundingMode.HALF_UP)";
                }
                return operands.get(0);
            case "negative":
                if (!operands.isEmpty()) {
                    return "(" + operands.get(0) + ").negate()";
                }
                return "BigDecimal.ZERO";
            default:
                return "/* TODO: calcop " + operator + " */ BigDecimal.ZERO";
        }
    }

    /**
     * Converts a rounding mode string to Java RoundingMode constant.
     */
    private String convertRoundingMode(String roundingMode) {
        if (roundingMode == null) return "RoundingMode.HALF_UP";
        switch (roundingMode) {
            case "HalfUp": return "RoundingMode.HALF_UP";
            case "HalfDown": return "RoundingMode.HALF_DOWN";
            case "HalfEven": return "RoundingMode.HALF_EVEN";
            case "Up": return "RoundingMode.UP";
            case "Down": return "RoundingMode.DOWN";
            case "Ceiling": return "RoundingMode.CEILING";
            case "Floor": return "RoundingMode.FLOOR";
            default: return "RoundingMode.HALF_UP";
        }
    }

    /**
     * Converts a FlexibleString expression (${varName}) to Java code.
     * If it's a plain value, returns it as-is.
     */
    private String convertFlexibleStringToJava(String expr) {
        if (expr == null) return "0";
        if (expr.startsWith("${") && expr.endsWith("}")) {
            // Extract variable name and convert to Java expression
            String varName = expr.substring(2, expr.length() - 1);
            // Use buildFieldAccess to handle undeclared variables (falls back to context.get())
            String fieldExpr = buildFieldAccess(varName);
            // Cast to int for scale parameter
            return "((Number) " + fieldExpr + ").intValue()";
        }
        // Plain numeric value
        return expr;
    }

    /**
     * Converts a rounding mode that may be a FlexibleString expression.
     */
    private String convertRoundingModeFlexible(String roundingMode) {
        if (roundingMode == null) return "RoundingMode.HALF_UP";
        if (roundingMode.startsWith("${") && roundingMode.endsWith("}")) {
            // Extract variable name and convert to RoundingMode
            String varName = roundingMode.substring(2, roundingMode.length() - 1);
            // Use buildFieldAccess to handle undeclared variables (falls back to context.get())
            String fieldExpr = buildFieldAccess(varName);
            return "RoundingMode.valueOf(String.valueOf(" + fieldExpr + ").toUpperCase().replace(\"-\", \"_\"))";
        }
        return convertRoundingMode(roundingMode);
    }

    /**
     * Converts an <iterate-map> element.
     * <p>Iterates over map entries using a for-each loop.</p>
     */
    private String convertIterateMap(Element element) {
        StringBuilder sb = new StringBuilder();
        String map = getAttr(element, "map");
        String key = getAttr(element, "key");
        String value = getAttr(element, "value");

        String javaKey = toJavaFieldName(key);
        String javaValue = toJavaFieldName(value);
        String mapExpr = buildFieldAccess(map);

        // Determine map type - if it's a GenericValue, we iterate over its fields
        String mapType = declaredVarTypes.get(toJavaFieldName(map));
        boolean isGenericValue = "GenericValue".equals(mapType);

        imports.add("java.util.Map");

        if (isGenericValue) {
            // For GenericValue, iterate over getAllFields().entrySet()
            sb.append(indent()).append("for (Map.Entry<String, Object> entry : ").append(mapExpr)
                    .append(".getAllFields().entrySet()) {").append(NEWLINE);
        } else {
            // For regular maps
            sb.append(indent()).append("for (Map.Entry<String, Object> entry : ((Map<String, Object>) ").append(mapExpr)
                    .append(").entrySet()) {").append(NEWLINE);
        }
        indentLevel++;

        // Track these as declared in the loop scope
        Set<String> savedDeclaredVars = new HashSet<>(declaredVars);
        Map<String, String> savedDeclaredVarTypes = new HashMap<>(declaredVarTypes);

        // Assign key and value variables - only declare if not already declared
        // If already declared as Object (from clear-field), keep the Object type - code using these vars
        // will need to cast as appropriate
        if (!declaredVars.contains(javaKey)) {
            sb.append(indent()).append("String ").append(javaKey).append(" = entry.getKey();").append(NEWLINE);
            declaredVars.add(javaKey);
            declaredVarTypes.put(javaKey, "String");
        } else {
            // Already declared, just assign - do NOT update the type since Java declaration is unchanged
            sb.append(indent()).append(javaKey).append(" = entry.getKey();").append(NEWLINE);
        }

        if (!declaredVars.contains(javaValue)) {
            sb.append(indent()).append("Object ").append(javaValue).append(" = entry.getValue();").append(NEWLINE);
            declaredVars.add(javaValue);
            declaredVarTypes.put(javaValue, "Object");
        } else {
            // Already declared, just assign
            sb.append(indent()).append(javaValue).append(" = entry.getValue();").append(NEWLINE);
        }

        // Process body
        for (Element child : childElementList(element)) {
            sb.append(convertElement(child));
        }

        // Restore outer scope
        declaredVars = savedDeclaredVars;
        declaredVarTypes = savedDeclaredVarTypes;

        indentLevel--;
        sb.append(indent()).append("}").append(NEWLINE);

        return sb.toString();
    }

    /**
     * Converts an <assert> element.
     * <p>Generates assertion checks. Used primarily in test simple-methods.</p>
     */
    private String convertAssert(Element element) {
        StringBuilder sb = new StringBuilder();

        // Process each child assertion
        for (Element child : childElementList(element)) {
            String nodeName = child.getNodeName();

            switch (nodeName) {
                case "not": {
                    Element inner = firstChildElement(child);
                    if (inner != null) {
                        String condition = buildAssertCondition(inner);
                        sb.append(indent()).append("assert !(").append(condition).append(") : \"Assertion failed: not ")
                                .append(inner.getNodeName()).append("\";").append(NEWLINE);
                    }
                    break;
                }
                case "if-empty":
                case "if-not-empty":
                case "if-compare":
                case "if-compare-field": {
                    String condition = buildAssertCondition(child);
                    sb.append(indent()).append("assert ").append(condition).append(" : \"Assertion failed: ")
                            .append(nodeName).append("\";").append(NEWLINE);
                    break;
                }
                default:
                    sb.append(indent()).append("// TODO: Convert assertion <").append(nodeName).append(">").append(NEWLINE);
            }
        }

        return sb.toString();
    }

    /**
     * Builds an assertion condition from an element.
     */
    private String buildAssertCondition(Element element) {
        String nodeName = element.getNodeName();

        switch (nodeName) {
            case "if-empty": {
                String field = getAttr(element, "field");
                String fieldExpr = buildFieldAccess(field);
                imports.add("org.ofbiz.base.util.UtilValidate");
                return "UtilValidate.isEmpty(" + fieldExpr + ")";
            }
            case "if-not-empty": {
                String field = getAttr(element, "field");
                String fieldExpr = buildFieldAccess(field);
                imports.add("org.ofbiz.base.util.UtilValidate");
                return "UtilValidate.isNotEmpty(" + fieldExpr + ")";
            }
            case "if-compare": {
                String field = getAttr(element, "field");
                String operator = getAttr(element, "operator");
                String value = getAttr(element, "value");
                String type = getAttr(element, "type");
                String fieldExpr = buildFieldAccess(field);
                return buildCompareExpression(fieldExpr, operator, value, type);
            }
            case "if-compare-field": {
                String field = getAttr(element, "field");
                String operator = getAttr(element, "operator");
                String toField = getAttr(element, "to-field");
                String type = getAttr(element, "type");
                String fieldExpr = buildFieldAccess(field);
                String toFieldExpr = buildFieldAccess(toField);
                return buildCompareFieldExpression(fieldExpr, operator, toFieldExpr, type);
            }
            default:
                return "true /* TODO: " + nodeName + " */";
        }
    }

    /**
     * Converts a &lt;field-to-list&gt; element.
     * This appends a field value to a list.
     */
    private String convertFieldToList(Element element) {
        StringBuilder sb = new StringBuilder();

        String field = getAttr(element, "field");
        String list = getAttr(element, "list");

        String javaField = toJavaFieldName(field);
        String javaList = toJavaFieldName(list);
        String fieldExpr = buildFieldAccess(field);

        // Ensure the list exists
        if (!declaredVars.contains(javaList)) {
            sb.append(indent()).append("List<Object> ").append(javaList).append(" = new LinkedList<>();").append(NEWLINE);
            declaredVars.add(javaList);
            declaredVarTypes.put(javaList, "List<Object>");
            imports.add("java.util.List");
            imports.add("java.util.LinkedList");
        }

        // Check list type for casting
        String listType = declaredVarTypes.get(javaList);
        boolean needsCast = listType == null ||
                            (!"List<Object>".equals(listType) &&
                             !listType.startsWith("List"));
        String addTarget = needsCast ? "((List<Object>) " + javaList + ")" : javaList;
        sb.append(indent()).append(addTarget).append(".add(").append(fieldExpr).append(");").append(NEWLINE);

        return sb.toString();
    }

    /**
     * Converts a &lt;make-next-seq-id&gt; element.
     * This generates the next sequence ID for a field on an entity.
     */
    private String convertMakeNextSeqId(Element element) {
        StringBuilder sb = new StringBuilder();

        String valueField = getAttr(element, "value-field");
        String seqFieldName = getAttr(element, "seq-field-name");
        String numericPadding = getAttr(element, "numeric-padding");

        String javaValueField = toJavaFieldName(valueField);
        String javaSeqField = toJavaFieldName(seqFieldName);

        // Generate the next sequence ID
        // In Simple-methods this calls delegator.setNextSubSeqId(entity, fieldName, padding, 1)
        if (isNotEmpty(numericPadding)) {
            sb.append(indent()).append("delegator.setNextSubSeqId(").append(javaValueField)
                    .append(", \"").append(seqFieldName).append("\", ")
                    .append(numericPadding).append(", 1);").append(NEWLINE);
        } else {
            sb.append(indent()).append("delegator.setNextSubSeqId(").append(javaValueField)
                    .append(", \"").append(seqFieldName).append("\", 5, 1);").append(NEWLINE);
        }

        // Also expose the sequence field as a standalone variable (simple-methods behavior)
        // The field value is accessible directly after make-next-seq-id
        if (!declaredVars.contains(javaSeqField)) {
            sb.append(indent()).append("Object ").append(javaSeqField).append(" = ")
                    .append(javaValueField).append(".get(\"").append(seqFieldName).append("\");").append(NEWLINE);
            declaredVars.add(javaSeqField);
            declaredVarTypes.put(javaSeqField, "Object");
        } else {
            sb.append(indent()).append(javaSeqField).append(" = ")
                    .append(javaValueField).append(".get(\"").append(seqFieldName).append("\");").append(NEWLINE);
        }

        return sb.toString();
    }

    /**
     * Converts a &lt;to-string&gt; element.
     * This converts a field value to a String.
     */
    private String convertToString(Element element) {
        StringBuilder sb = new StringBuilder();

        String field = getAttr(element, "field");
        String javaField = toJavaFieldName(field);

        // If the field is in a container (map.key), we need to handle it differently
        String containerField = getContainerField(field);
        if (containerField != null) {
            String accessField = getAccessField(field);
            String containerType = declaredVarTypes.get(containerField);
            boolean needsCast = !"GenericValue".equals(containerType) &&
                                !"Map<String, Object>".equals(containerType);
            String getTarget = needsCast ? "((Map<String, Object>) " + containerField + ")" : containerField;

            // Get, convert to string, and put back
            sb.append(indent()).append("{").append(NEWLINE);
            indentLevel++;
            sb.append(indent()).append("Object _val = ").append(getTarget).append(".get(\"").append(accessField).append("\");").append(NEWLINE);
            sb.append(indent()).append(getTarget).append(".put(\"").append(accessField).append("\", _val != null ? _val.toString() : null);").append(NEWLINE);
            indentLevel--;
            sb.append(indent()).append("}").append(NEWLINE);
        } else {
            // Simple variable - convert to string
            if (declaredVars.contains(javaField)) {
                sb.append(indent()).append(javaField).append(" = ").append(javaField)
                        .append(" != null ? ").append(javaField).append(".toString() : null;").append(NEWLINE);
            } else {
                sb.append(indent()).append("// WARNING: to-string on undeclared variable: ").append(javaField).append(NEWLINE);
            }
        }

        return sb.toString();
    }

    /**
     * Converts a &lt;call-object-method&gt; element.
     * This calls a method on an object.
     */
    private String convertCallObjectMethod(Element element) {
        StringBuilder sb = new StringBuilder();

        String objField = getAttr(element, "obj-field");
        String methodName = getAttr(element, "method-name");
        String retField = getAttr(element, "ret-field");

        String objExpr = buildFieldAccess(objField);

        // Collect parameters
        List<Element> stringParams = childElementList(element, "string");
        List<Element> fieldParams = childElementList(element, "field");
        StringBuilder args = new StringBuilder();

        for (Element str : stringParams) {
            if (args.length() > 0) args.append(", ");
            String value = getAttr(str, "value");
            args.append("\"").append(escapeString(value)).append("\"");
        }
        for (Element fld : fieldParams) {
            if (args.length() > 0) args.append(", ");
            String fieldRef = getAttr(fld, "field");
            args.append(buildFieldAccess(fieldRef));
        }

        // For common Number methods, cast the object appropriately
        String methodCall;
        String retType = "Object";
        if ("intValue".equals(methodName)) {
            methodCall = "((Number) " + objExpr + ").intValue()";
            retType = "int";
        } else if ("longValue".equals(methodName)) {
            methodCall = "((Number) " + objExpr + ").longValue()";
            retType = "long";
        } else if ("doubleValue".equals(methodName)) {
            methodCall = "((Number) " + objExpr + ").doubleValue()";
            retType = "double";
        } else if ("floatValue".equals(methodName)) {
            methodCall = "((Number) " + objExpr + ").floatValue()";
            retType = "float";
        } else if ("toString".equals(methodName) && args.length() == 0) {
            methodCall = "String.valueOf(" + objExpr + ")";
            retType = "String";
        } else if ("substring".equals(methodName)) {
            methodCall = "((String) " + objExpr + ").substring(" + args + ")";
            retType = "String";
        } else if ("length".equals(methodName) && args.length() == 0) {
            methodCall = "((String) " + objExpr + ").length()";
            retType = "int";
        } else if ("charAt".equals(methodName)) {
            methodCall = "((String) " + objExpr + ").charAt(" + args + ")";
            retType = "char";
        } else if ("indexOf".equals(methodName)) {
            methodCall = "((String) " + objExpr + ").indexOf(" + args + ")";
            retType = "int";
        } else if ("startsWith".equals(methodName) || "endsWith".equals(methodName)) {
            methodCall = "((String) " + objExpr + ")." + methodName + "(" + args + ")";
            retType = "boolean";
        } else if ("toLowerCase".equals(methodName) || "toUpperCase".equals(methodName) || "trim".equals(methodName)) {
            methodCall = "((String) " + objExpr + ")." + methodName + "()";
            retType = "String";
        } else if ("equals".equals(methodName)) {
            methodCall = objExpr + ".equals(" + args + ")";
            retType = "boolean";
        } else {
            methodCall = objExpr + "." + methodName + "(" + args + ")";
        }

        if (isNotEmpty(retField)) {
            String javaRetField = toJavaFieldName(retField);
            if (!declaredVars.contains(javaRetField)) {
                sb.append(indent()).append(retType).append(" ").append(javaRetField).append(" = ").append(methodCall).append(";").append(NEWLINE);
                declaredVars.add(javaRetField);
                declaredVarTypes.put(javaRetField, retType);
            } else {
                sb.append(indent()).append(javaRetField).append(" = ").append(methodCall).append(";").append(NEWLINE);
            }
        } else {
            sb.append(indent()).append(methodCall).append(";").append(NEWLINE);
        }

        return sb.toString();
    }

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
}
