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
import java.io.FileWriter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.LinkedHashSet;
import java.util.Locale;
import java.util.Set;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

/**
 * Base class for XML-to-Annotation converters.
 *
 * <p>Provides common utilities for generating Java annotation source code
 * from XML widget definitions.</p>
 *
 * <p>SCIPIO: 4.0.0: Added for XML-to-Annotation conversion support.</p>
 */
public abstract class XmlToAnnotationConverter {

    protected static final String INDENT = "    ";
    protected static final String NEWLINE = System.lineSeparator();

    protected final String packageName;
    protected final String className;
    protected final String componentName;
    protected final File outputDir;
    protected final File scriptOutputDir;

    /**
     * SCIPIO: 4.0.0: The source location URL (e.g., "component://setup/widget/SetupScreens.xml")
     * for generating location alias attributes in annotations.
     */
    protected String sourceLocation;

    // Collected scripts to extract
    protected final List<ExtractedScript> extractedScripts = new ArrayList<>();

    /**
     * SCIPIO: 4.0.0: Nested interface/class names already emitted for the
     * Java class currently being generated, tracked in lower-case form.
     *
     * <p>Windows' case-insensitive filesystem maps two distinct class files
     * (e.g. {@code Foo$listX.class} and {@code Foo$ListX.class}) to the SAME
     * file on disk, so the second javac writes silently clobbers the first.
     * At runtime {@code Class.getDeclaredClasses()} then throws
     * {@code NoClassDefFoundError} for the missing entry. This set lets
     * converters detect a same-file, case-insensitive name collision before
     * it is written out.</p>
     */
    protected final Set<String> usedInterfaceNamesLc = new LinkedHashSet<>();

    /**
     * SCIPIO: 4.0.0: Clears interface-name collision tracking. Subclasses
     * must call this once at the start of each {@link #convert(Document)}
     * run, so every generated Java class starts with a clean uniqueness
     * scope (names must only be unique within the same generated class).
     */
    protected void resetInterfaceNameTracking() {
        usedInterfaceNamesLc.clear();
    }

    /**
     * SCIPIO: 4.0.0: Ensures a nested interface/class name is unique within
     * the Java class currently being generated, on a CASE-INSENSITIVE basis.
     *
     * <p>When two XML widget names (e.g. form names "listLeads" and
     * "ListLeads") differ only by case, their sanitized interface names
     * collide on Windows' case-insensitive filesystem even though they are
     * distinct, valid Java identifiers. The first occurrence of a name is
     * kept unchanged; every later name that collides case-insensitively
     * gets a disambiguating {@code "Lc"} suffix appended (repeated if still
     * colliding) until it is unique.</p>
     *
     * <p>This only changes the carrier Java identifier. The emitted
     * annotation's {@code name} attribute (e.g. {@code @Form(name = ...)})
     * is derived separately and is NOT affected.</p>
     *
     * @param candidate the sanitized interface name (see {@code toInterfaceName})
     * @return a name guaranteed unique, case-insensitively, among names
     *         resolved since the last {@link #resetInterfaceNameTracking()}
     */
    protected String resolveUniqueInterfaceName(String candidate) {
        String result = candidate;
        while (!usedInterfaceNamesLc.add(result.toLowerCase(Locale.ROOT))) {
            result = result + "Lc";
        }
        return result;
    }

    /**
     * Creates a new converter.
     *
     * @param packageName Java package for generated class
     * @param className Java class name for generated class
     * @param componentName Component name (e.g., "setup")
     * @param outputDir Directory for generated Java file
     * @param scriptOutputDir Directory for extracted Groovy scripts
     */
    public XmlToAnnotationConverter(String packageName, String className, String componentName,
                                    File outputDir, File scriptOutputDir) {
        this.packageName = packageName;
        this.className = className;
        this.componentName = componentName;
        this.outputDir = outputDir;
        this.scriptOutputDir = scriptOutputDir;
    }

    /**
     * SCIPIO: 4.0.0: Sets the source location URL for generating location alias attributes.
     *
     * @param sourceLocation The component:// style URL (e.g., "component://setup/widget/SetupScreens.xml")
     */
    public void setSourceLocation(String sourceLocation) {
        this.sourceLocation = sourceLocation;
    }

    /**
     * SCIPIO: 4.0.0: Gets the source location URL.
     */
    public String getSourceLocation() {
        return sourceLocation;
    }

    /**
     * Converts the XML document to Java annotation source code.
     *
     * @param doc XML document to convert
     * @return Generated Java source code
     */
    public abstract String convert(Document doc);

    /**
     * Returns the import statements needed for this widget type.
     */
    protected abstract String getImports();

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

        // Write extracted scripts
        for (ExtractedScript script : extractedScripts) {
            script.writeToFile();
        }
    }

    /**
     * Gets the output Java file path.
     */
    public File getOutputFile() {
        File packageDir = new File(outputDir, packageName.replace('.', File.separatorChar));
        return new File(packageDir, className + ".java");
    }

    /**
     * Writes all extracted scripts to files.
     * SCIPIO: 4.0.0: Added to support Gradle task that calls convert() separately from file writing.
     */
    public void writeExtractedScripts() throws IOException {
        for (ExtractedScript script : extractedScripts) {
            script.writeToFile();
        }
    }

    // ========================================================================
    // Code Generation Utilities
    // ========================================================================

    /**
     * Generates indentation.
     */
    protected String indent(int level) {
        StringBuilder sb = new StringBuilder();
        for (int i = 0; i < level; i++) {
            sb.append(INDENT);
        }
        return sb.toString();
    }

    /**
     * Escapes a string for use in Java source code.
     */
    protected String escapeString(String s) {
        if (s == null) return null;
        return s.replace("\\", "\\\\")
                .replace("\"", "\\\"")
                .replace("\n", "\\n")
                .replace("\r", "\\r")
                .replace("\t", "\\t");
    }

    /**
     * Converts an XML attribute value to annotation string value.
     * Wraps in quotes and escapes as needed.
     */
    protected String toStringValue(String value) {
        if (value == null) return "\"\"";
        return "\"" + escapeString(value) + "\"";
    }

    /**
     * Generates an annotation attribute assignment if value is not empty.
     *
     * @param attrName Annotation attribute name
     * @param value Value (will be quoted)
     * @return "attrName = \"value\"" or empty string if value is empty
     */
    protected String attrIfNotEmpty(String attrName, String value) {
        if (isEmpty(value)) return "";
        return attrName + " = " + toStringValue(value);
    }

    /**
     * Generates an annotation attribute for a boolean.
     */
    protected String boolAttr(String attrName, boolean value, boolean defaultValue) {
        if (value == defaultValue) return "";
        return attrName + " = " + value;
    }

    /**
     * Generates an annotation attribute for an integer.
     */
    protected String intAttr(String attrName, int value, int defaultValue) {
        if (value == defaultValue) return "";
        return attrName + " = " + value;
    }

    /**
     * Parses a boolean from XML attribute value.
     */
    protected boolean parseBoolean(String value, boolean defaultValue) {
        if (isEmpty(value)) return defaultValue;
        return "true".equalsIgnoreCase(value) || "Y".equalsIgnoreCase(value);
    }

    /**
     * Parses an integer from XML attribute value.
     */
    protected int parseInt(String value, int defaultValue) {
        if (isEmpty(value)) return defaultValue;
        try {
            return Integer.parseInt(value);
        } catch (NumberFormatException e) {
            return defaultValue;
        }
    }

    /**
     * Joins non-empty strings with comma separator.
     */
    protected String joinAttrs(String... attrs) {
        StringBuilder sb = new StringBuilder();
        boolean first = true;
        for (String attr : attrs) {
            if (isNotEmpty(attr)) {
                if (!first) sb.append(", ");
                sb.append(attr);
                first = false;
            }
        }
        return sb.toString();
    }

    /**
     * Joins non-empty strings with comma+newline separator.
     */
    protected String joinAttrsMultiline(int indentLevel, String... attrs) {
        StringBuilder sb = new StringBuilder();
        boolean first = true;
        String indentStr = indent(indentLevel);
        for (String attr : attrs) {
            if (isNotEmpty(attr)) {
                if (!first) {
                    sb.append(",").append(NEWLINE).append(indentStr);
                }
                sb.append(attr);
                first = false;
            }
        }
        return sb.toString();
    }

    /**
     * Formats a long annotation string across multiple lines with proper indentation.
     * Adds line breaks after opening braces and before closing braces for nested structures.
     * SCIPIO: 4.0.0: Added to prevent long single-line annotations that cause parsing issues.
     * @param annotation The annotation string to format
     * @param baseIndent The base indentation level (number of 4-space indents)
     * @param maxLineLength Maximum line length before formatting kicks in
     */
    protected String formatAnnotation(String annotation, int baseIndent, int maxLineLength) {
        if (annotation == null || annotation.length() <= maxLineLength) {
            return annotation;
        }

        StringBuilder result = new StringBuilder();
        int currentIndent = baseIndent;
        int lineStart = 0;
        boolean inString = false;
        char prevChar = 0;

        for (int i = 0; i < annotation.length(); i++) {
            char c = annotation.charAt(i);

            // Track string literals to avoid breaking inside them
            if (c == '"' && prevChar != '\\') {
                inString = !inString;
            }

            if (!inString) {
                // Add newline after opening brace/bracket if followed by @
                if ((c == '{' || c == '(') && i + 1 < annotation.length()) {
                    char next = annotation.charAt(i + 1);
                    if (next == '@' || next == '{') {
                        result.append(c);
                        currentIndent++;
                        result.append(NEWLINE).append(indent(currentIndent));
                        lineStart = result.length();
                        prevChar = c;
                        continue;
                    }
                }
                // Add newline before closing brace if line is long
                if ((c == '}' || c == ')') && result.length() - lineStart > 60) {
                    if (currentIndent > baseIndent) {
                        currentIndent--;
                    }
                    result.append(NEWLINE).append(indent(currentIndent));
                    lineStart = result.length();
                }
                // Add newline after comma followed by @ annotation
                if (c == ',' && i + 2 < annotation.length()) {
                    char next1 = annotation.charAt(i + 1);
                    char next2 = annotation.charAt(i + 2);
                    if (next1 == ' ' && next2 == '@') {
                        result.append(c);
                        result.append(NEWLINE).append(indent(currentIndent));
                        i++; // skip the space
                        lineStart = result.length();
                        prevChar = c;
                        continue;
                    }
                }
            }
            result.append(c);
            prevChar = c;
        }

        return result.toString();
    }

    /**
     * Converts a kebab-case XML attribute name to camelCase Java name.
     * e.g., "extends-resource" -> "extendsResource"
     */
    protected String toCamelCase(String kebabCase) {
        if (kebabCase == null || !kebabCase.contains("-")) return kebabCase;
        StringBuilder sb = new StringBuilder();
        boolean capitalizeNext = false;
        for (char c : kebabCase.toCharArray()) {
            if (c == '-') {
                capitalizeNext = true;
            } else {
                sb.append(capitalizeNext ? Character.toUpperCase(c) : c);
                capitalizeNext = false;
            }
        }
        return sb.toString();
    }

    /**
     * Gets attribute value from element, returns empty string if not present.
     */
    protected String getAttr(Element element, String attrName) {
        return element.getAttribute(attrName);
    }

    /**
     * Gets attribute value, returns default if not present or empty.
     */
    protected String getAttr(Element element, String attrName, String defaultValue) {
        String value = element.getAttribute(attrName);
        return isEmpty(value) ? defaultValue : value;
    }

    /**
     * Creates an extracted script entry for a CDATA script block.
     *
     * @param widgetName Name of the widget containing the script
     * @param scriptIndex Index of the script within the widget
     * @param lang Script language (groovy, bsh, etc.)
     * @param code Script code content
     * @return Location string for @ScriptAction annotation
     */
    protected String extractScript(String widgetName, int scriptIndex, String lang, String code) {
        String filename = widgetName + "_script" + scriptIndex + "." + (isNotEmpty(lang) ? lang : "groovy");
        String location = "component://" + componentName + "/webapp/" + componentName +
                          "/WEB-INF/actions/generated/" + filename;

        ExtractedScript script = new ExtractedScript(filename, code);
        extractedScripts.add(script);

        return location;
    }

    /**
     * Holds information about an extracted script file.
     */
    protected class ExtractedScript {
        final String filename;
        final String content;

        ExtractedScript(String filename, String content) {
            this.filename = filename;
            this.content = content;
        }

        void writeToFile() throws IOException {
            if (scriptOutputDir == null) return;
            scriptOutputDir.mkdirs();
            File scriptFile = new File(scriptOutputDir, filename);
            try (FileWriter writer = new FileWriter(scriptFile)) {
                writer.write(content);
            }
        }
    }

    /**
     * Generates the class header with package, imports, and class declaration.
     */
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
        sb.append(getImports());
        sb.append(NEWLINE);

        // Class javadoc
        sb.append("/**").append(NEWLINE);
        sb.append(" * Auto-generated annotation-based widget definitions.").append(NEWLINE);
        sb.append(" *").append(NEWLINE);
        sb.append(" * <p>Generated from XML by convertXmlToAnnotation Gradle task.</p>").append(NEWLINE);
        sb.append(" *").append(NEWLINE);
        sb.append(" * <p>SCIPIO: 4.0.0: Auto-generated.</p>").append(NEWLINE);
        sb.append(" */").append(NEWLINE);

        // Class declaration
        sb.append("public class ").append(className).append(" {").append(NEWLINE);

        return sb.toString();
    }

    /**
     * Generates the class footer.
     */
    protected String generateClassFooter() {
        return "}" + NEWLINE;
    }

    // ========================================================================
    // DOM Utilities (to avoid dependency on UtilXml which requires Scipio runtime)
    // ========================================================================

    /**
     * Gets all direct child elements with the specified tag name.
     * Pure DOM implementation to avoid Scipio runtime dependencies.
     */
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

    /**
     * Gets all direct child elements.
     */
    protected List<Element> childElementList(Element parent) {
        return childElementList(parent, null);
    }

    /**
     * Gets the first direct child element with the specified tag name.
     */
    protected Element firstChildElement(Element parent, String tagName) {
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

    /**
     * Gets the text content of the first child element with the specified tag name.
     */
    protected String childElementValue(Element parent, String tagName) {
        Element child = firstChildElement(parent, tagName);
        if (child == null) return null;
        return child.getTextContent();
    }

    /**
     * Converts a name to a valid Java interface name.
     * E.g., "EditUser" -> "EditUser", "edit-user" -> "EditUser"
     */
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
        // Ensure starts with uppercase letter
        if (result.length() > 0 && Character.isDigit(result.charAt(0))) {
            result = "_" + result;
        }
        return result;
    }

    // ========================================================================
    // String Utilities (to avoid UtilValidate dependency)
    // ========================================================================

    /**
     * Checks if a string is empty or null.
     */
    protected static boolean isEmpty(String s) {
        return s == null || s.isEmpty();
    }

    /**
     * Checks if a string is not empty.
     */
    protected static boolean isNotEmpty(String s) {
        return s != null && !s.isEmpty();
    }
}
