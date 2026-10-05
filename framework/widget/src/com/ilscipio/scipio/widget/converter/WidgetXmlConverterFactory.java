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

import org.w3c.dom.Document;

/**
 * Factory for creating XML-to-Annotation converters based on widget type.
 *
 * <p>SCIPIO: 4.0.0: Added for XML-to-Annotation conversion support.</p>
 */
public class WidgetXmlConverterFactory {

    /**
     * Supported widget types.
     */
    public enum WidgetType {
        SCREENS,
        FORMS,
        MENUS,
        TREES
    }

    /**
     * Creates an appropriate converter based on the XML document's root element.
     *
     * @param doc XML document
     * @param packageName Java package for generated class
     * @param className Java class name for generated class
     * @param componentName Component name (e.g., "setup")
     * @param outputDir Directory for generated Java file
     * @param scriptOutputDir Directory for extracted Groovy scripts
     * @return Appropriate converter instance
     * @throws IllegalArgumentException if the XML type is not supported
     */
    public static XmlToAnnotationConverter createConverter(Document doc,
                                                           String packageName,
                                                           String className,
                                                           String componentName,
                                                           File outputDir,
                                                           File scriptOutputDir) {
        WidgetType type = detectXmlType(doc);
        return createConverter(type, packageName, className, componentName, outputDir, scriptOutputDir);
    }

    /**
     * Creates a converter for the specified widget type.
     *
     * @param type Widget type
     * @param packageName Java package for generated class
     * @param className Java class name for generated class
     * @param componentName Component name (e.g., "setup")
     * @param outputDir Directory for generated Java file
     * @param scriptOutputDir Directory for extracted Groovy scripts
     * @return Appropriate converter instance
     */
    public static XmlToAnnotationConverter createConverter(WidgetType type,
                                                           String packageName,
                                                           String className,
                                                           String componentName,
                                                           File outputDir,
                                                           File scriptOutputDir) {
        switch (type) {
            case SCREENS:
                return new ScreenXmlToAnnotationConverter(packageName, className, componentName,
                        outputDir, scriptOutputDir);
            case FORMS:
                return new FormXmlToAnnotationConverter(packageName, className, componentName,
                        outputDir, scriptOutputDir);
            case MENUS:
                return new MenuXmlToAnnotationConverter(packageName, className, componentName,
                        outputDir, scriptOutputDir);
            case TREES:
                return new TreeXmlToAnnotationConverter(packageName, className, componentName,
                        outputDir, scriptOutputDir);
            default:
                throw new IllegalArgumentException("Unsupported widget type: " + type);
        }
    }

    /**
     * Detects the widget type from the XML document's root element.
     *
     * @param doc XML document
     * @return Detected widget type
     * @throws IllegalArgumentException if the root element is not a recognized widget type
     */
    public static WidgetType detectXmlType(Document doc) {
        String rootElement = doc.getDocumentElement().getNodeName();
        return parseWidgetType(rootElement);
    }

    /**
     * Parses a widget type from a string.
     *
     * @param typeString Type string (e.g., "screens", "forms", "menus", "tree")
     * @return Parsed widget type
     * @throws IllegalArgumentException if the string is not a recognized widget type
     */
    public static WidgetType parseWidgetType(String typeString) {
        if (typeString == null) {
            throw new IllegalArgumentException("Widget type string cannot be null");
        }

        switch (typeString.toLowerCase()) {
            case "screens":
                return WidgetType.SCREENS;
            case "forms":
                return WidgetType.FORMS;
            case "menus":
                return WidgetType.MENUS;
            case "tree":
            case "trees":
                return WidgetType.TREES;
            default:
                throw new IllegalArgumentException("Unrecognized widget type: " + typeString +
                        ". Supported types: screens, forms, menus, tree");
        }
    }

    /**
     * Returns the conventional suffix for generated class names based on widget type.
     *
     * @param type Widget type
     * @return Conventional suffix (e.g., "Screens", "Forms")
     */
    public static String getClassNameSuffix(WidgetType type) {
        switch (type) {
            case SCREENS:
                return "Screens";
            case FORMS:
                return "Forms";
            case MENUS:
                return "Menus";
            case TREES:
                return "Trees";
            default:
                return "Widgets";
        }
    }

    /**
     * Derives a class name from an XML file name.
     *
     * <p>For example: "SetupForms.xml" -> "SetupForms"</p>
     *
     * @param xmlFileName XML file name (with or without path)
     * @return Derived class name
     */
    public static String deriveClassName(String xmlFileName) {
        // Extract base name without path
        String baseName = xmlFileName;
        int lastSep = Math.max(baseName.lastIndexOf('/'), baseName.lastIndexOf('\\'));
        if (lastSep >= 0) {
            baseName = baseName.substring(lastSep + 1);
        }

        // Remove .xml extension
        if (baseName.toLowerCase().endsWith(".xml")) {
            baseName = baseName.substring(0, baseName.length() - 4);
        }

        // Ensure valid Java identifier
        String className = toValidJavaIdentifier(baseName);

        return className;
    }

    /**
     * Derives a package name from a component name.
     *
     * @param componentName Component name (e.g., "setup")
     * @return Package name (e.g., "com.ilscipio.scipio.setup.widget")
     */
    public static String derivePackageName(String componentName) {
        String cleanName = toValidJavaIdentifier(componentName.toLowerCase());
        return "com.ilscipio.scipio." + cleanName + ".widget";
    }

    /**
     * Converts a string to a valid Java identifier.
     */
    private static String toValidJavaIdentifier(String s) {
        if (s == null || s.isEmpty()) {
            return "Generated";
        }

        StringBuilder sb = new StringBuilder();
        for (int i = 0; i < s.length(); i++) {
            char c = s.charAt(i);
            if (i == 0) {
                if (Character.isJavaIdentifierStart(c)) {
                    sb.append(c);
                } else if (Character.isJavaIdentifierPart(c)) {
                    sb.append('_').append(c);
                }
            } else {
                if (Character.isJavaIdentifierPart(c)) {
                    sb.append(c);
                } else {
                    sb.append('_');
                }
            }
        }

        return sb.length() > 0 ? sb.toString() : "Generated";
    }
}
