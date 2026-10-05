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
package com.ilscipio.scipio.ce.webapp.control.converter;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.util.*;
import java.util.HashSet;
import java.util.Set;

/**
 * Converts controller.xml to Java annotation-based controller definitions.
 *
 * <p>Generates @Request and @View annotated classes from controller XML.</p>
 *
 * <p>SCIPIO: 4.0.0: Added for XML-to-Annotation conversion support.</p>
 */
public class ControllerXmlToAnnotationConverter {

    protected static final String INDENT = "    ";
    protected static final String NEWLINE = System.lineSeparator();

    protected final String packageName;
    protected final String className;
    protected final String componentName;
    protected final String controllerName;
    protected final File outputDir;

    // Collect referenced classes for imports
    protected final Set<String> referencedClasses = new HashSet<>();

    // Track used interface names to avoid duplicates
    protected final Map<String, Integer> usedInterfaceNames = new HashMap<>();

    // Track used VIEW_ field names to avoid duplicates
    protected final Set<String> usedViewFieldNames = new HashSet<>();

    // Track used method names to avoid duplicates
    protected final Set<String> usedMethodNames = new HashSet<>();

    public ControllerXmlToAnnotationConverter(String packageName, String className, String componentName,
                                              String controllerName, File outputDir) {
        this.packageName = packageName;
        this.className = className;
        this.componentName = componentName;
        this.controllerName = controllerName;
        this.outputDir = outputDir;
    }

    /**
     * Converts the controller XML document to Java annotation source code.
     */
    public String convert(Document doc) {
        // Clear referenced classes and tracking sets from previous runs
        referencedClasses.clear();
        usedInterfaceNames.clear();
        usedViewFieldNames.clear();
        usedMethodNames.clear();

        Element root = doc.getDocumentElement();

        // First pass: collect all referenced Java classes for imports
        List<Element> requestMaps = childElementList(root, "request-map");
        for (Element requestMap : requestMaps) {
            Element event = firstChildElement(requestMap, "event");
            if (event != null) {
                String eventType = getAttr(event, "type");
                String eventPath = getAttr(event, "path");
                if ("java".equals(eventType) && isNotEmpty(eventPath)) {
                    referencedClasses.add(eventPath);
                }
            }
        }

        // Second pass: generate the actual code
        StringBuilder sb = new StringBuilder();
        sb.append(generateClassHeader(referencedClasses));

        // Generate View annotations from view-map elements
        List<Element> viewMaps = childElementList(root, "view-map");
        for (Element viewMap : viewMaps) {
            String viewCode = generateView(viewMap);
            if (isNotEmpty(viewCode)) {
                sb.append(NEWLINE);
                sb.append(viewCode);
            }
        }

        // Generate Request annotations from request-map elements
        for (Element requestMap : requestMaps) {
            String requestCode = generateRequest(requestMap);
            if (isNotEmpty(requestCode)) {
                sb.append(NEWLINE);
                sb.append(requestCode);
            }
        }

        sb.append(NEWLINE);
        sb.append(generateClassFooter());
        return sb.toString();
    }

    /**
     * Generates @View annotation for a view-map element.
     */
    protected String generateView(Element viewMap) {
        String name = getAttr(viewMap, "name");
        String type = getAttr(viewMap, "type");
        String page = getAttr(viewMap, "page");
        String info = getAttr(viewMap, "info");
        String contentType = getAttr(viewMap, "content-type");
        String encoding = getAttr(viewMap, "encoding");
        String noCache = getAttr(viewMap, "no-cache");
        String xFrameOptions = getAttr(viewMap, "x-frame-options");
        String strictTransportSecurity = getAttr(viewMap, "strict-transport-security");
        String access = getAttr(viewMap, "access");

        if (isEmpty(name)) return "";

        String fieldName = toFieldName(name);
        String viewFieldName = "VIEW_" + fieldName.toUpperCase();

        // SCIPIO: 4.0.0: case-only different view names (e.g. EntityImport vs entityImport) collide on the
        // Java constant name; suffix instead of silently dropping the second definition
        int viewSuffix = 2;
        String baseViewFieldName = viewFieldName;
        while (usedViewFieldNames.contains(viewFieldName)) {
            viewFieldName = baseViewFieldName + "_" + (viewSuffix++);
        }
        usedViewFieldNames.add(viewFieldName);

        StringBuilder sb = new StringBuilder();

        sb.append(INDENT).append("@com.ilscipio.scipio.ce.webapp.control.def.View(").append(NEWLINE);
        sb.append(INDENT).append(INDENT).append("name = ").append(toStringValue(name));

        // SCIPIO: 4.0.0: Always include type attribute since View annotation default is "default", not "screen"
        if (isNotEmpty(type)) {
            sb.append(",").append(NEWLINE);
            sb.append(INDENT).append(INDENT).append("type = ").append(toStringValue(type));
        }
        if (isNotEmpty(page)) {
            sb.append(",").append(NEWLINE);
            sb.append(INDENT).append(INDENT).append("page = ").append(toStringValue(page));
        }
        if (isNotEmpty(info)) {
            sb.append(",").append(NEWLINE);
            sb.append(INDENT).append(INDENT).append("info = ").append(toStringValue(info));
        }
        if (isNotEmpty(contentType)) {
            sb.append(",").append(NEWLINE);
            sb.append(INDENT).append(INDENT).append("contentType = ").append(toStringValue(contentType));
        }
        if (isNotEmpty(encoding)) {
            sb.append(",").append(NEWLINE);
            sb.append(INDENT).append(INDENT).append("encoding = ").append(toStringValue(encoding));
        }
        if ("true".equals(noCache)) {
            sb.append(",").append(NEWLINE);
            sb.append(INDENT).append(INDENT).append("noCache = \"true\"");
        }
        if (isNotEmpty(xFrameOptions) && !"sameorigin".equals(xFrameOptions)) {
            sb.append(",").append(NEWLINE);
            sb.append(INDENT).append(INDENT).append("xFrameOptions = ").append(toStringValue(xFrameOptions));
        }
        if (isNotEmpty(strictTransportSecurity)) {
            sb.append(",").append(NEWLINE);
            sb.append(INDENT).append(INDENT).append("strictTransportSecurity = ").append(toStringValue(strictTransportSecurity));
        }
        if (isNotEmpty(access)) {
            sb.append(",").append(NEWLINE);
            sb.append(INDENT).append(INDENT).append("access = ").append(toStringValue(access));
        }

        // Add controller reference
        sb.append(",").append(NEWLINE);
        sb.append(INDENT).append(INDENT).append("controller = ").append(toStringValue(controllerName));

        sb.append(NEWLINE);
        sb.append(INDENT).append(")").append(NEWLINE);
        sb.append(INDENT).append("public static final String ").append(viewFieldName).append(" = ").append(toStringValue(name)).append(";").append(NEWLINE);

        return sb.toString();
    }

    /**
     * Generates @Request + @Response annotations for a request-map element.
     */
    protected String generateRequest(Element requestMap) {
        String uri = getAttr(requestMap, "uri");
        String method = getAttr(requestMap, "method");

        if (isEmpty(uri)) return "";

        // Get security element
        Element security = firstChildElement(requestMap, "security");
        String https = security != null ? getAttr(security, "https") : "";
        String auth = security != null ? getAttr(security, "auth") : "";
        String cert = security != null ? getAttr(security, "cert") : "";
        String externalView = security != null ? getAttr(security, "external-view") : "";
        String directRequest = security != null ? getAttr(security, "direct-request") : "";

        // Get event element
        Element event = firstChildElement(requestMap, "event");
        String eventType = event != null ? getAttr(event, "type") : "";
        String eventPath = event != null ? getAttr(event, "path") : "";
        String eventInvoke = event != null ? getAttr(event, "invoke") : "";

        // Get response elements
        List<Element> responses = childElementList(requestMap, "response");

        String methodName = toMethodName(uri);

        // SCIPIO: 4.0.0: case-only different request uris (e.g. entityImport vs EntityImport) collide on the
        // Java member name; suffix instead of silently dropping the second request-map
        int methodSuffix = 2;
        String baseMethodName = methodName;
        while (usedMethodNames.contains(methodName)) {
            methodName = baseMethodName + "_" + (methodSuffix++);
        }
        usedMethodNames.add(methodName);

        StringBuilder sb = new StringBuilder();

        // @Request annotation
        sb.append(INDENT).append("@Request(").append(NEWLINE);
        sb.append(INDENT).append(INDENT).append("uri = ").append(toStringValue(uri));
        sb.append(",").append(NEWLINE);
        sb.append(INDENT).append(INDENT).append("controller = ").append(toStringValue(controllerName));

        if (isNotEmpty(method) && !"all".equals(method)) {
            sb.append(",").append(NEWLINE);
            sb.append(INDENT).append(INDENT).append("method = ").append(toStringValue(method));
        }
        if ("true".equals(https)) {
            sb.append(",").append(NEWLINE);
            sb.append(INDENT).append(INDENT).append("secure = \"true\"");
        }
        if ("true".equals(auth)) {
            sb.append(",").append(NEWLINE);
            sb.append(INDENT).append(INDENT).append("auth = \"true\"");
        }
        if ("true".equals(cert)) {
            sb.append(",").append(NEWLINE);
            sb.append(INDENT).append(INDENT).append("cert = \"true\"");
        }
        if ("false".equals(externalView)) {
            sb.append(",").append(NEWLINE);
            sb.append(INDENT).append(INDENT).append("externalView = \"false\"");
        }
        if ("false".equals(directRequest)) {
            sb.append(",").append(NEWLINE);
            sb.append(INDENT).append(INDENT).append("directRequest = \"false\"");
        }

        sb.append(NEWLINE);
        sb.append(INDENT).append(")").append(NEWLINE);

        // @Response annotations
        for (Element response : responses) {
            String respName = getAttr(response, "name");
            String respType = getAttr(response, "type");
            String respValue = getAttr(response, "value");
            String allowViewSave = getAttr(response, "allow-view-save");
            String saveLastView = getAttr(response, "save-last-view");
            String saveCurrentView = getAttr(response, "save-current-view");
            String saveHomeView = getAttr(response, "save-home-view");
            String statusCode = getAttr(response, "status-code");

            sb.append(INDENT).append("@Response(");
            List<String> respAttrs = new ArrayList<>();
            respAttrs.add("name = " + toStringValue(respName));
            respAttrs.add("type = " + toStringValue(respType));
            if (isNotEmpty(respValue)) {
                respAttrs.add("value = " + toStringValue(respValue));
            }
            if ("false".equals(allowViewSave)) {
                respAttrs.add("allowViewSave = \"false\"");
            }
            if ("true".equals(saveLastView)) {
                respAttrs.add("saveLastView = \"true\"");
            }
            if ("true".equals(saveCurrentView)) {
                respAttrs.add("saveCurrentView = \"true\"");
            }
            if ("true".equals(saveHomeView)) {
                respAttrs.add("saveHomeView = \"true\"");
            }
            if (isNotEmpty(statusCode)) {
                respAttrs.add("statusCode = " + toStringValue(statusCode));
            }
            sb.append(String.join(", ", respAttrs));
            sb.append(")").append(NEWLINE);
        }

        // Generate the method stub or interface
        // For java events with existing handlers, generate a delegation call
        // For service/simple events, generate an @Event annotated method
        if ("java".equals(eventType) && isNotEmpty(eventPath) && isNotEmpty(eventInvoke) && delegateResolvable(eventPath, eventInvoke)) { // SCIPIO: 4.0.0: dead targets fall back to the @Event form
            // Add to referenced classes for imports
            referencedClasses.add(eventPath);
            sb.append(INDENT).append("public static String ").append(methodName)
              .append("(HttpServletRequest request, HttpServletResponse response) throws Exception {").append(NEWLINE);
            sb.append(INDENT).append(INDENT).append("// Delegates to: ").append(eventPath).append(".").append(eventInvoke).append(NEWLINE);
            sb.append(INDENT).append(INDENT).append("return ").append(extractClassName(eventPath)).append(".").append(eventInvoke).append("(request, response);").append(NEWLINE);
            sb.append(INDENT).append("}").append(NEWLINE);
        } else if (isNotEmpty(eventType)) {
            // For non-java events, generate @Event annotated method that returns "success"
            // @Event annotation must be on a method, not interface
            sb.append(INDENT).append("@Event(");
            List<String> eventAttrs = new ArrayList<>();
            eventAttrs.add("type = " + toStringValue(eventType));
            if (isNotEmpty(eventPath)) {
                eventAttrs.add("path = " + toStringValue(eventPath));
            }
            if (isNotEmpty(eventInvoke)) {
                eventAttrs.add("invoke = " + toStringValue(eventInvoke));
            }
            sb.append(String.join(", ", eventAttrs));
            sb.append(")").append(NEWLINE);
            sb.append(INDENT).append("public static String ").append(methodName)
              .append("(HttpServletRequest request, HttpServletResponse response) throws Exception {").append(NEWLINE);
            sb.append(INDENT).append(INDENT).append("return \"success\"; // Event handled by @Event annotation").append(NEWLINE);
            sb.append(INDENT).append("}").append(NEWLINE);
        } else {
            // No event - simple request with responses only - use interface marker
            sb.append(INDENT).append("public interface ").append(getUniqueInterfaceName(uri)).append(" {}").append(NEWLINE);
        }

        return sb.toString();
    }

    /**
     * Extracts the simple class name from a fully qualified class name.
     */
    protected String extractClassName(String fqClassName) {
        if (fqClassName == null) return "";
        int lastDot = fqClassName.lastIndexOf('.');
        return lastDot >= 0 ? fqClassName.substring(lastDot + 1) : fqClassName;
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

    // ========================================================================
    // Code Generation Utilities
    // ========================================================================

    /**
     * SCIPIO: 4.0.0: A java event delegate is only generated when the target class and method exist on the conversion
     * classpath; otherwise the request keeps the reflective @Event form (the XML behaviour), instead of producing
     * uncompilable code for dead references (e.g. removed event classes).
     */
    /** SCIPIO: 4.0.0: Looks for the class source under the applications and framework component src dirs and checks the method name. */
    protected boolean sourceHasMethod(String className, String method) {
        String rel = className.replace('.', '/') + ".java";
        java.io.File root = new java.io.File(System.getProperty("user.dir"));
        if (!new java.io.File(root, "applications").isDirectory()) {
            // SCIPIO: 4.0.0: the Gradle daemon cwd is not always the workspace root; walk up from this class location
            try {
                java.io.File loc = new java.io.File(getClass().getProtectionDomain().getCodeSource().getLocation().toURI());
                for (java.io.File d = loc; d != null; d = d.getParentFile()) {
                    if (new java.io.File(d, "applications").isDirectory() && new java.io.File(d, "framework").isDirectory()) { root = d; break; }
                }
            } catch (Exception e) {
                // keep user.dir
            }
        }
        for (String base : new String[] { "applications", "framework", "specialpurpose", "hot-deploy" }) {
            java.io.File[] comps = new java.io.File(root, base).listFiles(java.io.File::isDirectory);
            if (comps == null) continue;
            for (java.io.File comp : comps) {
                java.io.File src = new java.io.File(new java.io.File(comp, "src"), rel);
                if (src.isFile()) {
                    try {
                        String body = new String(java.nio.file.Files.readAllBytes(src.toPath()), java.nio.charset.StandardCharsets.UTF_8);
                        return java.util.regex.Pattern.compile("\\bString\\s+" + java.util.regex.Pattern.quote(method) + "\\s*\\(").matcher(body).find();
                    } catch (java.io.IOException e) {
                        return false;
                    }
                }
            }
        }
        return false;
    }

    protected boolean delegateResolvable(String className, String method) {
        try {
            Class<?> cls = Class.forName(className, false, getClass().getClassLoader());
            cls.getMethod(method, javax.servlet.http.HttpServletRequest.class, javax.servlet.http.HttpServletResponse.class);
            return true;
        } catch (Throwable t) {
            if (sourceHasMethod(className, method)) {
                return true; // not on the conversion classpath, but present in the workspace sources
            }
            System.err.println("WARN: event target " + className + "." + method + " not resolvable at conversion time - emitting @Event form");
            return false;
        }
    }

    protected String generateClassHeader(Set<String> referencedClasses) {
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

        // Imports - use explicit imports to avoid naming conflicts with generated class names
        sb.append("import com.ilscipio.scipio.ce.webapp.control.def.View;").append(NEWLINE);
        sb.append("import com.ilscipio.scipio.ce.webapp.control.def.Request;").append(NEWLINE);
        sb.append("import com.ilscipio.scipio.ce.webapp.control.def.Response;").append(NEWLINE);
        sb.append("import com.ilscipio.scipio.ce.webapp.control.def.Responses;").append(NEWLINE);
        sb.append("import com.ilscipio.scipio.ce.webapp.control.def.Event;").append(NEWLINE);
        sb.append("import javax.servlet.http.HttpServletRequest;").append(NEWLINE);
        sb.append("import javax.servlet.http.HttpServletResponse;").append(NEWLINE);
        // Add referenced class imports
        for (String refClass : referencedClasses) {
            if (isNotEmpty(refClass) && refClass.contains(".")) {
                sb.append("import ").append(refClass).append(";").append(NEWLINE);
            }
        }
        sb.append(NEWLINE);

        // Class javadoc
        sb.append("/**").append(NEWLINE);
        sb.append(" * Auto-generated annotation-based controller definitions.").append(NEWLINE);
        sb.append(" *").append(NEWLINE);
        sb.append(" * <p>Generated from controller.xml by convertXmlToAnnotation Gradle task.</p>").append(NEWLINE);
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

    protected String toFieldName(String name) {
        if (isEmpty(name)) return "UNKNOWN";
        return name.replaceAll("[^a-zA-Z0-9]", "_");
    }

    protected String toMethodName(String uri) {
        if (isEmpty(uri)) return "unknown";
        StringBuilder sb = new StringBuilder();
        boolean capitalizeNext = false;
        boolean first = true;
        for (char c : uri.toCharArray()) {
            if (c == '-' || c == '_' || c == '.') {
                capitalizeNext = true;
            } else if (Character.isLetterOrDigit(c)) {
                if (first) {
                    sb.append(Character.toLowerCase(c));
                    first = false;
                } else if (capitalizeNext) {
                    sb.append(Character.toUpperCase(c));
                    capitalizeNext = false;
                } else {
                    sb.append(c);
                }
            }
        }
        return sb.toString();
    }

    protected String toInterfaceName(String name) {
        String n = toInterfaceNameRaw(name);
        // SCIPIO: 4.0.0: interface names that shadow the annotation types (uri "request", "view", ...) get a Def suffix
        return RESERVED_TYPE_NAMES.contains(n) ? n + "Def" : n;
    }

    private static final java.util.Set<String> RESERVED_TYPE_NAMES = new java.util.HashSet<>(java.util.Arrays.asList(
            "Request", "Response", "Responses", "View", "Event", "RequestMap", "ViewMap", "Views", "Requests"));

    protected String toInterfaceNameRaw(String name) {
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

    /**
     * Gets a unique interface name, appending a number if the name is already used.
     */
    protected String getUniqueInterfaceName(String uri) {
        String baseName = toInterfaceName(uri);

        // If this is the first time we've seen this name, use it as-is
        if (!usedInterfaceNames.containsKey(baseName)) {
            usedInterfaceNames.put(baseName, 1);
            return baseName;
        }

        // Otherwise, increment the counter and append the number
        int count = usedInterfaceNames.get(baseName);
        usedInterfaceNames.put(baseName, count + 1);
        return baseName + count;
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

    protected static boolean isEmpty(String s) {
        return s == null || s.isEmpty();
    }

    protected static boolean isNotEmpty(String s) {
        return s != null && !s.isEmpty();
    }
}
