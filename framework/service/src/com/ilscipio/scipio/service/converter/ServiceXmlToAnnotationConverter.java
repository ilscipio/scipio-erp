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
 * Converts services.xml to Java annotation-based service definitions.
 *
 * <p>Generates @Service annotated classes from service XML definitions.</p>
 *
 * <p>SCIPIO: 4.0.0: Added for XML-to-Annotation conversion support.</p>
 */
public class ServiceXmlToAnnotationConverter {

    protected static final String INDENT = "    ";
    protected static final String NEWLINE = System.lineSeparator();

    protected final String packageName;
    protected final String className;
    protected final String componentName;
    protected final File outputDir;

    public ServiceXmlToAnnotationConverter(String packageName, String className, String componentName, File outputDir) {
        this.packageName = packageName;
        this.className = className;
        this.componentName = componentName;
        this.outputDir = outputDir;
    }

    /**
     * Converts the services XML document to Java annotation source code.
     */
    public String convert(Document doc) {
        StringBuilder sb = new StringBuilder();

        Element root = doc.getDocumentElement();

        // Generate Service annotations from service elements
        List<Element> services = childElementList(root, "service");

        // SCIPIO: 4.0.0: Detect whether a GroupInvoke import is needed (any engine="group" service)
        boolean hasGroupService = false;
        for (Element service : services) {
            if ("group".equals(getAttr(service, "engine"))) {
                hasGroupService = true;
                break;
            }
        }
        sb.append(generateClassHeader(hasGroupService));

        for (Element service : services) {
            String serviceCode = generateService(service);
            if (isNotEmpty(serviceCode)) {
                sb.append(NEWLINE);
                sb.append(serviceCode);
            }
        }

        sb.append(NEWLINE);
        sb.append(generateClassFooter());
        return sb.toString();
    }

    /**
     * SCIPIO: 4.0.0: Converts a service-group file (groups.xml, groups_*.xml, service_groups.xml; root element
     * service-group) into @Service(engine = "group") definitions carrying @GroupInvoke entries.
     * NOTE: send-mode is not modelled ("all" is the only value in use and the GroupModel default).
     */
    public String convertServiceGroup(Document doc) {
        StringBuilder sb = new StringBuilder();
        Element root = doc.getDocumentElement();
        sb.append(generateClassHeader(true));
        for (Element group : childElementList(root, "group")) {
            String name = getAttr(group, "name");
            if (isEmpty(name)) continue;
            sb.append(NEWLINE);
            sb.append(INDENT).append("@Service(").append(NEWLINE);
            sb.append(INDENT).append(INDENT).append("name = ").append(toStringValue(name)).append(",").append(NEWLINE);
            sb.append(INDENT).append(INDENT).append("engine = \"group\"");
            List<Element> invokes = childElementList(group, "invoke");
            if (!invokes.isEmpty()) {
                sb.append(",").append(NEWLINE).append(INDENT).append(INDENT).append("invokes = {");
                boolean first = true;
                for (Element inv : invokes) {
                    if (!first) sb.append(", ");
                    String mode = getAttr(inv, "mode");
                    String rtc = getAttr(inv, "result-to-context");
                    sb.append("@GroupInvoke(name = ").append(toStringValue(getAttr(inv, "name")));
                    if (isNotEmpty(mode) && !"sync".equals(mode)) sb.append(", mode = ").append(toStringValue(mode));
                    sb.append(", resultToContext = ").append(toStringValue(isEmpty(rtc) ? "false" : rtc)).append(")");
                    first = false;
                }
                sb.append("}");
            }
            sb.append(NEWLINE).append(INDENT).append(")").append(NEWLINE);
            sb.append(INDENT).append("public interface ").append(toInterfaceName(name)).append(" {}").append(NEWLINE);
        }
        sb.append(NEWLINE).append(generateClassFooter());
        return sb.toString();
    }

    /**
     * Generates @Service annotation for a service element.
     */
    protected String generateService(Element service) {
        String name = getAttr(service, "name");
        String engine = getAttr(service, "engine");
        String location = getAttr(service, "location");
        String invoke = getAttr(service, "invoke");
        String defaultEntityName = getAttr(service, "default-entity-name");
        String auth = getAttr(service, "auth");
        String export = getAttr(service, "export");
        String validate = getAttr(service, "validate");
        String useTransaction = getAttr(service, "use-transaction");
        String requireNewTransaction = getAttr(service, "require-new-transaction");
        String transactionTimeout = getAttr(service, "transaction-timeout");
        String maxRetry = getAttr(service, "max-retry");
        String debug = getAttr(service, "debug");
        String semaphore = getAttr(service, "semaphore");
        String semaphoreWaitSeconds = getAttr(service, "semaphore-wait-seconds");
        String semaphoreSleep = getAttr(service, "semaphore-sleep");
        String log = getAttr(service, "log");
        String logEca = getAttr(service, "log-eca");
        String hideResultInLog = getAttr(service, "hide-result-in-log");
        String priority = getAttr(service, "priority");

        if (isEmpty(name)) return "";

        // Get description
        String description = childElementValue(service, "description");

        // Get auto-attributes elements
        List<Element> autoAttributes = childElementList(service, "auto-attributes");

        // Get attribute elements
        List<Element> attributes = childElementList(service, "attribute");

        // Get implements elements
        List<Element> implementsElems = childElementList(service, "implements");

        // Get permission-service element
        Element permissionService = firstChildElement(service, "permission-service");

        // Get required-permissions element
        Element requiredPermissions = firstChildElement(service, "required-permissions");

        // Handle group services
        Element groupElement = firstChildElement(service, "group");

        StringBuilder sb = new StringBuilder();
        String interfaceName = toInterfaceName(name);

        // Generate description comment if present
        if (isNotEmpty(description)) {
            sb.append(INDENT).append("/**").append(NEWLINE);
            sb.append(INDENT).append(" * ").append(escapeJavadoc(description)).append(NEWLINE);
            sb.append(INDENT).append(" */").append(NEWLINE);
        }

        // @Service annotation
        sb.append(INDENT).append("@Service(").append(NEWLINE);
        sb.append(INDENT).append(INDENT).append("name = ").append(toStringValue(name));

        // SCIPIO: 4.0.0: Emit engine/location/invoke so the annotation carries the actual
        // service implementation binding (previously computed but never written out).
        if (isNotEmpty(engine)) {
            sb.append(",").append(NEWLINE);
            sb.append(INDENT).append(INDENT).append("engine = ").append(toStringValue(engine));
        }
        if (isNotEmpty(location)) {
            sb.append(",").append(NEWLINE);
            sb.append(INDENT).append(INDENT).append("location = ").append(toStringValue(location));
        }
        if (isNotEmpty(invoke)) {
            sb.append(",").append(NEWLINE);
            sb.append(INDENT).append(INDENT).append("invoke = ").append(toStringValue(invoke));
        }

        if (isNotEmpty(description)) {
            sb.append(",").append(NEWLINE);
            sb.append(INDENT).append(INDENT).append("description = ").append(toStringValue(description));
        }
        if (isNotEmpty(defaultEntityName)) {
            sb.append(",").append(NEWLINE);
            sb.append(INDENT).append(INDENT).append("defaultEntityName = ").append(toStringValue(defaultEntityName));
        }
        if ("true".equals(auth)) {
            sb.append(",").append(NEWLINE);
            sb.append(INDENT).append(INDENT).append("auth = \"true\"");
        }
        if ("true".equals(export)) {
            sb.append(",").append(NEWLINE);
            sb.append(INDENT).append(INDENT).append("export = \"true\"");
        }
        if ("false".equals(validate)) {
            sb.append(",").append(NEWLINE);
            sb.append(INDENT).append(INDENT).append("validate = \"false\"");
        }
        if ("false".equals(useTransaction)) {
            sb.append(",").append(NEWLINE);
            sb.append(INDENT).append(INDENT).append("useTransaction = \"false\"");
        }
        if ("true".equals(requireNewTransaction)) {
            sb.append(",").append(NEWLINE);
            sb.append(INDENT).append(INDENT).append("requireNewTransaction = \"true\"");
        }
        if (isNotEmpty(transactionTimeout) && !"0".equals(transactionTimeout)) {
            sb.append(",").append(NEWLINE);
            sb.append(INDENT).append(INDENT).append("transactionTimeout = ").append(toStringValue(transactionTimeout));
        }
        if (isNotEmpty(maxRetry) && !"-1".equals(maxRetry)) {
            sb.append(",").append(NEWLINE);
            sb.append(INDENT).append(INDENT).append("maxRetry = ").append(toStringValue(maxRetry));
        }
        if ("true".equals(debug)) {
            sb.append(",").append(NEWLINE);
            sb.append(INDENT).append(INDENT).append("debug = \"true\"");
        }
        if (isNotEmpty(semaphore) && !"none".equals(semaphore)) {
            sb.append(",").append(NEWLINE);
            sb.append(INDENT).append(INDENT).append("semaphore = ").append(toStringValue(semaphore));
        }
        if (isNotEmpty(semaphoreWaitSeconds) && !"300".equals(semaphoreWaitSeconds)) {
            sb.append(",").append(NEWLINE);
            sb.append(INDENT).append(INDENT).append("semaphoreWaitSeconds = ").append(toStringValue(semaphoreWaitSeconds));
        }
        if (isNotEmpty(semaphoreSleep) && !"500".equals(semaphoreSleep)) {
            sb.append(",").append(NEWLINE);
            sb.append(INDENT).append(INDENT).append("semaphoreSleep = ").append(toStringValue(semaphoreSleep));
        }
        if (isNotEmpty(log)) {
            sb.append(",").append(NEWLINE);
            sb.append(INDENT).append(INDENT).append("log = ").append(toStringValue(log));
        }
        if (isNotEmpty(logEca)) {
            sb.append(",").append(NEWLINE);
            sb.append(INDENT).append(INDENT).append("logEca = ").append(toStringValue(logEca));
        }
        if ("true".equals(hideResultInLog)) {
            sb.append(",").append(NEWLINE);
            sb.append(INDENT).append(INDENT).append("hideResultInLog = \"true\"");
        }
        if (isNotEmpty(priority)) {
            sb.append(",").append(NEWLINE);
            sb.append(INDENT).append(INDENT).append("priority = ").append(toStringValue(priority));
        }

        // Generate implements
        if (!implementsElems.isEmpty()) {
            sb.append(",").append(NEWLINE);
            sb.append(INDENT).append(INDENT).append("implemented = {");
            boolean first = true;
            for (Element impl : implementsElems) {
                String serviceName = getAttr(impl, "service");
                String optional = getAttr(impl, "optional");
                if (!first) sb.append(", ");
                sb.append("@Implements(service = ").append(toStringValue(serviceName));
                if ("true".equals(optional)) {
                    sb.append(", optional = \"true\"");
                }
                sb.append(")");
                first = false;
            }
            sb.append("}");
        }

        // Generate entityAttributes (auto-attributes)
        if (!autoAttributes.isEmpty()) {
            sb.append(",").append(NEWLINE);
            sb.append(INDENT).append(INDENT).append("entityAttributes = {").append(NEWLINE);
            boolean first = true;
            for (Element autoAttr : autoAttributes) {
                if (!first) sb.append(",").append(NEWLINE);
                sb.append(generateEntityAttributes(autoAttr, 3));
                first = false;
            }
            sb.append(NEWLINE).append(INDENT).append(INDENT).append("}");
        }

        // Generate attributes
        if (!attributes.isEmpty()) {
            sb.append(",").append(NEWLINE);
            sb.append(INDENT).append(INDENT).append("attributes = {").append(NEWLINE);
            boolean first = true;
            for (Element attr : attributes) {
                if (!first) sb.append(",").append(NEWLINE);
                sb.append(generateAttribute(attr, 3));
                first = false;
            }
            sb.append(NEWLINE).append(INDENT).append(INDENT).append("}");
        }

        // Generate permission-service
        if (permissionService != null) {
            String permSvcName = getAttr(permissionService, "service-name");
            String mainAction = getAttr(permissionService, "main-action");
            String resourceDesc = getAttr(permissionService, "resource-description");
            sb.append(",").append(NEWLINE);
            sb.append(INDENT).append(INDENT).append("permissionService = @PermissionService(");
            sb.append("service = ").append(toStringValue(permSvcName));
            if (isNotEmpty(mainAction)) {
                sb.append(", mainAction = ").append(toStringValue(mainAction));
            }
            if (isNotEmpty(resourceDesc)) {
                sb.append(", resourceDescription = ").append(toStringValue(resourceDesc));
            }
            sb.append(")");
        }

        // SCIPIO: 4.0.0: Generate group invocations (for engine="group"), replacing the
        // previous comment-only invoke listing with the actual @GroupInvoke annotations
        // consumed by ModelServiceReader.createModelService().
        if ("group".equals(engine) && groupElement != null) {
            List<Element> groupInvokes = childElementList(groupElement, "invoke");
            if (!groupInvokes.isEmpty()) {
                sb.append(",").append(NEWLINE);
                sb.append(INDENT).append(INDENT).append("invokes = {");
                boolean first = true;
                for (Element inv : groupInvokes) {
                    String invokeName = getAttr(inv, "name");
                    String invokeMode = getAttr(inv, "mode");
                    // XSD default for result-to-context is "false", but GroupInvoke.resultToContext()
                    // annotation default is "true" -- always emit explicitly to avoid a silent
                    // behavior change when the XML attribute is absent.
                    String resultToContext = getAttr(inv, "result-to-context");
                    if (isEmpty(resultToContext)) {
                        resultToContext = "false";
                    }
                    if (!first) sb.append(", ");
                    sb.append("@GroupInvoke(name = ").append(toStringValue(invokeName));
                    if (isNotEmpty(invokeMode) && !"sync".equals(invokeMode)) {
                        sb.append(", mode = ").append(toStringValue(invokeMode));
                    }
                    sb.append(", resultToContext = ").append(toStringValue(resultToContext));
                    sb.append(")");
                    first = false;
                }
                sb.append("}");
            }
        }

        sb.append(NEWLINE);
        sb.append(INDENT).append(")").append(NEWLINE);
        sb.append(INDENT).append("public interface ").append(interfaceName).append(" {}").append(NEWLINE);

        return sb.toString();
    }

    /**
     * Generates @EntityAttributes annotation.
     */
    protected String generateEntityAttributes(Element autoAttr, int indentLevel) {
        String entityName = getAttr(autoAttr, "entity-name");
        String mode = getAttr(autoAttr, "mode");
        String include = getAttr(autoAttr, "include");
        String optional = getAttr(autoAttr, "optional");
        String prefix = getAttr(autoAttr, "prefix");
        String formDisplay = getAttr(autoAttr, "form-display");
        String allowHtml = getAttr(autoAttr, "allow-html");

        StringBuilder sb = new StringBuilder();
        sb.append(indent(indentLevel)).append("@EntityAttributes(");

        List<String> attrs = new ArrayList<>();
        if (isNotEmpty(entityName)) {
            attrs.add("entityName = " + toStringValue(entityName));
        }
        attrs.add("mode = " + toStringValue(mode));
        if (isNotEmpty(include) && !"all".equals(include)) {
            attrs.add("include = " + toStringValue(include));
        }
        if ("true".equals(optional)) {
            attrs.add("optional = \"true\"");
        }
        if (isNotEmpty(prefix)) {
            attrs.add("prefix = " + toStringValue(prefix));
        }
        if ("false".equals(formDisplay)) {
            attrs.add("formDisplay = \"false\"");
        }
        if (isNotEmpty(allowHtml) && !"none".equals(allowHtml)) {
            attrs.add("allowHtml = " + toStringValue(allowHtml));
        }

        // Check for exclude elements
        List<Element> excludes = childElementList(autoAttr, "exclude");
        if (!excludes.isEmpty()) {
            StringBuilder excl = new StringBuilder();
            excl.append("excludeFields = {");
            boolean first = true;
            for (Element exc : excludes) {
                String fieldName = getAttr(exc, "field-name");
                if (!first) excl.append(", ");
                excl.append(toStringValue(fieldName));
                first = false;
            }
            excl.append("}");
            attrs.add(excl.toString());
        }

        sb.append(String.join(", ", attrs));
        sb.append(")");
        return sb.toString();
    }

    /**
     * Generates @Attribute annotation.
     */
    protected String generateAttribute(Element attr, int indentLevel) {
        String name = getAttr(attr, "name");
        String type = getAttr(attr, "type");
        String mode = getAttr(attr, "mode");
        String optional = getAttr(attr, "optional");
        String defaultValue = getAttr(attr, "default-value");
        String entityName = getAttr(attr, "entity-name");
        String fieldName = getAttr(attr, "field-name");
        String formLabel = getAttr(attr, "form-label");
        String formDisplay = getAttr(attr, "form-display");
        String allowHtml = getAttr(attr, "allow-html");
        String access = getAttr(attr, "access");

        String description = childElementValue(attr, "description");

        StringBuilder sb = new StringBuilder();
        sb.append(indent(indentLevel)).append("@Attribute(");

        List<String> attrs = new ArrayList<>();
        attrs.add("name = " + toStringValue(name));
        if (isNotEmpty(type)) {
            attrs.add("type = " + toStringValue(type));
        }
        attrs.add("mode = " + toStringValue(mode));
        if ("true".equals(optional)) {
            attrs.add("optional = \"true\"");
        }
        if (isNotEmpty(defaultValue)) {
            attrs.add("defaultValue = " + toStringValue(defaultValue));
        }
        if (isNotEmpty(description)) {
            attrs.add("description = " + toStringValue(description));
        }
        if (isNotEmpty(entityName)) {
            attrs.add("entityName = " + toStringValue(entityName));
        }
        if (isNotEmpty(fieldName)) {
            attrs.add("fieldName = " + toStringValue(fieldName));
        }
        if (isNotEmpty(formLabel)) {
            attrs.add("formLabel = " + toStringValue(formLabel));
        }
        if ("false".equals(formDisplay)) {
            attrs.add("formDisplay = \"false\"");
        }
        if (isNotEmpty(allowHtml) && !"none".equals(allowHtml)) {
            attrs.add("allowHtml = " + toStringValue(allowHtml));
        }
        if (isNotEmpty(access)) {
            attrs.add("access = " + toStringValue(access));
        }

        sb.append(String.join(", ", attrs));
        sb.append(")");
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

    // ========================================================================
    // Code Generation Utilities
    // ========================================================================

    protected String generateClassHeader(boolean includeGroupInvoke) {
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
        sb.append("import com.ilscipio.scipio.service.def.*;").append(NEWLINE);
        if (includeGroupInvoke) {
            sb.append("import com.ilscipio.scipio.service.def.Service.GroupInvoke;").append(NEWLINE);
        }
        sb.append(NEWLINE);

        // Class javadoc
        sb.append("/**").append(NEWLINE);
        sb.append(" * Auto-generated annotation-based service definitions.").append(NEWLINE);
        sb.append(" *").append(NEWLINE);
        sb.append(" * <p>Generated from services.xml by convertXmlToAnnotation Gradle task.</p>").append(NEWLINE);
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

    protected String escapeJavadoc(String s) {
        if (s == null) return "";
        return s.replace("*/", "* /")
                .replace("\n", " ")
                .replace("\r", "");
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

    protected String childElementValue(Element parent, String tagName) {
        Element child = firstChildElement(parent, tagName);
        if (child == null) return null;
        return child.getTextContent();
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
