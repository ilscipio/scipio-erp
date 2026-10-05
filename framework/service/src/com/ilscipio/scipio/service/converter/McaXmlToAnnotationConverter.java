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
import java.util.ArrayList;
import java.util.List;

/**
 * Converts a service-mca.xml document to a class of {@literal @}Mca annotations.
 *
 * <p>SCIPIO: 4.0.0: Added; service-mca.xml was the last service definition type the converter
 * could not handle, so those rules had to stay in XML.</p>
 */
public class McaXmlToAnnotationConverter {

    protected static final String NEWLINE = System.lineSeparator();
    protected static final String INDENT = "    ";

    protected final String packageName;
    protected final String className;
    protected final String componentName;
    protected final File outputDir;

    public McaXmlToAnnotationConverter(String packageName, String className, String componentName, File outputDir) {
        this.packageName = packageName;
        this.className = className;
        this.componentName = componentName;
        this.outputDir = outputDir;
    }

    public String convert(Document doc) {
        List<Element> mcas = childElementList(doc.getDocumentElement(), "mca");
        if (mcas.isEmpty()) {
            // A file whose rules are all commented out must not overwrite an existing class.
            return "";
        }
        StringBuilder sb = new StringBuilder();
        sb.append(generateClassHeader());
        for (Element mca : mcas) {
            String code = generateMca(mca);
            if (!code.isEmpty()) {
                sb.append(NEWLINE).append(code);
            }
        }
        sb.append(NEWLINE).append("}").append(NEWLINE);
        return sb.toString();
    }

    protected String generateMca(Element mca) {
        String ruleName = getAttr(mca, "mail-rule-name");
        if (ruleName.isEmpty()) {
            return "";
        }
        List<String> conditions = new ArrayList<>();
        for (Element cond : childElementList(mca, "condition-field")) {
            conditions.add("@McaCondition(fieldName = " + quote(getAttr(cond, "field-name"))
                    + attrIfSet("operator", getAttr(cond, "operator"))
                    + attrIfSet("value", getAttr(cond, "value")) + ")");
        }
        for (Element cond : childElementList(mca, "condition-header")) {
            conditions.add("@McaCondition(headerName = " + quote(getAttr(cond, "header-name"))
                    + attrIfSet("operator", getAttr(cond, "operator"))
                    + attrIfSet("value", getAttr(cond, "value")) + ")");
        }
        for (Element cond : childElementList(mca, "condition-service")) {
            conditions.add("@McaCondition(serviceName = " + quote(getAttr(cond, "service-name")) + ")");
        }

        List<String> actions = new ArrayList<>();
        for (Element action : childElementList(mca, "action")) {
            StringBuilder a = new StringBuilder("@McaAction(service = " + quote(getAttr(action, "service")));
            String mode = getAttr(action, "mode");
            if (!mode.isEmpty() && !"sync".equals(mode)) {
                a.append(", mode = ").append(quote(mode));
            }
            String runAsUser = getAttr(action, "run-as-user");
            if (runAsUser.isEmpty()) {
                runAsUser = getAttr(action, "runAsUser");
            }
            if (!runAsUser.isEmpty()) {
                a.append(", runAsUser = ").append(quote(runAsUser));
            }
            if ("true".equals(getAttr(action, "persist"))) {
                a.append(", persist = true");
            }
            actions.add(a.append(")").toString());
        }

        StringBuilder sb = new StringBuilder();
        sb.append(INDENT).append("/**").append(NEWLINE);
        sb.append(INDENT).append(" * Mail condition action rule ").append(ruleName).append(".").append(NEWLINE);
        sb.append(INDENT).append(" */").append(NEWLINE);
        sb.append(INDENT).append("@Mca(").append(NEWLINE);
        sb.append(INDENT).append(INDENT).append("name = ").append(quote(ruleName));
        if (!conditions.isEmpty()) {
            sb.append(",").append(NEWLINE).append(INDENT).append(INDENT)
                    .append("conditions = {").append(String.join(", ", conditions)).append("}");
        }
        if (!actions.isEmpty()) {
            sb.append(",").append(NEWLINE).append(INDENT).append(INDENT)
                    .append("actions = {").append(String.join(", ", actions)).append("}");
        }
        sb.append(NEWLINE).append(INDENT).append(")").append(NEWLINE);
        sb.append(INDENT).append("public interface ").append(toInterfaceName(ruleName)).append(" {}").append(NEWLINE);
        return sb.toString();
    }

    protected String generateClassHeader() {
        StringBuilder sb = new StringBuilder();
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
        sb.append("package ").append(packageName).append(";").append(NEWLINE).append(NEWLINE);
        sb.append("import com.ilscipio.scipio.service.def.mca.*;").append(NEWLINE).append(NEWLINE);
        sb.append("/**").append(NEWLINE);
        sb.append(" * Mail condition action rules for the ").append(componentName).append(" component.").append(NEWLINE);
        sb.append(" *").append(NEWLINE);
        sb.append(" * <p>Generated from XML by the convertXmlToAnnotation Gradle task.</p>").append(NEWLINE);
        sb.append(" *").append(NEWLINE);
        sb.append(" * <p>SCIPIO: 4.0.0: Auto-generated.</p>").append(NEWLINE);
        sb.append(" */").append(NEWLINE);
        sb.append("public class ").append(className).append(" {").append(NEWLINE);
        return sb.toString();
    }

    protected String toInterfaceName(String ruleName) {
        StringBuilder sb = new StringBuilder();
        boolean upper = true;
        for (char c : ruleName.toCharArray()) {
            if (Character.isLetterOrDigit(c)) {
                sb.append(upper ? Character.toUpperCase(c) : c);
                upper = false;
            } else {
                upper = true;
            }
        }
        if (sb.length() == 0 || Character.isDigit(sb.charAt(0))) {
            sb.insert(0, "Rule");
        }
        return sb.toString();
    }

    protected String attrIfSet(String name, String value) {
        return value.isEmpty() ? "" : ", " + name + " = " + quote(value);
    }

    protected String quote(String value) {
        return "\"" + value.replace("\\", "\\\\").replace("\"", "\\\"") + "\"";
    }

    protected String getAttr(Element element, String name) {
        String value = element.getAttribute(name);
        return value != null ? value.trim() : "";
    }

    protected List<Element> childElementList(Element parent, String tagName) {
        List<Element> result = new ArrayList<>();
        NodeList children = parent.getChildNodes();
        for (int i = 0; i < children.getLength(); i++) {
            Node child = children.item(i);
            if (child.getNodeType() == Node.ELEMENT_NODE && tagName.equals(child.getNodeName())) {
                result.add((Element) child);
            }
        }
        return result;
    }
}
