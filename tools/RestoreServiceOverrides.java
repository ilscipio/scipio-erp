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
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import javax.xml.parsers.DocumentBuilderFactory;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;

/**
 * SCIPIO: 4.0.0: One-off repair tool. Restores {@code <override>} elements of the original services XML as
 * {@code overrideAttributes = { @OverrideAttribute(...) }} on the matching {@code @Service(name = "...")} annotation.
 *
 * Usage: java tools/RestoreServiceOverrides.java <dir-with-original-xml> <repo-root> [--dry-run]
 * The XML files are named {@code <component>__services*.xml}. Services that already declare overrideAttributes are left alone.
 */
public class RestoreServiceOverrides {

    static final String[][] ATTR_MAP = {
        {"name", "name"}, {"type", "type"}, {"entity-name", "entityName"}, {"field-name", "fieldName"}, {"mode", "mode"},
        {"optional", "optional"}, {"default-value", "defaultValue"}, {"form-label", "formLabel"}, {"form-display", "formDisplay"},
        {"allow-html", "allowHtml"}, {"type-convert", "typeConvert"}, {"access", "access"}, {"event-access", "eventAccess"}
    };

    public static void main(String[] args) throws Exception {
        Path xmlDir = Paths.get(args[0]);
        Path repo = Paths.get(args[1]);
        boolean dryRun = args.length > 2 && "--dry-run".equals(args[2]);
        // component -> service name -> override annotation lines
        Map<String, Map<String, List<String>>> byComponent = new LinkedHashMap<>();
        try (Stream<Path> files = Files.list(xmlDir)) {
            for (Path xml : files.filter(p -> p.getFileName().toString().endsWith(".xml")).collect(Collectors.toList())) {
                String fileName = xml.getFileName().toString();
                String component = fileName.substring(0, fileName.indexOf("__"));
                Document doc = DocumentBuilderFactory.newInstance().newDocumentBuilder().parse(xml.toFile());
                NodeList services = doc.getElementsByTagName("service");
                for (int i = 0; i < services.getLength(); i++) {
                    Element svc = (Element) services.item(i);
                    List<String> overrides = new ArrayList<>();
                    NodeList children = svc.getChildNodes();
                    for (int j = 0; j < children.getLength(); j++) {
                        if (!(children.item(j) instanceof Element)) continue;
                        Element c = (Element) children.item(j);
                        if (!"override".equals(c.getTagName())) continue;
                        StringBuilder sb = new StringBuilder("@OverrideAttribute(");
                        boolean first = true;
                        for (String[] m : ATTR_MAP) {
                            String v = c.getAttribute(m[0]);
                            if (v == null || v.isEmpty()) continue;
                            if (!first) sb.append(", ");
                            sb.append(m[1]).append(" = \"").append(v.replace("\\", "\\\\").replace("\"", "\\\"")).append("\"");
                            first = false;
                        }
                        sb.append(")");
                        overrides.add(sb.toString());
                    }
                    if (!overrides.isEmpty()) {
                        byComponent.computeIfAbsent(component, k -> new LinkedHashMap<>()).put(svc.getAttribute("name"), overrides);
                    }
                }
            }
        }
        int restored = 0, skippedExisting = 0, notFound = 0, filesChanged = 0;
        for (Map.Entry<String, Map<String, List<String>>> ce : byComponent.entrySet()) {
            String component = ce.getKey();
            List<Path> javaFiles = new ArrayList<>();
            for (String root : new String[] {"applications", "framework"}) {
                Path dir = repo.resolve(root).resolve(component).resolve("src");
                if (!Files.isDirectory(dir)) continue;
                try (Stream<Path> s = Files.walk(dir)) {
                    javaFiles.addAll(s.filter(p -> p.toString().endsWith(".java") && p.toString().replace('\\', '/').contains("/service/")).collect(Collectors.toList()));
                }
            }
            for (Map.Entry<String, List<String>> se : ce.getValue().entrySet()) {
                String serviceName = se.getKey();
                boolean done = false;
                for (Path javaFile : javaFiles) {
                    List<String> lines = Files.readAllLines(javaFile, StandardCharsets.UTF_8);
                    int nameIdx = -1;
                    Pattern namePat = Pattern.compile("^\\s*name\\s*=\\s*\"" + Pattern.quote(serviceName) + "\"\\s*,?\\s*$");
                    for (int i = 0; i < lines.size(); i++) {
                        if (namePat.matcher(lines.get(i)).matches() && i > 0 && lines.get(i - 1).trim().startsWith("@Service(")) {
                            nameIdx = i;
                            break;
                        }
                    }
                    if (nameIdx < 0) continue;
                    // find the closing "    )" of this annotation
                    int closeIdx = -1;
                    boolean hasOverrides = false;
                    for (int i = nameIdx; i < lines.size(); i++) {
                        String t = lines.get(i);
                        if (t.contains("overrideAttributes")) hasOverrides = true;
                        if (t.equals("    )") || t.equals("\t)")) { closeIdx = i; break; }
                        if (t.trim().startsWith("public interface")) break;
                    }
                    if (closeIdx < 0) continue;
                    done = true;
                    if (hasOverrides) { skippedExisting++; break; }
                    // ensure the previous line ends with a comma
                    int prev = closeIdx - 1;
                    while (prev > nameIdx && lines.get(prev).trim().isEmpty()) prev--;
                    String prevLine = lines.get(prev);
                    if (!prevLine.trim().endsWith(",")) lines.set(prev, prevLine + ",");
                    List<String> insert = new ArrayList<>();
                    insert.add("        overrideAttributes = {");
                    List<String> ov = se.getValue();
                    for (int k = 0; k < ov.size(); k++) {
                        insert.add("            " + ov.get(k) + (k < ov.size() - 1 ? "," : ""));
                    }
                    insert.add("        }");
                    lines.addAll(closeIdx, insert);
                    if (!dryRun) {
                        String content = String.join(System.lineSeparator(), lines) + System.lineSeparator();
                        Files.write(javaFile, content.getBytes(StandardCharsets.UTF_8));
                    }
                    restored++;
                    filesChanged++;
                    break;
                }
                if (!done) {
                    notFound++;
                    System.out.println("NOT FOUND: " + component + " / " + serviceName);
                }
            }
        }
        System.out.println("restored=" + restored + " skippedExisting=" + skippedExisting + " notFound=" + notFound + (dryRun ? " (dry run)" : ""));
    }
}
