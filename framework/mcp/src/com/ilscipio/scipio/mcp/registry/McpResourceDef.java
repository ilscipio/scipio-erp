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
package com.ilscipio.scipio.mcp.registry;

import java.util.Map;

/**
 * SCIPIO: 4.0.0: MCP resource or resource template definition.
 */
public final class McpResourceDef {

    @FunctionalInterface
    public interface Reader {
        /** Returns the resource text for the given uri; uriParams holds template placeholders. */
        String read(McpCallContext ctx, String uri, Map<String, String> uriParams) throws Exception;
    }

    private final String uri;
    private final String name;
    private final String description;
    private final String mimeType;
    private final boolean template;
    private final Reader reader;

    public McpResourceDef(String uri, String name, String description, String mimeType, Reader reader) {
        this.uri = uri;
        this.name = name;
        this.description = description != null ? description : "";
        this.mimeType = mimeType != null && !mimeType.isEmpty() ? mimeType : "text/plain";
        this.template = uri.contains("{");
        this.reader = reader;
    }

    public String getUri() { return uri; }
    public String getName() { return name; }
    public String getDescription() { return description; }
    public String getMimeType() { return mimeType; }
    public boolean isTemplate() { return template; }
    public Reader getReader() { return reader; }

    /** Matches a concrete uri against this definition; returns placeholder values or null when no match. */
    public Map<String, String> match(String concreteUri) {
        if (!template) {
            return uri.equals(concreteUri) ? java.util.Collections.emptyMap() : null;
        }
        java.util.List<String> names = new java.util.ArrayList<>();
        StringBuilder regex = new StringBuilder("^");
        java.util.regex.Matcher m = java.util.regex.Pattern.compile("\\{([A-Za-z0-9_]+)\\}").matcher(uri);
        int last = 0;
        while (m.find()) {
            regex.append(java.util.regex.Pattern.quote(uri.substring(last, m.start())));
            regex.append("([^/]+)");
            names.add(m.group(1));
            last = m.end();
        }
        regex.append(java.util.regex.Pattern.quote(uri.substring(last))).append("$");
        java.util.regex.Matcher cm = java.util.regex.Pattern.compile(regex.toString()).matcher(concreteUri);
        if (!cm.matches()) return null;
        Map<String, String> params = new java.util.LinkedHashMap<>();
        for (int i = 0; i < names.size(); i++) {
            params.put(names.get(i), cm.group(i + 1));
        }
        return params;
    }
}
