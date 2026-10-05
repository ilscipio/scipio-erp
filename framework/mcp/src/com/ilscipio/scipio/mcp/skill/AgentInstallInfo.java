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
package com.ilscipio.scipio.mcp.skill;

import java.util.LinkedHashMap;
import java.util.Map;

import javax.servlet.http.HttpServletRequest;

import org.ofbiz.base.util.UtilValidate;

import com.ilscipio.scipio.mcp.protocol.JsonRpc;
import com.ilscipio.scipio.mcp.security.McpConfig;

/**
 * SCIPIO: 4.0.0: Builds the connection settings an agent client needs: the hub URL and ready-to-paste snippets
 * for Claude Code, Claude Desktop, Cursor, VS Code and any generic Streamable HTTP client. Shared by the
 * {@code scipio_install_info} tool and the Webtools "Agent Access" pages.
 */
public final class AgentInstallInfo {

    public static final String TOKEN_PLACEHOLDER = "<token>";

    private AgentInstallInfo() {}

    /** {@code scheme://host[:port]} of the request, honouring X-Forwarded-Proto/Host only through the servlet API. */
    public static String baseUrl(HttpServletRequest request) {
        String scheme = request.getScheme();
        String host = request.getServerName();
        int port = request.getServerPort();
        boolean defaultPort = ("https".equals(scheme) && port == 443) || ("http".equals(scheme) && port == 80) || port <= 0;
        return scheme + "://" + host + (defaultPort ? "" : ":" + port);
    }

    /**
     * Connection info and snippets. {@code token} may be null; the snippets then carry a placeholder.
     * {@code webappName} selects the endpoint (default: the hub at {@code /admin}).
     */
    public static Map<String, Object> build(String baseUrl, String webappName, String token) {
        String segment = McpConfig.getPathSegment();
        String webapp = UtilValidate.isNotEmpty(webappName) ? webappName : "admin";
        String url = baseUrl + "/" + webapp + "/" + segment;
        String tok = UtilValidate.isNotEmpty(token) ? token : TOKEN_PLACEHOLDER;
        String auth = "Bearer " + tok;

        Map<String, Object> out = new LinkedHashMap<>();
        out.put("baseUrl", baseUrl);
        out.put("webapp", webapp);
        out.put("url", url);
        out.put("hubUrl", baseUrl + "/admin/" + segment);
        out.put("transport", "streamable-http");
        out.put("auth", "Authorization: Bearer <token> (create a token in Webtools > Agent Access > Tokens)");
        out.put("pluginDownloadUrl", baseUrl + "/admin/control/McpPluginDownload");
        out.put("tokenIsPlaceholder", !UtilValidate.isNotEmpty(token));

        Map<String, Object> snippets = new LinkedHashMap<>();
        snippets.put("claudeCode", "claude mcp add --transport http scipio \"" + url + "\" --header \"Authorization: " + auth + "\"");
        snippets.put("claudeCodePlugin", "claude plugin install ./scipio-claude-plugin   # after you download and unzip the plugin");
        snippets.put("cursor", "// ~/.cursor/mcp.json\n" + JsonRpc.writePretty(mcpServers(url, auth, null)));
        snippets.put("vscode", "// .vscode/mcp.json\n" + JsonRpc.writePretty(vscodeServers(url, auth)));
        snippets.put("claudeDesktop", "// Claude Desktop: Settings > Connectors > Add custom connector, URL below; or claude_desktop_config.json\n"
                + JsonRpc.writePretty(mcpServers(url, auth, "http")));
        snippets.put("generic", JsonRpc.writePretty(mcpServers(url, auth, "http")));
        snippets.put("curl", "curl -s -k -X POST \"" + url + "\" -H \"Authorization: " + auth + "\" -H \"Content-Type: application/json\" "
                + "-H \"MCP-Protocol-Version: 2025-06-18\" -d '{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"initialize\","
                + "\"params\":{\"protocolVersion\":\"2025-06-18\",\"capabilities\":{},\"clientInfo\":{\"name\":\"curl\",\"version\":\"1\"}}}'");
        out.put("snippets", snippets);
        return out;
    }

    private static Map<String, Object> mcpServers(String url, String auth, String type) {
        Map<String, Object> server = new LinkedHashMap<>();
        if (type != null) server.put("type", type);
        server.put("url", url);
        Map<String, Object> headers = new LinkedHashMap<>();
        headers.put("Authorization", auth);
        server.put("headers", headers);
        Map<String, Object> servers = new LinkedHashMap<>();
        servers.put("scipio", server);
        Map<String, Object> root = new LinkedHashMap<>();
        root.put("mcpServers", servers);
        return root;
    }

    private static Map<String, Object> vscodeServers(String url, String auth) {
        Map<String, Object> server = new LinkedHashMap<>();
        server.put("type", "http");
        server.put("url", url);
        Map<String, Object> headers = new LinkedHashMap<>();
        headers.put("Authorization", auth);
        server.put("headers", headers);
        Map<String, Object> servers = new LinkedHashMap<>();
        servers.put("scipio", server);
        Map<String, Object> root = new LinkedHashMap<>();
        root.put("servers", servers);
        return root;
    }
}
