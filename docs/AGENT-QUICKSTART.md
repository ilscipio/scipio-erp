# Connect an AI agent to Scipio ERP

This guide takes an administrator from a running Scipio 4.0 server to a working agent connection in
five steps. It covers Claude Code, Claude Desktop, Cursor, VS Code and any other client that speaks
MCP over Streamable HTTP.

## 1. Check the server

1. Open `https://<host>/admin/control/McpServers` as an administrator. The page lists every MCP
   server profile and its tools. Each application has its own endpoint: `/ordermgr/mcp`,
   `/accounting/mcp`, `/cms/mcp`, `/shop/mcp` and so on. The hub is `/admin/mcp`.
2. Confirm the server runs behind HTTPS. A plain HTTP request is refused unless
   `mcp.allowInsecure=true` in `framework/mcp/config/mcp.properties` (development only).
3. When a reverse proxy terminates TLS, set `mcp.trustedProxies` to the proxy addresses. Only
   those addresses may assert `X-Forwarded-Proto: https`.

## 2. Create a user for the agent

1. The seed user `scp-agent` (group `SCIPIO_AGENT`) exists after the seed data load. It has read
   access to every back-office application and no write permission.
2. For an agent that must write, create a dedicated user login and put it in a security group with
   the needed `<APP>_UPDATE` permissions, plus `MCP_ACCESS`, `MCP_GATEWAY` and `OFBTOOLS_VIEW`.
   Grant `MCP_CODE_WRITE` only when the agent must edit CMS templates or scripts.
3. Never give an agent the `system` or `admin` login. Tokens for `system` are refused.

## 3. Create a token

1. Open `https://<host>/admin/control/McpTokens`.
2. Fill in the user login, a token name, the webapps the token may use (`*` for all), the
   read-only flag, the expiry (default 90 days, maximum 365) and, for a shop token, a spend cap
   (`maxOrderAmount`).
3. Click "Create Token". Copy the token now. The page shows it once, together with ready-made
   connection snippets that already contain the token.

## 4. Connect the client

Open `https://<host>/admin/control/McpSkills`. It shows the hub endpoint and one snippet per
client. Replace `<token>` with the token from step 3.

Claude Code:

```
claude mcp add --transport http scipio "https://<host>/admin/mcp" --header "Authorization: Bearer <token>"
```

Claude Code with skills (recommended): click "Download agent plugin (zip)", unzip it, then:

```
export SCIPIO_MCP_TOKEN="<token>"
claude plugin install ./scipio-claude-plugin
```

Cursor (`~/.cursor/mcp.json`) and VS Code (`.vscode/mcp.json`) use the JSON shown on the page.
Claude Desktop: Settings > Connectors > Add custom connector, with the hub URL and the header.

Any other client: POST JSON-RPC 2.0 to the endpoint with the `Authorization: Bearer` header. The
`curl` snippet on the page shows the `initialize` call.

## 5. Verify

1. In the client, run the `scipio_whoami` tool. It returns the user, the permissions and the
   endpoint.
2. Run `scipio_list_apps` and `scipio_list_skills`. Read the `scipio-agent-quickstart` skill.
3. Call one read tool, for example `order_find` with `limit` 1.
4. Open `https://<host>/admin/control/McpAudit`. Every call appears as one audit row.

## Application endpoints

| Task | Endpoint | Skill |
|---|---|---|
| Everything, discovery | `/admin/mcp` | `scipio-agent-quickstart`, `scipio-service-explorer` |
| Orders | `/ordermgr/mcp` | `scipio-order-management` |
| Customers | `/partymgr/mcp` | `scipio-customer-management` |
| Catalog | `/catalog/mcp` | `scipio-catalog-management` |
| Inventory, shipments | `/facility/mcp` | `scipio-inventory-fulfilment` |
| Accounting | `/accounting/mcp` | `scipio-accounting` |
| CMS pages, templates | `/cms/mcp` | `scipio-cms-authoring` |
| Content | `/content/mcp` | `scipio-content-management` |
| Tasks, events | `/workeffort/mcp` | `scipio-work-management` |
| Human resources | `/humanres/mcp` | `scipio-human-resources` |
| Manufacturing | `/manufacturing/mcp` | `scipio-manufacturing` |
| Marketing, sales | `/marketing/mcp` | `scipio-marketing-sfa` |
| Shop assistant | `/shop/mcp` | `scipio-shop-assistant` |
| Search index | `/solr/mcp` | `scipio-search-index` |
| Setup | `/setup/mcp` | `scipio-setup` |

## Common problems

- `401 Invalid token`: the token is wrong, revoked, expired, or has no expiry date while
  `mcp.token.allowNoExpiry=false`. Create a new token.
- `403 HTTPS required`: the request came over plain HTTP. Use HTTPS or set `mcp.trustedProxies`.
- `403 Token user lacks OFBTOOLS/ORDERMGR_VIEW`: the user needs every base permission of the
  webapp. Add the group in Webtools > Security.
- `Denied: Service X requires ACCOUNTING_UPDATE`: the service belongs to another application.
  The user needs that application's permission, on every endpoint.
- New tool or skill does not appear: click "Reload agent registry" on the Skills page, or restart
  the client session (`initialize`).
