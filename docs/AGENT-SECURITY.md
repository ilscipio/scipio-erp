# Agent access security and operations

This document describes how Scipio 4.0 secures agent access through MCP, and what an operator must
do before and during production use. Read `docs/AGENT-QUICKSTART.md` for the setup steps and
`research/docs/agentic-mcp-design.md` for the design.

## 1. Principles

- A token is a user login. An agent has exactly the permissions of that user. No permission comes
  from the token itself.
- Default deny. A call passes only when every gate in section 3 allows it.
- One rule for every endpoint. The hub and the application endpoints apply the same policy.
- Everything is audited. Every tool call, denied or not, writes one `McpAuditLog` row.
- Executable code is a separate permission. CMS templates and scripts need `MCP_CODE_WRITE`.

## 2. Permissions and groups

| Permission | Grants |
|---|---|
| `MCP_ACCESS` | Use any MCP endpoint. Required for every token user. |
| `MCP_GATEWAY` | Call any service through `scipio_service` with action `call` (still subject to section 3). |
| `MCP_ENTITY_READ` | Read entities outside a server's allowlist through `scipio_entity` with action `find`. |
| `MCP_ENTITY_WRITE` | Write entities through the entity tools (also needs `ENTITY_MAINT`). |
| `MCP_ADMIN` | Manage tokens of any user, view the audit log, reload the registry, run services of components without a webapp. |
| `MCP_CODE_WRITE` | Write CMS FreeMarker templates, Groovy scripts and asset templates. Remote code execution by design. |
| `MCP_MAIL_SEND` | Send email through `scipio_document` with action `mail` from the approved templates in `mcp.mail.templates` (purchase order, statement, customer notice). The raw `sendMail*` services stay on the deny list. |
| `MANUFACTURING_FLOOR` | Shop floor device: `production_run` actions `scan`, `declare`, `task_set_status`, `shop_floor` action `tasks` without `MANUFACTURING_UPDATE`. A tool that names a `permission` the user holds passes the base write gate of its backing service; deny lists and the read-only token rule still apply. |

Seed groups:

- `SCIPIO_AGENT` (user `scp-agent`): `MCP_ACCESS`, `MCP_GATEWAY`, `OFBTOOLS_VIEW`, `CMS_VIEW` and
  `_VIEW` on every main application. Read-only by default.
- `SCIPIO_CUSTOMER_AGENT`: `MCP_ACCESS` only. For shop customers who use an assistant on
  `/shop/mcp`.
- `SCIPIO_FLOOR`: `MCP_ACCESS`, `OFBTOOLS_VIEW`, `MANUFACTURING_VIEW`, `MANUFACTURING_FLOOR`. For the QR
  device tokens that `scipio_admin` with action `device_token_create` issues (login `floor-<station>`, token limited to the
  manufacturing endpoint).
- `FULLADMIN`: every `MCP_*` permission, including `MCP_CODE_WRITE` and `MCP_MAIL_SEND`.

Give an agent a dedicated user per purpose. Grant `_UPDATE` only for the applications the agent
must change. Do not reuse a human administrator's login.

## 3. The policy engine

`McpPolicy` decides every call in this order. The first failing gate denies the call.

1. Server access: token allowed for the webapp, user holds `MCP_ACCESS`, user holds every base
   permission of the webapp at `_VIEW` (for example `OFBTOOLS_VIEW` and `ORDERMGR_VIEW`). The hub
   needs `MCP_ADMIN` or `OFBTOOLS_VIEW`.
2. Tool access: a read-only token may call read-only tools only. A tool's `permission` attribute
   is checked when set.
3. Service deny lists: `mcp.service.deny` (global), the server's `serviceDeny`, and
   `mcp.service.adminOnly` (needs `MCP_ADMIN`).
4. Gateway permission: a direct service call needs `MCP_GATEWAY`.
5. Read-only classification: an entity-auto create, update, delete or expire is a write. A tool's
   own `readOnly` flag never widens a write service into a read.
6. Service permissions: a service that declares `permissions` or `permissionService` enforces them
   itself.
7. Component rule: an unguarded service needs the base permission of the component that owns the
   service: `_VIEW` for a read, `_UPDATE` for a write, on every webapp base except the generic
   `OFBTOOLS`. A component without a webapp needs `MCP_ADMIN`, except the components listed in
   `mcp.gateway.openComponents` (default `common`), which use the endpoint's base permission.

Hand-written tools on a webapp without a base permission (the shop) are allowed only on a server
with `allowAnonymous=true`; every other permission-less webapp fails closed.

## 4. Tokens

- Format `scp_<id>_<secret>_<crc>`. Only the SHA-256 hash is stored. The secret is shown once.
- Expiry: default `mcp.token.defaultExpiryDays` (90), maximum `mcp.token.maxExpiryDays` (365).
  A token without an expiry date is rejected unless `mcp.token.allowNoExpiry=true`.
- `mcp.token.denyUsers` (default `system,anonymous`) can never own or use a token.
- `webapps` limits a token to a list of endpoints. `readOnly=Y` blocks every write tool.
- `remoteAddrAllow` limits a token to addresses or CIDR blocks.
- `maxOrderAmount` caps the total of an order placed through `shop_cart` with action `checkout` or `order` with action `create`.
- Revoke a token on Webtools > Agent Access > Tokens. Revocation is immediate.

## 5. Transport and input limits

- HTTPS is required. `X-Forwarded-Proto` is honoured only from `mcp.trustedProxies`.
- `Origin` must be in `mcp.origin.allow` (default: no browser origin passes). `Host` may be limited
  with `mcp.host.allow`.
- Body at most `mcp.request.maxBytes` (1 MB), JSON nesting at most 64, batch at most 20 requests,
  string 64 KB, array 1000 items, list limit 500, result 200 000 characters.
- Rate limits: `mcp.rateLimit.perMinute` per token, `anonymousPerMinute` per address,
  `failedAuthPerMinute` per address, `maxConcurrent` slots per token.

## 6. Data protection

- `mcp.entity.deny`: `UserLogin*`, `*Password*`, `*CreditCard*`, `*EftAccount*`,
  `*PaymentGatewayConfig*`, `*Secret*`, `Mcp*`, `Security*`, `SystemProperty`, `Tenant*`,
  `*Keystore*`, `X509*`, `JobSandbox*`. These tables are never readable or writable through the
  entity tools, for any user.
- `mcp.redact.fields` and `mcp.redact.patterns` mask sensitive values in results, argument echoes
  and audit rows. A protected field may not be used in a condition, a selection or an ordering of
  `scipio_entity` with action `find`; this closes the value-guessing side channel.
- Audit rows store redacted arguments, a bounded result (`mcp.audit.resultMaxChars`) for idempotent
  replay, and a `REPLAY` status when a stored result is returned.

## 7. Operations checklist

Before production:

1. `mcp.allowInsecure=false`; `mcp.trustedProxies` set when a proxy terminates TLS.
2. `DevAuthEvent` (developer auto-login) removed or gated; see the release checklist.
3. The `smoke` token and every test token revoked; `scp-agent` given only the groups you need.
4. `mcp.gateway.allowUnguarded` reviewed. Set it to `false` to allow only services that declare
   their own permissions and the tools that profiles declare explicitly.
5. `mcp.tool.disable` set for tools you do not want, for example `scipio_entity` with action `store`.
6. Run `tools/mcp-test.sh -t <admin> -a <scp-agent> security skills` and confirm 0 failures.

During operation:

- Review Webtools > Agent Access > Audit daily. Filter by `DENIED` to see blocked attempts.
- Rotate tokens before they expire: create a new one, update the client, revoke the old one.
- After a change to a profile, an extension or a skill, click "Reload agent registry".

Incident response:

1. Revoke the token (Tokens page). The next request fails with `401`.
2. Disable the user login when the account itself is suspect.
3. Read the audit rows of the token: `tokenId`, `toolName`, `argsSummary`, `remoteAddr`.
4. Undo data changes with the normal application tools; the audit row names the service.

## 8. Known limits in 4.0

- No OAuth. Bearer tokens only.
- No server-initiated push (SSE) and no elicitation. Confirmation is a client-side hint
  (`_meta.requiresConfirmation`).
- Rate limits are per JVM, not per cluster.
- An agent that reads business data can still be misled by adversarial text inside that data. The
  skills tell the agent to treat returned data as data, not instructions; the client must enforce
  it.
