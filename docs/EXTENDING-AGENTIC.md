# Extending Scipio's agent access

This guide shows how to add a tool, a server profile, a provider, a skill, a path handler, and a
permission to Scipio's agent access layer (`framework/mcp`). It also explains how a bearer token
maps to a user, how the policy engine decides a call, and how to run a smoke test with `curl`.
Read `framework/resources/templates/mcp/README.txt` first for the two starter files.

## 1. Add a tool

Scipio groups tools into composite **topic** tools. One topic tool (for example `order`) holds
several **actions** (`find`, `get`, `create`, ...). A client picks the action with an `action`
argument; the topic tool description lists the actions, and each parameter description says which
actions need it.

1. Open the class, or create a new one under your component's own `src/.../mcp/` package.
2. Declare the topic once on the `@McpServer` (or `@McpServerExtension`) annotation:
   `topics = { @McpTopic(name = "invoice", title = "Invoices", order = 10, featured = true,
   description = "Invoices: find, read, create, change status.") }`.
3. Add a public static method for a hand-written action. The first parameter is always
   `McpCallContext ctx`. Each other parameter carries an `@McpParam` with an explicit `name`.
   Annotate the method with `@McpTool(topic = "invoice", name = "find", description = "...",
   readOnly = true)`.
4. Wrap a service directly as an action with `@McpServiceTool(service = "createInvoice",
   topic = "invoice", name = "create", description = "...", readOnly = false)` on the
   `@McpServer` annotation's `serviceTools` array; no Java method is needed.
5. Return a `Map`, a `List`, a `GenericValue`, a `String`, or an `McpResult` from a hand-written
   method. Convert a raw service result with `ResultConverter.toJsonMap(result)` before you
   return it.
6. Throw `McpToolException` for a tool error, or `McpToolException.denied(msg)` for a permission
   denial.

```java
@McpServer(name = "order", ...,
        topics = { @McpTopic(name = "order", title = "Orders", order = 10, featured = true,
                description = "Orders: find, read, approve, ship.") },
        serviceTools = {
            @McpServiceTool(service = "createOrder", topic = "order", name = "create",
                    description = "Create a sales order.", readOnly = false)
        })
public class OrderMcp {

    @McpTool(topic = "order", name = "find", description = "Find orders by id, status, or party.",
            readOnly = true)
    public static Object findOrders(McpCallContext ctx,
            @McpParam(name = "orderId", required = false) String orderId,
            @McpParam(name = "statusId", required = false) String statusId,
            @McpParam(name = "limit", required = false) Integer limit) {
        Map<String, Object> result = ctx.runService("findOrders", ctx.serviceContext(
                Map.of("orderId", orderId, "statusId", statusId, "limit", ctx.limit(limit))));
        return ResultConverter.toJsonMap(result);
    }
}
```

A client calls the topic tool `order` with `{"action": "find", "orderId": "..."}`. A read-only
action needs only the webapp's `_VIEW` permission and works with a `readOnly=Y` token. Every other
action needs `_UPDATE`, unless the tool's own `permission` attribute names a different one.
`tools/list` shows the per-action flags under each tool's `_meta.scipio.actions`; audit and usage
logs record the call as `tool:action` (for example `order:find`).

### 1.1 Draft, then approve

Scipio models approval with statuses. A write tool creates in the draft status (production run
`PRUN_CREATED`, purchase order `ORDER_CREATED`, return `RETURN_REQUESTED`, quote `QUO_CREATED`,
MRP proposal `REQ_PROPOSED`, invoice `INVOICE_IN_PROCESS`) and a second tool moves the record on
(`production_run_release`, `order_approve`, `return_set_status`, `mrp_proposal_approve`,
`invoice_set_status`). Mark the second tool `destructive = "true", requiresConfirmation = true`;
clients see `_meta.requiresConfirmation` and ask the person. Do not add a pending-action table.

### 1.2 Import tools: diff, then apply

An `*_import` tool takes `rows` (array of objects), `apply` (default false) and `force`. Use
`com.ilscipio.scipio.mcp.tool.ImportDiff`: the first pass fills `creates`, `updates`, `unchanged`
and `doubts`; the tool writes only when `diff.canWrite()` (apply and no doubts, or force). The
row readers (`ImportDiff.str/decimal/integer`) accept alias keys and decimal commas.

### 1.3 Documents and mail

`document_render(type, id)` (core tool on every server) renders the FOP screens the back office
prints (invoice, order, return, quote, production_run, production_run_labels, shipment_label)
through the content component's `createFileFromScreen` and returns the PDF as an embedded
resource block (`McpResult.blob`). Add a type in `DocumentTools.TYPES`.
`mail_send_template(templateId, partyIdTo|sendTo, text, bodyParameters, attachmentType,
attachmentId)` sends one of the `EmailTemplateSetting` rows listed in `mcp.mail.templates`
through `sendMailFromScreen`; it needs `MCP_MAIL_SEND`. A hand-written tool may call
`DocumentTools.send(ctx, args)` (see `purchase_order_send`).

## 2. Add a server profile

A server profile is one class annotated with `@McpServer`. One profile usually covers one
component.

1. Copy `framework/resources/templates/mcp/ExampleMcp.java` into your component.
2. Set `name`, `component` (the directory name, for example `order`), and `description`.
3. Declare each topic with `topics = { @McpTopic(name = "...", title = "...", order = 10,
   featured = true, description = "...") }`. Add the actions with `@McpTool` methods and
   `serviceTools = { @McpServiceTool(...) }` entries, as in section 1.
4. List the entities the server may read through `entities`, and the services ranked first in a
   `scipio_service` search through `featuredServices`. A featured service search hit is not itself
   callable as a tool; wrap it with `@McpServiceTool` when an agent should call it directly.
5. No build file change is needed. The `scipio-component` Gradle plugin adds `framework:mcp` to every
   application, addon and hot-deploy component. Set `scipioComponent { agentTools.set(false) }` to
   opt out.
6. Set `allowAnonymous = true` only for a public-facing server, such as the shop assistant, and mark
   the public actions `access = McpAccess.PUBLIC`.
7. Give every topic and action an `order` (lower first) when the tool list order matters; featured
   topics sort before others at equal order. The profile's own tools always come before the core
   tools.

Converting an older, pre-topic profile (a plain mapping of old tool names to new topic/action
names) by hand is tedious; run `tools/mcp-topicize.py <file> <map.json>` to apply the mapping in
one pass.

## 2.1 Extend an existing server from another component

An addon, a hot-deploy component or a customer project adds tools to a server it does not own with
`@McpServerExtension`. The server's source stays unchanged.

1. Copy `framework/resources/templates/mcp/ExampleMcpExtension.java` into your component.
2. Set `server` to the server name (for example `order`). Add `topics`, `featuredServices`,
   `entities`, `serviceTools` and `providers` as on `@McpServer`. A `topics` entry may add actions
   to a topic the server already declares, or declare a new one.
3. Add `@McpTool`, `@McpResource` and `@McpPrompt` methods as in section 1. Use your own action
   name prefix; an action name the topic already defines is skipped with a warning.
4. Reload the registry (section 2.2). The new actions appear on the server's endpoint and in
   `scipio_apps` (action `tools`).

## 2.2 Disable a tool, reload the registry

- `mcp.tool.disable` in `mcp.properties` lists tool names (`scipio_entity`), `server.tool` pairs
  (`shop.shop_cart`), or `tool:action` pairs (`order:set_status`) that are removed from every tool
  list.
- Webtools > Agent Access > Skills > "Reload agent registry" (or the `scipio_admin` hub tool,
  action `reload_registry`, `MCP_ADMIN`) drops the server registry, the skills, the service catalog
  and the policy caches. They rebuild on the next MCP request. A new class still needs the normal
  component reload or a restart; a new or changed `SKILL.md` needs only the reload.

## 3. Add a provider

A provider covers a tool set that a static annotation cannot express, such as a list of tools built
at run time from data.

1. Implement `com.ilscipio.scipio.mcp.registry.McpToolProvider`.
2. List the class in `@McpServer(providers = {MyProvider.class})`.
3. Follow the pattern in the built-in providers, `CoreToolProvider` and `SkillProvider`, for the
   exact method shape.

## 4. Add a skill

A skill is one `SKILL.md` file that tells an agent how to use a server's tools.

1. Copy `framework/resources/templates/mcp/SKILL.md` into
   `applications/<app>/skills/<name>/SKILL.md` (or the matching path under `framework/`,
   `addons/`, or `hot-deploy/`).
2. Fill in the frontmatter: `name`, one-sentence `description`, and `metadata.scipio-server` (the
   server name from step 2).
3. Write the body in short, direct sentences. Use a numbered list for a procedure. State exact tool
   names, field names, and status ids.
4. Keep the file 60 to 120 lines. Write every tool name in backticks; the registry checks each
   backticked tool-like name against the real tools when it builds and reports a wrong name as a
   warning in the log, in `scipio_apps` (action `skill_list`) and on Webtools > Agent Access >
   Skills.
5. Click "Reload agent registry" on the Skills page (or restart). The skill then appears in
   `scipio_apps` (action `skill_list`), as the resource `scipio://skills/<name>`, in the plugin
   zip download and in `./gradlew assembleAgentPlugin`.

## 5. Add a path handler

A path handler serves one URL prefix in every webapp at once, ahead of the normal controller path
check. `framework/mcp` uses this for `/mcp` itself; use it only for a new agent-facing path, not
for ordinary application screens.

1. Implement `org.ofbiz.webapp.control.WebappPathHandler`:

   ```java
   public interface WebappPathHandler {
       String pathPrefix();
       boolean handle(HttpServletRequest req, HttpServletResponse res, WebappInfo info);
   }
   ```

2. Annotate the class with `@WebappPathHandlerDef`. Set `priority` only when two handlers may claim
   the same prefix; a lower number runs first.
3. Give the class a public no-argument constructor. `WebappPathHandlerRegistry` discovers it
   automatically; no registration file is needed.
4. Never call `request.getSession()` inside a path handler. This rule keeps the path free of
   `Visit` rows and free of any dev auto-login escalation.

## 6. Add a permission

Follow the same `SecurityPermission` seed pattern the `framework/mcp` component itself uses.

1. Add one `SecurityPermission` row to a seed data XML file in your component's `data/` directory:

   ```xml
   <SecurityPermission permissionId="EXAMPLE_AGENT_ACTION"
       description="Let an agent run the example action."/>
   ```

2. Grant the permission to `SCIPIO_AGENT` when every agent should have it by default, and to
   `FULLADMIN` so a human admin always has it too:

   ```xml
   <SecurityGroupPermission groupId="SCIPIO_AGENT" permissionId="EXAMPLE_AGENT_ACTION"/>
   <SecurityGroupPermission groupId="FULLADMIN" permissionId="EXAMPLE_AGENT_ACTION"/>
   ```

3. Reference the permission from a tool's `permission` attribute, or leave the tool on the default
   webapp base permission when a dedicated permission is not needed.

## 7. How a token maps to a user

An MCP access token is not a separate credential type. Every `McpAccessToken` row points at one
`userLoginId`. The token lets an agent act as that user; it grants no permission of its own.

- Scipio 4.0 ships a seed user, `scp-agent`, with no password. It works through a token only. It
  belongs to the `SCIPIO_AGENT` security group.
- `SCIPIO_AGENT` starts read-only across the main applications. Add a permission to it, as in
  section 6, to widen it.
- A token owner, or a user with `MCP_ADMIN`, may create a token for themself. Only `MCP_ADMIN` may
  create a token for a different user. The logins in `mcp.token.denyUsers` (`system`, `anonymous`)
  can never own a token.
- Every token expires: default `mcp.token.defaultExpiryDays` (90), at most
  `mcp.token.maxExpiryDays` (365). A row without an expiry is rejected at authentication unless
  `mcp.token.allowNoExpiry=true`.
- A token that carries `readOnly=Y` blocks every tool that is not classified read-only, no matter
  what the underlying user could otherwise do.
- `maxOrderAmount` caps the total of an order placed through `shop_checkout` or `order_create`.
- `MCP_CODE_WRITE` is the permission for tools that write executable code (CMS templates, scripts,
  assets). `FULLADMIN` holds it; `SCIPIO_AGENT` does not.

## 8. How the policy engine decides a call

`McpPolicy` checks a service call, in this fixed order. The first matching rule decides the call.

1. A hard denylist always denies the call (`mcp.service.deny`, case-insensitive globs):
   `*SecurityGroup*`, `*SecurityPermission*`, `entityImport*`, `entityExport*`, `*Sql*`,
   `runService`, `purge*`, `*McpAccessToken*`, `updatePassword`, `resetPassword*`,
   `scheduleService*`, `*JobSandbox*`, `sendMail*`, `*Keystore*`, `*X509*`. The
   `mcp.service.adminOnly` list (`*UserLogin*`, `*Password*`) needs `MCP_ADMIN`.
2. The server's own `serviceDeny` list denies the call next.
3. A direct call (not a declared tool) needs `MCP_GATEWAY`. A read-only token may only call a
   service that classifies as read-only.
4. A service that declares `permissions` or `permissionService` is allowed through; the service
   itself enforces those permissions at run time.
5. Component rule: an unguarded service needs the base permission of the component that owns the
   service, on every endpoint including the hub: `_VIEW` when the service classifies as read-only,
   `_UPDATE` otherwise, on every base except the generic `OFBTOOLS` (`_ADMIN` always passes). A
   service whose component has no webapp needs `MCP_ADMIN`, unless the component is listed in
   `mcp.gateway.openComponents` (default `common`); then the endpoint's base permission applies.

Read-only classification: an entity-auto service is read-only unless its `invoke` is `create`,
`update`, `delete` or `expire`; any other service is read-only only when its name starts with
`get`, `find`, `list`, `search`, `lookup`, `count`, `calc`, `calculate`, `is`, `has`, `describe`
or `check` and `mcp.gateway.readOnlyHeuristic=true`. A tool's `readOnly` flag never widens the
classification of the service it wraps; both must agree. Every other service counts as a write,
and as destructive.

## 9. The curl smoke test

Run this test after you add a server profile or a tool, to confirm the endpoint answers before you
connect a real client.

1. Create a token for a test user (through the webtools "Agent Access" pages, or the
   `createMcpAccessToken` service). Copy the returned `token` value once; it is not shown again.
2. Open a session and list the tools:

   ```bash
   curl -s -k -X POST "https://localhost:8443/admin/mcp" \
       -H "Authorization: Bearer <token>" \
       -H "Content-Type: application/json" \
       -H "MCP-Protocol-Version: 2025-06-18" \
       -d '{"jsonrpc":"2.0","id":1,"method":"initialize",
            "params":{"protocolVersion":"2025-06-18",
                      "capabilities":{},
                      "clientInfo":{"name":"smoke-test","version":"1.0"}}}' \
       -D - -o /tmp/mcp-init.json
   ```

   Read the `Mcp-Session-Id` response header; every later call needs it.

3. List the tools with that session:

   ```bash
   curl -s -k -X POST "https://localhost:8443/admin/mcp" \
       -H "Authorization: Bearer <token>" \
       -H "Content-Type: application/json" \
       -H "Mcp-Session-Id: <session-id>" \
       -d '{"jsonrpc":"2.0","id":2,"method":"tools/list","params":{}}'
   ```

4. Call one read-only tool:

   ```bash
   curl -s -k -X POST "https://localhost:8443/admin/mcp" \
       -H "Authorization: Bearer <token>" \
       -H "Content-Type: application/json" \
       -H "Mcp-Session-Id: <session-id>" \
       -d '{"jsonrpc":"2.0","id":3,"method":"tools/call",
            "params":{"name":"scipio_whoami","arguments":{}}}'
   ```

5. Confirm the response is HTTP 200, carries no stack trace, and the tool result matches the token
   user. A 404 on a later call means the session expired; return to step 2.
6. Test a denied path: call a tool with a wrong or expired token, and confirm the response reports
   a permission error instead of a server error.

## 10. Checklist before you ship a change

- [ ] The new tool, server, or provider compiles as part of the component's own build.
- [ ] Every write tool sets `readOnly = false` and states a `permission`, or relies on the webapp
      base permission on purpose.
- [ ] A new permission is granted to `FULLADMIN`, and to `SCIPIO_AGENT` only when every agent
      should have it by default.
- [ ] A new skill is 60 to 120 lines, packages through `./gradlew assembleAgentPlugin`, and names
      real tool and action names only.
- [ ] The curl smoke test in section 9 passes for both an allowed call and a denied call.
- [ ] A status, money or stock change sets `destructive = "true", requiresConfirmation = true`; a
      create lands in the draft status (section 1.1).
- [ ] A new action is named in the application's `SKILL.md` and has one check in `tools/mcp-test.sh`.

## Scenario test suite

`tools/mcp-test.sh` runs end-to-end scenarios against a running server with curl only.

```
tools/mcp-test.sh -t <admin-token> -c <customer-token> -a <agent-token> all
tools/mcp-test.sh -t <admin-token> product        # virtual product, two variants, prices, category
tools/mcp-test.sh -t <admin-token> user           # person, user login, CUSTOMER role
tools/mcp-test.sh -t <admin-token> -c <customer-token> order invoice
tools/mcp-test.sh -t <admin-token> cms            # page template, Groovy script, page, render (gateway)
tools/mcp-test.sh -t <admin-token> -a <agent-token> cmstools   # the cms_page/cms_template tools, MCP_CODE_WRITE gate
tools/mcp-test.sh -t <admin-token> -a <agent-token> security   # fail-closed policy, proxy header, deny lists
tools/mcp-test.sh -t <admin-token> skills         # every skill loads and names real tools
```

The admin token must belong to a FULLADMIN user. The customer token must belong to a shop customer in the
`SCIPIO_CUSTOMER_AGENT` group with a postal address (DemoCustomer works). The agent token belongs to
`scp-agent` (group `SCIPIO_AGENT`); without it the security scenario skips the agent checks. The exit code
is the number of failed checks. Add a scenario as one `scenario_<name>()` function and one `case` entry.

## Install a client

`docs/AGENT-QUICKSTART.md` is the end-user guide. In short: Webtools > Agent Access > Tokens creates a
token and shows the connection snippets with the token filled in; Webtools > Agent Access > Skills shows
the snippets for Claude Code, Claude Desktop, Cursor, VS Code and curl, offers "Download agent plugin
(zip)" (built by the running server, no Gradle needed) and "Reload agent registry". The hub tool
`scipio_admin` (action `install_info`) returns the same data to an agent. `./gradlew assembleAgentPlugin`
builds the same zip from the source tree for CI.

## App-defined core tools ("often used" tools)

Each application declares its core functionality as topics and actions in its `@McpServer` profile:

- `topics = { @McpTopic(name = "...", featured = true) }` for the topic tools listed first.
- Hand-written `@McpTool(topic = "...", name = "...")` methods.
- `serviceTools = { @McpServiceTool(service = "...", topic = "...", name = "...") }` for actions
  that wrap a service directly.
- `featuredServices = { "createOrder", ... }` ranks a service first in a `scipio_service` search.
  It does not create a callable tool by itself; wrap the service with `@McpServiceTool` for that.

Agents discover and use them in three ways:

1. On the app's own endpoint (`/ordermgr/mcp`, `/accounting/mcp`, ...) the topic tools appear first
   in `tools/list`, each with its actions under `_meta.scipio.actions`.
2. `scipio_apps` (action `tools`, any endpoint) lists the app tools per app, featured first, with
   the app's endpoint URL. Pass `application` to list one app; pass `featuredOnly` for the short
   list.
3. `scipio_apps` (action `call`, any endpoint, useful on the hub) runs one app tool, with its own
   `action` argument, under the app's own permission gate.

`scipio_apps` (action `list`) shows every deployed webapp with its `mcpUrl` and its `coreTools`
names.
