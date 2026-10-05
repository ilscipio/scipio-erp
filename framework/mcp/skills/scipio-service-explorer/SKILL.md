---
name: scipio-service-explorer
description: How to discover, inspect and call Scipio ERP services and entities safely through the Scipio MCP tools. Use before any task that needs a service you have not used yet.
metadata:
  scipio-server: admin
---

# Scipio service explorer

Scipio ERP exposes about 2000 services. Each MCP endpoint (`/<webapp>/mcp`) gives you the same core tools.
The `/admin/mcp` hub covers every application. An application endpoint such as `/ordermgr/mcp` scopes the
catalog to that application and adds dedicated tools.

## Procedure

1. Call `scipio_whoami`. Note the user, the permissions, and the store context.
2. Prefer a dedicated action (for example `order` action `find`) when the tool list has one. Dedicated actions are curated
   and marked `featured`.
3. Call `scipio_service` with action `search` with two or three words that describe the goal (for example `create order`,
   `inventory available`, `party contact`). Rows are ranked: featured first, then services the UI uses, then the
   most called. `callable=false` rows show the reason.
4. Call `scipio_service` with action `describe` for the chosen service. Read `inputSchema.required` and `permissions`.
5. Run `scipio_service` with action `call` with `dryRun: true` first when the service changes data. Fix validation errors.
6. Run the service. Pass `idempotencyKey` (any unique string) when you may retry. A repeat with the same key
   returns the stored result and does not run twice.
7. Read the result. `successMessage` and the OUT parameters are returned. Errors come back as `isError` with
   the service message.

## Entities

- Call `scipio_entity` with actions `list` and `describe` to show tables and fields.
- Call `scipio_entity` with action `find` to read records with simple conditions (`eq`, `ne`, `lt`, `le`, `gt`, `ge`, `like`, `in`,
  `notIn`, `isNull`, `notNull`). Use `fields` and `limit` to keep results small.
- Call `scipio_entity` with actions `store` and `remove` to bypass service logic. Use them only when no service exists
  and only with explicit user approval. They need `ENTITY_MAINT` and `MCP_ENTITY_WRITE`.

## Conventions

- Dates: ISO-8601 (`2026-01-31T10:00:00Z`) or `yyyy-MM-dd HH:mm:ss`.
- Money: strings with two decimals (`"12.50"`), never floats.
- Ids: Scipio ids are strings (`orderId`, `partyId`, `productId`).
- Status ids follow the pattern `ORDER_APPROVED`, `PARTY_ENABLED`, `PRODUCT_ACTIVE`.

## Permission rule for service calls

- A service belongs to the application component that defines it. A call through `scipio_service` action `call` needs
  that application's base permission: `<APP>_VIEW` for a read service, `<APP>_UPDATE` for a write service,
  on every endpoint, including the hub. Example: `createInvoice` needs `ACCOUNTING_UPDATE` even when called
  from `/ordermgr/mcp`.
- A service that declares its own permission check enforces that check itself.
- A framework service without an application (no webapp) needs `MCP_ADMIN`, except the `common` services,
  which use the endpoint's own base permission.
- `callable` and `reason` in `scipio_service` action `search` rows already reflect this rule for the current user.

## Safety

- Every call runs as the token user. Permission errors mean the user lacks a security group, not that the
  tool is broken. Report the missing permission to the user.
- Do not loop over `scipio_service` action `call` with write services without a plan the user approved.
- Results over 200 KB are truncated. Use `limit` and `fields`.
