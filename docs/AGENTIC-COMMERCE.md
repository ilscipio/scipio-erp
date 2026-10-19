# Agentic Commerce Protocol (ACP)

Scipio 4.0 implements the merchant side of the Agentic Commerce Protocol, the open standard of
OpenAI and Stripe (https://agenticcommerce.dev, spec version 2026-04-17). An AI agent, for example
ChatGPT, creates a checkout session in your store, fills in the buyer and the address, and pays with a
Stripe Shared Payment Token. Scipio prices the cart, creates the order and sends signed order webhooks.

The code is in the component `applications/acp`. The settings are in
`applications/acp/config/acp.properties`.

## Setup

1. Check that the component `acp` loads. It is listed in `applications/component-load.xml`.
2. Check `enabled=true` in `acp.properties`. When it is `false`, every ACP path answers 404.
3. Create the agent token. The demo login `scp-acp-agent` (group `SCIPIO_ACP_AGENT`) has the permission
   `ACP_CHECKOUT`. Run the service `createMcpAccessToken` with `userLoginId=scp-acp-agent`. The token is
   an MCP access token. Copy it when the service returns it.
4. Set up the payment (see "Payment" below): a Stripe secret key or the Scipio hub.
5. Optional: set `webhook.url` and `webhook.secret` for order webhooks.
6. Optional: give the agent the feed URL. The job `acpGenerateFeeds` builds the feed once a day.
7. For the root host, serve the discovery document at `/.well-known/acp.json` (see "Discovery").

Use HTTPS. The MCP rules of `docs/AGENT-SECURITY.md` apply to the token.

## Endpoints

Every store webapp answers ACP. The examples use the webapp `/shop`.

| Method | Path | Auth | Purpose |
|---|---|---|---|
| GET | `/.well-known/acp.json` | none | Discovery on the root host |
| GET | `/shop/.well-known/acp.json` | none | Discovery of one store |
| POST | `/shop/acp/checkout_sessions` | Bearer | Create a session |
| GET, POST | `/shop/acp/checkout_sessions/{id}` | Bearer | Read or update a session |
| POST | `/shop/acp/checkout_sessions/{id}/complete` | Bearer | Pay and create the order |
| POST | `/shop/acp/checkout_sessions/{id}/cancel` | Bearer | Cancel a session |
| POST | `/shop/acp/mcp` | Bearer | MCP binding |
| GET | `/shop/acp/feed/products.jsonl` | Bearer | Product feed (JSON Lines) |
| GET | `/shop/acp/feed/metadata.json` | Bearer | Feed metadata |

The MCP binding offers the tools `create_checkout_session`, `get_checkout_session`,
`update_checkout_session`, `complete_checkout_session` and `cancel_checkout_session`.

### Headers

- `Authorization: Bearer <token>`. The login of the token needs the permission `ACP_CHECKOUT`.
- `API-Version`. Required. Use `2026-04-17`. The setting `api.supportedVersions` lists the accepted versions.
- `Idempotency-Key`. Required on every POST. A replay returns the first answer and the header
  `Idempotent-Replayed`. A second request while the first runs gives 409 `idempotency_in_flight`.
  The same key with another body gives 422 `idempotency_conflict`.
- `Signature` and `Timestamp`. Required only when `signature.secret` is set. The signature is the base64
  HMAC-SHA256 of `<Timestamp>.<raw body>`.

## Request flow

```bash
BASE=https://shop.example.com
TOKEN=<access token of scp-acp-agent>
H=(-H "Authorization: Bearer $TOKEN" -H "API-Version: 2026-04-17" -H "Content-Type: application/json")

# 1. Create a session. The status is not_ready_for_payment: the address is missing.
curl -s -X POST "$BASE/shop/acp/checkout_sessions" "${H[@]}" -H "Idempotency-Key: $(uuidgen)" \
  -d '{"currency":"usd","line_items":[{"id":"GZ-1000","quantity":2}]}'

# 2. Update with the buyer and the address. The status becomes ready_for_payment.
curl -s -X POST "$BASE/shop/acp/checkout_sessions/<id>" "${H[@]}" -H "Idempotency-Key: $(uuidgen)" \
  -d '{"buyer":{"first_name":"Jane","last_name":"Doe","email":"jane.doe@example.com"},
       "fulfillment_details":{"name":"Jane Doe","address":{"name":"Jane Doe","line_one":"1234 Chat Road",
       "city":"San Francisco","state":"CA","country":"US","postal_code":"94131"}}}'

# 3. Complete with the Shared Payment Token. The answer holds the order.
curl -s -X POST "$BASE/shop/acp/checkout_sessions/<id>/complete" "${H[@]}" -H "Idempotency-Key: $(uuidgen)" \
  -d '{"payment_data":{"handler_id":"card_tokenized","instrument":{"type":"card",
       "credential":{"type":"spt","token":"spt_..."}}}}'
```

The session answer lists the line items, the totals in minor units (cents), the `fulfillment_options`
(the shipment methods of the store), the `messages` (for example a missing address) and the `status`.
To choose a shipping option, update with `selected_fulfillment_options`:
`[{"type":"shipping","option_id":"<id>","item_ids":[]}]`.

## Pricing

Scipio prices a session with the store cart engine. It uses the catalog prices, the stock, the
promotions and discount codes (discount extension), the shipping options of the store and the tax.
All amounts are in minor units. Each order gets a guest customer. The sales channel of the order is
`ACP_SALES_CHANNEL`.

## Payment

The agent pays with a Stripe Shared Payment Token (`spt_...`). Set `payment.handler`:

| Value | Meaning |
|---|---|
| `stripe` | The store Stripe account. Scipio creates the payment, records it (payment method type `EXT_ACP_STRIPE`) and approves the order. |
| `hub` | The Scipio hosted hub (payment method type `EXT_STRIPE_HUB`, settings in `hubcheckout.properties`). The hub records the payment with its Stripe webhook. |
| `auto` | Use `hub` when `hubcheckout.properties` has `storeId`, `url` and `key`. Else use `stripe` when a secret key is set. |

Set the Stripe secret key in the SystemProperty `stripe.secretKey` or in the environment variable
`ACP_STRIPE_SECRET_KEY`. Do not write it in the file.

A card that needs 3-D Secure is declined with `requires_3ds`. 3-D Secure interventions are not supported yet.

## Order webhooks

Set `webhook.url` and `webhook.secret`. Scipio sends `order_create` and `order_update` with the full Order
object. Each request has the header `Merchant-Signature: t=<unix>,v1=<hex>`. The value `v1` is the hex
HMAC-SHA256 of `<t>.<body>` with the webhook secret. Check the signature and reject an old `t`.

A failed delivery retries with backoff (job `acpDeliverWebhooks`). After `webhook.maxAttempts` the delivery
is parked. To test, run `tools/acp-webhook-listener.py` and set `webhook.url` to its address.

## Product feed

The job `acpGenerateFeeds` runs daily. It writes the feed for each store:

- `GET /shop/acp/feed/products.jsonl`: the products, one JSON object per line.
- `GET /shop/acp/feed/metadata.json`: the metadata of the last run.

When the store has more than one catalog, set `feed.prodCatalogId`. `feed.maxProducts` limits the size.

## Discovery

Each store answers `GET /shop/.well-known/acp.json` without authentication. Agents look for the
document on the root host: `GET /.well-known/acp.json`. The root path belongs to the launcher webapp,
so Scipio answers it there. When a reverse proxy serves the root host, forward `/.well-known/acp.json`
to the server.

## Settings

Set each value in `acp.properties`, in the SystemProperty (resource `acp`) or per product store with the
suffix `.<productStoreId>`, for example `stripe.secretKey.SCIPIO_STORE`. An environment variable
such as `ACP_STRIPE_SECRET_KEY` wins over the file.

| Setting | Default | Meaning |
|---|---|---|
| `enabled` | `true` | Master switch. Off: 404 on every ACP path. |
| `api.version` | `2026-04-17` | The protocol version of the server. |
| `api.supportedVersions` | five versions, 2025-09-29 to 2026-04-17 | Accepted values of `API-Version`. |
| `excludedWebapps` | empty | Webapps that never answer ACP. |
| `permission` | `ACP_CHECKOUT` | The permission that a token login needs. |
| `rateLimit.perMinute` | `120` | Requests per minute per token. |
| `session.ttlMinutes` | `360` | Minutes until an open session expires. |
| `idempotency.ttlMinutes` | `1440` | Minutes that an idempotency key is kept. |
| `payment.handler` | `auto` | `stripe`, `hub` or `auto`. |
| `stripe.secretKey` | empty | Secret key of the store Stripe account. |
| `stripe.apiBase` | `https://api.stripe.com` | The Stripe API address. |
| `stripe.apiVersion` | `2026-09-30.preview` | The Stripe API version that accepts the shared token. |
| `stripe.accountId` | empty | Account id (`acct_...`) that the handler config shows to the agent. |
| `webhook.url` | empty | Webhook address of the agent. Empty: no webhooks. |
| `webhook.secret` | empty | HMAC secret of the webhook. |
| `webhook.maxAttempts` | `8` | Attempts before a delivery is parked. |
| `signature.secret` | empty | When set, every request needs `Signature` and `Timestamp`. |
| `signature.maxSkewSeconds` | `300` | The largest age of `Timestamp`. |
| `feed.prodCatalogId` | empty | The catalog of the feed. |
| `feed.maxProducts` | `50000` | The largest number of feed products. |
| `link.terms_of_use` | `/control/legal` | Seller policy link in the session. |
| `link.privacy_policy` | `/control/privacyCenter` | Seller policy link in the session. |
| `link.return_policy` | `/control/policies` | Seller policy link in the session. |
| `link.shipping_policy` | `/control/policies` | Seller policy link in the session. |

## Limits

- Stripe is the only payment provider. The agent must send a Shared Payment Token.
- 3-D Secure interventions are not supported. The payment is declined with `requires_3ds`.
- Each order has a guest customer. ACP does not create customer accounts.
- A session expires after `session.ttlMinutes`.
- The rate limit is `rateLimit.perMinute` requests per token.
- The feed holds at most `feed.maxProducts` products.

## Testing

`tools/acp-test.sh` runs 29 checks against a running server: discovery, auth, feed, create, update,
shipping options, idempotency, complete, cancel and the MCP binding.

```bash
ACP_TOKEN=<token> ACP_SPT=<spt_... test token> tools/acp-test.sh -b https://localhost:8443 -w shop
```

Without `ACP_SPT`, the complete step expects a declined answer. The script passes against a local server
with a real Stripe test token. To test webhooks, set `ACP_WEBHOOK_PORT` and run
`tools/acp-webhook-listener.py`.
