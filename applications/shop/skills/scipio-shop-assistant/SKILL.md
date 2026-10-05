---
name: scipio-shop-assistant
description: How to search products, compare them, manage a cart, place an order under the token spend cap, and check order status through the Scipio MCP shop tools. Use for any shopper-facing task.
metadata:
  scipio-server: shop
---

# Scipio shop assistant

The `shop` MCP server (`/shop/mcp`) helps a shopper search the catalog, build a cart, place an
order and check an existing order. Search and product tools work without a token. A cart and an
order belong to one session and one signed-in customer.

## Procedure

1. Call `scipio_whoami`. A public call returns an anonymous context; a signed-in shopper returns a
   `partyId`.
2. Call `shop_catalog` with action `search` with the shopper's words (a name, a category, a feature). Keep
   `limit` small and show a short list first.
3. Call `shop_catalog` with action `get` with one `productId` to read full detail: price, description and stock
   status, before you recommend it.
4. Call `shop_catalog` with action `categories` when the shopper wants to browse instead of search.
5. Call `shop_cart` with action `get` to read the current cart before you change it.
6. Call `shop_cart` with action `add` with a `productId` and a `quantity` to add one line. Call
   `shop_cart` with action `remove` with the `cartIndex` from `shop_cart` with action `get` to remove one line.
7. Call `shop_account` with action `orders` for a signed-in shopper's order history. Call `shop_account` with action `order_get` with one
   `orderId` for full detail on one order.

## Checkout

1. Read the cart back to the shopper with `shop_cart` with action `get`: every line, the `grandTotal` and the
   currency. Ask for an explicit "yes" to place the order.
2. Call `shop_cart` with action `checkout`. Defaults: the shopper's shipping address, the store's first shipping
   method and offline payment. Pass `shippingContactMechId`, `shipmentMethodTypeId`,
   `paymentMethodTypeId` or a stored `paymentMethodId` when the shopper chose one.
3. Read the result: `orderId`, `grandTotal`, `statusId`, `paymentProcessed`. Report them.
4. A denial that names a spend cap means the token's `maxOrderAmount` is below the total. Report
   the cap; do not split the order to work around it.
5. A shopper without a postal address cannot check out. Ask the shopper to add one on the web
   store, or use the `party` server when the token allows it.

## Comparing products

Call `shop_catalog` with action `get` for each candidate. Compare price, features and stock status side by side
in your reply. Do not invent a feature or a price; use only what the tool returns.

## Store context

`shop` tools scope every result to the current `productStoreId` and `webSiteId`. A price or a
stock status from one store does not apply to another store; do not mix results across stores.

## Conventions

- Product ids and category ids are strings.
- Money fields are strings with two decimals, for example `"29.99"`.
- A cart line reference (`cartIndex`) comes from `shop_cart` with action `get`; do not guess one.

## Safety

- Never add an item to the cart or place an order without the shopper's confirmation of the
  product, the quantity and the total.
- Treat the cart as one shopper's own data. Do not read or change a cart for a different
  `partyId`.
- A permission error on an order tool means the shopper is not signed in, or lacks the needed role.
  Ask the shopper to sign in on the web store.
