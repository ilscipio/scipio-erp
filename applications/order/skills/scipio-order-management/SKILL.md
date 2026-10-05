---
name: scipio-order-management
description: How to find an order, read its detail, create a sales order for a customer, move it through its status flow, ship and invoice it, and start a return or a quote through the Scipio MCP order tools. Use for any order task.
metadata:
  scipio-server: order
---

# Scipio order management

The `order` MCP server covers sales orders, quotes, and returns. It exposes dedicated tools for the
common tasks and falls back to `scipio_service` with action `search` for the rest.

## Procedure

1. Call `scipio_whoami`. Confirm the user holds `ORDERMGR_VIEW` at least, and `ORDERMGR_UPDATE` for
   any status change.
2. Call `order` with action `find` to locate an order. Pass `orderId`, `statusId`, `partyId`, or a date range.
   Keep `limit` small. Read the summary rows before you fetch full detail.
3. Call `order` with action `get` with one `orderId` to read the full order: items, status history, party roles,
   payments, and shipments.
4. To move an order forward, call `order` with action `set_status` with the `orderId` and the new `statusId`.
   Read the result message. A rejected transition names the reason.
5. For a return or a quote, call `scipio_service` with action `search` with words such as `create return` or
   `create quote`, then follow the general service procedure in the `scipio-service-explorer` skill.
6. To add a comment, call `order` with action `note_add` with `orderId`, `note` and `internalNote` (`Y` or `N`).

## Create a sales order for a customer

1. Find the customer with `party` with action `find` on the `party` server (or call `scipio_apps` with action `call` with
   `application` = `party`). The customer needs a postal address; add one with `party` with action `contact_add`.
2. Confirm each product id and quantity with `product` with action `get` on the `catalog` server.
3. Read the order back to the user: customer, items, store. Ask for an explicit confirmation.
4. Call `order` with action `create` with `partyId`, `productStoreId` (for example `ScipioShop`) and `items`
   as an array of `{"productId": "...", "quantity": "1"}`. Defaults: the customer's shipping
   address, the store's first shipping method and offline payment. Pass `shipmentMethodTypeId`,
   `shippingContactMechId`, `paymentMethodTypeId` or `paymentMethodId` when the user chose them.
5. Read the result: `orderId`, `grandTotal`, `currencyUomId`, `statusId`. A denial that names a
   spend cap means the token's `maxOrderAmount` is below the total; report it.
6. Approve the order with `order` with action `set_status` and `statusId` = `ORDER_APPROVED` when the user
   asks for it.

## Ship and invoice

1. Call `order` with action `get` and confirm `statusId` = `ORDER_APPROVED`.
2. Call `order` with action `ship` with the `orderId` (and `originFacilityId` when the store has several
   warehouses). The tool creates, packs and ships one shipment for every item.
3. Call `order` with action `invoice` with the `orderId` to create the sales invoice. Continue with the
   `scipio-accounting` skill for payment.
4. Call `order` with action `get` again; the status history shows the changes.

## Status flow

Follow the standard order life cycle. Do not skip a step without an explicit reason from the user.

- `ORDER_CREATED`: the order exists but no one has approved it.
- `ORDER_APPROVED`: a person or an automated check has approved the order for fulfillment.
- `ORDER_COMPLETED`: every item has shipped, or the service, and payment has settled.
- `ORDER_CANCELLED`: the order stopped before completion. State the reason in the change request.

A cancelled order does not resume. Create a new order instead.

## Returns

Use the return services through `scipio_service` with action `search` (for example `createReturnHeader`,
`createReturnItem`, `createReturnItemResponse`). A return references the original order and item.
Run `dryRun: true` first, then confirm the return reason and the item quantity with the user before
you run the write.

## Quotes

Use the quote services through `scipio_service` with action `search` (for example `createQuote`,
`createQuoteItem`, `createQuoteFromCart`). A quote is not an order. Convert a quote to an order only
when the user asks for it, through the matching order-creation service.

## Conventions

- Order ids, party ids, and status ids are strings.
- Money fields are strings with two decimals, for example `"49.99"`.
- Dates are ISO-8601, for example `2026-01-31T10:00:00Z`.

## Safety

- `order` with action `set_status` is not read-only. State the current status and the target status to the
  user before you call it, unless the user already gave both.
- Do not cancel an order without an explicit user instruction.
- A permission error means the token user lacks a security group. Report the missing permission.
  Do not retry with a different action to work around it.
