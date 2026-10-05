---
name: scipio-inventory-fulfilment
description: How to read stock levels, receive inventory, and find, inspect and ship shipments through the Scipio MCP facility and order tools. Use for any warehouse or fulfilment task.
metadata:
  scipio-server: facility
---

# Scipio inventory and fulfilment

The `facility` MCP server (`/facility/mcp`) covers stock per facility and shipments. Shipping an
order end to end also uses `order` with action `ship` on the `order` server (`/ordermgr/mcp`), or
call `scipio_apps` with action `call` from the hub.

## Procedure: check and receive stock

1. Call `scipio_whoami`. Confirm `FACILITY_VIEW` for reads and `FACILITY_UPDATE` for writes.
2. Call `inventory` with action `get` with a `productId`, and a `facilityId` to limit the answer to one warehouse.
   Read `availableToPromiseTotal` (sellable now) and `quantityOnHandTotal` (physically present).
3. To receive goods, call `inventory` with action `receive` with `productId`, `facilityId`, `quantityAccepted`,
   `inventoryItemTypeId` = `NON_SERIAL_INV_ITEM` and `unitCost`. Confirm the quantity with the
   user first.
4. Call `inventory` with action `get` again and confirm the totals rose by the received quantity.

## Procedure: ship an approved order

1. Call `order` with action `get` (order server) and confirm `statusId` = `ORDER_APPROVED` and that the items are
   in stock at the shipping facility (`inventory` with action `get`).
2. Call `order` with action `ship` with the `orderId` and, when the store has several warehouses,
   `originFacilityId`. The tool creates the shipment, packs it and marks it shipped.
3. Call `shipment` with action `find` with `primaryOrderId` = the order id. Keep the `shipmentId`.
4. Call `shipment` with action `get` with the `shipmentId`. Read `statusHistory` and `routeSegments` (carrier and
   tracking data).
5. Report the shipment id, status and tracking number to the user.

## Reads

- `shipment` action `find`: by `shipmentId`, `statusId`, `shipmentTypeId`, `primaryOrderId` or `partyIdTo`.
- `shipment` action `get`: items, status history, route segments.
- `inventory` action `adjust_start`: opens a physical inventory count; record variances through the
  service catalog (call `scipio_service` with action `search` with `physical inventory variance`).

## Status ids

- Shipment: `SHIPMENT_INPUT`, `SHIPMENT_SCHEDULED`, `SHIPMENT_PICKED`, `SHIPMENT_PACKED`,
  `SHIPMENT_SHIPPED`, `SHIPMENT_DELIVERED`, `SHIPMENT_CANCELLED`.

## Conventions

- Quantities are decimal strings, for example `"5"` or `"2.5"`.
- A facility id is a string such as `WebStoreWarehouse`.
- Available-to-promise can be lower than on-hand when orders reserve stock.

## Safety

- `inventory` with action `receive` and `order` with action `ship` change stock and order state. State product, quantity,
  facility and order to the user before you call them.
- Do not ship an order that is not `ORDER_APPROVED`.
- A permission error means the token user lacks `FACILITY_UPDATE` or `ORDERMGR_UPDATE`. Report it.
