---
name: scipio-manufacturing
description: How to explode a bill of material, find and inspect production runs, create a production run and move it through its statuses, declare shop floor work, read work center load, run MRP and review its proposals, and read product standard costs through the Scipio MCP manufacturing tools. Use for any production, planning, costing or BOM task.
metadata:
  scipio-server: manufacturing
---

# Scipio manufacturing

The `manufacturing` MCP server (`/manufacturing/mcp`) covers bills of material (BOM), routings,
work centers, production runs, shop floor declarations, MRP and standard costs. A production run
is a work effort of type `PROD_ORDER_HEADER` with task children (`PROD_ORDER_TASK`).

## Procedure: plan and start a production run

1. Call `scipio_whoami`. Confirm `MANUFACTURING_VIEW` for reads and `MANUFACTURING_UPDATE` for writes.
2. Call `bom` with action `get` with the `productId` and the `quantity` to build. Read `components`.
3. Check parts availability with `inventory` with action `get` on the `facility` server, or read `shortages` from
   `shop_floor` with action `dashboard`.
4. Call `production_run` with action `create` with `productId`, `pRQuantity`, `startDate`, `facilityId` and an
   optional `routingId` and `workEffortName`. Keep the returned `productionRunId`.
   For a sales order call `production_run` with action `create_for_order` with `orderId` instead.
5. Call `production_run` with action `get` with the `productionRunId`. Read `tasks` and `goods`.
6. Call `production_run` with action `set_status` with `productionRunId` and `statusId`:
   `PRUN_SCHEDULED`, then `PRUN_DOC_PRINTED`, then `PRUN_RUNNING`. Confirm each step with the user.
7. When production is finished, set `PRUN_COMPLETED` and then `PRUN_CLOSED`. Finished goods are
   received into the facility by the completion step.

## Procedure: declare shop floor work

1. Call `shop_floor` with action `tasks` with the `fixedAssetId` of the work center. Each task carries
   `canStart`, `canDeclare`, `canComplete` and its `components` with needed and issued quantities.
2. Start a task with `production_run` with action `task_set_status` (`statusId` = `PRUN_RUNNING`).
3. Declare with `production_run` with action `declare`: `quantityProduced`, `quantityRejected` with a
   `reasonEnumId`, `setupMinutes`, `taskMinutes`, `comments`, and `backflush` = true to issue the
   planned components in proportion to the produced quantity.
4. Complete the task with `production_run` with action `task_set_status` (`statusId` = `PRUN_COMPLETED`).
5. Read rejects with `production_run` with action `rejects`.

## Procedure: run MRP and review proposals

1. Call `mrp` with action `run` with `mrpName` and `facilityId`. Keep the returned `mrpId`.
2. Call `mrp` with action `find` to read the header: `statusId`, `eventCount`, `proposedProductionRuns`,
   `proposedPurchases`, `errorCount`.
3. Call `mrp` with action `proposals` for the facility. An `INTERNAL_REQUIREMENT` is a proposed production run,
   a `PRODUCT_REQUIREMENT` is a proposed purchase.
4. To accept a proposal call `purchase_order` with action `requirement_approve` on the `order` server with `requirementId` and
   `statusId` = `REQ_APPROVED`. An approved internal requirement creates its production run.

## Reads

- `shop_floor` action `dashboard`: run counts, late runs, running tasks, upcoming runs, shortages, MRP proposals,
  last MRP run, work center load for the next days.
- `shop_floor` action `work_center_load`: capacity and load minutes per work center and day, plus the tasks.
- `production_run` action `find`: by `productionRunId`, `currentStatusId`, `facilityId` or produced `productId`.
- `production_run` action `get`: header, tasks, produced and consumed goods.
- `bom` action `get`: components for a quantity. `bom` action `where_used`: every assembly that uses a product.
- `bom` action `cost_get`: standard cost split into material, labor, overhead and routing; pass
  `recalculate` = true to roll the cost up first.
- Routings, calendars, BOM edits: call `scipio_service` with action `search` with `routing`, `calendar` or `bom assoc`.

## Status ids

- Production run and task: `PRUN_CREATED`, `PRUN_SCHEDULED`, `PRUN_DOC_PRINTED`, `PRUN_RUNNING`,
  `PRUN_COMPLETED`, `PRUN_CLOSED`, `PRUN_CANCELLED`.
- MRP run: `MRP_RUNNING`, `MRP_FINISHED`, `MRP_FAILED`.
- Reject reasons: `PRUN_REJ_MATERIAL`, `PRUN_REJ_MACHINE`, `PRUN_REJ_OPERATOR`,
  `PRUN_REJ_INSPECTION`, `PRUN_REJ_OTHER`.

## Conventions

- Quantities are decimal strings. Minutes are integers; the tools convert them to milliseconds.
- `startDate`, `fromDate` and `thruDate` are ISO-8601.
- A production run id, a task id and a routing id are work effort ids (strings).
- Demo scenario: product `MF_BIKE` (city bike) with sub-assemblies `MF_WHEEL` and `MF_FRAME`,
  work centers `MF_WC_*`, facility `ScipioShopWarehouse`.

## Safety

- Each status change issues or consumes inventory. State the run, the current status and the target
  status before you call `production_run` with action `set_status` or `production_run` with action `task_set_status`.
- `production_run` with action `declare` with `backflush` moves inventory. Repeat the quantities to the user first.
- `mrp` with action `run` deletes the previous proposed requirements of the facility before it plans again.
- Do not cancel a running production run without an explicit user instruction.
- A permission error means the token user lacks `MANUFACTURING_UPDATE`. Report it.
