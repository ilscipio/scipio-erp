---
name: scipio-accounting
description: How to find and read invoices and payments, invoice an order, record and apply a payment, and change an invoice status through the Scipio MCP accounting tools. Use for any billing or payment task.
metadata:
  scipio-server: accounting
---

# Scipio accounting

The `accounting` MCP server (`/accounting/mcp`) covers invoices, payments and their application.
General ledger, billing accounts and financial accounts are reachable through the service catalog.

## Procedure: invoice an order

1. Call `scipio_whoami`. Confirm `ACCOUNTING_VIEW` for reads and `ACCOUNTING_UPDATE` for writes.
2. Call `invoice` with action `find` with `partyId` or a date range to check that the order has no invoice yet.
3. Call `invoice` with action `create_from_order` with the `orderId`. Keep the returned `invoiceId`. The order
   must be approved.
4. Call `invoice` with action `get` with the `invoiceId`. Read `items`, `total` and `outstanding`.
5. Call `invoice` with action `set_status` with `statusId` = `INVOICE_READY` when the invoice is complete. Confirm
   with the user first.

## Procedure: record and apply a payment

1. Call `invoice` with action `get` to read the `outstanding` amount and the `currencyUomId`.
2. Call `payment` with action `create` with `paymentTypeId` (for example `CUSTOMER_PAYMENT`),
   `paymentMethodTypeId` (for example `EXT_OFFLINE`), `partyIdFrom` (the customer), `partyIdTo`
   (the company), `amount`, `currencyUomId` and `statusId` = `PMNT_RECEIVED`. Keep the
   `paymentId`.
3. Call `payment` with action `apply` with `paymentId`, `invoiceId` and `amountApplied`.
4. Call `invoice` with action `get` again. `outstanding` must have dropped by the applied amount. An invoice that
   reaches zero moves to `INVOICE_PAID` on its own.

## Reads

- `invoice` action `find`: by `invoiceId`, `invoiceTypeId`, `statusId`, `partyId` (bill-to or bill-from) or
  invoice date range. Each row carries `total` and `outstanding`.
- `payment` action `find`: by `paymentId`, `paymentTypeId`, `statusId`, `partyId` or effective date range.
- Resource `scipio://invoice/{invoiceId}`: one invoice as JSON.

## Status ids

- Invoice: `INVOICE_IN_PROCESS`, `INVOICE_READY`, `INVOICE_PAID`, `INVOICE_CANCELLED`,
  `INVOICE_WRITEOFF`.
- Payment: `PMNT_NOT_PAID`, `PMNT_RECEIVED`, `PMNT_SENT`, `PMNT_CONFIRMED`, `PMNT_CANCELLED`.

## Conventions

- Money is a string with two decimals, for example `"120.00"`. Never round on your own.
- `partyIdFrom` on a sales invoice is the company; `partyId` is the customer.
- A cancelled invoice does not resume. Create a new one.

## Safety

- `invoice` action `set_status`, `payment` action `create` and `payment` action `apply` change financial records. State the
  invoice, the amount and the status to the user before you call them.
- Never apply more than the `outstanding` amount.
- A permission error means the token user lacks `ACCOUNTING_UPDATE`. Report it; do not retry through
  entity tools.
