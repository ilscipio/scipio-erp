---
name: scipio-catalog-management
description: How to find a product, read its price and category, check its inventory, and create or classify a product through the Scipio MCP catalog tools. Use for any catalog task.
metadata:
  scipio-server: catalog
---

# Scipio catalog management

The `product` MCP server covers products, prices, categories, and inventory. It exposes dedicated
tools for lookup and falls back to `scipio_service` with action `search` for creation and change.

## Procedure

1. Call `scipio_whoami`. Confirm the user holds `CATALOG_VIEW` at least, and `CATALOG_UPDATE` for
   any create or change. Inventory checks also need `FACILITY_VIEW`.
2. Call `product` with action `find` to locate a product. Pass a product id, a name, an internal name, or a
   category id. Keep `limit` small.
3. Call `product` with action `get` with one `productId` to read the full record: type, prices, features, and
   category membership.
4. Call `product` with action `category_list` to read the category tree, or one category's members.
5. To create a product, set its price, or place it in a category, search with
   `scipio_service` with action `search`, then run the service through `scipio_service` with action `call` with `dryRun: true`
   first.

## Creating a product

Run `createProduct`
(`applications/product/src/com/ilscipio/scipio/product/service/Services.java`) with the product id,
the product type, and the internal name. Confirm the `productTypeId` with the user; it is not
optional and it does not change later without a separate service.

## Prices

Run `createProductPrice` to set one price for one product, one currency, and one price type (for
example `DEFAULT_PRICE`, `LIST_PRICE`). A product can carry more than one active price row at once;
state the price type and the currency to the user before you run the write.

## Categories

Run `createProductCategory` to add a new category. Run `addProductToCategory` to place an existing
product into an existing category. A product may belong to more than one category.

## Inventory

Inventory lives on `InventoryItem` records, scoped to a facility. Call `scipio_entity` with action `find` on
`InventoryItem` for a read, or search `scipio_service` with action `search` for the inventory services (words
such as `inventory available`) for a reserved or a promised quantity.

## Conventions

- Product ids, category ids, and facility ids are strings.
- Money fields are strings with two decimals, for example `"19.99"`.
- Dates are ISO-8601, for example `2026-01-31T10:00:00Z`.

## Safety

- Creating a product, a price, or a category change changes what the storefront shows. State the
  fields to the user before you run the write, unless the user already gave every field.
- Never create a duplicate product without checking `product` with action `find` first.
- A permission error means the token user lacks a security group. Report the missing permission.
