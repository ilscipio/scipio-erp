---
name: scipio-setup
description: How to read the setup state of a Scipio installation, inspect a product store with its web sites, shipping and payment settings, and update store settings through the Scipio MCP setup tools. Use for configuration questions and first-time setup.
metadata:
  scipio-server: setup
---

# Scipio setup

The `setup` MCP server (`/setup/mcp`) shows how the installation is configured: companies, product
stores, web sites, shipping methods and payment settings. Most writes go through catalog and party
services; this server curates the reads and the store update.

## Procedure: review the configuration

1. Call `scipio_whoami`. Confirm `SETUP_VIEW` for reads and `CATALOG_UPDATE` for `store_update`.
2. Call `setup` with action `status`. Read `companies` (parties with accounting preferences) and
   `productStores` with their `webSiteIds`, `shipmentMethods` and `paymentSettings` counts.
3. Call `setup` with action `store_get` with a `productStoreId` to read one store in full: web sites, shipment methods,
   payment settings, catalogs and facilities.
4. Report gaps: a store without a web site, without a shipment method or without a payment setting
   cannot sell.

## Procedure: change store settings

1. Call `setup` with action `store_get` and read the current values.
2. Call `setup` with action `store_update` with `productStoreId` and only the fields to change, for example
   `storeName`, `defaultCurrencyUomId`, `defaultLocaleString`, `requireInventory`,
   `checkInventory`. Confirm with the user first.
3. Call `setup` with action `store_get` again to verify.

## Other setup work

- New store, web site, company or tax authority: call `scipio_service` with action `search` with the words
  `create product store`, `create web site`, `create party group`, `setup tax authority`, then
  follow the `scipio-service-explorer` skill.
- Shipping methods and payment settings: call `scipio_service` with action `search` with `store shipment method` or
  `store payment setting`.

## Conventions

- A product store id is a string such as `ScipioShop`.
- Currency is an ISO code such as `EUR` or `USD`.
- Locale is a language tag such as `en_US` or `de`.

## Safety

- Store settings affect every order. State each field and its new value before `setup` with action `store_update`.
- Do not change the pay-to party or the currency of a store with existing orders without an
  explicit user instruction.
- A permission error means the token user lacks `CATALOG_UPDATE` or `SETUP_VIEW`. Report it.
