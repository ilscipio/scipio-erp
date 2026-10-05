---
name: scipio-search-index
description: How to run a product keyword search, check the search index status and rebuild the index through the Scipio MCP solr tools. Use when search results look stale or a product does not appear in search.
metadata:
  scipio-server: solr
---

# Scipio search index (Solr)

The `solr` MCP server (`/solr/mcp`) covers the product search index that the storefront and the
catalog search use. Product data itself lives in the `catalog` server.

## Procedure: check why a product is missing from search

1. Call `scipio_whoami`. Confirm `SOLRADM_VIEW` for reads and `SOLRADM_ADMIN` for a rebuild.
2. Call `solr` with action `status`. When `ready` is false, report it; the index is not reachable.
3. Call `solr` with action `search` with the product name as `query`. Add `queryFilter` such as
   `productStoreId:ScipioShop` to limit the answer to one store.
4. When the product exists in the catalog (`product` with action `get` on the `catalog` server) but not in the
   search result, the index is stale. Continue with the rebuild procedure.

## Procedure: rebuild the index

1. State to the user that a rebuild reads the whole catalog and can take minutes.
2. Call `solr` with action `reindex`. Pass `onlyIfDirty` = `true` to rebuild only when the system marked the
   index dirty.
3. Call `solr` with action `status` and then `solr` with action `search` again to confirm the product appears.

## Reads

- `solr` action `search`: keyword search; `sortBy` and `limit` control the result.
- Index configuration and data status services: call `scipio_service` with action `search` with `solr`.

## Conventions

- `query` uses Solr syntax; a plain word list works for most cases.
- `queryFilter` uses `field:value` syntax.
- Results carry the document fields of the index, not the full product record.

## Safety

- `solr` with action `reindex` needs confirmation and `SOLRADM_ADMIN`. Do not run it in a loop.
- A single missing product is usually fixed by a normal product update, which reindexes that
  product; prefer that over a full rebuild.
- A permission error means the token user lacks a `SOLRADM` permission. Report it.
