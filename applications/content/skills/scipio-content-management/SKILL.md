---
name: scipio-content-management
description: How to find and read generic content records, and create or replace text content through the Scipio MCP content tools. Use for documents, text blocks and data resources that are not CMS web pages.
metadata:
  scipio-server: content
---

# Scipio content management

The `content` MCP server (`/content/mcp`) manages generic content records: a `Content` row that
points at a `DataResource`, which holds the text or file. Web pages of the storefront belong to the
`cms` server, not here.

## Procedure: create a text block

1. Call `scipio_whoami`. Confirm `CONTENTMGR_VIEW` for reads and `CONTENTMGR_UPDATE` for writes.
2. Call `content` with action `find` with `nameLike` or `contentTypeId` to check for an existing record.
3. Call `content` with action `create_text` with `contentName`, `textData`, `mimeTypeId` (`text/plain` or
   `text/html`), `contentTypeId` (default `DOCUMENT`), `description` and `localeString`. Keep the
   returned `contentId` and `dataResourceId`.
4. Call `content` with action `get` with the `contentId` and confirm `textData` matches.

## Procedure: replace text

1. Call `content` with action `get` to read the current `textData` and confirm the record is electronic text.
2. Call `content` with action `update_text` with `contentId` and the new `textData`. The whole body is replaced.
3. Call `content` with action `get` again to verify.

## Reads

- `content` action `find`: by `contentId`, `contentTypeId`, `statusId` or `nameLike`.
- `content` action `get`: the content row, its data resource, `textData`, roles and associations.
- Associations link content into trees (`ContentAssoc`); create them through the service catalog
  (call `scipio_service` with action `search` with `content assoc`).

## Status ids

- `CTNT_INITIAL_DRAFT`, `CTNT_IN_PROGRESS`, `CTNT_PUBLISHED`, `CTNT_DEACTIVATED`.

## Conventions

- `textData` is stored as given. HTML is not sanitized; write only what the user approved.
- `localeString` is a language tag such as `en` or `de`.
- Ids are strings.

## Safety

- `content` action `update_text` overwrites the full body. Read it first and keep a copy in your reply when
  the change is large.
- Do not deactivate or delete content without an explicit user instruction.
- A permission error means the token user lacks `CONTENTMGR_UPDATE`. Report it.
