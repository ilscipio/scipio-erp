---
name: scipio-cms-authoring
description: How to find, create, version, publish and verify CMS pages, and how to write page templates (FreeMarker) and scripts (Groovy) at run time through the Scipio MCP cms tools. Use for any web page or template task.
metadata:
  scipio-server: cms
---

# Scipio CMS authoring

The `cms` MCP server (`/cms/mcp`) edits the live web site. A page has a template, one or more
content versions, and a primary path. A template is FreeMarker code and may carry Groovy scripts
that run on every render. Template and script writes are executable code: they need
`MCP_CODE_WRITE` and a confirmation from the user.

## Procedure: publish a page

1. Call `scipio_whoami`. Confirm `CMS_VIEW` for reads and `CMS_UPDATE` for page writes.
2. Call `cms_page` with action `find` with `webSiteId` (for example `cmsSite`), `nameLike` or `pathLike` to check
   whether the page exists. Call `cms_page` with action `get` to read its versions and mappings.
3. Pick a template. Call `cms_page` with action `find` on an existing page to see its `pageTemplateId`, or read a
   template with `cms_template` with action `get`. Create a new template only when no existing one fits.
4. Call `cms_page` with action `create` with `webSiteId`, `pageTemplateId`, `pageName` and `primaryPath`
   (for example `/about`). Keep the returned `pageId`.
5. Call `cms_page` with action `version_add` with the `pageId` and `content`: a JSON object string whose keys are
   the template's variables, for example `{"title":"About us","body":"<p>...</p>"}`. Keep the
   returned `versionId`.
6. Call `cms_page` with action `publish` with `pageId` and `versionId`. The site now serves this version.
7. Call `cms_page` with action `render` with the `pageId`. Read the HTML and confirm the content appears.
8. To change a published page, repeat steps 5 to 7. Old versions stay available.

## Procedure: write a template or a script

1. Confirm the user holds `MCP_CODE_WRITE`. Without it, stop and report the missing permission.
2. Tell the user what code you will write and why. Wait for the confirmation.
3. Call `cms_template` with action `create` with `templateName`, `webSiteId` and `templateBody` (FreeMarker).
   Read page attribute values as `${cmsContent.title!""}`, with a default for every value, so an
   empty content version still renders.
4. To attach server-side logic, call `cms_template` with action `script_update` with `pageTemplateId`, `templateName`,
   `scriptLang` = `groovy`, `standalone` = `N`, `inputPosition` and `templateBody` (Groovy). The
   script sets variables in `context` that the template reads.
5. To change an existing template, call `cms_template` with action `version_add` with the new body, then
   call `cms_template` with action `publish` with the returned `versionId`.
6. Render one page that uses the template with `cms_page` with action `render`. Fix errors before you continue.

## Other tools

- `cms_page` action `update`: rename a page or change its description or path.
- `cms_page` actions `unpublish` and `delete`: take a page down or remove it. Both need confirmation.
- `cms_template` action `script_get`: read a script body before you change it.
- `cms_site` action `asset_upsert`: reusable FreeMarker fragments (needs `MCP_CODE_WRITE`).
- `cms_site` action `menu_upsert`: menus as JSON.
- Media upload and view mappings: use `scipio_service` with action `search` with `cms media` or `cms mapping`.

## Conventions

- `content` in `cms_page` action `version_add` is a string that contains JSON, not a nested object.
- Paths start with `/` and are unique per web site.
- The content JSON keys are the attribute names the template reads through `cmsContent`; they must match exactly.

## Safety

- Never write Groovy or FreeMarker that reads files, runs commands or calls the network unless the
  user asked for exactly that and holds `MCP_CODE_WRITE`.
- Keep the old version. Do not delete a page to "replace" it; add a version and publish it.
- HTML in `content` is served as-is. Do not include scripts or content from untrusted sources.
