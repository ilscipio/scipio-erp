/*
 * Scipio Commerce
 * Copyright (C) Ilscipio GmbH
 *
 * This file is part of Scipio Commerce. Scipio Commerce is free software: you
 * can redistribute it and modify it under the terms of the GNU Affero General
 * Public License, version 3, as published by the Free Software Foundation.
 * Scipio Commerce is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License
 * for more details. You should have received a copy of the license with this
 * work (file LICENSE). If not, see <https://www.gnu.org/licenses/agpl-3.0.html>.
 * A commercial license is available from Ilscipio GmbH.
 *
 * SPDX-License-Identifier: AGPL-3.0-only
 */
package com.ilscipio.scipio.cms.mcp;

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import org.ofbiz.base.component.ComponentConfig;
import org.ofbiz.base.util.HttpClient;
import org.ofbiz.base.util.SSLUtil;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericEntityException;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.entity.condition.EntityCondition;
import org.ofbiz.entity.condition.EntityOperator;
import org.ofbiz.entity.util.EntityQuery;
import org.ofbiz.webapp.WebAppUtil;

import com.ilscipio.scipio.mcp.catalog.ResultConverter;
import com.ilscipio.scipio.mcp.def.McpParam;
import com.ilscipio.scipio.mcp.def.McpResource;
import com.ilscipio.scipio.mcp.def.McpServer;
import com.ilscipio.scipio.mcp.def.McpServiceTool;
import com.ilscipio.scipio.mcp.def.McpTool;
import com.ilscipio.scipio.mcp.def.McpTopic;
import com.ilscipio.scipio.mcp.protocol.JsonRpc;
import com.ilscipio.scipio.mcp.registry.McpCallContext;
import com.ilscipio.scipio.mcp.registry.McpToolException;
import com.ilscipio.scipio.mcp.skill.AgentInstallInfo;

/**
 * SCIPIO: 4.0.0: MCP server profile for the cms component: pages, page templates, scripts, assets, menus and media.
 *
 * <p>Page content writes need {@code CMS_UPDATE}. Anything that writes executable code (a FreeMarker template body,
 * a Groovy script, an asset template) also needs {@code MCP_CODE_WRITE} and asks the client for confirmation:
 * such a write is remote code execution by design.</p>
 */
@McpServer(name = "cms", title = "Scipio CMS", component = "cms",
        description = "CMS: pages, templates, scripts, menus and media for web sites.",
        featuredServices = {"cmsCreatePage", "cmsUpdatePageInfo", "cmsGetPages", "cmsAddPageVersion", "cmsActivatePageVersion",
                "cmsCreatePageTemplate", "cmsUploadMediaFile", "cmsGetMediaFiles", "cmsCreateUpdateMenu", "cmsGetMenus",
                "cmsCreateUpdateViewMapping", "cmsCopyPage"},
        entities = {"CmsPage", "CmsPageVersion", "CmsPageTemplate", "CmsPageTemplateVersion", "CmsAssetTemplate",
                "CmsMenu", "CmsViewMapping", "CmsPageAuthorization", "CmsProcessMapping", "CmsScriptTemplate",
                "CmsPageTemplateScriptAssoc", "CmsMediaFile"},
        serviceTools = {
            @McpServiceTool(service = "cmsCopyPage",
                    topic = "cms_page",
                    name = "copy",
                    description = "Copy a page to a new primary path.",
                    readOnly = false,
                    destructive = "false",
                    order = 31),
            @McpServiceTool(service = "cmsCreateUpdateViewMapping",
                    topic = "cms_site",
                    name = "view_mapping_set",
                    description = "Create or update a view-name mapping for a page.",
                    readOnly = false,
                    destructive = "false",
                    order = 90),
            @McpServiceTool(service = "cmsGetMediaFiles",
                    topic = "cms_site",
                    name = "media_find",
                    description = "Find media files by web site or name.",
                    readOnly = true,
                    destructive = "false",
                    order = 67),
            @McpServiceTool(service = "cmsUploadMediaFile",
                    topic = "cms_site",
                    name = "media_upload",
                    description = "Upload a media file to a web site.",
                    readOnly = false,
                    destructive = "false",
                    order = 68),
            @McpServiceTool(service = "cmsCreatePage", topic = "cms_page", name = "create",
                    description = "Create a page from a template with a primary path.", readOnly = false, destructive = "false", order = 30,
                    exclude = {"txTimeout", "path", "primaryTargetPath", "primaryPathFromContextRoot"}),
            @McpServiceTool(service = "cmsUpdatePageInfo", topic = "cms_page", name = "update",
                    description = "Update page name, description or primary path.", readOnly = false, order = 35,
                    exclude = {"txTimeout"}),
            @McpServiceTool(service = "cmsAddPageVersion", topic = "cms_page", name = "version_add",
                    description = "Add a new content version to a page.", readOnly = false, destructive = "false", order = 40, exclude = {"request"}),
            @McpServiceTool(service = "cmsActivatePageVersion", topic = "cms_page", name = "publish",
                    description = "Publish one content version of a page.", readOnly = false, requiresConfirmation = true, order = 45, exclude = {"request"}),
            @McpServiceTool(service = "cmsUnpublishPage", topic = "cms_page", name = "unpublish",
                    description = "Unpublish a page; the site stops serving it.",
                    readOnly = false, destructive = "true", requiresConfirmation = true, order = 46, exclude = {"request"}),
            @McpServiceTool(service = "cmsDeletePage", topic = "cms_page", name = "delete",
                    description = "Delete a page and its versions.", readOnly = false, destructive = "true", requiresConfirmation = true, order = 47),
            @McpServiceTool(service = "cmsCreatePageTemplate", topic = "cms_template", name = "create",
                    description = "Create a page template with a FreeMarker body.", readOnly = false, destructive = "false", requiresConfirmation = true, permission = "MCP_CODE_WRITE", order = 50,
                    exclude = {"txTimeout"}),
            @McpServiceTool(service = "cmsAddPageTemplateVersion", topic = "cms_template", name = "version_add",
                    description = "Add a new FreeMarker body version to a template.",
                    readOnly = false, destructive = "false", requiresConfirmation = true, permission = "MCP_CODE_WRITE", order = 55),
            @McpServiceTool(service = "cmsActivatePageTemplateVersion", topic = "cms_template", name = "publish",
                    description = "Activate one version of a page template.", readOnly = false, requiresConfirmation = true, order = 56),
            @McpServiceTool(service = "cmsUpdatePageTemplateScript", topic = "cms_template", name = "script_update",
                    description = "Attach or update a Groovy script on a template.", readOnly = false, destructive = "true", requiresConfirmation = true, permission = "MCP_CODE_WRITE", order = 60),
            @McpServiceTool(service = "cmsCreateUpdateAsset", topic = "cms_site", name = "asset_upsert",
                    description = "Create or update a reusable FreeMarker asset template.",
                    readOnly = false, requiresConfirmation = true, permission = "MCP_CODE_WRITE", order = 70, exclude = {"txTimeout"}),
            @McpServiceTool(service = "cmsCreateUpdateMenu", topic = "cms_site", name = "menu_upsert",
                    description = "Create or update a CMS menu.", readOnly = false, order = 80),
            @McpServiceTool(service = "cmsGetCmsWebSites", topic = "cms_site", name = "websites",
                    description = "List web sites hooked into the CMS system.", readOnly = true, order = 15),
            @McpServiceTool(service = "cmsGetMenu", topic = "cms_site", name = "menu_get",
                    description = "Get one CMS menu's JSON definition.", readOnly = true, order = 37),
            @McpServiceTool(service = "cmsGetRedirects", topic = "cms_site", name = "redirects",
                    description = "List CMS URL redirects for a web site.", readOnly = true, order = 36),
            @McpServiceTool(service = "cmsExportDataAsXmlInline", topic = "cms_site", name = "export_xml",
                    description = "Export CMS data as inline XML text.",
                    readOnly = true, order = 38),
            @McpServiceTool(service = "cmsUpdatePageTemplateInfo", topic = "cms_template", name = "update",
                    description = "Update a page template's name, description or web site.",
                    readOnly = false, order = 51, exclude = {"txTimeout"}),
            @McpServiceTool(service = "cmsCreateUpdateScriptTemplate", topic = "cms_template", name = "script_create",
                    description = "Create or update a script template.",
                    readOnly = false, destructive = "false", requiresConfirmation = true, permission = "MCP_CODE_WRITE", order = 61),
            @McpServiceTool(service = "cmsCreateUpdateAttribute", topic = "cms_page", name = "attribute_set",
                    description = "Create or update a custom attribute on a template.",
                    readOnly = false, order = 63, exclude = {"request"}),
            @McpServiceTool(service = "cmsAddRemovePageViewMappings", topic = "cms_page", name = "view_mappings_set",
                    description = "Add or remove view-name mappings on a page.",
                    readOnly = false, order = 64),
            @McpServiceTool(service = "cmsCreateUpdateAssetAssoc", topic = "cms_site", name = "asset_assoc_set",
                    description = "Attach or update an asset template on a page template.",
                    readOnly = false, order = 65),
            @McpServiceTool(service = "cmsUpdateMediaFile", topic = "cms_site", name = "media_update",
                    description = "Update a media file's metadata.",
                    readOnly = false, order = 66),
            @McpServiceTool(service = "cmsDeleteMediaFile", topic = "cms_site", name = "media_delete",
                    description = "Delete a media file.",
                    readOnly = false, destructive = "true", requiresConfirmation = true, order = 71),
            @McpServiceTool(service = "cmsRebuildMediaVariants", topic = "cms_site", name = "media_variants_rebuild",
                    description = "Recreate auto-resized image variants for media files.",
                    readOnly = false, destructive = "true", requiresConfirmation = true, order = 72),
            @McpServiceTool(service = "cmsImportXmlData", topic = "cms_site", name = "import_xml",
                    description = "Import CMS data from inline XML text.",
                    readOnly = false, destructive = "true", requiresConfirmation = true, permission = "MCP_CODE_WRITE", order = 85,
                    exclude = {"uploadedFile", "_uploadedFile_fileName", "_uploadedFile_contentType", "txTimeout"})
        },
        topics = {
            @McpTopic(name = "cms_page", title = "Pages", order = 10, featured = true,
                    description = "Pages: find, read, render, create, publish, version, delete."),
            @McpTopic(name = "cms_template", title = "Templates", order = 20, featured = true,
                    description = "Page templates: get, create, publish, version, attach scripts."),
            @McpTopic(name = "cms_site", title = "Web sites and media", order = 30,
                    description = "Web sites: menus, redirects, media, view mappings, import, export.")
        })
public final class CmsMcp {

    private CmsMcp() {}

    @McpTool(topic = "cms_page", name = "find", description = "Find CMS pages by web site, name or path.", readOnly = true, order = 10)
    public static Object findPages(McpCallContext ctx,
            @McpParam(name = "webSiteId", description = "e.g. cmsSite", required = false) String webSiteId,
            @McpParam(name = "nameLike", description = "Partial page name match", required = false) String nameLike,
            @McpParam(name = "pathLike", description = "Partial primary path match, e.g. /about", required = false) String pathLike,
            @McpParam(name = "limit", required = false) Integer limit) throws McpToolException {
        Delegator delegator = ctx.getDelegator();
        try {
            List<EntityCondition> conds = new ArrayList<>();
            if (webSiteId != null) conds.add(EntityCondition.makeCondition("webSiteId", webSiteId));
            if (nameLike != null) conds.add(EntityCondition.makeCondition("pageName", EntityOperator.LIKE, "%" + nameLike + "%"));
            if (pathLike != null) {
                List<String> ids = new ArrayList<>();
                for (GenericValue m : EntityQuery.use(delegator).from("CmsProcessMapping")
                        .where(EntityCondition.makeCondition("sourcePath", EntityOperator.LIKE, "%" + pathLike + "%")).queryList()) {
                    String pid = m.getString("primaryForPageId") != null ? m.getString("primaryForPageId") : m.getString("pageId");
                    if (pid != null) ids.add(pid);
                }
                if (ids.isEmpty()) return new ArrayList<>();
                conds.add(EntityCondition.makeCondition("pageId", EntityOperator.IN, ids));
            }
            List<Map<String, Object>> out = new ArrayList<>();
            for (GenericValue page : EntityQuery.use(delegator).from("CmsPage").where(conds).orderBy("pageName").maxRows(ctx.limit(limit)).queryList()) {
                out.add(pageRow(delegator, page));
            }
            return out;
        } catch (GenericEntityException e) {
            throw new McpToolException("Page search failed: " + e.getMessage());
        }
    }

    @McpTool(topic = "cms_page", name = "get", description = "Get a page with path, template, versions and mappings.", readOnly = true, order = 20)
    public static Object getPage(McpCallContext ctx,
            @McpParam(name = "pageId", required = true) String pageId) throws McpToolException {
        Delegator delegator = ctx.getDelegator();
        try {
            GenericValue page = EntityQuery.use(delegator).from("CmsPage").where("pageId", pageId).queryOne();
            if (page == null) throw new McpToolException("Page not found: " + pageId);
            Map<String, Object> out = pageRow(delegator, page);
            List<Map<String, Object>> versions = new ArrayList<>();
            for (GenericValue v : EntityQuery.use(delegator).from("CmsPageVersion").where("pageId", pageId).orderBy("-createdStamp").queryList()) {
                Map<String, Object> row = new LinkedHashMap<>();
                row.put("versionId", v.getString("versionId"));
                row.put("createdBy", v.getString("createdBy"));
                row.put("createdStamp", ResultConverter.toJson(v.getTimestamp("createdStamp")));
                row.put("content", textOfContent(delegator, v.getString("contentId")));
                versions.add(row);
            }
            out.put("versions", versions);
            if (delegator.getModelEntity("CmsPageVersionState") != null) {
                out.put("versionState", ResultConverter.toJson(EntityQuery.use(delegator).from("CmsPageVersionState").where("pageId", pageId).queryList()));
            }
            out.put("mappings", ResultConverter.toJson(EntityQuery.use(delegator).from("CmsProcessMapping").where("pageId", pageId).queryList()));
            return out;
        } catch (GenericEntityException e) {
            throw new McpToolException("Failed to load page " + pageId + ": " + e.getMessage());
        }
    }

    @McpTool(topic = "cms_page", name = "render", description = "Render a published page and return its URL and HTML.", readOnly = true, order = 25)
    public static Object renderPage(McpCallContext ctx,
            @McpParam(name = "pageId", description = "Page id; its primary path is rendered", required = false) String pageId,
            @McpParam(name = "path", description = "Path under the web site, e.g. /about (alternative to pageId)", required = false) String path,
            @McpParam(name = "webSiteId", description = "Web site id; default: the page's web site", required = false) String webSiteId,
            @McpParam(name = "maxChars", description = "Max HTML characters to return, default 20000", required = false) Integer maxChars) throws McpToolException {
        Delegator delegator = ctx.getDelegator();
        try {
            if (UtilValidate.isEmpty(path)) {
                if (UtilValidate.isEmpty(pageId)) throw new McpToolException("pageId or path is required");
                GenericValue page = EntityQuery.use(delegator).from("CmsPage").where("pageId", pageId).queryOne();
                if (page == null) throw new McpToolException("Page not found: " + pageId);
                if (UtilValidate.isEmpty(webSiteId)) webSiteId = page.getString("webSiteId");
                GenericValue mapping = EntityQuery.use(delegator).from("CmsProcessMapping").where("primaryForPageId", pageId).queryFirst();
                if (mapping == null) throw new McpToolException("Page " + pageId + " has no primary path mapping");
                path = mapping.getString("sourcePath");
            }
            if (UtilValidate.isEmpty(webSiteId)) throw new McpToolException("webSiteId is required with path");
            ComponentConfig.WebappInfo wi;
            try {
                wi = WebAppUtil.getWebappInfoFromWebsiteId(webSiteId);
            } catch (Exception e) {
                throw new McpToolException("Could not resolve the webapp of web site " + webSiteId + ": " + e.getMessage());
            }
            if (wi == null) throw new McpToolException("No webapp serves web site " + webSiteId);
            String url = AgentInstallInfo.baseUrl(ctx.getRequest().getHttpRequest()) + wi.getContextRoot() + (path.startsWith("/") ? path : "/" + path);
            HttpClient client = new HttpClient(url);
            client.setAllowUntrusted(true);
            client.setHostVerificationLevel(SSLUtil.HOSTCERT_NO_CHECK);
            String html;
            int status;
            try {
                html = client.get();
                status = client.getResponseCode();
            } catch (Exception e) {
                throw new McpToolException("Render request to " + url + " failed: " + e.getMessage());
            }
            int max = maxChars != null && maxChars > 0 ? maxChars : 20000;
            Map<String, Object> out = new LinkedHashMap<>();
            out.put("url", url);
            out.put("status", status);
            out.put("length", html != null ? html.length() : 0);
            out.put("html", html != null && html.length() > max ? html.substring(0, max) + "\n...[truncated]" : html);
            return out;
        } catch (GenericEntityException e) {
            throw new McpToolException("Render failed: " + e.getMessage());
        }
    }

    @McpTool(topic = "cms_template", name = "get", description = "Get a page template with its active body and scripts.", readOnly = true, order = 48)
    public static Object getTemplate(McpCallContext ctx,
            @McpParam(name = "pageTemplateId", required = true) String pageTemplateId) throws McpToolException {
        Delegator delegator = ctx.getDelegator();
        try {
            GenericValue tpl = EntityQuery.use(delegator).from("CmsPageTemplate").where("pageTemplateId", pageTemplateId).queryOne();
            if (tpl == null) throw new McpToolException("Page template not found: " + pageTemplateId);
            Map<String, Object> out = new LinkedHashMap<>();
            out.put("template", ResultConverter.toJson(tpl));
            out.put("activeBody", textOfContent(delegator, tpl.getString("activeContentId")));
            List<Map<String, Object>> versions = new ArrayList<>();
            for (GenericValue v : EntityQuery.use(delegator).from("CmsPageTemplateVersion").where("pageTemplateId", pageTemplateId).orderBy("-createdStamp").queryList()) {
                Map<String, Object> row = new LinkedHashMap<>();
                row.put("versionId", v.getString("versionId"));
                row.put("contentId", v.getString("contentId"));
                row.put("active", v.getString("contentId") != null && v.getString("contentId").equals(tpl.getString("activeContentId")));
                row.put("createdBy", v.getString("createdBy"));
                row.put("createdStamp", ResultConverter.toJson(v.getTimestamp("createdStamp")));
                versions.add(row);
            }
            out.put("versions", versions);
            List<Map<String, Object>> scripts = new ArrayList<>();
            for (GenericValue assoc : EntityQuery.use(delegator).from("CmsPageTemplateScriptAssoc").where("pageTemplateId", pageTemplateId).orderBy("inputPosition").queryList()) {
                GenericValue script = EntityQuery.use(delegator).from("CmsScriptTemplate").where("scriptTemplateId", assoc.getString("scriptTemplateId")).queryOne();
                Map<String, Object> row = new LinkedHashMap<>();
                row.put("scriptTemplateId", assoc.getString("scriptTemplateId"));
                row.put("inputPosition", ResultConverter.toJson(assoc.get("inputPosition")));
                row.put("invokeName", assoc.getString("invokeName"));
                if (script != null) {
                    row.put("templateName", script.getString("templateName"));
                    row.put("scriptLang", script.getString("scriptLang"));
                    row.put("standalone", script.getString("standalone"));
                    row.put("body", textOfContent(delegator, script.getString("contentId")));
                }
                scripts.add(row);
            }
            out.put("scripts", scripts);
            return out;
        } catch (GenericEntityException e) {
            throw new McpToolException("Failed to load page template " + pageTemplateId + ": " + e.getMessage());
        }
    }

    @McpTool(topic = "cms_template", name = "script_get", description = "Get a script template with its body.", readOnly = true, order = 58)
    public static Object getScript(McpCallContext ctx,
            @McpParam(name = "scriptTemplateId", required = true) String scriptTemplateId) throws McpToolException {
        Delegator delegator = ctx.getDelegator();
        try {
            GenericValue script = EntityQuery.use(delegator).from("CmsScriptTemplate").where("scriptTemplateId", scriptTemplateId).queryOne();
            if (script == null) throw new McpToolException("Script template not found: " + scriptTemplateId);
            Map<String, Object> out = ResultConverter.toJsonMap(script);
            out.put("body", textOfContent(delegator, script.getString("contentId")));
            return out;
        } catch (GenericEntityException e) {
            throw new McpToolException("Failed to load script " + scriptTemplateId + ": " + e.getMessage());
        }
    }

    @McpResource(uri = "scipio://cms/page/{pageId}", name = "CMS page", description = "One CMS page as JSON: info, path, versions with content, mappings.",
            mimeType = "application/json")
    public static String pageResource(McpCallContext ctx, Map<String, String> uriParams) throws McpToolException {
        return JsonRpc.writePretty(getPage(ctx, uriParams.get("pageId")));
    }

    private static Map<String, Object> pageRow(Delegator delegator, GenericValue page) throws GenericEntityException {
        Map<String, Object> row = new LinkedHashMap<>();
        row.put("pageId", page.getString("pageId"));
        row.put("pageName", page.getString("pageName"));
        row.put("webSiteId", page.getString("webSiteId"));
        row.put("pageTemplateId", page.getString("pageTemplateId"));
        row.put("description", page.getString("description"));
        GenericValue mapping = EntityQuery.use(delegator).from("CmsProcessMapping").where("primaryForPageId", page.getString("pageId")).queryFirst();
        row.put("primaryPath", mapping != null ? mapping.getString("sourcePath") : null);
        row.put("active", mapping != null ? mapping.getString("active") : null);
        return row;
    }

    /** Text body behind a Content record (Content -> DataResource -> ElectronicText), or null. */
    private static String textOfContent(Delegator delegator, String contentId) throws GenericEntityException {
        if (UtilValidate.isEmpty(contentId)) return null;
        GenericValue content = EntityQuery.use(delegator).from("Content").where("contentId", contentId).queryOne();
        if (content == null || content.getString("dataResourceId") == null) return null;
        GenericValue text = EntityQuery.use(delegator).from("ElectronicText").where("dataResourceId", content.getString("dataResourceId")).queryOne();
        return text != null ? text.getString("textData") : null;
    }
}
