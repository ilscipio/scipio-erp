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
package com.ilscipio.scipio.content.mcp;

import java.nio.ByteBuffer;
import java.util.ArrayList;
import java.util.Base64;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;

import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericEntityException;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.entity.condition.EntityCondition;
import org.ofbiz.entity.condition.EntityOperator;
import org.ofbiz.entity.util.EntityQuery;

import com.ilscipio.scipio.mcp.catalog.ResultConverter;
import com.ilscipio.scipio.mcp.def.McpParam;
import com.ilscipio.scipio.mcp.def.McpServer;
import com.ilscipio.scipio.mcp.def.McpServiceTool;
import com.ilscipio.scipio.mcp.def.McpTool;
import com.ilscipio.scipio.mcp.def.McpTopic;
import com.ilscipio.scipio.mcp.registry.McpCallContext;
import com.ilscipio.scipio.mcp.registry.McpToolException;

/**
 * SCIPIO: 4.0.0: MCP server profile for the content component: generic content, data resources and associations.
 */
@McpServer(name = "content", title = "Scipio Content", component = "content",
        description = "Content: find, read, create, publish and organize content records.",
        featuredServices = {"createContent", "updateContent", "createContentAssoc", "createDataResource",
                "updateDataResource", "createContentApproval", "createElectronicText", "updateElectronicText"},
        entities = {"Content", "ContentAssoc", "ContentRole", "DataResource", "ElectronicText", "ContentType", "ContentPurpose"},
        serviceTools = {
            @McpServiceTool(service = "createContentAssoc", topic = "content", name = "assoc_set",
                    description = "Associate two content records as parent and child.", readOnly = false, destructive = "false", order = 45),
            @McpServiceTool(service = "createContentRole", topic = "content", name = "role_add",
                    description = "Add a party role to a content record.", readOnly = false, destructive = "false", order = 46),
            @McpServiceTool(service = "updateContent", topic = "content", name = "set_status",
                    description = "Change a content record's status.", readOnly = false, destructive = "true", requiresConfirmation = true, order = 75),
            @McpServiceTool(service = "updateContentApproval", topic = "content", name = "approval_update",
                    description = "Update a content approval record's status.", readOnly = false, destructive = "true", requiresConfirmation = true, order = 76),
            @McpServiceTool(service = "createWebSitePathAlias", topic = "content", name = "website_path_alias_set",
                    description = "Create a website path alias.", readOnly = false, destructive = "false", order = 47),
            @McpServiceTool(service = "createWebSiteContent", topic = "content", name = "website_content_add",
                    description = "Attach a content record to a website.", readOnly = false, destructive = "false", order = 48),
            @McpServiceTool(service = "createBlogEntry", topic = "content", name = "blog_entry_create",
                    description = "Create a blog entry.", readOnly = false, destructive = "false", order = 41),
            @McpServiceTool(service = "createSurvey", topic = "content", name = "survey_create",
                    description = "Create a survey.", readOnly = false, destructive = "false", order = 49),
            @McpServiceTool(service = "createSurveyQuestion", topic = "content", name = "survey_question_add",
                    description = "Add a question to a survey.", readOnly = false, destructive = "false", order = 50),
            @McpServiceTool(service = "createContentKeyword", topic = "content", name = "keyword_add",
                    description = "Add a search keyword to a content record.", readOnly = false, destructive = "false", order = 51),
            @McpServiceTool(service = "createDataResourceAndText", topic = "content", name = "data_resource_create",
                    description = "Create a data resource with an inline text body.", readOnly = false, destructive = "false", order = 42)
        },
        topics = {
            @McpTopic(name = "content", title = "Content", order = 10, featured = true,
                    description = "Content: find, read, create, publish, approve, tag, embed.")
        })
public final class ContentMcp {

    private ContentMcp() {}

    @McpTool(topic = "content", name = "find", description = "Find content records by id, type, status or name.", readOnly = true, order = 10)
    public static Object findContent(McpCallContext ctx,
            @McpParam(name = "contentId", required = false) String contentId,
            @McpParam(name = "contentTypeId", description = "e.g. DOCUMENT, TEMPLATE, PUBLISH_POINT", required = false) String contentTypeId,
            @McpParam(name = "statusId", description = "e.g. CTNT_IN_PROGRESS, CTNT_PUBLISHED", required = false) String statusId,
            @McpParam(name = "nameLike", description = "Partial content name match", required = false) String nameLike,
            @McpParam(name = "limit", required = false) Integer limit) throws McpToolException {
        Delegator delegator = ctx.getDelegator();
        try {
            List<EntityCondition> conds = new ArrayList<>();
            if (contentId != null) conds.add(EntityCondition.makeCondition("contentId", contentId));
            if (contentTypeId != null) conds.add(EntityCondition.makeCondition("contentTypeId", contentTypeId));
            if (statusId != null) conds.add(EntityCondition.makeCondition("statusId", statusId));
            if (nameLike != null) conds.add(EntityCondition.makeCondition("contentName", EntityOperator.LIKE, "%" + nameLike + "%"));
            List<GenericValue> rows = EntityQuery.use(delegator).from("Content").where(conds)
                    .orderBy("-createdStamp").maxRows(ctx.limit(limit)).queryList();
            List<Map<String, Object>> out = new ArrayList<>();
            for (GenericValue c : rows) {
                Map<String, Object> row = new LinkedHashMap<>();
                for (String f : new String[] {"contentId", "contentTypeId", "contentName", "description", "statusId", "mimeTypeId", "dataResourceId", "ownerContentId"}) {
                    row.put(f, c.getString(f));
                }
                out.add(row);
            }
            return out;
        } catch (GenericEntityException e) {
            throw new McpToolException("Content search failed: " + e.getMessage());
        }
    }

    @McpTool(topic = "content", name = "get", description = "Get a content record with its body, roles and associations.", readOnly = true, order = 20)
    public static Object getContent(McpCallContext ctx,
            @McpParam(name = "contentId", required = true) String contentId) throws McpToolException {
        Delegator delegator = ctx.getDelegator();
        try {
            GenericValue content = EntityQuery.use(delegator).from("Content").where("contentId", contentId).queryOne();
            if (content == null) throw new McpToolException("Content not found: " + contentId);
            Map<String, Object> out = new LinkedHashMap<>();
            out.put("content", ResultConverter.toJson(content));
            String drId = content.getString("dataResourceId");
            if (drId != null) {
                GenericValue dr = EntityQuery.use(delegator).from("DataResource").where("dataResourceId", drId).queryOne();
                out.put("dataResource", ResultConverter.toJson(dr));
                GenericValue text = EntityQuery.use(delegator).from("ElectronicText").where("dataResourceId", drId).queryOne();
                if (text != null) out.put("textData", text.getString("textData"));
            }
            out.put("roles", ResultConverter.toJson(EntityQuery.use(delegator).from("ContentRole").where("contentId", contentId).filterByDate().queryList()));
            out.put("associationsFrom", ResultConverter.toJson(EntityQuery.use(delegator).from("ContentAssoc").where("contentId", contentId).filterByDate().queryList()));
            out.put("associationsTo", ResultConverter.toJson(EntityQuery.use(delegator).from("ContentAssoc").where("contentIdTo", contentId).filterByDate().queryList()));
            return out;
        } catch (GenericEntityException e) {
            throw new McpToolException("Failed to load content " + contentId + ": " + e.getMessage());
        }
    }

    @McpTool(topic = "content", name = "create_text", description = "Create a text content record with a body.", readOnly = false, destructive = "false", order = 30)
    public static Object createTextContent(McpCallContext ctx,
            @McpParam(name = "contentName", required = true) String contentName,
            @McpParam(name = "textData", description = "The text or HTML body", required = true) String textData,
            @McpParam(name = "contentTypeId", description = "Default DOCUMENT", required = false) String contentTypeId,
            @McpParam(name = "mimeTypeId", description = "Default text/plain; use text/html for HTML", required = false) String mimeTypeId,
            @McpParam(name = "description", required = false) String description,
            @McpParam(name = "statusId", description = "Default CTNT_IN_PROGRESS", required = false) String statusId,
            @McpParam(name = "localeString", description = "e.g. en, de", required = false) String localeString) throws McpToolException {
        String mime = UtilValidate.isNotEmpty(mimeTypeId) ? mimeTypeId : "text/plain";
        Map<String, Object> dr = new LinkedHashMap<>();
        dr.put("dataResourceTypeId", "ELECTRONIC_TEXT");
        dr.put("dataResourceName", contentName);
        dr.put("mimeTypeId", mime);
        if (localeString != null) dr.put("localeString", localeString);
        String dataResourceId = (String) ctx.runService("createDataResource", dr).get("dataResourceId");
        Map<String, Object> et = new LinkedHashMap<>();
        et.put("dataResourceId", dataResourceId);
        et.put("textData", textData);
        ctx.runService("createElectronicText", et);
        Map<String, Object> c = new LinkedHashMap<>();
        c.put("contentTypeId", UtilValidate.isNotEmpty(contentTypeId) ? contentTypeId : "DOCUMENT");
        c.put("contentName", contentName);
        if (description != null) c.put("description", description);
        c.put("dataResourceId", dataResourceId);
        c.put("mimeTypeId", mime);
        c.put("statusId", UtilValidate.isNotEmpty(statusId) ? statusId : "CTNT_IN_PROGRESS");
        if (localeString != null) c.put("localeString", localeString);
        String contentId = (String) ctx.runService("createContent", c).get("contentId");
        Map<String, Object> out = new LinkedHashMap<>();
        out.put("contentId", contentId);
        out.put("dataResourceId", dataResourceId);
        return out;
    }

    @McpTool(topic = "content", name = "update_text", description = "Replace the text body of a content record.", readOnly = false, order = 40)
    public static Object updateTextContent(McpCallContext ctx,
            @McpParam(name = "contentId", required = true) String contentId,
            @McpParam(name = "textData", required = true) String textData) throws McpToolException {
        Delegator delegator = ctx.getDelegator();
        try {
            GenericValue content = EntityQuery.use(delegator).from("Content").where("contentId", contentId).queryOne();
            if (content == null) throw new McpToolException("Content not found: " + contentId);
            String drId = content.getString("dataResourceId");
            if (drId == null) throw new McpToolException("Content " + contentId + " has no data resource");
            Map<String, Object> et = new LinkedHashMap<>();
            et.put("dataResourceId", drId);
            et.put("textData", textData);
            ctx.runService("updateElectronicText", et);
            Map<String, Object> out = new LinkedHashMap<>();
            out.put("contentId", contentId);
            out.put("dataResourceId", drId);
            out.put("length", textData.length());
            return out;
        } catch (GenericEntityException e) {
            throw new McpToolException("Failed to update content " + contentId + ": " + e.getMessage());
        }
    }

    private static final long MAX_UPLOAD_BYTES = 20L * 1024 * 1024;

    @McpTool(topic = "content", name = "upload", description = "Upload a file as a content record.", readOnly = false, destructive = "false", order = 43)
    public static Object uploadContent(McpCallContext ctx,
            @McpParam(name = "fileName", required = true) String fileName,
            @McpParam(name = "base64", description = "Base64-encoded file content, max 20 MB decoded", required = true) String base64,
            @McpParam(name = "contentTypeId", description = "Default DOCUMENT", required = false) String contentTypeId,
            @McpParam(name = "mimeTypeId", description = "Derived from the fileName extension when omitted", required = false) String mimeTypeId,
            @McpParam(name = "contentName", required = false) String contentName,
            @McpParam(name = "dataResourceTypeId", description = "Default LOCAL_FILE", required = false) String dataResourceTypeId,
            @McpParam(name = "parentContentId", description = "Links the new content as SUB_CONTENT of this content", required = false) String parentContentId) throws McpToolException {
        byte[] bytes;
        try {
            bytes = Base64.getDecoder().decode(base64);
        } catch (IllegalArgumentException e) {
            throw new McpToolException("Invalid base64 content: " + e.getMessage());
        }
        if (bytes.length > MAX_UPLOAD_BYTES) {
            throw new McpToolException("File too large: " + bytes.length + " bytes (limit " + MAX_UPLOAD_BYTES + ")");
        }
        String mime = UtilValidate.isNotEmpty(mimeTypeId) ? mimeTypeId : mimeTypeFromFileName(fileName);
        String drTypeId = UtilValidate.isNotEmpty(dataResourceTypeId) ? dataResourceTypeId : "LOCAL_FILE";
        Map<String, Object> params = new LinkedHashMap<>();
        params.put("dataResourceTypeId", drTypeId);
        params.put("dataResourceName", UtilValidate.isNotEmpty(contentName) ? contentName : fileName);
        params.put("mimeTypeId", mime);
        params.put("contentTypeId", UtilValidate.isNotEmpty(contentTypeId) ? contentTypeId : "DOCUMENT");
        params.put("uploadedFile", ByteBuffer.wrap(bytes));
        params.put("_uploadedFile_fileName", fileName);
        params.put("_uploadedFile_contentType", mime);
        Map<String, Object> res = ctx.runService("createContentFromUploadedFile", params);
        String newContentId = (String) res.get("contentId");
        String newDataResourceId = (String) res.get("dataResourceId");
        if (UtilValidate.isNotEmpty(parentContentId) && UtilValidate.isNotEmpty(newContentId)) {
            Map<String, Object> assoc = new LinkedHashMap<>();
            assoc.put("contentId", parentContentId);
            assoc.put("contentIdTo", newContentId);
            assoc.put("contentAssocTypeId", "SUB_CONTENT");
            ctx.runService("createContentAssoc", assoc);
        }
        Map<String, Object> out = new LinkedHashMap<>();
        out.put("contentId", newContentId);
        out.put("dataResourceId", newDataResourceId);
        out.put("mimeTypeId", mime);
        out.put("bytes", bytes.length);
        return out;
    }

    private static String mimeTypeFromFileName(String fileName) {
        String ext = "";
        if (fileName != null) {
            int dot = fileName.lastIndexOf('.');
            if (dot >= 0 && dot < fileName.length() - 1) {
                ext = fileName.substring(dot + 1).toLowerCase(Locale.ROOT);
            }
        }
        switch (ext) {
            case "png": return "image/png";
            case "jpg":
            case "jpeg": return "image/jpeg";
            case "gif": return "image/gif";
            case "pdf": return "application/pdf";
            case "txt": return "text/plain";
            case "csv": return "text/csv";
            case "svg": return "image/svg+xml";
            case "webp": return "image/webp";
            default: return "application/octet-stream";
        }
    }
}
