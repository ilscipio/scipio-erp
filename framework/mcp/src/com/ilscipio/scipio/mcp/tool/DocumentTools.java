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
package com.ilscipio.scipio.mcp.tool;

import java.io.File;
import java.nio.file.Files;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.base.util.string.FlexibleStringExpander;
import org.ofbiz.entity.GenericEntityException;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.entity.util.EntityQuery;

import com.ilscipio.scipio.mcp.registry.McpCallContext;
import com.ilscipio.scipio.mcp.registry.McpResult;
import com.ilscipio.scipio.mcp.registry.McpToolDef;
import com.ilscipio.scipio.mcp.registry.McpToolException;
import com.ilscipio.scipio.mcp.registry.McpTopicTool;
import com.ilscipio.scipio.mcp.security.McpConfig;
import com.ilscipio.scipio.mcp.security.McpPolicy;

/**
 * SCIPIO: 4.0.0: Core tools that reuse the existing document engine: {@code document_render} (FOP screens to PDF
 * through the content component's {@code createFileFromScreen}) and {@code mail_send_template} (approved
 * {@code EmailTemplateSetting} rows through {@code sendMailFromScreen}, guarded by {@code MCP_MAIL_SEND}).
 */
public final class DocumentTools {

    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    /** One printable document type: the FOP screen, the id parameter, the owning component (for the _VIEW gate). */
    public static final class DocType {
        public final String type;
        public final String screen;
        public final String idParam;
        public final String component;
        public final String[] extraIdParams;

        DocType(String type, String screen, String idParam, String component, String... extraIdParams) {
            this.type = type;
            this.screen = screen;
            this.idParam = idParam;
            this.component = component;
            this.extraIdParams = extraIdParams;
        }
    }

    private static final Map<String, DocType> TYPES = new LinkedHashMap<>();
    static {
        add(new DocType("invoice", "component://accounting/widget/AccountingPrintScreens.xml#InvoicePDF", "invoiceId", "accounting", "invoiceIds"));
        add(new DocType("order", "component://order/widget/ordermgr/OrderPrintScreens.xml#OrderPDF", "orderId", "order"));
        add(new DocType("return", "component://order/widget/ordermgr/OrderPrintScreens.xml#ReturnPDF", "returnId", "order"));
        add(new DocType("quote", "component://order/widget/ordermgr/QuoteScreens.xml#QuoteReport", "quoteId", "order"));
        add(new DocType("production_run", "component://manufacturing/widget/manufacturing/JobshopScreens.xml#ProductionRunPdf", "productionRunId", "manufacturing"));
        add(new DocType("production_run_labels", "component://manufacturing/widget/manufacturing/BarcodeScreens.xml#ProductionRunLabels", "productionRunId", "manufacturing"));
        add(new DocType("shipment_label", "component://manufacturing/widget/manufacturing/ReportScreens.xml#ShipmentLabel", "shipmentId", "manufacturing"));
    }

    private static void add(DocType t) { TYPES.put(t.type, t); }

    public static DocType docType(String type) { return type != null ? TYPES.get(type.trim().toLowerCase()) : null; }

    public static List<String> docTypes() { return new ArrayList<>(TYPES.keySet()); }

    private DocumentTools() {}

    /** The {@code scipio_document} tool: render a PDF, or mail from a template with an optional PDF attached. */
    public static McpToolDef documentTool() {
        return McpTopicTool.build("scipio_document", "Documents", "Business documents: render as PDF, send by mail from a template.", 90, false,
                Arrays.asList(documentRender(), mailSendTemplate()));
    }

    // ---- render ----

    public static McpToolDef documentRender() {
        Map<String, Object> props = new LinkedHashMap<>();
        props.put("type", prop("string", "Document type: " + String.join(", ", TYPES.keySet())));
        props.put("id", prop("string", "Record id of that type"));
        return McpToolDef.builder("render").title("Render document (PDF)")
                .description("Render one business document as PDF, returned as an embedded resource.")
                .inputSchema(schema(props, "type", "id")).readOnly(true).idempotent(true).source("DocumentTools").order(90)
                .executor((ctx, args) -> {
                    DocType t = docType(str(args, "type"));
                    if (t == null) throw new McpToolException("Unknown document type; use one of " + TYPES.keySet());
                    String id = str(args, "id");
                    if (UtilValidate.isEmpty(id)) throw new McpToolException("id is required");
                    requireView(ctx, t.component);
                    byte[] pdf = render(ctx, t, id);
                    String fileName = t.type + "-" + safeId(id) + ".pdf";
                    Map<String, Object> out = new LinkedHashMap<>();
                    out.put("type", t.type);
                    out.put("id", id);
                    out.put("fileName", fileName);
                    out.put("mimeType", "application/pdf");
                    out.put("bytes", pdf.length);
                    out.put("note", "The PDF is the embedded resource content block (base64 blob) of this result.");
                    return McpResult.blob(out, pdf, "application/pdf", fileName);
                }).build();
    }

    /** Renders one document type to PDF bytes through the content component's createFileFromScreen service. */
    public static byte[] render(McpCallContext ctx, DocType t, String id) throws McpToolException {
        // SCIPIO: 4.0.0: no filePath: createFileFromScreen writes to its tenant-scoped output folder (a hosted store may not pass a path)
        {
            Map<String, Object> screenContext = new LinkedHashMap<>();
            screenContext.put(t.idParam, id);
            for (String extra : t.extraIdParams) screenContext.put(extra, Collections.singletonList(id));
            screenContext.put("userLogin", ctx.getUserLogin());
            screenContext.put("locale", ctx.getLocale());
            screenContext.put("timeZone", ctx.getTimeZone());
            Map<String, Object> params = new LinkedHashMap<>();
            params.put("screenLocation", t.screen);
            params.put("screenContext", screenContext);
            params.put("contentType", "application/pdf");
            params.put("fileName", t.type + "-" + safeId(id) + "-");
            Map<String, Object> res = ctx.runService("createFileFromScreen", params);
            Object fo = res.get("fileOutput");
            if (!(fo instanceof File) || !((File) fo).isFile()) {
                throw new McpToolException("Document rendering returned no file for " + t.type + " " + id);
            }
            File f = (File) fo;
            try {
                return Files.readAllBytes(f.toPath());
            } catch (java.io.IOException e) {
                throw new McpToolException("Cannot read the rendered document: " + e.getMessage());
            } finally {
                if (!f.delete()) Debug.logWarning("[MCP] could not delete temp document " + f, module);
            }
        }
    }

    private static String safeId(String id) {
        return id.replaceAll("[^A-Za-z0-9_.-]", "_");
    }

    private static void requireView(McpCallContext ctx, String component) throws McpToolException {
        List<List<String>> bases = McpPolicy.componentBasePermissions(component);
        if (bases.isEmpty()) {
            ctx.requirePermission("MCP_ADMIN");
            return;
        }
        for (List<String> b : bases) {
            if (McpPolicy.hasBasePermission(ctx.getRequest(), b, "_VIEW")) return;
        }
        List<String> first = bases.get(0);
        throw McpToolException.denied("Permission " + first.get(first.size() - 1) + "_VIEW required for this document");
    }

    // ---- mail ----

    public static McpToolDef mailSendTemplate() {
        Map<String, Object> props = new LinkedHashMap<>();
        props.put("templateId", prop("string", "Approved EmailTemplateSetting id: " + String.join(", ", McpConfig.getMailTemplates())));
        props.put("partyIdTo", prop("string", "Recipient party; its primary email address is used"));
        props.put("sendTo", prop("string", "Recipient email address (instead of partyIdTo)"));
        props.put("subject", prop("string", "Subject; default is the template subject (may use ${...} from bodyParameters)"));
        props.put("text", prop("string", "Body text; required when the template has no body screen"));
        Map<String, Object> bp = prop("object", "Values for the template and the attachment screen, e.g. {\"orderId\":\"10000\"}");
        bp.put("additionalProperties", true);
        props.put("bodyParameters", bp);
        props.put("attachmentType", prop("string", "Optional PDF attachment type (see action render)"));
        props.put("attachmentId", prop("string", "Id for the attachment type"));
        return McpToolDef.builder("mail").title("Send mail from template")
                .description("Send one email from an approved template, with an optional PDF attached; needs MCP_MAIL_SEND.")
                .inputSchema(schema(props, "templateId")).readOnly(false).destructive(false).idempotent(false)
                .requiresConfirmation(true).permission("MCP_MAIL_SEND").source("DocumentTools").order(91)
                .executor((ctx, args) -> McpResult.ok(send(ctx, args))).build();
    }

    @SuppressWarnings("unchecked")
    public static Map<String, Object> send(McpCallContext ctx, Map<String, Object> args) throws McpToolException {
        ctx.requirePermission("MCP_MAIL_SEND");
        String templateId = str(args, "templateId");
        if (UtilValidate.isEmpty(templateId) || !McpConfig.getMailTemplates().contains(templateId)) {
            throw new McpToolException("templateId must be one of " + McpConfig.getMailTemplates());
        }
        GenericValue tpl;
        try {
            tpl = EntityQuery.use(ctx.getDelegator()).from("EmailTemplateSetting").where("emailTemplateSettingId", templateId).cache().queryOne();
        } catch (GenericEntityException e) {
            throw new McpToolException("Template lookup failed: " + e.getMessage());
        }
        if (tpl == null) throw new McpToolException("EmailTemplateSetting " + templateId + " is not loaded (seed data of framework/mcp)");
        String partyIdTo = str(args, "partyIdTo");
        String sendTo = str(args, "sendTo");
        if (UtilValidate.isEmpty(sendTo) && UtilValidate.isNotEmpty(partyIdTo)) sendTo = primaryEmail(ctx, partyIdTo);
        if (UtilValidate.isEmpty(sendTo)) throw new McpToolException("sendTo or a partyIdTo with a primary email address is required");
        if (!sendTo.matches("[^\\s@]+@[^\\s@]+\\.[^\\s@]+")) throw new McpToolException("sendTo is not an email address: " + sendTo);

        Map<String, Object> bodyParameters = new LinkedHashMap<>();
        Object bp = args.get("bodyParameters");
        if (bp instanceof Map) bodyParameters.putAll((Map<String, Object>) bp);
        String text = str(args, "text");
        if (text != null) bodyParameters.put("text", text);
        if (partyIdTo != null) bodyParameters.put("partyId", partyIdTo);
        bodyParameters.put("userLogin", ctx.getUserLogin());
        bodyParameters.put("locale", ctx.getLocale());

        Map<String, Object> params = new LinkedHashMap<>();
        params.put("sendTo", sendTo);
        if (UtilValidate.isNotEmpty(tpl.getString("fromAddress"))) params.put("sendFrom", tpl.getString("fromAddress"));
        if (UtilValidate.isNotEmpty(tpl.getString("ccAddress"))) params.put("sendCc", tpl.getString("ccAddress"));
        if (UtilValidate.isNotEmpty(tpl.getString("bccAddress"))) params.put("sendBcc", tpl.getString("bccAddress"));
        String subject = str(args, "subject");
        if (UtilValidate.isEmpty(subject)) subject = tpl.getString("subject");
        if (UtilValidate.isEmpty(subject)) throw new McpToolException("subject is required (the template has none)");
        subject = FlexibleStringExpander.expandString(subject, bodyParameters, ctx.getLocale());
        params.put("subject", subject);
        params.put("bodyParameters", bodyParameters);
        if (UtilValidate.isNotEmpty(tpl.getString("bodyScreenLocation"))) {
            params.put("bodyScreenUri", tpl.getString("bodyScreenLocation"));
            params.put("contentType", UtilValidate.isNotEmpty(tpl.getString("contentType")) ? tpl.getString("contentType") : "text/html");
        } else {
            if (UtilValidate.isEmpty(text)) throw new McpToolException("text is required: template " + templateId + " has no body screen");
            params.put("bodyText", text);
            params.put("contentType", "text/plain");
        }
        String attachment = null;
        String attachmentType = str(args, "attachmentType");
        String attachmentId = str(args, "attachmentId");
        if (UtilValidate.isNotEmpty(attachmentType)) {
            DocType t = docType(attachmentType);
            if (t == null) throw new McpToolException("Unknown attachmentType; use one of " + TYPES.keySet());
            if (UtilValidate.isEmpty(attachmentId)) throw new McpToolException("attachmentId is required with attachmentType");
            requireView(ctx, t.component);
            bodyParameters.put(t.idParam, attachmentId);
            for (String extra : t.extraIdParams) bodyParameters.put(extra, Collections.singletonList(attachmentId));
            params.put("xslfoAttachScreenLocation", t.screen);
            attachment = t.type + "-" + safeId(attachmentId) + ".pdf";
            params.put("attachmentName", attachment);
        } else if (UtilValidate.isNotEmpty(tpl.getString("xslfoAttachScreenLocation"))) {
            params.put("xslfoAttachScreenLocation", tpl.getString("xslfoAttachScreenLocation"));
            attachment = templateId.toLowerCase() + ".pdf";
            params.put("attachmentName", attachment);
        }
        Map<String, Object> res = ctx.runService("sendMailFromScreen", params);
        Map<String, Object> out = new LinkedHashMap<>();
        out.put("sent", true);
        out.put("templateId", templateId);
        out.put("sendTo", sendTo);
        out.put("subject", subject);
        if (attachment != null) out.put("attachment", attachment);
        if (res.get("messageId") != null) out.put("messageId", String.valueOf(res.get("messageId")));
        return out;
    }

    private static String primaryEmail(McpCallContext ctx, String partyId) throws McpToolException {
        try {
            List<GenericValue> purposes = EntityQuery.use(ctx.getDelegator()).from("PartyContactMechPurpose")
                    .where("partyId", partyId, "contactMechPurposeTypeId", "PRIMARY_EMAIL").filterByDate().orderBy("-fromDate").queryList();
            for (GenericValue p : purposes) {
                GenericValue cm = EntityQuery.use(ctx.getDelegator()).from("ContactMech").where("contactMechId", p.getString("contactMechId")).queryOne();
                if (cm != null && "EMAIL_ADDRESS".equals(cm.getString("contactMechTypeId")) && UtilValidate.isNotEmpty(cm.getString("infoString"))) {
                    return cm.getString("infoString");
                }
            }
            for (GenericValue pcm : EntityQuery.use(ctx.getDelegator()).from("PartyAndContactMech")
                    .where("partyId", partyId, "contactMechTypeId", "EMAIL_ADDRESS").filterByDate().orderBy("-fromDate").queryList()) {
                if (UtilValidate.isNotEmpty(pcm.getString("infoString"))) return pcm.getString("infoString");
            }
        } catch (GenericEntityException e) {
            throw new McpToolException("Email lookup failed for party " + partyId + ": " + e.getMessage());
        }
        throw new McpToolException("Party " + partyId + " has no email address; pass sendTo or add one with contact_mech_add");
    }

    // ---- schema helpers (same shape as CoreToolProvider) ----

    static Map<String, Object> prop(String type, String description) {
        Map<String, Object> m = new LinkedHashMap<>();
        m.put("type", type);
        m.put("description", description);
        return m;
    }

    static Map<String, Object> schema(Map<String, Object> props, String... required) {
        Map<String, Object> s = McpToolDef.emptyObjectSchema();
        s.put("properties", props);
        if (required.length > 0) s.put("required", Arrays.asList(required));
        return s;
    }

    static String str(Map<String, Object> args, String key) {
        Object v = args.get(key);
        return v != null ? String.valueOf(v).trim() : null;
    }
}
