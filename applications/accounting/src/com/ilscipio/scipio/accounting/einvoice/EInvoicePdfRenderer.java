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
package com.ilscipio.scipio.accounting.einvoice;

import java.io.ByteArrayOutputStream;
import java.io.StringReader;
import java.io.StringWriter;
import java.util.Map;

import javax.xml.transform.stream.StreamSource;

import org.apache.fop.apps.FOUserAgent;
import org.apache.fop.apps.Fop;
import org.apache.fop.apps.MimeConstants;
import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.collections.MapStack;
import org.ofbiz.base.util.collections.RenderMapStack;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.util.EntityUtilProperties;
import org.ofbiz.service.DispatchContext;
import org.ofbiz.webapp.view.ApacheFopWorker;
import org.ofbiz.widget.renderer.ScreenRenderer;
import org.ofbiz.widget.renderer.ScreenStringRenderer;
import org.ofbiz.widget.renderer.macro.MacroScreenRenderer;
import org.xml.sax.SAXParseException;

/**
 * Renders an XSL-FO screen to a PDF; with PDF/A-3b for the visible part of a hybrid e-invoice (ZUGFeRD / Factur-X).
 * PDF/A needs embedded fonts: the FO decorator uses fop.properties fop.font.family (Noto Sans, from fop.xconf).
 */
public final class EInvoicePdfRenderer {

    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    public static final String PDF_A_3B = "PDF/A-3b";

    private EInvoicePdfRenderer() {}

    @SuppressWarnings("unchecked") // FOUserAgent.getRendererOptions() is a raw Map
    public static byte[] render(DispatchContext dctx, String screenLocation, Map<String, Object> parameters, String pdfAMode) throws EInvoiceException {
        Delegator delegator = dctx.getDelegator();
        StringWriter writer = new StringWriter();
        try {
            MapStack<String> context = RenderMapStack.createRenderContext();
            ScreenStringRenderer foRenderer = new MacroScreenRenderer(EntityUtilProperties.getPropertyValue("widget", "screenfop.name", delegator),
                    EntityUtilProperties.getPropertyValue("widget", "screenfop.screenrenderer", delegator));
            ScreenRenderer screens = ScreenRenderer.makeWithEnvAwareFetching(writer, context, foRenderer);
            screens.populateContextForService(dctx, parameters);
            context.putAll(parameters);
            screens.render(screenLocation);

            FOUserAgent userAgent = ApacheFopWorker.getFactoryInstance().newFOUserAgent();
            if (pdfAMode != null) {
                userAgent.getRendererOptions().put("pdf-a-mode", pdfAMode);
            }
            ByteArrayOutputStream out = new ByteArrayOutputStream();
            Fop fop = ApacheFopWorker.createFopInstance(out, MimeConstants.MIME_PDF, userAgent);
            ApacheFopWorker.transform(new StreamSource(new StringReader(writer.toString())), null, fop);
            return out.toByteArray();
        } catch (Exception e) {
            logFoLine(e, writer.toString());
            throw new EInvoiceException("PDF: " + e.getMessage(), e);
        }
    }

    /** Logs the FO line of a parse error: it shows which data or template broke the XSL-FO. */
    private static void logFoLine(Throwable e, String fo) {
        for (Throwable t = e; t != null; t = t.getCause()) {
            if (t instanceof SAXParseException) {
                int lineNumber = ((SAXParseException) t).getLineNumber();
                String[] lines = fo.split("\n", -1);
                if (lineNumber > 0 && lineNumber <= lines.length) {
                    Debug.logWarning("E-invoice PDF: invalid XSL-FO at line " + lineNumber + ": " + lines[lineNumber - 1].trim(), module);
                }
                return;
            }
            if (t instanceof javax.xml.transform.TransformerException && ((javax.xml.transform.TransformerException) t).getException() != null
                    && ((javax.xml.transform.TransformerException) t).getException() != t.getCause()) {
                logFoLine(((javax.xml.transform.TransformerException) t).getException(), fo);
                return;
            }
        }
    }
}
