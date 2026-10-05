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

package org.ofbiz.widget.test;

import java.io.InputStream;

import org.apache.tika.metadata.Metadata;
import org.apache.tika.parser.ParseContext;
import org.apache.tika.parser.pdf.PDFParser;
import org.apache.tika.sax.BodyContentHandler;
import org.ofbiz.base.start.Start;
import org.ofbiz.base.util.HttpClient;
import org.ofbiz.base.util.HttpClientException;
import org.ofbiz.base.util.SSLUtil;
import org.ofbiz.service.testtools.OFBizTestCase;


public class WidgetMacroLibraryTests extends OFBizTestCase {

    protected String screenUrl = "https://localhost:8443/webtools/control/WebtoolsLayoutDemo" ; //use existing screen to present most of layout use case
    protected final String authentificationQuery = "?USERNAME=admin&PASSWORD=ofbiz";

    public WidgetMacroLibraryTests(String name) {
        super(name);
    }

    /*
     * Prepare the http client to call the demo layout screen
     */
    public HttpClient initHttpClient() throws HttpClientException {
        HttpClient http = new HttpClient();
        http.followRedirects(true);
        http.setAllowUntrusted(true);
        http.setHostVerificationLevel(SSLUtil.getHostCertNoCheck());
        return http;
    }

    public void testHtmlMacroLibrary() throws Exception {
        HttpClient http = initHttpClient();
        if (Start.getInstance().getConfig().portOffset != 0) {
            Integer port = 8443 + Start.getInstance().getConfig().portOffset;
            screenUrl = screenUrl.replace("8443", port.toString());
        }
        http.setUrl(screenUrl.concat(authentificationQuery));
        String screenOutString = http.post();
        assertNotNull("Response failed from ofbiz", screenOutString);
        assertEquals("Response contentType isn't good : " + http.getResponseContentType(), "text/html;charset=UTF-8", http.getResponseContentType());

        //Test if a ftl macro error is present
        assertFalse("Html Screen contains Macro on error : see " + screenUrl + " for more detail", screenOutString.contains("FreeMarker template error:"));
    }

    public void testTextMacroLibrary() throws Exception {
        String screentextUrl = screenUrl.concat("Text");
        HttpClient http = initHttpClient();
        http.setUrl(screentextUrl.concat(authentificationQuery));
        String screenOutString = http.post();
        assertNotNull("Response failed from ofbiz", screenOutString);
        assertEquals("Response contentType isn't good : " + http.getResponseContentType(), "text/html;charset=UTF-8", http.getResponseContentType());

        //Test if a ftl macro error is present
        assertFalse("Text Screen contains Macro on error : see " + screentextUrl + " for more detail", screenOutString.contains("FreeMarker template error:"));
    }

    public void testXmlMacroLibrary() throws Exception {
        String screenxmlUrl = screenUrl.concat("Xml");
        HttpClient http = initHttpClient();
        http.setUrl(screenxmlUrl.concat(authentificationQuery));
        String screenOutString = http.post();
        assertNotNull("Response failed from ofbiz", screenOutString);
        assertEquals("Response contentType isn't good : " + http.getResponseContentType(), "text/xml;charset=UTF-8", http.getResponseContentType());

        //Test if a ftl macro error is present
        assertFalse("Xml Screen contains Macro on error : see " + screenxmlUrl + " for more detail", screenOutString.contains("FreeMarker template error:"));
    }

    public void testCsvMacroLibrary() throws Exception {
        String screencsvUrl = screenUrl.concat("Csv");
        HttpClient http = initHttpClient();
        http.setUrl(screencsvUrl.concat(authentificationQuery));
        String screenOutString = http.post();
        assertNotNull("Response failed from ofbiz", screenOutString);
        assertEquals("Response contentType isn't good : " + http.getResponseContentType(), "text/csv;charset=UTF-8", http.getResponseContentType());

        //Test if a ftl macro error is present
        assertFalse("Csv Screen contains Macro on error : see " + screencsvUrl + " for more detail", screenOutString.contains("FreeMarker template error:"));
    }

    public void testXlsMacroLibrary() throws Exception {
        String screenxlsUrl = screenUrl.concat("Xls");
        HttpClient http = initHttpClient();
        http.setUrl(screenxlsUrl.concat(authentificationQuery));
        String screenOutString = http.post();
        assertNotNull("Response failed from ofbiz", screenOutString);
        assertEquals("Response contentType isn't good : " + http.getResponseContentType(), "application/vnd.ms-excel;charset=UTF-8", http.getResponseContentType());

        //Test if a ftl macro error is present
        assertFalse("Csv Screen contains Macro on error : see " + screenxlsUrl + " for more detail", screenOutString.contains("FreeMarker template error:"));
    }

    public void testFopMacroLibrary() throws Exception {
        String screentextUrl = screenUrl.concat("Fop");
        HttpClient http = initHttpClient();
        http.setUrl(screentextUrl.concat(authentificationQuery));
        //FIXME need to check if the stream is an application-pdf that don't contains ftl stack trace
        InputStream screenInputStream = (InputStream) http.postStream();
        assertNotNull("Response failed from ofbiz", screenInputStream);
        assertEquals("Response contentType isn't good : " + http.getResponseContentType(), "application/pdf;charset=UTF-8", http.getResponseContentType());

        String screenOutString = "";
        try {
            BodyContentHandler handler = new BodyContentHandler(Integer.MAX_VALUE);
            Metadata metadata = new Metadata();
            new PDFParser().parse(screenInputStream, handler, metadata, new ParseContext());
            screenOutString = handler.toString();
        } finally {
            screenInputStream.close();
        }
        //Test if a ftl macro error is present
        assertFalse("Fop Screen contains Macro on error : see " + screentextUrl + " for more detail", screenOutString.contains("FreeMarker template error:"));
    }
}
