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
package com.ilscipio.scipio.cms.test;

import org.ofbiz.base.start.Start;
import org.ofbiz.base.util.HttpClient;
import org.ofbiz.base.util.HttpClientException;
import org.ofbiz.base.util.SSLUtil;
import org.ofbiz.service.testtools.OFBizTestCase;

/**
 * CMS screen rendering tests — verifies CMS screen endpoints render without 404 errors
 * or FreeMarker template errors.
 *
 * <p>SCIPIO: 4.0.0: Added.</p>
 */
public class CmsScreenRenderTest extends OFBizTestCase {

    private static final String AUTH_QUERY = "?USERNAME=admin&PASSWORD=ofbiz";

    public CmsScreenRenderTest(String name) {
        super(name);
    }

    private String getBaseUrl() {
        int port = 8443;
        if (Start.getInstance().getConfig().portOffset != 0) {
            port += Start.getInstance().getConfig().portOffset;
        }
        return "https://localhost:" + port;
    }

    private HttpClient initHttpClient() throws HttpClientException {
        HttpClient http = new HttpClient();
        http.followRedirects(true);
        http.setAllowUntrusted(true);
        http.setHostVerificationLevel(SSLUtil.getHostCertNoCheck());
        return http;
    }

    private String fetchPage(String mount, String view) throws Exception {
        HttpClient http = initHttpClient();
        http.setUrl(getBaseUrl() + "/" + mount + "/control/" + view + AUTH_QUERY);
        String response = http.post();
        assertNotNull("No response from [" + mount + "/" + view + "]", response);
        return response;
    }

    private void assertNoScreenErrors(String response, String view) {
        assertFalse("View [" + view + "] renders 404 screen",
                response.contains("CommonScreens$_404#404"));
        assertFalse("View [" + view + "] shows 'not found' error",
                response.contains("was not found on this server"));
    }

    private void assertScreenRendered(String response, String view, String expectedScreenClass) {
        assertNoScreenErrors(response, view);
        assertFalse("View [" + view + "] contains FreeMarker error",
                response.contains("FreeMarker template error:"));
        if (expectedScreenClass != null) {
            assertTrue("View [" + view + "] should render screen containing [" + expectedScreenClass + "]",
                    response.contains(expectedScreenClass));
        }
    }

    // === CMS Screens ===

    public void testCmsMain() throws Exception {
        String r = fetchPage("cms", "main");
        assertScreenRendered(r, "main", "CMSScreens$main#main");
        assertTrue("CMS main should be substantial", r.length() > 30000);
    }

    public void testCmsPages() throws Exception {
        assertScreenRendered(fetchPage("cms", "pages"), "pages", "CMSScreens$pages#pages");
    }

    public void testCmsTemplates() throws Exception {
        assertScreenRendered(fetchPage("cms", "templates"), "templates", "CMSScreens$templates#templates");
    }

    public void testCmsEditTemplate() throws Exception {
        assertScreenRendered(fetchPage("cms", "editTemplate"), "editTemplate", "CMSScreens$editTemplate");
    }

    public void testCmsAssets() throws Exception {
        assertScreenRendered(fetchPage("cms", "assets"), "assets", "CMSScreens$assets#assets");
    }

    public void testCmsEditAsset() throws Exception {
        assertScreenRendered(fetchPage("cms", "editAsset"), "editAsset", "CMSScreens$editAsset");
    }

    public void testCmsContentAssets() throws Exception {
        assertScreenRendered(fetchPage("cms", "contentAssets"), "contentAssets", "CMSScreens$contentAssets");
    }

    public void testCmsScripts() throws Exception {
        assertScreenRendered(fetchPage("cms", "scripts"), "scripts", "CMSScreens$scripts#scripts");
    }

    public void testCmsEditScript() throws Exception {
        assertScreenRendered(fetchPage("cms", "editScript"), "editScript", "CMSScreens$editScript");
    }

    public void testCmsMedia() throws Exception {
        assertScreenRendered(fetchPage("cms", "media"), "media", "CMSScreens$media#media");
    }

    public void testCmsEditMedia() throws Exception {
        assertScreenRendered(fetchPage("cms", "editMedia"), "editMedia", "CMSScreens$editMedia");
    }

    public void testCmsRedirects() throws Exception {
        assertScreenRendered(fetchPage("cms", "redirects"), "redirects", "CMSScreens$redirects");
    }

    public void testCmsRobots() throws Exception {
        assertScreenRendered(fetchPage("cms", "robots"), "robots", "CMSScreens$robots");
    }

    public void testCmsCustomImageSizePresets() throws Exception {
        assertScreenRendered(fetchPage("cms", "customImageSizePresets"), "customImageSizePresets", "CMSScreens$customImageSizePresets");
    }

    public void testCmsDataImport() throws Exception {
        assertScreenRendered(fetchPage("cms", "CmsDataImport"), "CmsDataImport", "CMSScreens$CmsDataImport");
    }

    public void testCmsDataExport() throws Exception {
        assertScreenRendered(fetchPage("cms", "CmsDataExport"), "CmsDataExport", "CMSScreens$CmsDataExport");
    }

    // === Webtools/Admin Screens ===

    public void testAdminMain() throws Exception {
        String r = fetchPage("admin", "main");
        assertNoScreenErrors(r, "main");
        assertTrue("Admin main should be substantial", r.length() > 20000);
    }

    public void testAdminEntitymaint() throws Exception {
        String r = fetchPage("admin", "entitymaint");
        assertScreenRendered(r, "entitymaint", "EntityScreens$EntityMaint");
        assertTrue("entitymaint should be substantial", r.length() > 100000);
    }

    public void testAdminFindGeo() throws Exception {
        String r = fetchPage("admin", "FindGeo");
        assertNoScreenErrors(r, "FindGeo");
        assertTrue("FindGeo should be substantial", r.length() > 20000);
    }

    public void testAdminServiceList() throws Exception {
        assertNoScreenErrors(fetchPage("admin", "ServiceList"), "ServiceList");
    }

    public void testAdminEntitySQLProcessor() throws Exception {
        assertNoScreenErrors(fetchPage("admin", "EntitySQLProcessor"), "EntitySQLProcessor");
    }

    public void testAdminFindJob() throws Exception {
        assertNoScreenErrors(fetchPage("admin", "FindJob"), "FindJob");
    }

    public void testAdminFindUtilCache() throws Exception {
        assertNoScreenErrors(fetchPage("admin", "FindUtilCache"), "FindUtilCache");
    }

    public void testAdminLogView() throws Exception {
        assertNoScreenErrors(fetchPage("admin", "LogView"), "LogView");
    }

    public void testAdminThreadList() throws Exception {
        assertNoScreenErrors(fetchPage("admin", "threadList"), "threadList");
    }

    // === Cross-Component Main Screens ===

    public void testAccountingMain() throws Exception {
        String r = fetchPage("accounting", "main");
        assertNoScreenErrors(r, "accounting/main");
        assertTrue("Accounting main should be substantial", r.length() > 50000);
    }

    public void testCatalogMain() throws Exception {
        assertNoScreenErrors(fetchPage("catalog", "main"), "catalog/main");
    }

    public void testFacilityMain() throws Exception {
        assertNoScreenErrors(fetchPage("facility", "main"), "facility/main");
    }

    public void testHumanresMain() throws Exception {
        assertNoScreenErrors(fetchPage("humanres", "main"), "humanres/main");
    }

    public void testManufacturingMain() throws Exception {
        assertNoScreenErrors(fetchPage("manufacturing", "main"), "manufacturing/main");
    }

    public void testWorkeffortMain() throws Exception {
        assertNoScreenErrors(fetchPage("workeffort", "main"), "workeffort/main");
    }

    public void testContentMain() throws Exception {
        assertNoScreenErrors(fetchPage("content", "main"), "content/main");
    }

    public void testCrmMain() throws Exception {
        assertNoScreenErrors(fetchPage("crm", "main"), "crm/main");
    }
}
