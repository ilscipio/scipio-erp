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
package com.ilscipio.scipio.commerceprofile.test;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.ofbiz.base.util.UtilDateTime;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.entity.util.EntityQuery;
import org.ofbiz.service.ServiceUtil;
import org.ofbiz.service.testtools.OFBizTestCase;

/**
 * W1-02 test in a running server (the OFBiz test runner): a store login cannot run code actions; CMS code edits
 * are denied. The test creates three logins (owner of a store, staff, operator), turns the hosted profile on for
 * the JVM and calls the services through the dispatcher, the same path that MCP tools, gateway calls, web
 * screens and jobs use.
 *
 * <p>Run: {@code gradlew.bat runTest -PtestComponent=commerce-profile -PtestCase=hosted-profile-server-test}</p>
 */
public class HostedProfileServerTest extends OFBizTestCase {

    private static final String OWNER = "w102-owner";
    private static final String STAFF = "w102-staff";
    private static final String OPS = "w102-ops";

    private String hostedBefore;

    public HostedProfileServerTest(String name) {
        super(name);
    }

    @Override
    protected void setUp() throws Exception {
        hostedBefore = System.getProperty("scipio.hosted");
        System.setProperty("scipio.hosted", "true");
        createLogin(OWNER, "TENANT_OWNER");
        createLogin(STAFF, "TENANT_STAFF");
        createLogin(OPS, "SCIPIO_OPS");
    }

    @Override
    protected void tearDown() throws Exception {
        if (hostedBefore == null) System.clearProperty("scipio.hosted");
        else System.setProperty("scipio.hosted", hostedBefore);
        for (String id : new String[] {OWNER, STAFF, OPS}) {
            delegator.removeByAnd("UserLoginSecurityGroup", UtilMisc.toMap("userLoginId", id));
            delegator.removeByAnd("UserLogin", UtilMisc.toMap("userLoginId", id));
        }
    }

    private void createLogin(String id, String groupId) throws Exception {
        delegator.removeByAnd("UserLoginSecurityGroup", UtilMisc.toMap("userLoginId", id));
        delegator.removeByAnd("UserLogin", UtilMisc.toMap("userLoginId", id));
        delegator.create("UserLogin", UtilMisc.toMap("userLoginId", id, "currentPassword", "{SHA}x", "enabled", "Y"));
        delegator.create("UserLoginSecurityGroup", UtilMisc.toMap("userLoginId", id, "groupId", groupId, "fromDate", UtilDateTime.nowTimestamp()));
    }

    private GenericValue login(String id) throws Exception {
        return EntityQuery.use(delegator).from("UserLogin").where("userLoginId", id).queryOne();
    }

    private Map<String, Object> run(String service, String userLoginId, Object... kv) throws Exception {
        Map<String, Object> ctx = new HashMap<>();
        for (int i = 0; i < kv.length; i += 2) ctx.put((String) kv[i], kv[i + 1]);
        ctx.put("userLogin", login(userLoginId));
        return dispatcher.runSync(service, ctx);
    }

    /** True when the call does not run: the permission check throws, or a rule returns an error. */
    private boolean refused(String service, String userLoginId, Object... kv) throws Exception {
        try {
            return ServiceUtil.isError(run(service, userLoginId, kv));
        } catch (org.ofbiz.service.GenericServiceException e) {
            return true; // ServiceAuthException: the login lacks the permission of the service
        }
    }

    private static String message(Map<String, Object> result) {
        return ServiceUtil.getErrorMessage(result);
    }

    private static boolean refusedByProfile(Map<String, Object> result) {
        return ServiceUtil.isError(result) && message(result) != null && message(result).contains("hosted store");
    }

    private boolean holds(String groupId, String permissionId) throws Exception {
        return EntityQuery.use(delegator).from("SecurityGroupPermission").where("groupId", groupId, "permissionId", permissionId).queryCount() > 0;
    }

    public void testSeedGroups() throws Exception {
        for (String g : new String[] {"TENANT_OWNER", "TENANT_STAFF", "TENANT_CONTENT", "SCIPIO_OPS"}) {
            assertNotNull("group " + g, EntityQuery.use(delegator).from("SecurityGroup").where("groupId", g).queryOne());
        }
        for (String g : new String[] {"TENANT_OWNER", "TENANT_STAFF", "TENANT_CONTENT"}) {
            for (String p : new String[] {"MCP_CODE_WRITE", "MCP_ENTITY_WRITE", "MCP_ADMIN", "CMS_CODE_UPDATE", "HOSTED_OPS", "WEBTOOLS_VIEW", "WEBTOOLS_UPDATE"}) {
                assertFalse(g + " holds " + p, holds(g, p));
            }
            List<GenericValue> all = EntityQuery.use(delegator).from("SecurityGroupPermission").where("groupId", g).queryList();
            for (GenericValue gp : all) assertFalse(g + " holds an ADMIN permission: " + gp.getString("permissionId"),
                    gp.getString("permissionId").endsWith("_ADMIN") && !"SETUP_ADMIN".equals(gp.getString("permissionId")));
        }
        assertTrue(holds("SCIPIO_OPS", "HOSTED_OPS"));
        assertTrue(holds("SCIPIO_OPS", "MCP_CODE_WRITE"));
    }

    /** The owner passes the CMS permission service (CMS_CREATE, CMS_UPDATE): only the guard stops the call. */
    public void testStoreLoginCannotEditCmsCode() throws Exception {
        assertTrue(holds("TENANT_OWNER", "CMS_UPDATE"));
        String[][] calls = {
            {"cmsCreatePageTemplate", "templateName", "w102 template", "webSiteId", "cmsdemo"},
            {"cmsAddPageTemplateVersion", "pageTemplateId", "x", "content", "<p>${Static}</p>"},
            {"cmsUpdatePageTemplateScript", "pageTemplateId", "x"},
            {"cmsCreateUpdateScriptTemplate", "templateName", "w102 script"},
            {"cmsCreateUpdateAsset", "assetName", "w102 asset"},
            {"cmsImportXmlData", "xmlText", "<entity-engine-xml/>"},
        };
        for (String[] call : calls) {
            Object[] kv = new Object[call.length - 1];
            System.arraycopy(call, 1, kv, 0, kv.length);
            Map<String, Object> result = run(call[0], OWNER, kv);
            assertTrue(call[0] + " must be refused for a store owner: " + result, refusedByProfile(result));
        }
        // The staff login has no CMS_CREATE: refused, by the permission service or the guard.
        assertTrue(refused("cmsCreatePageTemplate", STAFF, "templateName", "w102 template"));
    }

    public void testStoreLoginCannotRunCodeOrLoadData() throws Exception {
        String[] services = {"hostedGuardCode", "entityImport", "entityImportDir", "createJobSandbox", "testGroovy", "createFile"};
        for (String s : services) {
            assertTrue(s + " must be refused for a store owner", refused(s, OWNER));
        }
        assertTrue(refusedByProfile(run("hostedGuardCode", OWNER)));
    }

    public void testStoreLoginCannotChooseTheMailServerOrAFilePath() throws Exception {
        Map<String, Object> mail = run("sendMail", OWNER, "sendTo", "a@example.com", "subject", "x", "body", "x",
                "sendVia", "smtp.attacker.example", "authUser", "u", "authPass", "p");
        assertTrue("sendMail with a mail server: " + mail, refusedByProfile(mail));
        Map<String, Object> file = run("createFileFromScreen", OWNER, "screenUrl", "component://shop/widget/CommonScreens.xml#main", "filePath", "/tmp");
        assertTrue("createFileFromScreen with a path: " + file, refusedByProfile(file));
    }

    /**
     * document_render (framework/mcp DocumentTools.render) calls createFileFromScreen with a screen, a screenContext, a
     * contentType and a file name, and no path. A store owner must get the PDF into the store output folder.
     */
    public void testStoreOwnerCanRenderADocument() throws Exception {
        java.util.Map<String, Object> screenContext = new java.util.HashMap<>();
        String orderId = OrderToInvoiceSmokeTest.createSalesOrder(dispatcher, delegator, login("system"));
        screenContext.put("orderId", orderId);
        Map<String, Object> result = run("createFileFromScreen", OWNER,
                "screenLocation", "component://order/widget/ordermgr/OrderPrintScreens.xml#OrderPDF",
                "screenContext", screenContext, "contentType", "application/pdf", "fileName", "order-" + orderId + "-");
        assertFalse("not refused by the profile: " + result, refusedByProfile(result));
        assertTrue("PDF rendered: " + result, ServiceUtil.isSuccess(result));
        Object fo = result.get("fileOutput");
        assertTrue("file " + fo, fo instanceof java.io.File && ((java.io.File) fo).isFile());
        String base = org.ofbiz.entity.tenant.TenantFiles.scopePath(org.ofbiz.entity.util.EntityUtilProperties
                .getPropertyValue("content", "content.output.path", "/output", delegator), delegator);
        assertTrue("under the store folder", ((java.io.File) fo).getCanonicalPath().startsWith(new java.io.File(base).getCanonicalPath()));
        ((java.io.File) fo).delete();
        // A path, a path in the name and a foreign screen are still refused.
        assertTrue(refused("createFileFromScreen", OWNER, "screenLocation", "component://order/widget/x.xml#Y", "fileName", "../x", "contentType", "application/pdf"));
        assertTrue(refused("createFileFromScreen", OWNER, "screenLocation", "/etc/x.xml#Y", "fileName", "x", "contentType", "application/pdf"));
    }

    public void testEntityImportOnlyForSetupFiles() throws Exception {
        // The permission check of entityImport (Webtools) may throw before the guard result is read: both mean refused.
        assertTrue(refused("entityImport", OWNER, "fulltext", "<entity-engine-xml/>"));
        assertTrue(refused("entityImport", OWNER, "filename", System.getProperty("ofbiz.home") + "/applications/mcp/data/x.xml"));
        // A setup file passes the guard (the import itself may then fail on the login permission of Webtools).
        Map<String, Object> ok = null;
        try {
            ok = run("entityImport", OWNER, "filename", System.getProperty("ofbiz.home") + "/applications/commonext/data/ShippingData.xml", "checkDataOnly", "Y");
        } catch (org.ofbiz.service.GenericServiceException e) {
            // permission service of the import: not the profile
        }
        if (ok != null) assertFalse("setup file must pass the guard: " + ok, refusedByProfile(ok));
    }

    public void testOperatorPassesTheGuard() throws Exception {
        assertTrue(ServiceUtil.isSuccess(run("hostedGuardCode", OPS)));
        assertTrue(ServiceUtil.isSuccess(run("hostedGuardCmsCode", OPS)));
        assertTrue(ServiceUtil.isSuccess(run("hostedGuardCode", "system")));
        // Through the real service: not refused by the profile (it may still fail on its input).
        Map<String, Object> result = run("cmsCreatePageTemplate", OPS, "templateName", "w102 ops template");
        assertFalse("operator must pass the guard: " + result, refusedByProfile(result));
    }

    public void testNothingChangesWhenNotHosted() throws Exception {
        System.setProperty("scipio.hosted", "false");
        assertTrue(ServiceUtil.isSuccess(run("hostedGuardCode", OWNER)));
        assertTrue(ServiceUtil.isSuccess(run("hostedGuardCmsCode", OWNER)));
        Map<String, Object> result = run("cmsCreatePageTemplate", OWNER, "templateName", "w102 template");
        assertFalse("not hosted: no refusal by the profile: " + result, refusedByProfile(result));
    }
}
