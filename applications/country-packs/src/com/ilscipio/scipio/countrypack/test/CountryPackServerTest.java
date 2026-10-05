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
package com.ilscipio.scipio.countrypack.test;

import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;

import org.ofbiz.base.util.UtilDateTime;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.entity.util.EntityQuery;
import org.ofbiz.service.ServiceUtil;
import org.ofbiz.service.testtools.OFBizTestCase;

import com.ilscipio.scipio.compliance.ComplianceChecklist;
import com.ilscipio.scipio.compliance.LegalDocumentWorker;

/**
 * W1-03 test in a running server (the OFBiz test runner): a new DE store shows the LUCID task and live legal pages from
 * templates. The test makes a store and a store-owner login (group TENANT_OWNER, as a hosted owner), applies the pack "de"
 * through the service, and reads the pages with the call that the shop page {@code legal/<slug>} uses.
 *
 * <p>Run: see docs/wp/W1-03.md, section 4.</p>
 */
public class CountryPackServerTest extends OFBizTestCase {

    private static final String STORE = "W103DE";
    private static final String OWNER_PARTY = "W103OWNER";
    private static final String LOGIN = "w103-owner";
    private static final String[] SLUGS = {"imprint", "terms", "privacy", "cookies", "withdrawal", "returns", "accessibility"};

    public CountryPackServerTest(String name) {
        super(name);
    }

    @Override
    protected void setUp() throws Exception {
        cleanUp();
        delegator.create("Party", UtilMisc.toMap("partyId", OWNER_PARTY, "partyTypeId", "PARTY_GROUP"));
        delegator.create("ProductStore", UtilMisc.toMap("productStoreId", STORE, "storeName", "W1-03 DE store", "payToPartyId", OWNER_PARTY));
        delegator.create("UserLogin", UtilMisc.toMap("userLoginId", LOGIN, "currentPassword", "{SHA}x", "enabled", "Y"));
        delegator.create("UserLoginSecurityGroup", UtilMisc.toMap("userLoginId", LOGIN, "groupId", "TENANT_OWNER", "fromDate", UtilDateTime.nowTimestamp()));
    }

    @Override
    protected void tearDown() throws Exception {
        cleanUp();
    }

    private void cleanUp() throws Exception {
        for (String entity : new String[] {"CountryPackTask", "CountryPackAssignment", "LegalDocument", "StoreComplianceProfile"}) {
            delegator.removeByAnd(entity, UtilMisc.toMap("productStoreId", STORE));
        }
        delegator.removeByAnd("EprRegistration", UtilMisc.toMap("partyId", OWNER_PARTY));
        delegator.removeByAnd("ProductStore", UtilMisc.toMap("productStoreId", STORE));
        delegator.removeByAnd("Party", UtilMisc.toMap("partyId", OWNER_PARTY));
        delegator.removeByAnd("UserLoginSecurityGroup", UtilMisc.toMap("userLoginId", LOGIN));
        delegator.removeByAnd("UserLogin", UtilMisc.toMap("userLoginId", LOGIN));
    }

    private Map<String, Object> run(String service, Object... kv) throws Exception {
        Map<String, Object> ctx = new HashMap<>();
        for (int i = 0; i < kv.length; i += 2) {
            ctx.put((String) kv[i], kv[i + 1]);
        }
        ctx.put("userLogin", EntityQuery.use(delegator).from("UserLogin").where("userLoginId", LOGIN).queryOne());
        return dispatcher.runSync(service, ctx);
    }

    @SuppressWarnings("unchecked")
    public void testNewDeStoreShowsLucidTaskAndLiveLegalPages() throws Exception {
        Map<String, Object> applied = run("countryPackApply", "productStoreId", STORE, "packId", "de");
        assertTrue("apply: " + ServiceUtil.getErrorMessage(applied), ServiceUtil.isSuccess(applied));
        Map<String, Object> result = (Map<String, Object>) applied.get("result");
        assertEquals("HOME", result.get("role"));

        // the LUCID task
        Map<String, Object> status = (Map<String, Object>) run("countryPackStatus", "productStoreId", STORE).get("status");
        List<Map<String, Object>> packs = (List<Map<String, Object>>) status.get("packs");
        assertEquals(1, packs.size());
        Map<String, Object> lucid = null;
        for (Map<String, Object> t : (List<Map<String, Object>>) packs.get(0).get("tasks")) {
            if ("lucid".equals(t.get("taskId"))) {
                lucid = t;
            }
        }
        assertNotNull("LUCID task", lucid);
        assertEquals("NEEDS_YOU", lucid.get("status"));
        assertEquals(Boolean.TRUE, lucid.get("required"));
        assertEquals("LUCID registration number", lucid.get("numberLabel"));
        assertEquals(Boolean.FALSE, packs.get(0).get("marketOpen"));

        // the live legal pages: the same call as the shop page legal/<slug>
        for (String slug : SLUGS) {
            Map<String, Object> doc = LegalDocumentWorker.getDisplayDocument(delegator, STORE, slug, Locale.GERMAN);
            assertNotNull("page " + slug, doc);
            assertEquals("page " + slug + " is a published text, not the fallback", Boolean.FALSE, doc.get("isTemplate"));
            String html = String.valueOf(doc.get("bodyHtml"));
            assertTrue("page " + slug + " carries the mark: " + html, html.contains("TEMPLATE – not legal advice"));
        }
        assertEquals("Impressum", LegalDocumentWorker.getDisplayDocument(delegator, STORE, "imprint", Locale.GERMAN).get("title"));

        // the profile
        GenericValue profile = EntityQuery.use(delegator).from("StoreComplianceProfile").where("productStoreId", STORE).queryOne();
        assertNotNull(profile);
        assertEquals("EU,DE", profile.getString("jurisdictions"));
        assertEquals("EU_OPT_IN", profile.getString("consentMode"));

        // repeat: nothing new, no second version of a text
        Map<String, Object> again = (Map<String, Object>) run("countryPackApply", "productStoreId", STORE, "packId", "de").get("result");
        assertTrue(((List<?>) again.get("publishedDocuments")).isEmpty());
        assertEquals(1L, EntityQuery.use(delegator).from("LegalDocument").where("productStoreId", STORE, "docTypeId", "LEGDOC_IMPRINT").queryCount());

        // the LUCID number needs a value; the number reaches the imprint and the compliance checklist
        Map<String, Object> noValue = run("countryPackCompleteTask", "productStoreId", STORE, "packId", "de", "taskId", "lucid");
        assertTrue("a number is required", ServiceUtil.isError(noValue));
        Map<String, Object> done = run("countryPackCompleteTask", "productStoreId", STORE, "packId", "de", "taskId", "lucid", "value", "DE3000000000001");
        assertTrue(ServiceUtil.getErrorMessage(done), ServiceUtil.isSuccess(done));
        String imprint = String.valueOf(LegalDocumentWorker.getDisplayDocument(delegator, STORE, "imprint", Locale.GERMAN).get("bodyHtml"));
        assertTrue("imprint shows the LUCID number: " + imprint, imprint.contains("DE3000000000001"));
        String epr = null;
        for (Map<String, String> row : ComplianceChecklist.run(delegator, STORE, Locale.GERMAN)) {
            if ("epr".equals(row.get("id"))) {
                epr = row.get("status");
            }
        }
        assertEquals("OK", epr);
    }

    public void testPermissionAndUnknownInput() throws Exception {
        // a login without the permission is refused
        delegator.removeByAnd("UserLoginSecurityGroup", UtilMisc.toMap("userLoginId", LOGIN));
        delegator.create("UserLoginSecurityGroup", UtilMisc.toMap("userLoginId", LOGIN, "groupId", "TENANT_CONTENT", "fromDate", UtilDateTime.nowTimestamp()));
        assertTrue(ServiceUtil.isError(run("countryPackApply", "productStoreId", STORE, "packId", "de")));
        assertEquals(0L, EntityQuery.use(delegator).from("CountryPackAssignment").where("productStoreId", STORE).queryCount());
        // unknown pack and unknown store
        delegator.removeByAnd("UserLoginSecurityGroup", UtilMisc.toMap("userLoginId", LOGIN));
        delegator.create("UserLoginSecurityGroup", UtilMisc.toMap("userLoginId", LOGIN, "groupId", "TENANT_OWNER", "fromDate", UtilDateTime.nowTimestamp()));
        assertTrue(ServiceUtil.isError(run("countryPackApply", "productStoreId", STORE, "packId", "xx")));
        assertTrue(ServiceUtil.isError(run("countryPackApply", "productStoreId", "NO_SUCH_STORE", "packId", "de")));
    }
}
