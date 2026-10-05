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
package com.ilscipio.scipio.compliance.service;

import java.util.List;
import java.util.Locale;
import java.util.Map;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilDateTime;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericEntityException;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.entity.util.EntityQuery;
import org.ofbiz.security.Security;
import org.ofbiz.service.DispatchContext;
import org.ofbiz.service.ServiceUtil;

import com.ilscipio.scipio.compliance.LegalDocumentWorker;
import com.ilscipio.scipio.compliance.ThirdPartyServiceRegistry;

/**
 * Service implementations of the compliance component.
 *
 * <p>SCIPIO: 4.0.0: Added.</p>
 */
public class ComplianceServiceImpl {

    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    private ComplianceServiceImpl() {}

    public static Map<String, Object> publishLegalDocument(DispatchContext dctx, Map<String, ? extends Object> context) {
        Delegator delegator = dctx.getDelegator();
        Security security = dctx.getSecurity();
        GenericValue userLogin = (GenericValue) context.get("userLogin");
        if (!security.hasPermission("COMPLIANCE_CREATE", userLogin) && !security.hasPermission("COMPLIANCE_ADMIN", userLogin)) {
            return ServiceUtil.returnError("Permission COMPLIANCE_CREATE is required to publish a legal text.");
        }
        String productStoreId = (String) context.get("productStoreId");
        String localeString = UtilValidate.isNotEmpty((String) context.get("localeString")) ? (String) context.get("localeString") : "en";
        GenericValue docType = LegalDocumentWorker.getDocTypeBySlug(delegator, (String) context.get("docTypeId"));
        if (docType == null) {
            return ServiceUtil.returnError("Unknown legal document type: " + context.get("docTypeId"));
        }
        String docTypeId = docType.getString("enumId");
        String bodyText = (String) context.get("bodyText");
        String title = (String) context.get("title");
        boolean fromTemplate = false;
        if (UtilValidate.isEmpty(bodyText)) {
            String template = LegalDocumentWorker.getTemplateText(docType.getString("enumCode"), new Locale(localeString.split("_")[0]));
            if (template == null) {
                return ServiceUtil.returnError("No text given and no template for " + docTypeId + " in " + localeString);
            }
            bodyText = template;
            fromTemplate = true;
        }
        try {
            GenericValue store = EntityQuery.use(delegator).from("ProductStore").where("productStoreId", productStoreId).queryOne();
            if (store == null) {
                return ServiceUtil.returnError("Unknown product store: " + productStoreId);
            }
            List<GenericValue> versions = EntityQuery.use(delegator).from("LegalDocument")
                    .where("productStoreId", productStoreId, "docTypeId", docTypeId, "localeString", localeString)
                    .orderBy("-versionNum").queryList();
            long versionNum = versions.isEmpty() ? 1L : versions.get(0).getLong("versionNum") + 1L;
            for (GenericValue old : versions) {
                if (LegalDocumentWorker.STATUS_PUBLISHED.equals(old.getString("statusId"))) {
                    old.set("statusId", "LDS_ARCHIVED");
                    old.store();
                }
            }
            GenericValue doc = delegator.makeValue("LegalDocument", UtilMisc.toMap(
                    "legalDocumentId", delegator.getNextSeqId("LegalDocument"),
                    "productStoreId", productStoreId, "docTypeId", docTypeId, "localeString", localeString,
                    "versionNum", versionNum, "statusId", LegalDocumentWorker.STATUS_PUBLISHED,
                    "title", UtilValidate.isNotEmpty(title) ? title : null,
                    "bodyText", bodyText, "fromTemplate", fromTemplate ? "Y" : "N",
                    "registryHash", ThirdPartyServiceRegistry.getRegistryHash(delegator, productStoreId),
                    "changeNote", context.get("changeNote"),
                    "publishedDate", UtilDateTime.nowTimestamp(),
                    "publishedByUserLogin", userLogin != null ? userLogin.getString("userLoginId") : null));
            doc.create();
            Map<String, Object> result = ServiceUtil.returnSuccess("Published " + docTypeId + " version " + versionNum);
            result.put("legalDocumentId", doc.getString("legalDocumentId"));
            result.put("versionNum", versionNum);
            return result;
        } catch (GenericEntityException e) {
            Debug.logError(e, "Could not publish legal document", module);
            return ServiceUtil.returnError("Could not publish the legal text: " + e.getMessage());
        }
    }

    public static Map<String, Object> clearComplianceCaches(DispatchContext dctx, Map<String, ? extends Object> context) {
        ThirdPartyServiceRegistry.clearCache();
        return ServiceUtil.returnSuccess();
    }
}
