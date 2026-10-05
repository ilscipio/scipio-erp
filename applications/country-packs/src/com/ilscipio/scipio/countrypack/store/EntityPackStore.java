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
package com.ilscipio.scipio.countrypack.store;

import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.ofbiz.base.util.UtilDateTime;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericEntityException;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.entity.condition.EntityCondition;
import org.ofbiz.entity.condition.EntityOperator;
import org.ofbiz.entity.util.EntityQuery;

import com.ilscipio.scipio.compliance.ThirdPartyServiceRegistry;
import com.ilscipio.scipio.countrypack.core.PackStore;
import com.ilscipio.scipio.countrypack.core.Role;
import com.ilscipio.scipio.countrypack.core.TaskStatus;

/**
 * {@link PackStore} on the entity engine. Legal texts go into {@code LegalDocument} of the compliance component (published,
 * flag {@code fromTemplate=Y}), so the shop page {@code legal/<slug>} shows them at once and the seller edits them there.
 * The profile goes into {@code StoreComplianceProfile}.
 *
 * <p>SCIPIO: 4.0.0: Added (W1-03).</p>
 */
public final class EntityPackStore implements PackStore {
    private static final Set<String> NUMERIC_PROFILE_FIELDS = Set.of("withdrawalDays", "returnDays", "legalGuaranteeYears", "retentionYears", "consentVersion");

    private final Delegator delegator;
    private final String userLoginId;

    public EntityPackStore(Delegator delegator, String userLoginId) {
        this.delegator = delegator;
        this.userLoginId = userLoginId;
    }

    private static RuntimeException wrap(GenericEntityException e) {
        return new IllegalStateException("Database error: " + e.getMessage(), e);
    }

    @Override
    public boolean storeExists(String storeId) {
        try {
            return EntityQuery.use(delegator).from("ProductStore").where("productStoreId", storeId).queryOne() != null;
        } catch (GenericEntityException e) {
            throw wrap(e);
        }
    }

    @Override
    public List<Assignment> assignments(String storeId) {
        return assignmentsWhere(UtilMisc.toMap("productStoreId", storeId));
    }

    @Override
    public List<Assignment> allAssignments() {
        return assignmentsWhere(UtilMisc.<String, Object>toMap());
    }

    private List<Assignment> assignmentsWhere(Map<String, Object> where) {
        try {
            List<Assignment> out = new ArrayList<>();
            for (GenericValue v : EntityQuery.use(delegator).from("CountryPackAssignment").where(where).orderBy("productStoreId", "packId").queryList()) {
                out.add(new Assignment(v.getString("productStoreId"), v.getString("packId"), Role.valueOf(v.getString("roleId")), v.getLong("packVersion").intValue()));
            }
            return out;
        } catch (GenericEntityException e) {
            throw wrap(e);
        }
    }

    @Override
    public void saveAssignment(Assignment a) {
        try {
            delegator.makeValue("CountryPackAssignment", UtilMisc.toMap("productStoreId", a.storeId(), "packId", a.packId(),
                    "roleId", a.role().name(), "packVersion", (long) a.version(), "appliedDate", UtilDateTime.nowTimestamp())).createOrStore();
        } catch (GenericEntityException e) {
            throw wrap(e);
        }
    }

    @Override
    public List<TaskRow> tasks(String storeId, String packId) {
        try {
            List<TaskRow> out = new ArrayList<>();
            for (GenericValue v : EntityQuery.use(delegator).from("CountryPackTask").where("productStoreId", storeId, "packId", packId)
                    .orderBy("sinceVersion", "taskId").queryList()) {
                out.add(new TaskRow(storeId, packId, v.getString("taskId"), TaskStatus.valueOf(v.getString("statusId")), "Y".equals(v.getString("required")),
                        v.getString("title"), v.getString("kind"), v.getString("linkUrl"), v.getString("numberLabel"), v.getString("detail"),
                        v.getString("valueText"), v.getString("note"), v.getLong("sinceVersion").intValue()));
            }
            return out;
        } catch (GenericEntityException e) {
            throw wrap(e);
        }
    }

    @Override
    public void saveTask(TaskRow t) {
        try {
            GenericValue v = delegator.makeValue("CountryPackTask");
            v.set("productStoreId", t.storeId());
            v.set("packId", t.packId());
            v.set("taskId", t.taskId());
            v.set("statusId", t.status().name());
            v.set("required", t.required() ? "Y" : "N");
            v.set("title", t.title());
            v.set("kind", t.kind());
            v.set("linkUrl", t.link());
            v.set("numberLabel", t.numberLabel());
            v.set("detail", t.detail());
            v.set("valueText", t.value());
            v.set("note", t.note());
            v.set("sinceVersion", (long) t.sinceVersion());
            Timestamp doneDate = null;
            if (t.status().closed()) {
                // keep the old date while the task stays closed
                GenericValue old = EntityQuery.use(delegator).from("CountryPackTask")
                        .where("productStoreId", t.storeId(), "packId", t.packId(), "taskId", t.taskId()).queryOne();
                boolean wasClosed = old != null && old.getString("statusId") != null && TaskStatus.valueOf(old.getString("statusId")).closed();
                doneDate = wasClosed && old.getTimestamp("doneDate") != null ? old.getTimestamp("doneDate") : UtilDateTime.nowTimestamp();
            }
            v.set("doneDate", doneDate);
            v.createOrStore();
        } catch (GenericEntityException e) {
            throw wrap(e);
        }
    }

    @Override
    public boolean recordRegistration(String storeId, String schemeId, String countryGeoId, String number) {
        try {
            GenericValue store = EntityQuery.use(delegator).from("ProductStore").where("productStoreId", storeId).queryOne();
            String partyId = store == null ? null : store.getString("payToPartyId");
            if (UtilValidate.isEmpty(partyId) || "_NA_".equals(partyId)) {
                return false;
            }
            for (GenericValue old : EntityQuery.use(delegator).from("EprRegistration")
                    .where("partyId", partyId, "schemeId", schemeId, "countryGeoId", countryGeoId).queryList()) {
                if (old.get("thruDate") == null) {
                    if (number.equals(old.getString("registrationNumber"))) {
                        return true;
                    }
                    // end-date the old number only when no other store of this party uses it
                    if (soleUser(storeId, partyId, old.getString("registrationNumber"))) {
                        old.set("thruDate", UtilDateTime.nowTimestamp());
                        old.store();
                    }
                }
            }
            delegator.makeValue("EprRegistration", UtilMisc.toMap("eprRegistrationId", delegator.getNextSeqId("EprRegistration"),
                    "partyId", partyId, "countryGeoId", countryGeoId, "schemeId", schemeId, "registrationNumber", number,
                    "fromDate", UtilDateTime.nowTimestamp())).create();
            return true;
        } catch (GenericEntityException e) {
            throw wrap(e);
        }
    }

    /** True when no other store with the same pay-to party has a closed task with this number. */
    private boolean soleUser(String storeId, String partyId, String number) throws GenericEntityException {
        List<EntityCondition> conds = new ArrayList<>();
        conds.add(EntityCondition.makeCondition("valueText", number));
        conds.add(EntityCondition.makeCondition("statusId", EntityOperator.IN, List.of("DONE", "DONE_BY_SCIPIO")));
        conds.add(EntityCondition.makeCondition("productStoreId", EntityOperator.NOT_EQUAL, storeId));
        for (GenericValue task : EntityQuery.use(delegator).from("CountryPackTask").where(EntityCondition.makeCondition(conds, EntityOperator.AND)).queryList()) {
            GenericValue other = EntityQuery.use(delegator).from("ProductStore").where("productStoreId", task.getString("productStoreId")).queryOne();
            if (other != null && partyId.equals(other.getString("payToPartyId"))) {
                return false;
            }
        }
        return true;
    }

    @Override
    public void closeRegistration(String storeId, String schemeId, String countryGeoId, String number) {
        try {
            GenericValue store = EntityQuery.use(delegator).from("ProductStore").where("productStoreId", storeId).queryOne();
            String partyId = store == null ? null : store.getString("payToPartyId");
            if (UtilValidate.isEmpty(partyId) || "_NA_".equals(partyId) || UtilValidate.isEmpty(number) || !soleUser(storeId, partyId, number)) {
                return;
            }
            for (GenericValue reg : EntityQuery.use(delegator).from("EprRegistration")
                    .where("partyId", partyId, "schemeId", schemeId, "countryGeoId", countryGeoId, "registrationNumber", number).queryList()) {
                if (reg.get("thruDate") == null) {
                    reg.set("thruDate", UtilDateTime.nowTimestamp());
                    reg.store();
                }
            }
        } catch (GenericEntityException e) {
            throw wrap(e);
        }
    }

    @Override
    public void raiseConsentMode(String storeId) {
        try {
            GenericValue profile = EntityQuery.use(delegator).from("StoreComplianceProfile").where("productStoreId", storeId).queryOne();
            if (profile == null) {
                return;
            }
            String mode = profile.getString("consentMode");
            if (UtilValidate.isEmpty(mode) || "US_OPT_OUT".equals(mode)) {
                profile.set("consentMode", "EU_OPT_IN");
                profile.store();
            }
        } catch (GenericEntityException e) {
            throw wrap(e);
        }
    }

    @Override
    public boolean hasPublishedDocument(String storeId, String docTypeId, String locale) {
        try {
            // a text in de_DE counts as a text in de
            for (GenericValue d : EntityQuery.use(delegator).from("LegalDocument")
                    .where("productStoreId", storeId, "docTypeId", docTypeId, "statusId", "LDS_PUBLISHED").queryList()) {
                String l = d.getString("localeString");
                if (l != null && (l.equals(locale) || l.startsWith(locale + "_") || l.startsWith(locale + "-"))) {
                    return true;
                }
            }
            return false;
        } catch (GenericEntityException e) {
            throw wrap(e);
        }
    }

    @Override
    public void publishDocument(String storeId, String docTypeId, String locale, String title, String body, String changeNote) {
        try {
            List<GenericValue> versions = EntityQuery.use(delegator).from("LegalDocument")
                    .where("productStoreId", storeId, "docTypeId", docTypeId, "localeString", locale).orderBy("-versionNum").queryList();
            long versionNum = versions.isEmpty() ? 1L : versions.get(0).getLong("versionNum") + 1L;
            delegator.makeValue("LegalDocument", UtilMisc.toMap(
                    "legalDocumentId", delegator.getNextSeqId("LegalDocument"),
                    "productStoreId", storeId, "docTypeId", docTypeId, "localeString", locale,
                    "versionNum", versionNum, "statusId", "LDS_PUBLISHED", "title", title, "bodyText", body, "fromTemplate", "Y",
                    "registryHash", ThirdPartyServiceRegistry.getRegistryHash(delegator, storeId),
                    "changeNote", changeNote, "publishedDate", UtilDateTime.nowTimestamp(), "publishedByUserLogin", userLoginId)).create();
        } catch (GenericEntityException e) {
            throw wrap(e);
        }
    }

    @Override
    public void mergeProfile(String storeId, List<String> jurisdictions, Map<String, String> defaults) {
        try {
            GenericValue profile = EntityQuery.use(delegator).from("StoreComplianceProfile").where("productStoreId", storeId).queryOne();
            if (profile == null) {
                profile = delegator.makeValue("StoreComplianceProfile", UtilMisc.toMap("productStoreId", storeId));
            }
            Set<String> merged = new LinkedHashSet<>();
            if (UtilValidate.isNotEmpty(profile.getString("jurisdictions"))) {
                for (String j : profile.getString("jurisdictions").split(",")) {
                    if (!j.isBlank()) {
                        merged.add(j.trim());
                    }
                }
            }
            merged.addAll(jurisdictions);
            profile.set("jurisdictions", String.join(",", merged));
            for (Map.Entry<String, String> e : defaults.entrySet()) {
                if (profile.get(e.getKey()) == null || (profile.get(e.getKey()) instanceof String && ((String) profile.get(e.getKey())).isBlank())) {
                    profile.set(e.getKey(), NUMERIC_PROFILE_FIELDS.contains(e.getKey()) ? (Object) Long.valueOf(e.getValue()) : e.getValue());
                }
            }
            profile.createOrStore();
        } catch (GenericEntityException e) {
            throw wrap(e);
        }
    }
}
