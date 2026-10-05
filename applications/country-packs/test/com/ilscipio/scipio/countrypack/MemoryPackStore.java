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
package com.ilscipio.scipio.countrypack;

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import com.ilscipio.scipio.countrypack.core.PackStore;

/** {@link PackStore} in memory, for the unit tests of the engine. */
class MemoryPackStore implements PackStore {
    final Set<String> stores = new LinkedHashSet<>();
    final Map<String, Assignment> assignments = new LinkedHashMap<>();
    final Map<String, TaskRow> tasks = new LinkedHashMap<>();
    /** key store|docType|locale to the text */
    final Map<String, String> documents = new LinkedHashMap<>();
    final Map<String, String> titles = new LinkedHashMap<>();
    final Set<String> jurisdictions = new LinkedHashSet<>();
    final Map<String, String> profile = new LinkedHashMap<>();
    final List<String> registrations = new ArrayList<>();
    final List<String> closedRegistrations = new ArrayList<>();
    boolean hasOwnerParty = true;

    MemoryPackStore(String... storeIds) {
        stores.addAll(List.of(storeIds));
    }

    static String docKey(String storeId, String docTypeId, String locale) {
        return storeId + "|" + docTypeId + "|" + locale;
    }

    @Override
    public boolean storeExists(String storeId) {
        return stores.contains(storeId);
    }

    @Override
    public List<Assignment> assignments(String storeId) {
        List<Assignment> out = new ArrayList<>();
        for (Assignment a : assignments.values()) {
            if (a.storeId().equals(storeId)) {
                out.add(a);
            }
        }
        return out;
    }

    @Override
    public List<Assignment> allAssignments() {
        return new ArrayList<>(assignments.values());
    }

    @Override
    public void saveAssignment(Assignment a) {
        assignments.put(a.storeId() + "|" + a.packId(), a);
    }

    @Override
    public List<TaskRow> tasks(String storeId, String packId) {
        List<TaskRow> out = new ArrayList<>();
        for (TaskRow t : tasks.values()) {
            if (t.storeId().equals(storeId) && t.packId().equals(packId)) {
                out.add(t);
            }
        }
        return out;
    }

    @Override
    public void saveTask(TaskRow t) {
        tasks.put(t.storeId() + "|" + t.packId() + "|" + t.taskId(), t);
    }

    @Override
    public boolean recordRegistration(String storeId, String schemeId, String countryGeoId, String number) {
        if (!hasOwnerParty) {
            return false;
        }
        registrations.add(schemeId + "|" + countryGeoId + "|" + number);
        return true;
    }

    @Override
    public void closeRegistration(String storeId, String schemeId, String countryGeoId, String number) {
        closedRegistrations.add(schemeId + "|" + countryGeoId + "|" + number);
    }

    @Override
    public void raiseConsentMode(String storeId) {
        String mode = profile.get("consentMode");
        if (mode == null || mode.isBlank() || "US_OPT_OUT".equals(mode)) {
            profile.put("consentMode", "EU_OPT_IN");
        }
    }

    @Override
    public boolean hasPublishedDocument(String storeId, String docTypeId, String locale) {
        String prefix = storeId + "|" + docTypeId + "|";
        return documents.keySet().stream().anyMatch(k -> k.startsWith(prefix)
                && (k.equals(prefix + locale) || k.startsWith(prefix + locale + "_")));
    }

    @Override
    public void publishDocument(String storeId, String docTypeId, String locale, String title, String body, String changeNote) {
        documents.put(docKey(storeId, docTypeId, locale), body);
        titles.put(docKey(storeId, docTypeId, locale), title);
    }

    @Override
    public void mergeProfile(String storeId, List<String> juris, Map<String, String> defaults) {
        jurisdictions.addAll(juris);
        defaults.forEach(profile::putIfAbsent);
    }
}
