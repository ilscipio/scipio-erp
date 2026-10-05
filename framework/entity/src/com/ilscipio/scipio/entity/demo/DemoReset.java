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
package com.ilscipio.scipio.entity.demo;

import java.io.InputStream;
import java.math.BigDecimal;
import java.net.URL;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilProperties;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.entity.model.ModelEntity;
import org.ofbiz.entity.util.EntityDataLoader;

/**
 * SCIPIO: 4.0.0: Public demo support. The rows of the shipped data files are the baseline of a demo instance, so the
 * demo can undo what visitors change: user preferences at each login (LoginWorker) and the CMS every hour
 * (cmsDemoReset). Off unless general.properties sets demo.reset.enabled=true. Never switch it on for a real store:
 * the CMS reset deletes CMS rows that are not in the data files.
 * <p>
 * The baseline holds only the entities in {@link #ENTITIES} and entities whose name starts with "Cms". It is read
 * once, on first use, from the data files of the readers in demo.reset.readers.
 */
public final class DemoReset {

    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    /** Entities besides the Cms* entities that the baseline keeps. */
    public static final Set<String> ENTITIES = Collections.unmodifiableSet(new LinkedHashSet<>(Arrays.asList(
            "UserLogin", "UserPreference", "Content", "ContentAssoc", "DataResource", "DataResourceAttribute", "ElectronicText",
            "ImageDataResource", "VideoDataResource", "AudioDataResource", "DocumentDataResource", "OtherDataResource")));

    private static final Map<String, Map<String, List<GenericValue>>> baselines = new ConcurrentHashMap<>();

    private DemoReset() {
    }

    public static boolean isEnabled() {
        return UtilProperties.getPropertyAsBoolean("general", "demo.reset.enabled", false);
    }

    /** The data file rows of the entity (empty list when none); reads the data files on the first call. */
    public static List<GenericValue> getBaseline(Delegator delegator, String entityName) {
        List<GenericValue> values = getBaselines(delegator).get(entityName);
        return values != null ? values : Collections.emptyList();
    }

    private static Map<String, List<GenericValue>> getBaselines(Delegator delegator) {
        Map<String, List<GenericValue>> baseline = baselines.get(delegator.getDelegatorName());
        if (baseline == null) {
            synchronized (DemoReset.class) {
                baseline = baselines.get(delegator.getDelegatorName());
                if (baseline == null) {
                    baseline = readBaseline(delegator);
                    baselines.put(delegator.getDelegatorName(), baseline);
                }
            }
        }
        return baseline;
    }

    private static boolean isBaselineEntity(String entityName) {
        return ENTITIES.contains(entityName) || entityName.startsWith("Cms");
    }

    private static Map<String, List<GenericValue>> readBaseline(Delegator delegator) {
        long start = System.currentTimeMillis();
        List<String> readers = new ArrayList<>();
        for (String r : UtilProperties.getPropertyValue("general", "demo.reset.readers", "seed,seed-initial,demo,ext,ext-demo").split(",")) {
            if (!r.trim().isEmpty()) {
                readers.add(r.trim());
            }
        }
        String helperName = delegator.getGroupHelperName("org.ofbiz");
        Map<String, List<GenericValue>> out = new HashMap<>();
        int files = 0;
        int rows = 0;
        for (URL url : EntityDataLoader.getUrlList(helperName, readers)) {
            // most data files hold none of the baseline entities: skip them without an XML parse
            String text;
            try (InputStream in = url.openStream()) {
                text = new String(in.readAllBytes(), StandardCharsets.UTF_8);
            } catch (Exception e) {
                Debug.logWarning("Demo reset: cannot read data file " + url + ": " + e.getMessage(), module);
                continue;
            }
            final String content = text;
            if (!content.contains("<Cms") && ENTITIES.stream().noneMatch(n -> content.contains("<" + n + " ") || content.contains("<" + n + ">"))) {
                continue;
            }
            try {
                for (GenericValue value : delegator.readXmlDocument(url)) {
                    if (isBaselineEntity(value.getEntityName())) {
                        out.computeIfAbsent(value.getEntityName(), k -> new ArrayList<>()).add(value);
                        rows++;
                    }
                }
                files++;
            } catch (Exception e) {
                Debug.logWarning("Demo reset: cannot parse data file " + url + ": " + e.getMessage(), module);
            }
        }
        Debug.logInfo("Demo reset: baseline of " + rows + " rows from " + files + " data files (readers " + readers + ") in "
                + (System.currentTimeMillis() - start) + " ms", module);
        return out;
    }

    /** The primary key of a value as a map key. */
    public static String pkKey(GenericValue value) {
        return value.getEntityName() + ":" + value.getPkShortValueString();
    }

    /** Index of values by {@link #pkKey}; a later value with the same key (a later data file) wins, as in a data load. */
    public static Map<String, GenericValue> byPk(Collection<GenericValue> values) {
        Map<String, GenericValue> out = new HashMap<>();
        for (GenericValue v : values) {
            out.put(pkKey(v), v);
        }
        return out;
    }

    /** True if a field that the data file row sets (not a key or stamp field) has another value in the database row. */
    public static boolean differs(GenericValue fileValue, GenericValue dbValue) {
        ModelEntity model = fileValue.getModelEntity();
        for (String field : model.getNoPkFieldNames()) {
            if (ModelEntity.STAMP_FIELD_LIST.contains(field) || !fileValue.containsKey(field)) {
                continue;
            }
            Object a = fileValue.get(field);
            Object b = dbValue.get(field);
            if (a instanceof BigDecimal && b instanceof BigDecimal) {
                if (((BigDecimal) a).compareTo((BigDecimal) b) != 0) {
                    return true;
                }
            } else if (!Objects.equals(a, b)) {
                return true;
            }
        }
        return false;
    }
}
