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
package com.ilscipio.scipio.acp;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Locale;
import java.util.stream.Collectors;

import org.ofbiz.base.util.UtilProperties;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.util.EntityUtilProperties;

/**
 * SCIPIO: 4.0.0: ACP settings (acp.properties). A value is read in this order: the environment variable
 * {@code ACP_<KEY>_<STORE>}, the key {@code <key>.<productStoreId>}, the environment variable {@code ACP_<KEY>},
 * then {@code <key>} (SystemProperty first, then the file). Dots become underscores in variable names.
 */
public final class AcpConfig {

    public static final String RESOURCE = "acp";

    /** Seam for tests: environment lookup. */
    static volatile java.util.function.Function<String, String> env = System::getenv;

    private AcpConfig() {}

    public static String get(Delegator delegator, String productStoreId, String key, String def) {
        if (UtilValidate.isNotEmpty(productStoreId)) {
            String v = envValue(key + "." + productStoreId);
            if (v == null) v = prop(delegator, key + "." + productStoreId);
            if (v != null) return v;
        }
        String v = envValue(key);
        if (v == null) v = prop(delegator, key);
        return v != null ? v : def;
    }

    public static String get(Delegator delegator, String key, String def) {
        return get(delegator, null, key, def);
    }

    public static int getInt(Delegator delegator, String key, int def) {
        try {
            return Integer.parseInt(get(delegator, key, String.valueOf(def)).trim());
        } catch (NumberFormatException e) {
            return def;
        }
    }

    public static List<String> getList(Delegator delegator, String key) {
        String v = get(delegator, key, "");
        if (UtilValidate.isEmpty(v)) return Collections.emptyList();
        return Arrays.stream(v.split(",")).map(String::trim).filter(s -> !s.isEmpty()).collect(Collectors.toList());
    }

    public static boolean isEnabled(Delegator delegator) {
        return !"false".equalsIgnoreCase(get(delegator, "enabled", "true").trim());
    }

    public static String apiVersion(Delegator delegator) {
        return get(delegator, "api.version", "2026-04-17").trim();
    }

    public static List<String> supportedVersions(Delegator delegator) {
        List<String> l = getList(delegator, "api.supportedVersions");
        return l.isEmpty() ? Collections.singletonList(apiVersion(delegator)) : l;
    }

    static String envName(String key) {
        // stripe.secretKey -> ACP_STRIPE_SECRET_KEY
        return "ACP_" + key.replaceAll("([a-z0-9])([A-Z])", "$1_$2").replace('.', '_').replace('-', '_').toUpperCase(Locale.ROOT);
    }

    private static String envValue(String key) {
        String v;
        try {
            v = env.apply(envName(key));
        } catch (SecurityException e) {
            return null;
        }
        return UtilValidate.isEmpty(v) ? null : v.trim();
    }

    private static String prop(Delegator delegator, String key) {
        String v = delegator != null ? EntityUtilProperties.getPropertyValue(RESOURCE, key, delegator)
                : UtilProperties.getPropertyValue(RESOURCE, key);
        return UtilValidate.isEmpty(v) ? null : v.trim();
    }
}
