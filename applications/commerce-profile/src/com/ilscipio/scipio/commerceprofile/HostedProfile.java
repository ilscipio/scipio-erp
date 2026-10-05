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
package com.ilscipio.scipio.commerceprofile;

import java.util.concurrent.atomic.AtomicBoolean;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilProperties;

/**
 * SCIPIO: 4.0.0: The switch of the hosted profile (blueprint section 5, 09 section 6.3).
 *
 * <p>The system property {@code scipio.hosted} wins over the property in {@code commerce-profile.properties}.
 * The pod JVM sets {@code -Dscipio.hosted=true}. The switch fails closed: only an explicit {@code false} turns the
 * hosted rules off. Any other value ({@code 1}, {@code yes}, {@code TRUE }) and a missing value mean hosted; an
 * unknown value logs a warning. The file in the source tree says {@code false}, so a self-hosted server and the dev
 * server keep the full code rights.</p>
 */
public final class HostedProfile {

    private static final String MODULE = HostedProfile.class.getName();

    public static final String RESOURCE = "commerce-profile";
    public static final String HOSTED_PROPERTY = "scipio.hosted";

    /** Test seam: replaces the property lookup (null = system property, then properties file). */
    static volatile Boolean hostedOverride;

    private static final AtomicBoolean WARNED = new AtomicBoolean();

    private HostedProfile() {}

    public static boolean isHosted() {
        Boolean override = hostedOverride;
        if (override != null) return override;
        String value = System.getProperty(HOSTED_PROPERTY);
        if (value == null || value.trim().isEmpty()) {
            value = UtilProperties.getPropertyValue(RESOURCE, HOSTED_PROPERTY);
        }
        return parse(value);
    }

    /** Fail closed: true unless the value is an explicit "false" (any case, spaces allowed). */
    static boolean parse(String value) {
        String v = value != null ? value.trim() : "";
        if (v.equalsIgnoreCase("false")) return false;
        if (!v.equalsIgnoreCase("true") && WARNED.compareAndSet(false, true)) {
            Debug.logWarning("Hosted profile: " + HOSTED_PROPERTY + " is " + (v.isEmpty() ? "missing" : "\"" + v + "\"")
                    + " (not true or false); the hosted rules stay ON", MODULE);
        }
        return true;
    }
}
