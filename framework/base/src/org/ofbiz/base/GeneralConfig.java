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
package org.ofbiz.base;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.base.util.UtilProperties;
import org.ofbiz.base.util.string.FlexibleStringExpander;

import java.net.InetAddress;
import java.util.HashMap;
import java.util.Map;

/**
 * General/base system config - some configurations in general.properties - facade class to help prevent dependency issues (SCIPIO).
 * NOTE: This class does not have access to most applications, intentionally.
 */
public abstract class GeneralConfig {
    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    private static final String instanceId = FlexibleStringExpander.expandString( // SCIPIO: Added simple expansion support + moved from JobManager
            UtilProperties.getPropertyValue("general", "unique.instanceId", "scipio0"),
            UtilMisc.toMap());

    private static final InetAddress inetAddress; // moved from VisitHandler
    static {
        InetAddress tmpAddress = null;
        try {
            tmpAddress = InetAddress.getLocalHost();
        } catch (java.net.UnknownHostException e) {
            Debug.logError("Unable to get server's internet address (system may crash): " + e.toString(), module);
        }
        inetAddress = tmpAddress;
    }

    private static final Map<String, Object> commonPropertiesMap = makeCommonPropertiesMap(new HashMap<>());

    /**
     * Returns the instanceId configured in general.properties.
     */
    public static String getInstanceId() {
        return instanceId;
    }

    /**
     * Returns the current server's localhost address and hostname.
     */
    public static InetAddress getLocalhostAddress() { return inetAddress; }

    /** TODO: REVIEW: WARN: Currently this can return a static map, but this might forcefully change in future */
    public static Map<String, Object> getCommonPropertiesMap() { return commonPropertiesMap; }

    public static <T extends Map<String, Object>> T makeCommonPropertiesMap(T out) {
        out.put("instanceId", getInstanceId());
        out.put("localHostName", getLocalhostAddress().getHostName());
        out.put("localIpAddr", getLocalhostAddress().getHostName());
        return out;
    }
}
