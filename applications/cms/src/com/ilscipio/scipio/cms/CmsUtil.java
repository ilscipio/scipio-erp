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
package com.ilscipio.scipio.cms;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilProperties;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericEntityException;

import com.ilscipio.scipio.party.PartyUtil;

/**
 * Scipio CmsUtil - various cms helper methods
 *
 */
public abstract class CmsUtil {

    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    private static final boolean DEBUG = UtilProperties.getPropertyAsBoolean("cms", "debug", false);

    static {
        if (DEBUG) {
            Debug.logInfo("Cms: Debug mode enabled", module);
        }
    }

    private CmsUtil() {

    }

    /**
     * Returns a person's full name in a manner appropriate for display within CMS.
     *
     * @param delegator
     * @param partyId
     * @return
     * @throws GenericEntityException
     */
    public static String getPersonDisplayName(Delegator delegator,
            String partyId) throws GenericEntityException {
        // Currently delegates to a standard method.
        return PartyUtil.getPersonFullName(delegator, partyId);
    }

    /**
     * Substitute (factored out control) for Debug.verboseOn(). Callers should check this first
     * and then log with Debug.logInfo, NOT Debug.logVerbose, for now (to be reviewed)...
     * NOTE: This method is more log-message oriented compared to {@link #debugOn()}.
     */
    public static boolean verboseOn() {
        //return Debug.verboseOn() || DEBUG;
        return DEBUG;
    }

    /**
     * Substitute (factored out control) for Debug.verboseOn(). Callers should check this first
     * and then log with Debug.logInfo, NOT Debug.logVerbose, for now (to be reviewed)...
     * NOTE: This method is more functionality-oriented compared to {@link #verboseOn()}.
     */
    public static boolean debugOn() {
        return DEBUG;
    }

}
