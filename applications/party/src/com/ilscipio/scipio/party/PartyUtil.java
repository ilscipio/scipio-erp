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
package com.ilscipio.scipio.party;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericEntityException;
import org.ofbiz.entity.GenericValue;

/**
 * SCIPIO: General purpose Party utility functions
 */
public final class PartyUtil {

    private PartyUtil() {
    }


    /**
     * SCIPIO: Return a person's full name. Defaults to partyId if empty.
     *
     * @param delegator
     * @param partyId
     * @return
     * @throws GenericEntityException
     */
    public static String getPersonFullName(Delegator delegator,
            String partyId) throws GenericEntityException {

        GenericValue user = delegator.findOne("Person", true,UtilMisc.toMap("partyId", partyId));

        if (UtilValidate.isNotEmpty(user)) {
            return user.getString("firstName")+" "+user.getString("lastName");
        }
        else {
            return partyId;
        }
    }


}
