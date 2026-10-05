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
/**
 * SCIPIO: prepares tax authority list and entries for display in a drop-down.
 */

taxAuthorityList = from('TaxAuthority').orderBy("taxAuthPartyId", "taxAuthGeoId").queryList();
context.taxAuthorityList = taxAuthorityList;
 
infoList = [];
if (taxAuthorityList) {
    for(taxAuthority in taxAuthorityList) {
        def info = [:];
        info.putAll(taxAuthority);
        info.taxAuthCombinedId = taxAuthority.taxAuthGeoId + "::" + taxAuthority.taxAuthPartyId;
        info.party = from("PartyNameView").where("partyId", taxAuthority.taxAuthPartyId).queryOne();
        info.geo = from("Geo").where("geoId", taxAuthority.taxAuthGeoId).queryOne();
        infoList.add(info);
    }
}
context.taxAuthorityInfoList = infoList;
