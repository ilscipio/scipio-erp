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

import java.util.HashMap;
import org.ofbiz.party.contact.ContactMechWorker;
import org.ofbiz.base.util.UtilHttp;
import org.ofbiz.base.util.UtilMisc;

// SCIPIO: Some patches to prevent crash for missing userLogin

/* puts the following in the context: "contactMech", "contactMechId",
        "partyContactMech", "partyContactMechPurposes", "contactMechTypeId",
        "contactMechType", "purposeTypes", "postalAddress", "telecomNumber",
        "requestName", "donePage", "tryEntity", "contactMechTypes"
 */
target = [:];
ContactMechWorker.getContactMechAndRelated(request, userLogin?.partyId, target);
context.putAll(target);


if (!security.hasEntityPermission("PARTYMGR", "_VIEW", request) && !context.partyContactMech && context.contactMech) {
    context.canNotView = true;
} else {
    context.canNotView = false;
}

preContactMechTypeId = parameters.preContactMechTypeId;
if (preContactMechTypeId) context.preContactMechTypeId = preContactMechTypeId;

paymentMethodId = parameters.paymentMethodId;
if (paymentMethodId) context.paymentMethodId = paymentMethodId;

cmNewPurposeTypeId = parameters.contactMechPurposeTypeId;
if (cmNewPurposeTypeId) {
    contactMechPurposeType = from("ContactMechPurposeType").where("contactMechPurposeTypeId", cmNewPurposeTypeId).queryOne();
    if (contactMechPurposeType) {
        context.contactMechPurposeType = contactMechPurposeType;
    } else {
        cmNewPurposeTypeId = null;
    }
    context.cmNewPurposeTypeId = cmNewPurposeTypeId;
}

tryEntity = context.tryEntity;

contactMechData = context.contactMech;
if (!tryEntity) contactMechData = parameters;
if (!contactMechData) contactMechData = [:];
if (contactMechData) context.contactMechData = contactMechData;

partyContactMechData = context.partyContactMech;
if (!tryEntity) partyContactMechData = parameters;
if (!partyContactMechData) partyContactMechData = [:];
if (partyContactMechData) context.partyContactMechData = partyContactMechData;

postalAddressData = context.postalAddress;
if (!tryEntity) postalAddressData = parameters;
if (!postalAddressData) postalAddressData = [:];
if (postalAddressData) context.postalAddressData = postalAddressData;

telecomNumberData = context.telecomNumber;
if (!tryEntity) telecomNumberData = parameters;
if (!telecomNumberData) telecomNumberData = [:];
if (telecomNumberData) context.telecomNumberData = telecomNumberData;

// load the geo names for selected countries and states/regions
if (parameters.countryGeoId) {
    geoValue = from("Geo").where("geoId", parameters.countryGeoId).cache(true).queryOne();
    if (geoValue) {
        context.selectedCountryName = geoValue.geoName;
    }
} else if (postalAddressData?.countryGeoId) {
    geoValue = from("Geo").where("geoId", postalAddressData.countryGeoId).cache(true).queryOne();
    if (geoValue) {
        context.selectedCountryName = geoValue.geoName;
    }
}

if (parameters.stateProvinceGeoId) {
    geoValue = from("Geo").where("geoId", parameters.stateProvinceGeoId).cache(true).queryOne();
    if (geoValue) {
        context.selectedStateName = geoValue.geoId;
    }
} else if (postalAddressData?.stateProvinceGeoId) {
    geoValue = from("Geo").where("geoId", postalAddressData.stateProvinceGeoId).cache(true).queryOne();
    if (geoValue) {
        context.selectedStateName = geoValue.geoId;
    }
}
