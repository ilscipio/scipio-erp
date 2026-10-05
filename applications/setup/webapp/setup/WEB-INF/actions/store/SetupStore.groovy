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
import org.ofbiz.base.util.*;
import org.ofbiz.entity.util.*;
import com.ilscipio.scipio.setup.*;

final module = "SetupStore.groovy";

facilityData = context.facilityData ?: [:];

facilityId = facilityData.facilityId;
partyId = context.partyId;
facilities = null;
if (partyId) {
    facilities = delegator.findByAnd("Facility", ["ownerPartyId":partyId], null, false);
}
context.facilities = facilities;
context.facilityId = facilityId;

// SPECIAL: ProductStore.inventoryFacilityId could have weird config and not be in above list
storeInventoryFacilityOk = true;
inventoryFacilityId = context.productStore?.inventoryFacilityId;
inventoryFacility = null;
if (inventoryFacilityId) {
    storeInventoryFacilityOk = false;
    if (facilities) {
        for(fac in facilities) {
            if (fac.facilityId == inventoryFacilityId) {
                storeInventoryFacilityOk = true;
                inventoryFacility = fac;
                break;
            }
        }
    }
    if (!inventoryFacility) {
        inventoryFacility = delegator.findOne("Facility", [facilityId:inventoryFacilityId], false);
    }
}
context.storeInventoryFacilityOk = storeInventoryFacilityOk;
context.inventoryFacility = inventoryFacility;

context.productStoreFacilityMissing = (context.productStore && !inventoryFacilityId);

// SPECIAL: if there's no productStore yet, we transfer the facility into parameters.inventoryFacilityId
// so that the newly created one will always be preselected
if (!productStore && !parameters.inventoryFacilityId) {
    parameters.inventoryFacilityId = facilityId;
}

partyAcctgPref = context.partyAcctgPref;
if (partyId && partyAcctgPref == null) {
    partyAcctgPref = context.setupStepStates?.accounting?.stepData.partyAcctgPref;
    // TODO: REMOVE THIS FALLBACK ONCE ACCOUNTING WORKS
    if (partyAcctgPref == null && context.setupStepStates?.accounting?.completed != true) {
        partyAcctgPref = delegator.findOne("PartyAcctgPreference", [partyId:partyId], false);
    }
}
context.partyAcctgPref = partyAcctgPref;

currencyUomList = delegator.findByAnd("Uom", [uomTypeId:"CURRENCY_MEASURE"], ["description"], true);
context.currencyUomList = currencyUomList;

defaultDefaultCurrencyUomId = partyAcctgPref?.baseCurrencyUomId ?: context.defaultSystemCurrencyUomId;
context.defaultDefaultCurrencyUomId = defaultDefaultCurrencyUomId;

defaultDefaultLocaleString = UtilProperties.getPropertyValue("scipiosetup", "store.defaultLocaleString");
context.defaultDefaultLocaleString = defaultDefaultLocaleString;

defaultVisualThemeSetId = UtilProperties.getPropertyValue("scipiosetup", "website.visualThemeSetId", "ECOMMERCE");
context.defaultVisualThemeSetId = defaultVisualThemeSetId;

defaultVisualThemeId = UtilProperties.getPropertyValue("scipiosetup", "store.visualThemeId", "EC_DEFAULT");
context.defaultVisualThemeId = defaultVisualThemeId;

visualThemeList = delegator.findByAnd("VisualTheme", [visualThemeSetId:defaultVisualThemeSetId], ["description"], false);
context.visualThemeList = visualThemeList;
