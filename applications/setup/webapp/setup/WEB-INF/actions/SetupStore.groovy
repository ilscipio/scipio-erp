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

/*
<drop-down allow-empty="false" no-current-selected-key="${defaultOrganizationPartyCurrencyUomId}">
<entity-options key-field-name="uomId" description="${description} - ${abbreviation}" entity-name="Uom">
    <entity-constraint name="uomTypeId" operator="equals" value="CURRENCY_MEASURE"/>
    <entity-order-by field-name="description"/>
</entity-options>
</drop-down>
*/
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
