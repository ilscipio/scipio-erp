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
