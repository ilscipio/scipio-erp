/**
 * SCIPIO: SETUP interactive Tax Authorities data prep.
 */

import org.ofbiz.base.util.*;
import org.ofbiz.entity.GenericValue
import org.ofbiz.entity.condition.*;
import org.ofbiz.entity.util.*;
 
final module = "EditTaxAuthorities.groovy";

orgPartyId = context.orgPartyId;

EntityConditionList conds = EntityCondition.makeCondition([
        EntityCondition.makeCondition("partyId", EntityOperator.EQUALS, orgPartyId),
    ],
    EntityOperator.AND
);

List<GenericValue> partyTaxAuthInfos = EntityQuery.use(delegator).from("PartyTaxAuthInfo").where(conds).queryList();

context.partyTaxAuthInfos = partyTaxAuthInfos;