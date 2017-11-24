/**
 * SCIPIO: SETUP interactive Fiscal Periods data prep.
 */

import org.ofbiz.base.util.*;
import org.ofbiz.entity.GenericValue
import org.ofbiz.entity.condition.*;
import org.ofbiz.entity.util.*;
 
final module = "EditJournals.groovy";

orgPartyId = context.orgPartyId;

EntityConditionList conds = EntityCondition.makeCondition([    
        EntityCondition.makeCondition("organizationPartyId", EntityOperator.EQUALS, orgPartyId),
    ],
    EntityOperator.AND
);

List<GenericValue> glJournals = EntityQuery.use(delegator).from("GlJournal").where(conds).queryList();
for (GenericValue glJournal in glJournals) {
    Debug.log("Gl Journal id =====>  " + glJournal.glJournalId);    
}

context.glJournals = glJournals;