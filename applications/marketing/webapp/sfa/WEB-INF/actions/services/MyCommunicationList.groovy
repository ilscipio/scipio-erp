import javolution.util.FastList

import org.ofbiz.base.util.Debug
import org.ofbiz.entity.condition.EntityCondition
import org.ofbiz.entity.condition.EntityOperator
                

Debug.log("partyId =============> " + context.partyId);
List mainAndExprs = FastList.newInstance();
mainAndExprs.add(EntityCondition.makeCondition("statusId", EntityOperator.EQUALS, "COM_UNKNOWN_PARTY"));
mainAndExprs.add(EntityCondition.makeCondition("partyId", EntityOperator.EQUALS, context.partyId));
mainAndExprs.add(EntityCondition.makeCondition("roleStatusId", EntityOperator.NOT_EQUAL, "COM_ROLE_COMPLETED"));
commEventsUnknown = from("CommunicationEventAndRole").where(mainAndExprs).queryList();
Debug.log("commEventsUnknown ============> " + commEventsUnknown);
                
commEventDraft = from("CommunicationEventAndRole").where(["statusId" : "COM_PENDING", "roleTypeId" : "ORIGINATOR", "partyId" : context.partyId]).queryList();
Debug.log("commEventDraft ============> " + commEventDraft);

commEventProgress = from("CommunicationEventAndRole").where(["statusId" : "COM_IN_PROGRESS", "roleTypeId" : "ORIGINATOR", "partyId" : context.partyId]).queryList();
Debug.log("commEventProgress ============> " + commEventProgress);

context.commEventsUnknown = commEventsUnknown;
context.commEventDraft = commEventDraft;
context.commEventProgress = commEventProgress;