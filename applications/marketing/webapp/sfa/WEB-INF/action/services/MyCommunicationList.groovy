import org.ofbiz.base.util.Debug
import org.ofbiz.entity.condition.EntityCondition
import org.ofbiz.entity.condition.EntityOperator

                
List mainAndExprs = [];

mainAndExprs.add(EntityCondition.makeCondition("partyId", EntityOperator.EQUALS, context.partyId));

statusId = parameters.statusId;
if (statusId)
    mainAndExprs.add(EntityCondition.makeCondition("statusId", EntityOperator.EQUALS, statusId));

roleStatusId = parameters.roleStatusId;
if (roleStatusId)
    mainAndExprs.add(EntityCondition.makeCondition("roleStatusId", EntityOperator.EQUALS, roleStatusId));
    
partyIdTo = parameters.partyIdTo;    
if (partyIdTo)
    mainAndExprs.add(EntityCondition.makeCondition("partyIdTo", EntityOperator.EQUALS, partyIdTo));
    
commEventList = from("CommunicationEventAndRole").where(mainAndExprs).queryList();

context.commEventList = commEventList;