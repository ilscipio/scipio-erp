import java.sql.Timestamp

import org.ofbiz.base.util.Debug
import org.ofbiz.base.util.UtilDateTime
import org.ofbiz.base.util.UtilMisc
import org.ofbiz.base.util.UtilProperties
import org.ofbiz.base.util.UtilRandom
import org.ofbiz.entity.*
import org.ofbiz.entity.util.*
import org.ofbiz.service.ServiceUtil

import com.ilscipio.scipio.ce.demoSuite.dataGenerator.dataObject.AbstractDataObject
import com.ilscipio.scipio.ce.demoSuite.dataGenerator.service.DataGeneratorGroovyBaseScript

public class WorkEffortData extends DataGeneratorGroovyBaseScript {

    WorkEffortData() {
        Debug.logInfo("-=-=-=- DEMO DATA CREATION SERVICE - WORK EFFORT DATA-=-=-=-", "");
    }

    void init() {
        String partyId = (context.userLogin) ? context.userLogin.partyId : "Company";
        workEffortTypeIdsAndStatusKeys = new ArrayList(workEffortTypeIdsAndStatus.keySet());

        context.partyId = partyId;
        context.workEffortTypeIdsAndStatusKeys = workEffortTypeIdsAndStatusKeys;
    }

    List prepareData(int index, AbstractDataObject workEffortData) throws Exception {
        List<GenericValue> toBeStored = new LinkedList<GenericValue>();
        List<GenericValue> workEffortEntrys = new ArrayList<GenericValue>();


        Map<String, Object> workEffortFields = UtilMisc.toMap("workEffortId", workEffortData.getId(), "workEffortTypeId", workEffortTypeId, "currentStatusId", currentStatusId, "workEffortName", workEffortName, "description", workEffortName + " description", "createdDate", createdDate);
        GenericValue workEffort = delegator.makeValue("WorkEffort", workEffortFields);
        toBeStored.add(workEffort);

        fields = UtilMisc.toMap("workEffortId", workEffortId, "partyId", context.partyId, "roleTypeId", "INTERNAL_ORGANIZATIO", "fromDate", createdDate, "statusId", partyStatusId);

        GenericValue workEffortPartyAssignment = delegator.makeValue("WorkEffortPartyAssignment", fields);
        toBeStored.add(workEffortPartyAssignment);

        fields = UtilMisc.toMap("workEffortId", workEffortId, "fixedAssetId", fixedAssetId, "fromDate", createdDate, "statusId", assetStatusId);
        GenericValue workEffortFixedAssetAssign = delegator.makeValue("WorkEffortFixedAssetAssign", fields);
        toBeStored.add(workEffortFixedAssetAssign);


        return toBeStored;
    }
}