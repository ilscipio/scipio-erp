import org.ofbiz.base.util.Debug
import org.ofbiz.base.util.UtilMisc
import org.ofbiz.base.util.UtilRandom
import org.ofbiz.entity.*
import org.ofbiz.entity.util.*

import com.ilscipio.scipio.ce.demoSuite.dataGenerator.dataObject.AbstractDataObject
import com.ilscipio.scipio.ce.demoSuite.dataGenerator.helper.AbstractDemoDataHelper.DataTypeEnum
import com.ilscipio.scipio.ce.demoSuite.dataGenerator.service.DataGeneratorGroovyBaseScript

public class WorkEffortData extends DataGeneratorGroovyBaseScript {

    WorkEffortData() {
        Debug.logInfo("-=-=-=- DEMO DATA CREATION SERVICE - WORK EFFORT DATA-=-=-=-", "");
    }

    public String getDataType() {
        return DataTypeEnum.WORKEFFORT;
    }

    void init() {
        partyGroupCount = from("PartyRole").where("roleTypeId", "INTERNAL_ORGANIZATIO").queryCount();
        if (partyGroupCount == 0) {
            throw new Exception("This service depends on party group data to be present. Please load party group data or generate party group demo data first and try again.");
        }
        totalPartyGroupCount = (partyGroupCount  < Integer.MAX_VALUE) ? (int) partyGroupCount : Integer.MAX_VALUE - 1;

        String partyGroupId = context.partyGroupId ?: null;

        EntityFindOptions efo = new EntityFindOptions();
        efo.setMaxRows(1);

        // If no partyGroupId is passed, pick one randomly
        if (!partyGroupId) {
            efo.setOffset(UtilRandom.getRandomInt(0, context.totalPartyGroupCount - 1));
            //            Debug.log("party group offset ======> " + efo.getOffset());
            partyGroups = from("PartyRole").where("roleTypeId", "INTERNAL_ORGANIZATIO").query(efo);
            if (partyGroups) {
                partyGroupId = partyGroups[0].getString("partyId");
            }
        }
        if (!partyGroupId)
            throw new Exception("Party group not found or invalid.");

        context.partyGroupId = partyGroupId;
    }

    List prepareData(int index, AbstractDataObject workEffortData) throws Exception {
        List<GenericValue> toBeStored = new LinkedList<GenericValue>();
        List<GenericValue> workEffortEntrys = new ArrayList<GenericValue>();

        Map<String, Object> workEffortFields = UtilMisc.toMap("workEffortId", workEffortData.getId(), "workEffortTypeId", workEffortData.getType(), "currentStatusId", workEffortData.getStatus(),
                "workEffortName", workEffortData.getName(), "description", workEffortData.getName() + " description", "createdDate", workEffortData.getCreatedDate());
        toBeStored.add(delegator.makeValue("WorkEffort", workEffortFields));

        fields = UtilMisc.toMap("workEffortId", workEffortData.getId(), "partyId", context.partyGroupId, "roleTypeId", "INTERNAL_ORGANIZATIO", "fromDate",
                workEffortData.getCreatedDate(), "statusId", workEffortData.getPartyStatus());
        toBeStored.add(delegator.makeValue("WorkEffortPartyAssignment", fields));

        fields = UtilMisc.toMap("workEffortId", workEffortData.getId(), "fixedAssetId", workEffortData.getAssetStatus(), "fromDate", workEffortData.getCreatedDate(), "statusId", workEffortData.getStatus());
        toBeStored.add(delegator.makeValue("WorkEffortFixedAssetAssign", fields));
        return toBeStored;
    }
}