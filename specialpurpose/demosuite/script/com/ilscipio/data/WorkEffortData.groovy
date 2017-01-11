import java.sql.Timestamp

import org.ofbiz.base.util.Debug
import org.ofbiz.base.util.UtilDateTime
import org.ofbiz.base.util.UtilMisc
import org.ofbiz.base.util.UtilProperties
import org.ofbiz.base.util.UtilRandom
import org.ofbiz.entity.*
import org.ofbiz.entity.util.*
import org.ofbiz.service.ServiceUtil

import com.ilscipio.scipio.ce.demoSuite.dataGenerator.service.DataGeneratorGroovyBaseScript

public class WorkEffortData extends DataGeneratorGroovyBaseScript {

    WorkEffortData() {
        Debug.logInfo("-=-=-=- DEMO DATA CREATION SERVICE - WORK EFFORT DATA-=-=-=-", "");
    }

    // WorkEffortTypeIds
    final static List<String> workEffortTypeIds = [
        "ACTIVITY",
        "ASSET_USAGE",
        "AVAILABLE",
        "BUSINESS_TRAVEL",
        "EVENT",
        "MEETING",
        "MILESTONE",
        "PERSONAL_TIMEOFF",
        "PHASE",
        "PHASE_TEMPLATE",
        "PROD_ORDER_HEADER",
        "PROD_ORDER_TASK",
        "PROGRAM",
        "PROJECT",
        "PROJECT_TEMPLATE",
        "PUBLIC_HOLIDAY",
        "PUBLISH_PROPS",
        "ROU_TASK",
        "ROUTING",
        "SCRUM_PROJECT",
        "SCRUM_SPRINT",
        "SCRUM_TASK",
        "SCRUM_TASK_ERROR",
        "SCRUM_TASK_IMPL",
        "SCRUM_TASK_INST",
        "SCRUM_TASK_TEST",
        "TASK",
        "TASK_TEMPLATE",
        "TEMPLATE",
        "TRAINING",
        "WORK_FLOW"
    ]

    // workEffortTypeIdsAndStatus
    final static Map<String, List<String>> workEffortTypeIdsAndStatus = [
        "TASK" : [
            "CAL_DECLINED",
            "CAL_DELEGATED",
            "CAL_COMPLETED",
            "CAL_CANCELLED",
            "CAL_ACCEPTED"
        ],
        "PROD_ORDER_TASK" : [
            "PRUN_CANCELLED",
            "PRUN_COMPLETED",
            "PRUN_CLOSED",
            "PRUN_CREATED",
            "PRUN_RUNNING",
            "PRUN_SCHEDULED",
            "PRUN_DOC_PRINTED"
        ],
        "EVENT" : [
            "CAL_DECLINED",
            "CAL_DELEGATED",
            "CAL_COMPLETED",
            "CAL_CANCELLED",
            "CAL_CONFIRMED",
            "CAL_TENTATIVE"
        ],
        "ACTIVITY" : [
            "CAL_DECLINED",
            "CAL_DELEGATED",
            "CAL_COMPLETED",
            "CAL_CANCELLED",
            "CAL_NEEDS_ACTION",
            "CAL_SENT"]
    ]

    final static Map<String, List<String>> fixedAssetAndTypes = [
        "EQUIPMENT" : [
            "DEMO_FORKLIFT_01",
            "DEMO_FORKLIFT_02",
            "DEMO_HVAC_01",
            "DEMO_HVAC_02",
            "DEMO_PROJECTOR"
        ],
        "VEHICLE" : [
            "DEMO_VEHICLE_01",
            "DEMO_VEHICLE_02"
        ],
        "GROUP_EQUIPMENT" : [
            "DEMO_BOOK_GROUP",
            "DEMO_FOOD_GROUP",
            "DEMO_MACHINE_GROUP",
            "WORKCENTER_COST"
        ],
        "PRODUCTION_EQUIPMENT" : [
            "DEMO_BOOK",
            "DEMO_FOOD",
            "DEMO_MACHINE",
            "DEMO_PROD_EQUIPMT_1",
            "DEMO_PROD_EQUIPMT_2"]
    ]

    final static List<String> workEffortPartyAssignmentStatus = [
        "PRTYASGN_ASSIGNED",
        "PRTYASGN_OFFERED",
        "PRTYASGN_UNASSIGNED"
    ]
    final static List<String> workEffortAssetAssignmentStatus = [
        "FA_ASGN_ASSIGNED",
        "FA_ASGN_DENIED",
        "FA_ASGN_REQUESTED"
    ]

    void init() {
        String partyId = (context.userLogin) ? context.userLogin.partyId : "Company";
        workEffortTypeIdsAndStatusKeys = new ArrayList(workEffortTypeIdsAndStatus.keySet());
        
        context.partyId = partyId;
        context.workEffortTypeIdsAndStatusKeys = workEffortTypeIdsAndStatusKeys;
    }

    List prepareData(int index) throws Exception {
        List<GenericValue> toBeStored = new LinkedList<GenericValue>();
        List<GenericValue> workEffortEntrys = new ArrayList<GenericValue>();
        Map fields;
        
        if (Boolean.TRUE.equals(context.autoCreatePartyRoles)) {
            fields = ["partyId": context.partyId, "roleTypeId" : "INTERNAL_ORGANIZATIO"];
            GenericValue partyRole = delegator.findOne("PartyRole", fields, false);
            if (partyRole == null) {
                partyRole = delegator.makeValue("PartyRole", fields);
                toBeStored.add(partyRole);
            }
        }

        String workEffortId = "GEN_" + delegator.getNextSeqId("demo-workEffortId");
        index = UtilRandom.random(context.workEffortTypeIdsAndStatusKeys);
        String workEffortTypeId = context.workEffortTypeIdsAndStatusKeys.get(index);
        workEffortTypeIdsAndStatusList = workEffortTypeIdsAndStatus.get(workEffortTypeId);
        String currentStatusId = workEffortTypeIdsAndStatusList.get(UtilRandom.random(workEffortTypeIdsAndStatusList));
        String workEffortName = "Demo WorkEffort " + workEffortId;
        minDate = UtilDateTime.nowDate();
        if (context.minDate != null)
            minDate = new Date(context.minDate.getTime());

        Timestamp createdDate = Timestamp.valueOf(UtilRandom.generateRandomDate(minDate, context));
        fields = UtilMisc.toMap("workEffortId", workEffortId, "workEffortTypeId", workEffortTypeId, "currentStatusId", currentStatusId, "workEffortName", workEffortName, "description", workEffortName + " description", "createdDate", createdDate);
        GenericValue workEffort = delegator.makeValue("WorkEffort", fields);
        toBeStored.add(workEffort);

        String partyStatusId = workEffortPartyAssignmentStatus.get(UtilRandom.random(workEffortPartyAssignmentStatus));
        fields = UtilMisc.toMap("workEffortId", workEffortId, "partyId", context.partyId, "roleTypeId", "INTERNAL_ORGANIZATIO", "fromDate", createdDate, "statusId", partyStatusId);
        GenericValue workEffortPartyAssignment = delegator.makeValue("WorkEffortPartyAssignment", fields);
        toBeStored.add(workEffortPartyAssignment);

        String assetStatusId = workEffortAssetAssignmentStatus.get(UtilRandom.random(workEffortAssetAssignmentStatus));
        String fixedAssetTypeId;
        if (workEffortTypeId.equals("TASK"))
            fixedAssetTypeId = "EQUIPMENT";
        else if (workEffortTypeId.equals("PROD_ORDER_TASK"))
            fixedAssetTypeId = "PRODUCTION_EQUIPMENT";
        else if (workEffortTypeId.equals("EVENT"))
            fixedAssetTypeId = "GROUP_EQUIPMENT";
        else if (workEffortTypeId.equals("ACTIVITY"))
            fixedAssetTypeId = "VEHICLE";
        else
            fixedAssetTypeId = "VEHICLE";

        fixedAssetAndTypesList = fixedAssetAndTypes.get(fixedAssetTypeId);
        fixedAssetId = fixedAssetAndTypesList.get(UtilRandom.random(fixedAssetAndTypesList));

        fields = UtilMisc.toMap("workEffortId", workEffortId, "fixedAssetId", fixedAssetId, "fromDate", createdDate, "statusId", assetStatusId);
        GenericValue workEffortFixedAssetAssign = delegator.makeValue("WorkEffortFixedAssetAssign", fields);
        toBeStored.add(workEffortFixedAssetAssign);

        return toBeStored;
    }
}