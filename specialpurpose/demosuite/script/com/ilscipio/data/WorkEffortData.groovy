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
import org.ofbiz.base.util.Debug
import org.ofbiz.base.util.UtilMisc
import org.ofbiz.base.util.UtilRandom
import org.ofbiz.entity.*
import org.ofbiz.entity.util.*

import com.ilscipio.scipio.ce.demoSuite.dataGenerator.DataGeneratorProvider
import com.ilscipio.scipio.ce.demoSuite.dataGenerator.dataObject.AbstractDataObject
import com.ilscipio.scipio.ce.demoSuite.dataGenerator.helper.AbstractDemoDataHelper.DataTypeEnum
import com.ilscipio.scipio.ce.demoSuite.dataGenerator.service.DataGeneratorGroovyBaseScript
import com.ilscipio.scipio.ce.demoSuite.dataGenerator.util.DemoSuiteDataGeneratorUtil.DataGeneratorProviders


@DataGeneratorProvider(providers=[DataGeneratorProviders.LOCAL])
public class WorkEffortData extends DataGeneratorGroovyBaseScript {
    private static final String module = "WorkEffortData.groovy";
    
    WorkEffortData() {
        Debug.logInfo("-=-=-=- DEMO DATA CREATION SERVICE - WORK EFFORT DATA-=-=-=-", module);
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
            efo.setOffset(UtilRandom.getRandomInt(0, totalPartyGroupCount - 1));
            //            Debug.log("party group offset ======> " + efo.getOffset());
            partyGroups = from("PartyRole").where("roleTypeId", "INTERNAL_ORGANIZATIO").query(efo);
            if (partyGroups) {
                partyGroupId = partyGroups[0].getString("partyId");
            }
        }
        if (!partyGroupId) {
            throw new Exception("Party group not found or invalid.");
        }
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

        fields = UtilMisc.toMap("workEffortId", workEffortData.getId(), "fixedAssetId", workEffortData.getFixedAsset(), "fromDate", workEffortData.getCreatedDate(), "statusId", workEffortData.getAssetStatus());
        toBeStored.add(delegator.makeValue("WorkEffortFixedAssetAssign", fields));
        return toBeStored;
    }
}