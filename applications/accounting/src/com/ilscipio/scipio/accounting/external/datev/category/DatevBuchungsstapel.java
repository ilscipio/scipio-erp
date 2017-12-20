package com.ilscipio.scipio.accounting.external.datev.category;

import java.util.List;
import java.util.Map;

import org.ofbiz.base.util.Debug;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericEntityException;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.entity.condition.EntityCondition;
import org.ofbiz.entity.condition.EntityJoinOperator;
import org.ofbiz.entity.util.EntityQuery;
import org.ofbiz.entity.util.EntityUtil;

import com.ilscipio.scipio.accounting.external.OperationStats;
import com.ilscipio.scipio.accounting.external.datev.DatevException;
import com.ilscipio.scipio.accounting.external.datev.DatevHelper;
import com.ilscipio.scipio.accounting.external.datev.stats.DatevBuchungsstapelStats;

public class DatevBuchungsstapel extends AbstractDatevDataCategory {
    private static final String module = DatevBuchungsstapel.class.getName();

    private final List<GenericValue> datevTransactionEntryDefinitions;
    private final List<String> datevTransactionFieldNames;

    public DatevBuchungsstapel(Delegator delegator, DatevHelper datevHelper) throws DatevException {
        super(delegator, datevHelper);
        try {
            EntityCondition datevFieldDefinitionsCond = EntityCondition.makeCondition("dataCategoryId", EntityJoinOperator.EQUALS, "BUCHUNGSSTAPEL");

            this.datevTransactionEntryDefinitions = EntityQuery.use(delegator).from("DatevFieldDefinition").where(EntityCondition.makeCondition(datevFieldDefinitionsCond),
                    EntityJoinOperator.AND, EntityCondition.makeConditionWhere("METADATA IS NULL OR METADATA = 'N'")).queryList();
            this.datevTransactionFieldNames = EntityUtil.getFieldListFromEntityList(datevTransactionEntryDefinitions, "fieldName", true);

        } catch (GenericEntityException e) {
            throw new DatevException("Internal error. Cannot initialize DATEV importer tool.");
        }
    }

    @Override
    public String[] getFieldNames() {
        String[] fieldNames = new String[datevTransactionFieldNames.size()];
        return datevTransactionFieldNames.toArray(fieldNames);
    }

    @Override
    public void processRecord(int index, Map<String, String> recordMap) throws DatevException {
        for (String fieldName : datevTransactionFieldNames) {
            String value = recordMap.get(fieldName);
            if (Debug.isOn(Debug.VERBOSE)) {
                Debug.logInfo("Processing record [" + index + "] field [" + fieldName + "]: " + value, module);
            }

        }
    }

    @Override
    public boolean validateField(int position, String value) throws DatevException {
        return validateField(EntityUtil.getFirst(
                EntityUtil.filterByCondition(datevTransactionEntryDefinitions, EntityCondition.makeCondition("sequenceNum", EntityJoinOperator.EQUALS, position))), value);
    }

    @Override
    public boolean validateField(String fieldName, String value) throws DatevException {
        return validateField(EntityUtil
                .getFirst(EntityUtil.filterByCondition(datevTransactionEntryDefinitions, EntityCondition.makeCondition("fieldName", EntityJoinOperator.EQUALS, fieldName))), value);
    }

    @Override
    public Class<? extends OperationStats> getOperationStatsClass() throws DatevException {
        return DatevBuchungsstapelStats.class;
    }

}
