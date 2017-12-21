package com.ilscipio.scipio.accounting.external.datev.category;

import java.util.Map;

import org.ofbiz.base.util.Debug;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.condition.EntityCondition;
import org.ofbiz.entity.condition.EntityJoinOperator;
import org.ofbiz.entity.util.EntityUtil;

import com.ilscipio.scipio.accounting.external.OperationResults;
import com.ilscipio.scipio.accounting.external.OperationStats;
import com.ilscipio.scipio.accounting.external.datev.DatevException;
import com.ilscipio.scipio.accounting.external.datev.DatevHelper;
import com.ilscipio.scipio.accounting.external.datev.results.DatevBuchungsstapelResults;
import com.ilscipio.scipio.accounting.external.datev.stats.DatevBuchungsstapelStats;

public class DatevBuchungsstapel extends AbstractDatevDataCategory {
    private static final String module = DatevBuchungsstapel.class.getName();

    public DatevBuchungsstapel(Delegator delegator, DatevHelper datevHelper) throws DatevException {
        super(delegator, datevHelper);
    }

    @Override
    public void processRecord(int index, Map<String, String> recordMap) throws DatevException {
        for (String fieldName : getDatevFieldNames()) {
            String value = recordMap.get(fieldName);
            if (Debug.isOn(Debug.VERBOSE)) {
                Debug.logInfo("Processing record [" + index + "] field [" + fieldName + "]: " + value, module);
            }

        }
    }

    @Override
    public boolean validateField(int position, String value) throws DatevException {
        return validateField(
                EntityUtil.getFirst(EntityUtil.filterByCondition(getDatevFieldDefinitions(), EntityCondition.makeCondition("sequenceNum", EntityJoinOperator.EQUALS, position))),
                value);
    }

    @Override
    public boolean validateField(String fieldName, String value) throws DatevException {
        return validateField(
                EntityUtil.getFirst(EntityUtil.filterByCondition(getDatevFieldDefinitions(), EntityCondition.makeCondition("fieldName", EntityJoinOperator.EQUALS, fieldName))),
                value);
    }

    @Override
    public Class<? extends OperationStats> getOperationStatsClass() throws DatevException {
        return DatevBuchungsstapelStats.class;
    }

    @Override
    public Class<? extends OperationResults> getOperationResultsClass() throws DatevException {
        return DatevBuchungsstapelResults.class;
    }

}
