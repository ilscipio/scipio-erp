package com.ilscipio.scipio.accounting.external.datev.category;

import java.util.Map;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericEntityException;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.entity.condition.EntityCondition;
import org.ofbiz.entity.condition.EntityJoinOperator;
import org.ofbiz.entity.util.EntityUtil;

import com.ilscipio.scipio.accounting.external.BaseOperationResults;
import com.ilscipio.scipio.accounting.external.BaseOperationStats;
import com.ilscipio.scipio.accounting.external.datev.DatevException;
import com.ilscipio.scipio.accounting.external.datev.DatevHelper;

public class DatevDebitorenKreditorenStammdaten extends AbstractDatevDataCategory {

    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    public DatevDebitorenKreditorenStammdaten(Delegator delegator, DatevHelper datevHelper) throws DatevException {
        super(delegator, datevHelper);
    }

    @Override
    public void processRecord(int index, Map<String, String> recordMap) throws DatevException {
        Map<String, GenericValue> recordsToStore = UtilMisc.newMap();
        try {
            for (String fieldName : recordMap.keySet()) {
//                String fieldId = fieldDefinition.getString("fieldId");
//                String fieldName = fieldDefinition.getString("fieldName");
                String value = recordMap.get(fieldName);
                // if (Debug.isOn(Debug.VERBOSE)) {
                Debug.logInfo("Processing record [" + index + "] field [" + fieldName + "]: " + value, module);
                // }
                
                GenericValue fieldDefinition = EntityUtil.getFirst(EntityUtil.filterByAnd(getDatevFieldDefinitions(), UtilMisc.toMap("fieldName", fieldName)));
                GenericValue mappingDefinition = getDatevMappingDefinitions().get(fieldDefinition.getString("fieldId"));
                if (UtilValidate.isNotEmpty(mappingDefinition)) {
                    String entityName = mappingDefinition.getString("entityName");
                    String entityField = mappingDefinition.getString("entityField");

                    if (recordsToStore.containsKey(entityName)) {
                        GenericValue recordToStore = recordsToStore.get(entityName);
                        recordToStore.put(fieldName, value);
                    } else {
                        recordsToStore.put(entityName, delegator.create(entityName, UtilMisc.toMap(entityField, value)));
                    }
                } else {
                    // TODO: Add custom implementation here
                }
            }

            for (String entity : recordsToStore.keySet()) {
                GenericValue recordToStore = recordsToStore.get(entity);
                if (!recordToStore.containsPrimaryKey()) {
                    recordToStore.setNextSeqId();
                }
                recordToStore.store();
            }
        } catch (GenericEntityException e) {
            Debug.logError(e, module);
        }
    }

    @Override
    public boolean validateField(String fieldName, String value) throws DatevException {
        return validateField(
                EntityUtil.getFirst(EntityUtil.filterByCondition(getDatevFieldDefinitions(), EntityCondition.makeCondition("fieldName", EntityJoinOperator.EQUALS, fieldName))),
                value);
    }

    @Override
    public boolean validateField(int position, String value) throws DatevException {
        return validateField(
                EntityUtil.getFirst(EntityUtil.filterByCondition(getDatevFieldDefinitions(), EntityCondition.makeCondition("sequenceNum", EntityJoinOperator.EQUALS, position))),
                value);
    }

    @Override
    public Class<? extends BaseOperationStats> getOperationStatsClass() throws DatevException {
        return null;
    }

    @Override
    public Class<? extends BaseOperationResults> getOperationResultsClass() throws DatevException {
        return null;
    }

}
