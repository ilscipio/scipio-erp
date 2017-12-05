package com.ilscipio.scipio.accounting.datev;

import java.util.List;
import java.util.Map;

import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericEntityException;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.entity.condition.EntityCondition;
import org.ofbiz.entity.condition.EntityOperator;
import org.ofbiz.entity.util.EntityQuery;
import org.ofbiz.entity.util.EntityUtil;

import javolution.util.FastMap;

public class DatevHelper {

    private final List<GenericValue> datevTransactionEntryDefinitions;
    private final List<GenericValue> datevMetadataTransactionEntryDefinitions;

    private Map<String, Boolean> metaHeaderFieldsFound = FastMap.newInstance();
    private Map<String, Boolean> headerFieldsFound = FastMap.newInstance();

    public DatevHelper(Delegator delegator) throws GenericEntityException {
        this.datevTransactionEntryDefinitions = EntityQuery.use(delegator)
                .from("DatevTransactionEntryDefinition").where(EntityCondition.makeCondition(EntityOperator.OR, UtilMisc
                        .toList(EntityCondition.makeCondition("metadata", EntityOperator.EQUALS, null), EntityCondition.makeCondition("metadata", EntityOperator.EQUALS, "N"))))
                .queryList();

        this.datevMetadataTransactionEntryDefinitions = EntityQuery.use(delegator).from("DatevTransactionEntryDefinition")
                .where(EntityCondition.makeCondition("metadata", EntityOperator.EQUALS, "Y"), EntityCondition.makeCondition("metadata", EntityOperator.EQUALS, "N")).queryList();

    }

    public void findMetaHeader(Map<String, String> record) {
        List<String> datevMetadataTransactionFieldNames = EntityUtil.getFieldListFromEntityList(datevMetadataTransactionEntryDefinitions, "fieldName", true);

        for (String field : datevMetadataTransactionFieldNames) {
            metaHeaderFieldsFound.put(field, record.containsKey(field));
        }
    }

    public void findHeader(Map<String, String> record) {
        List<String> datevTransactionFieldNames = EntityUtil.getFieldListFromEntityList(datevTransactionEntryDefinitions, "fieldName", true);

        for (String metaField : datevTransactionFieldNames) {
            headerFieldsFound.put(metaField, record.containsKey(metaField));
        }

    }

}
