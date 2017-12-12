package com.ilscipio.scipio.accounting.datev;

import java.util.List;
import java.util.Map;

import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericEntityException;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.entity.condition.EntityCondition;
import org.ofbiz.entity.condition.EntityOperator;
import org.ofbiz.entity.util.EntityQuery;
import org.ofbiz.entity.util.EntityUtil;

public class DatevHelper {
    private static final String module = DatevHelper.class.getName();

    private final List<GenericValue> datevTransactionEntryDefinitions;
    private final List<GenericValue> datevMetadataTransactionEntryDefinitions;
    private final List<String> datevTransactionFieldNames;
    private final List<String> datevMetadataTransactionFieldNames;

    // private final ResultSet datevTransactionEntryDefinitionsResultSet;
    // private final ResultSet
    // datevMetadataTransactionEntryDefinitionsResultSet;

    // private Map<String, Boolean> metaHeaderFieldsFound =
    // FastMap.newInstance();
    // private Map<String, Boolean> headerFieldsFound = FastMap.newInstance();

    public DatevHelper(Delegator delegator) throws GenericEntityException {
        this.datevTransactionEntryDefinitions = EntityQuery.use(delegator).from("DatevTransactionEntryDefinition")
                .where(EntityCondition.makeConditionWhere("METADATA IS NULL OR METADATA = 'N'")).queryList();
        this.datevTransactionFieldNames = EntityUtil.getFieldListFromEntityList(datevTransactionEntryDefinitions, "fieldName", true);

        this.datevMetadataTransactionEntryDefinitions = EntityQuery.use(delegator).from("DatevTransactionEntryDefinition")
                .where(EntityCondition.makeCondition("metadata", EntityOperator.EQUALS, "Y")).queryList();
        this.datevMetadataTransactionFieldNames = EntityUtil.getFieldListFromEntityList(datevMetadataTransactionEntryDefinitions, "fieldName", true);
    }

    public boolean isHeader(Map<String, String> record) {
        boolean isHeader = true;

        for (String field : datevMetadataTransactionFieldNames) {
            if (!datevMetadataTransactionEntryDefinitions.contains(field)) {
                isHeader = false;
                break;
            }
        }

        if (!isHeader)
            isHeader = true;
        else
            return isHeader;

        for (String field : datevTransactionFieldNames) {
            if (!datevTransactionEntryDefinitions.contains(field)) {
                isHeader = false;
                break;
            }
        }

        return isHeader;
    }

    public String[] getDatevTransactionFieldNames() {
        return (String[]) datevTransactionFieldNames.toArray();
    }

    public Object[] getDatevMetadataTransactionFieldNames() {
        return datevMetadataTransactionFieldNames.toArray();
    }

}
