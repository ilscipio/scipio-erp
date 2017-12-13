package com.ilscipio.scipio.accounting.datev;

import java.util.Iterator;
import java.util.List;

import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericEntityException;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.entity.condition.EntityCondition;
import org.ofbiz.entity.condition.EntityOperator;
import org.ofbiz.entity.util.EntityQuery;
import org.ofbiz.entity.util.EntityUtil;

import com.ilscipio.scipio.accounting.datev.DatevException.DATEV_ERROR_TYPE;

public class DatevHelper {
    private static final String module = DatevHelper.class.getName();

    private final List<GenericValue> datevTransactionEntryDefinitions;
    private final List<GenericValue> datevMetadataTransactionEntryDefinitions;
    private final List<String> datevTransactionFieldNames;

    // private final ResultSet datevTransactionEntryDefinitionsResultSet;
    // private final ResultSet
    // datevMetadataTransactionEntryDefinitionsResultSet;

    // private Map<String, Boolean> metaHeaderFieldsFound =
    // FastMap.newInstance();
    // private Map<String, Boolean> headerFieldsFound = FastMap.newInstance();

    public DatevHelper(Delegator delegator) throws DatevException {
        try {
            this.datevTransactionEntryDefinitions = EntityQuery.use(delegator).from("DatevTransactionEntryDefinition")
                    .where(EntityCondition.makeConditionWhere("METADATA IS NULL OR METADATA = 'N'")).queryList();

            this.datevTransactionFieldNames = EntityUtil.getFieldListFromEntityList(datevTransactionEntryDefinitions, "fieldName", true);

            this.datevMetadataTransactionEntryDefinitions = EntityQuery.use(delegator).from("DatevTransactionEntryDefinition")
                    .where(EntityCondition.makeCondition("metadata", EntityOperator.EQUALS, "Y")).queryList();
        } catch (GenericEntityException e) {
            throw new DatevException(DATEV_ERROR_TYPE.FATAL, "Internal error. CSV parse error.");
        }
    }

    public boolean isMetaHeader(Iterator<String> metaHeaderIter) throws DatevException {
        boolean isMetaHeader = true;

        for (int i = 0; metaHeaderIter.hasNext(); i++) {
            GenericValue fieldDefinition = null;
            try {
                fieldDefinition = datevMetadataTransactionEntryDefinitions.get(i);
            } catch (IndexOutOfBoundsException e) {
//                throw new DatevException(DATEV_ERROR_TYPE.WARNING,
//                        "Metadata header size doesn't match the expected size [" + datevMetadataTransactionEntryDefinitions.size() + "]");
                isMetaHeader = false;
            }
            if (UtilValidate.isNotEmpty(fieldDefinition)) {
                String metaHeaderValue = metaHeaderIter.next();
                if (!validateField(fieldDefinition, metaHeaderValue)) {
                    isMetaHeader = false;
                    break;
                };
            }
        }

        return isMetaHeader;
    }

    public String[] getDatevTransactionFieldNames() {
        String[] fieldNames = new String[datevTransactionFieldNames.size()];
        return datevTransactionFieldNames.toArray(fieldNames);
    }

    private boolean validateField(GenericValue fieldDefinition, Object value) {
        String fieldName = fieldDefinition.getString("fieldName");
        String type = fieldDefinition.getString("type");
        int length = fieldDefinition.getInteger("length");
        int scale = fieldDefinition.getInteger("scale");
        int maxLength = fieldDefinition.getInteger("maxLength");
        String fieldFormat = fieldDefinition.getString("format");
        boolean required = fieldDefinition.getBoolean("required");

        return true;
    }

}
