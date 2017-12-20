package com.ilscipio.scipio.accounting.external.datev;

import java.lang.reflect.Constructor;
import java.util.Iterator;
import java.util.Map;

import org.ofbiz.entity.Delegator;

import com.ilscipio.scipio.accounting.external.OperationStats;
import com.ilscipio.scipio.accounting.external.OperationStats.NotificationLevel;
import com.ilscipio.scipio.accounting.external.OperationStats.NotificationScope;
import com.ilscipio.scipio.accounting.external.OperationStats.Stat;
import com.ilscipio.scipio.accounting.external.datev.category.AbstractDatevDataCategory;

public class DatevHelper {
    private static final String module = DatevHelper.class.getName();

    private final String orgPartyId;
    private final OperationStats stats;
    private final AbstractDatevDataCategory dataCategory;

    public DatevHelper(Delegator delegator, String orgPartyId, Class<? extends AbstractDatevDataCategory> dataCategoryClass) throws DatevException {
        this.orgPartyId = orgPartyId;
        try {
            Constructor<? extends AbstractDatevDataCategory> datevDataCategoryConstructor = dataCategoryClass.getConstructor(Delegator.class, DatevHelper.class);
            this.dataCategory = datevDataCategoryConstructor.newInstance(delegator, this);
            this.stats = dataCategory.getOperationStatsClass().newInstance();
        } catch (Exception e) {
            throw new DatevException("Internal error. Cannot initialize DATEV helper.");
        }

    }

    public void processRecord(int index, Map<String, String> recordMap) throws DatevException {
        dataCategory.processRecord(index, recordMap);
    }

    public String[] getFieldNames() throws DatevException {
        return dataCategory.getFieldNames();
    }

    public boolean validateField(Object o, String value) throws DatevException {
        if (o instanceof String)
            return dataCategory.validateField(String.valueOf(o), value);
        else if (o instanceof Integer)
            return dataCategory.validateField((Integer) o, value);
        return false;
    }

    public String getOrgPartyId() {
        return orgPartyId;
    }

    public boolean isMetaHeader(Iterator<String> metaHeader) throws DatevException {
        return dataCategory.isMetaHeader(metaHeader);
    }

    public OperationStats getStats() {
        return stats;
    }

    public void addRecordStat(String message, NotificationLevel level, int position, Map<String, String> value, boolean valid) {
        addStat(stats.new RecordStat(message, level, position, value, valid));
    }

    public void addStat(String message, NotificationScope scope, NotificationLevel level) {
        addStat(stats.new Stat(message, scope, level));
    }

    private void addStat(Stat stat) {
        stats.addStat(stat);
    }
}
