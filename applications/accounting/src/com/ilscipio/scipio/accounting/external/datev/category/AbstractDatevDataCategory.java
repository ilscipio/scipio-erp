package com.ilscipio.scipio.accounting.external.datev.category;

import java.util.Iterator;
import java.util.Map;

import org.ofbiz.entity.Delegator;

import com.ilscipio.scipio.accounting.external.OperationStats;
import com.ilscipio.scipio.accounting.external.datev.DatevException;
import com.ilscipio.scipio.accounting.external.datev.DatevHelper;

public abstract class AbstractDatevDataCategory {

    final Delegator delegator;
    final DatevHelper datevHelper;

    public AbstractDatevDataCategory(Delegator delegator, DatevHelper datevHelper) {
        this.delegator = delegator;
        this.datevHelper = datevHelper;
    }

    public abstract void processRecord(Map<String, String> recordMap) throws DatevException;

    public abstract boolean validateField(String fieldName, String value) throws DatevException;

    public abstract boolean validateField(int position, String value) throws DatevException;

    public abstract boolean isMetaHeader(Iterator<String> metaHeader) throws DatevException;

    public abstract Class<? extends OperationStats> getOperationStatsClass() throws DatevException;

    public abstract String[] getFieldNames() throws DatevException;

}
