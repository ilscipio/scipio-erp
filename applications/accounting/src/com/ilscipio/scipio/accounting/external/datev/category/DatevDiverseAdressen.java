package com.ilscipio.scipio.accounting.external.datev.category;

import java.util.Map;

import org.ofbiz.entity.Delegator;

import com.ilscipio.scipio.accounting.external.BaseOperationResults;
import com.ilscipio.scipio.accounting.external.BaseOperationStats;
import com.ilscipio.scipio.accounting.external.datev.DatevException;
import com.ilscipio.scipio.accounting.external.datev.DatevHelper;

public class DatevDiverseAdressen extends AbstractDatevDataCategory {

    public DatevDiverseAdressen(Delegator delegator, DatevHelper datevHelper) throws DatevException {
        super(delegator, datevHelper);
        // TODO Auto-generated constructor stub
    }

    @Override
    public void processRecord(int index, Map<String, String> recordMap) throws DatevException {
        // TODO Auto-generated method stub
    }

    @Override
    public boolean validateField(String fieldName, String value) throws DatevException {
        // TODO Auto-generated method stub
        return false;
    }

    @Override
    public boolean validateField(int position, String value) throws DatevException {
        // TODO Auto-generated method stub
        return false;
    }

    @Override
    public Class<? extends BaseOperationStats> getOperationStatsClass() throws DatevException {
        // TODO Auto-generated method stub
        return null;
    }

    @Override
    public Class<? extends BaseOperationResults> getOperationResultsClass() throws DatevException {
        // TODO Auto-generated method stub
        return null;
    }

}
