package com.ilscipio.scipio.accounting.external.datev.category;

import java.util.Map;

import org.ofbiz.entity.Delegator;

import com.ilscipio.scipio.accounting.external.OperationResults;
import com.ilscipio.scipio.accounting.external.OperationStats;
import com.ilscipio.scipio.accounting.external.datev.DatevException;
import com.ilscipio.scipio.accounting.external.datev.DatevHelper;

public class DatevTextschlussel extends AbstractDatevDataCategory {

    public DatevTextschlussel(Delegator delegator, DatevHelper datevHelper) throws DatevException {
        super(delegator, datevHelper);
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
    public Class<? extends OperationStats> getOperationStatsClass() throws DatevException {
        // TODO Auto-generated method stub
        return null;
    }

    @Override
    public Class<? extends OperationResults> getOperationResultsClass() throws DatevException {
        // TODO Auto-generated method stub
        return null;
    }

}
