package com.ilscipio.scipio.accounting.external.datev.category;

import java.util.Iterator;
import java.util.Map;

import org.ofbiz.entity.Delegator;

import com.ilscipio.scipio.accounting.external.OperationStats;
import com.ilscipio.scipio.accounting.external.datev.DatevException;
import com.ilscipio.scipio.accounting.external.datev.DatevHelper;

public class DebitorenKreditorenStammdaten extends AbstractDatevDataCategory {

    public DebitorenKreditorenStammdaten(Delegator delegator, DatevHelper datevHelper) {
        super(delegator, datevHelper);
        // TODO Auto-generated constructor stub
    }

    @Override
    public void processRecord(Map<String, String> recordMap) throws DatevException {
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
    public boolean isMetaHeader(Iterator<String> metaHeader) throws DatevException {
        // TODO Auto-generated method stub
        return false;
    }

    @Override
    public Class<? extends OperationStats> getOperationStatsClass() throws DatevException {
        // TODO Auto-generated method stub
        return null;
    }

    @Override
    public String[] getFieldNames() throws DatevException {
        // TODO Auto-generated method stub
        return null;
    }

}
