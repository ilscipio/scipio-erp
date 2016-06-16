package com.ilscipio.scipio.ce.demoSuite.dataGenerator.service

import javolution.util.FastMap

import org.ofbiz.base.util.Debug
import org.ofbiz.base.util.UtilProperties
import org.ofbiz.base.util.UtilValidate
import org.ofbiz.entity.GenericEntityException
import org.ofbiz.service.ServiceUtil
import org.ofbiz.service.engine.GroovyBaseScript

abstract class DataGeneratorGroovyBaseScript extends GroovyBaseScript {
    private static final Integer DATA_GENERATOR_MAX_RECORDS = UtilProperties.getPropertyAsInteger("general", "data.generator.max.records", 50);
    private static final String resource_error = "DemoSuiteUiLabels";
    
    private LinkedList toBeStored = null;
    
    public int numRecords = 0;
    
    Map storeData() {        
        Map result = FastMap.newInstance();
        if (UtilValidate.isNotEmpty(toBeStored)) {
            Locale locale = (Locale) context.get("locale");        
            Debug.log("store data ======> " + toBeStored.size());
            try {
                delegator.storeAll(toBeStored);
//                result.put("generatedDataStats", toBeStored);
            } catch (GenericEntityException e) {
                return ServiceUtil.returnError(UtilProperties.getMessage(resource_error, "OrderErrorCannotStoreStatusChanges", locale) + e.getMessage());
            }
        }
        return result;
    }
    
    private int getNumRecordsToBeGenerated() {
        Integer num = DATA_GENERATOR_MAX_RECORDS;
        if (context.num && context.num < num)
            num = context.num;
        Debug.log("NumRecordsToBeGenerated ======> " + num);
        return num;
    }

    public abstract LinkedList prepareData();
    
    def run() {
        numRecords = getNumRecordsToBeGenerated();
        toBeStored = prepareData();
        storeData();
    }

}
