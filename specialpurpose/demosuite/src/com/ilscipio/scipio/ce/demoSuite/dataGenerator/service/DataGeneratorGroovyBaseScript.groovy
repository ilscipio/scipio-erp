package com.ilscipio.scipio.ce.demoSuite.dataGenerator.service

import javolution.util.FastList
import javolution.util.FastMap

import org.ofbiz.base.util.Debug
import org.ofbiz.base.util.UtilProperties
import org.ofbiz.base.util.UtilValidate
import org.ofbiz.entity.GenericValue
import org.ofbiz.service.ServiceUtil
import org.ofbiz.service.engine.GroovyBaseScript

abstract class DataGeneratorGroovyBaseScript extends GroovyBaseScript {
    private static final Integer DATA_GENERATOR_MAX_RECORDS = UtilProperties.getPropertyAsInteger("general", "data.generator.max.records", 50);
    private static final String resource_error = "DemoSuiteUiLabels";

    DataGeneratorGroovyBaseScript() {
    }
    
    private static List<Map<String, DataGeneratorStat>> dataGeneratorStats = FastList.newInstance();

    private void storeData(List toBeStored) {
        Map result = FastMap.newInstance();
        if (UtilValidate.isNotEmpty(toBeStored)) {
            Locale locale = (Locale) context.get("locale");
            Debug.log("store data ======> " + toBeStored.size());
            for (GenericValue value : toBeStored) {
                String entityName = value.getEntityName();
                DataGeneratorStat stat;
                if (result.containsKey(entityName)) {
                    stat = result.get(entityName);
                } else {
                    stat = new DataGeneratorStat(entityName);
                }
                try {
                    GenericValue createdValue = delegator.create(value, true, false);
                    if (UtilValidate.isNotEmpty(createdValue)) {
                        int stored = stat.getStored();
                        stat.setStored(stored + 1);
                    } else {
                        int failed = stat.getFailed();
                        stat.setFailed(failed + 1);
                    }
                } catch (Exception e) {
                    stat.setFailed(stat.getFailed()++);
                }
                Debug.log("stat =============> " + stat);
                result.put(entityName, stat);
            }
        }
        dataGeneratorStats.add(result);
    }

    private int getNumRecordsToBeGenerated() {
        Integer num = DATA_GENERATOR_MAX_RECORDS;
        if (context.num && context.num < num)
            num = context.num;
        Debug.log("NumRecordsToBeGenerated ======> " + num);
        return num;
    }

    public abstract List<GenericValue> prepareData();

    public abstract void init();

    def Map run() {
        Map result = ServiceUtil.returnSuccess();
        try {
            init();
            int numRecords = getNumRecordsToBeGenerated();
            for (int i = 0; i < numRecords; i++) {
                List toBeStored = prepareData();
                storeData(toBeStored);
            }
            result.put("generatedDataStats", dataGeneratorStats);
        } catch (Exception e) {
            ServiceUtil.returnError(e.getMessage());
        }
        return result;
    }
}
