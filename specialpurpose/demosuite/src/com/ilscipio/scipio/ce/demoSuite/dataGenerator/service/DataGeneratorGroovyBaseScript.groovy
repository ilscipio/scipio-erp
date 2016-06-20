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

    private List<Map<String, DataGeneratorStat>> dataGeneratorStats;

    DataGeneratorGroovyBaseScript() {
        dataGeneratorStats = FastList.newInstance();
    }

    /**
     * Stores the list of values passed, collecting stats per entity. 
     * It is meant to be called within a loop in run() n times, where n is determined by getNumRecordsToBeGenerated().
     * @param toBeStored
     */
    private void storeData(List toBeStored) {
        Map result = FastMap.newInstance();
        if (UtilValidate.isNotEmpty(toBeStored)) {
            Locale locale = (Locale) context.get("locale");
            for (GenericValue value : toBeStored) {
                String entityName = value.getEntityName();
                DataGeneratorStat stat;
                if (result.containsKey(entityName)) {
                    stat = result.get(entityName);                    
                } else {                    
                    stat = new DataGeneratorStat(entityName);                    
                }                
                try {
                    GenericValue createdValue = delegator.create(value);
                    if (UtilValidate.isEmpty(createdValue))
                        throw new Exception("createdValue is null");
                    int stored = stat.getStored();                   
                    stat.setStored(stored + 1);                             
                } catch (Exception e) {
                    int failed = stat.getFailed();                    
                    stat.setFailed(failed + 1);                    
                }                
                result.put(entityName, stat);                
            }
        }
        dataGeneratorStats.add(result);
    }

    /**
     * Checks the number of times prepareData() must be looped. 
     * If no 'num' (service param) is passed or it is greater than the max defined in general.properties#data.generator.max.records, 
     * that max value is taken instead.
     * @return
     */
    public int getNumRecordsToBeGenerated() {
        Integer num = DATA_GENERATOR_MAX_RECORDS;
        if (context.num && context.num < num)
            num = context.num;
        return num;
    }

    /**
     * The method that extended classes must override in order to prepare the data to be generated. 
     * It is meant to be called within a loop in run() n times, where n is determined by getNumRecordsToBeGenerated().
     * @return
     */
    public abstract List<GenericValue> prepareData(int index);

    /**
     * All the logic that will be used later on in the prepareData() must be initialized here. 
     * It basically comprehends all the stuff that doesn't need to be looped later. 
     * Devs are responsible to set all that stuff in context in order to be available in prepareData()
     */
    public abstract void init();

    /**
     * Generates and stores demo data.
     */
    def Map run() {
        Map result = ServiceUtil.returnSuccess();
        try {
            init();
            int numRecords = getNumRecordsToBeGenerated();
            for (int i = 0; i < numRecords; i++) {
                List toBeStored = prepareData(i);
                storeData(toBeStored);
            }
            result.put("generatedDataStats", dataGeneratorStats);
        } catch (Exception e) {
            Debug.logError(e.getMessage(), "");
            ServiceUtil.returnError(e.getMessage());
        }
        return result;
    }
}
