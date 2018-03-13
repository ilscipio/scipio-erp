package com.ilscipio.scipio.ce.demoSuite.dataGenerator.service

import java.sql.Timestamp

import org.ofbiz.base.util.Debug
import org.ofbiz.base.util.UtilDateTime
import org.ofbiz.base.util.UtilProperties
import org.ofbiz.base.util.UtilValidate
import org.ofbiz.entity.GenericValue
import org.ofbiz.entity.transaction.TransactionUtil;
import org.ofbiz.service.ServiceUtil
import org.ofbiz.service.engine.GroovyBaseScript

import com.ilscipio.scipio.ce.demoSuite.dataGenerator.DataGenerator
import com.ilscipio.scipio.ce.demoSuite.dataGenerator.dataObject.DemoDataObject
import com.ilscipio.scipio.ce.demoSuite.dataGenerator.helper.DemoDataHelper

import javolution.util.FastList
import javolution.util.FastMap


// FIXME?: revisit reuse pattern; in Ofbiz GroovyBaseScript is not meant to be hardcoded
abstract class DataGeneratorGroovyBaseScript extends GroovyBaseScript {
    private static final Integer DATA_GENERATOR_MAX_RECORDS = UtilProperties.getPropertyAsInteger("demosuite", "demosuite.test.data.max.records", 50);
	private static final String DATA_GENERATOR_DEFAULT_PROVIDER = UtilProperties.getPropertyValue("demosuite", "demosuite.test.data.default.provider", "jfairy");
	
    private static final String resource_error = "DemoSuiteUiLabels";
    public static final String module = DataGeneratorGroovyBaseScript.class.getName();
    
    private List<Map<String, DataGeneratorStat>> dataGeneratorStats;
    private int totalFailed = 0;
    private int totalStored = 0;
    
    private int numRecords = DATA_GENERATOR_MAX_RECORDS;

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
                    boolean beginTransaction = TransactionUtil.begin();                    
                    GenericValue createdValue = delegator.create(value);
                    TransactionUtil.commit(beginTransaction)
                    if (UtilValidate.isEmpty(createdValue))
                        throw new Exception("createdValue is null");
                    int stored = stat.getStored();                   
                    stat.setStored(stored + 1);
                    totalStored++;
                    stat.getGeneratedValues().add(createdValue);                            
                } catch (Exception e) {
                    TransactionUtil.rollback();                    
                    int failed = stat.getFailed();                    
                    stat.setFailed(failed + 1);       
                    totalFailed++;
                }                
                result.put(entityName, stat);                
            }
        }
        dataGeneratorStats.add(result);
    }

//    /**
//     * Checks the number of times prepareData() must be looped. 
//     * If no 'num' (service param) is passed or it is greater than the max defined in general.properties#data.generator.max.records, 
//     * that max value is taken instead.
//     * @return
//     */
//    public void setNumRecordsToBeGenerated(numRecords) {
//        this.numRecords = numRecords;
//    }
    

    /**
     * The method that extended classes must override in order to prepare the data to be generated. 
     * It is meant to be called within a loop in run() n times, where n is determined by getNumRecordsToBeGenerated().
     * @return
     */
    public abstract List<GenericValue> prepareData(int index, DemoDataObject data) throws Exception;

    /**
     * All the logic that will be used later on in the prepareData() must be initialized here. 
     * It basically comprehends all the stuff that doesn't need to be looped later. 
     * Devs are responsible to set all that stuff in context in order to be available in prepareData()
     */
    public abstract void init();
	
	/**
	 * 
	 */
	public abstract String getDataType();	
	
    /**
     * Generates and stores demo data.
     */
    def Map run() {
        try {
			context.dataType = getDataType();
            sanitizeDates();
			initDataGenerator();
            init();     
			DataGenerator generator = context.generator;
			List<DemoDataObject> data = generator.retrieveData();
			numRecords = data.size();			
            for (int i = 0; i < numRecords; i++) {
                List toBeStored = prepareData(i, data.get(i));
                if (toBeStored)
                    storeData(toBeStored);
            }
        } catch (Exception e) {
            Debug.logError(e, module);
            // TODO: localize (but exception message cannot be localized)
            return ServiceUtil.returnError("Fatal error while generating data (aborted): " + e.getMessage());
        }
        // TODO: localize 
        Map result = (totalFailed > 0) ? ServiceUtil.returnFailure("Failed to store " + totalFailed 
            + " records (" + totalStored + " stored successfully)") : ServiceUtil.returnSuccess();
        result.put("generatedDataStats", dataGeneratorStats);
        return result;
    }
    
    /**
     * Sanitizes minDate & maxDate context params. If any inconsistency is found, 
     * they will be automatically updated with a span of time of 30 days between them. 
     * It is recommended to use UtilRandom.generateRandomTimestamp(context) within prepareData()
     * in order to get a random date between them.
     */
    private void sanitizeDates() {
        Timestamp minDate = null;
        Timestamp maxDate = null;        
        if (context.minDate)
            minDate = context.minDate;
        if (context.maxDate)
            maxDate = context.maxDate;
        
        if (minDate && maxDate) {            
            int intervalDays = UtilDateTime.getIntervalInDays(minDate, maxDate);
            // If minDate is greater than maxDate, set maxDate to the current time and minDate 30 days before
            if (intervalDays < 0) {
                maxDate = UtilDateTime.nowTimestamp();
                minDate = UtilDateTime.adjustTimestamp(maxDate, Calendar.DAY_OF_YEAR, -30);
            }
        } else if (!minDate && maxDate) {
            //  If minDate is not present but maxDate is, set minDate 3 months before maxDate
            minDate = UtilDateTime.adjustTimestamp(maxDate, Calendar.DAY_OF_YEAR, -30);
        } else if (minDate && !maxDate) {
            //  If maxDate is not present but minDate is, set maxDate to current time. 
            // If that makes makes minDate greater than maxDate, set minDate 30 before current date.
            maxDate = UtilDateTime.nowTimestamp();
            int intervalDays = UtilDateTime.getIntervalInDays(minDate, maxDate);
            if (intervalDays < 0) {                
                minDate = UtilDateTime.adjustTimestamp(maxDate, Calendar.DAY_OF_YEAR, -30);
            }
        } else if (!minDate && !maxDate) {
            maxDate = UtilDateTime.nowTimestamp();
            minDate = UtilDateTime.adjustTimestamp(maxDate, Calendar.DAY_OF_YEAR, -30);
        }    
        context.minDate = minDate;
        context.maxDate = maxDate;
    }
	
	private void initDataGenerator() {
		String dataGeneratorProvider = context.dataGeneratorProvider;
		if (!dataGeneratorProvider) {
			dataGeneratorProvider = getDefaultProvider();
		}
		Class<? extends DataGenerator> clazz = Class.forName(UtilProperties.getPropertyValue("demosuite", "demosuite.test.data.provider." + dataGeneratorProvider + ".class"));
		Class<? extends DemoDataHelper> helperClazz = null;
		String demoDataHelperClass = UtilProperties.getPropertyValue("demosuite", "demosuite.test.data.provider." + dataGeneratorProvider + "." + getDataType() + ".helper.class");
		
		if (demoDataHelperClass) {
			helperClazz = Class.forName(demoDataHelperClass);
		} else {
			demoDataHelperClass = UtilProperties.getPropertyValue("demosuite", "demosuite.test.data.provider.default.party.helper.class");
			helperClazz = Class.forName(demoDataHelperClass);
		}
		try {
			DemoDataHelper helper = (DemoDataHelper) helperClazz.getConstructor(Map.class).newInstance(context);
			DataGenerator generator = (DataGenerator) clazz.getConstructor(DemoDataHelper.class).newInstance(helper);
			context.generator = generator;
		} catch (Exception e) {
			Debug.logError(e, module);
		}	
	}
	
	
	public String getDefaultProvider() {
		return DATA_GENERATOR_DEFAULT_PROVIDER;
	}
}
