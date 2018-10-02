package com.ilscipio.scipio.ce.demoSuite.dataGenerator.impl;

import java.util.List;

import com.ilscipio.scipio.ce.demoSuite.dataGenerator.DataGenerator;
import com.ilscipio.scipio.ce.demoSuite.dataGenerator.dataObject.DemoDataObject;
import com.ilscipio.scipio.ce.demoSuite.dataGenerator.helper.DemoDataHelper;

/**
 * Demo data generator that doesn't rely in any external data provider
 * @author jsoto
 *
 */
public class LocalDataGenerator extends DataGenerator {
    
    private final static String LOCAL_DATA_GENERATOR = "local";

    public LocalDataGenerator(DemoDataHelper helper) {
        super(helper);
    }

    @Override
    public List<? extends DemoDataObject> retrieveData() throws Exception {
        
        return null;
    }

    @Override
    public DemoDataObject handleData(Object result, String format) {
        
        return null;
    }

    @Override
    public String getDataGeneratorName() {        
        return LOCAL_DATA_GENERATOR;
    }

}
