package com.ilscipio.cato.ce.demoSuite.dataGenerator;

import java.util.List;

import com.ilscipio.cato.ce.demoSuite.dataGenerator.dataObject.DemoDataObject;

public class DatabaseTestDataDataGenerator<T extends DemoDataObject> extends ThirdPartyDataGenerator<T> {
    private static String DATABASETESTDATA_DATA_GENERATOR = "databaseTestData";

//    private DatabaseTestDataDataGenerator<T> databaseTestDataDataGenerator;

    private final Class<T> returnObjectType;

    public DatabaseTestDataDataGenerator(Class<T> returnObjectType) {
        this.returnObjectType = returnObjectType;
    }

//    @Override
//    protected ThirdPartyDataGenerator<T> create(Class<T> returnObjectType) {
//        if (databaseTestDataDataGenerator == null)
//            databaseTestDataDataGenerator = new DatabaseTestDataDataGenerator<T>(returnObjectType);
//        return null;
//    }

    @Override
    protected String getDataGeneratorName() {
        return DATABASETESTDATA_DATA_GENERATOR;
    }

    @Override
    protected List<T> retrieveData(Integer count, String schema, String format) {
        return null;
    }

}
