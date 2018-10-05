package com.ilscipio.scipio.ce.demoSuite.dataGenerator.helper;

import java.util.List;
import java.util.Map;
import java.util.Properties;

import org.ofbiz.base.util.UtilProperties;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.entity.Delegator;

import com.ilscipio.scipio.ce.demoSuite.dataGenerator.dataObject.DemoDataObject;
import com.ilscipio.scipio.ce.demoSuite.dataGenerator.dataObject.DemoDataOrder;
import com.ilscipio.scipio.ce.demoSuite.dataGenerator.dataObject.party.DemoDataParty;

public abstract class DemoDataHelper {
    private static final Integer DATA_GENERATOR_MAX_RECORDS = UtilProperties.getPropertyAsInteger("demosuite", "demosuite.test.data.max.records", 50);
    
    public enum dataTypeEnum {
        PARTY(DemoDataParty.class), PRODUCT(DemoDataObject.class), ORDER(DemoDataOrder.class), TRANSACTION(DemoDataObject.class);

        private final Class<? extends DemoDataObject> demoDataObject;

        dataTypeEnum(Class<? extends DemoDataObject> o) {
            this.demoDataObject = o;
        }

        Class<? extends DemoDataObject> getReturnObjectClass() {
            return demoDataObject;
        }
    }

    private final Map<String, Object> context;
    private final Delegator delegator;
    private final String dataType;
    private final Integer count;
    private final Properties properties;
    private DataGeneratorSettings settings;

    public DemoDataHelper(Map<String, Object> context, Class<? extends DataGeneratorSettings> settingsClass) throws Exception {
        this.delegator = (Delegator) context.get("delegator");
        this.context = context;
        if (!context.containsKey("dataType"))
            throw new Exception("dataType not found");
        this.dataType = (String) context.get("dataType");
        if (UtilValidate.isEmpty(dataTypeEnum.valueOf(dataType))) {
            throw new Exception("dataType not valid");
        }
        if (context.containsKey("num") && UtilValidate.isNotEmpty(context.get("num"))) {
            this.count = (Integer) context.get("num");
        } else {
            this.count = DATA_GENERATOR_MAX_RECORDS;
        }
        this.settings = settingsClass.getConstructor(Delegator.class).newInstance(delegator);
        this.properties = UtilProperties.getProperties("demosuite");
    }

    public Class<? extends DemoDataObject> getReturnObjectClass() {
        return dataTypeEnum.valueOf(dataType).getReturnObjectClass();
    }

    public String getDataType() {
        return dataType;
    }

    public Integer getCount() {
        return count;
    }

    public DataGeneratorSettings getSettings() {
        return settings;
    }

    public Map<String, Object> getContext() {
        return context;
    }

    public Properties getProperties() {
        return properties;
    }

    public Delegator getDelegator() {
        return delegator;
    }
    
    public boolean generateAddress() {
        return (boolean) getContext().get("generateAddress");
    }

    public boolean generateUserLogin() {
        return (boolean) getContext().get("generateUserLogin");
    }
    
    public boolean generateOrder() {
        return (boolean) getContext().get("generateOrder");        
    }
    
    public boolean generateTransaction() {
        return (boolean) getContext().get("generateTransaction");        
    }
    
    public boolean generateWorkeffort() {
        return (boolean) getContext().get("generateWorkeffort");        
    }

    public static abstract class DataGeneratorSettings {
        public DataGeneratorSettings(Delegator delegator) {
        }

        public abstract List<Object> getFields();
    }

}
