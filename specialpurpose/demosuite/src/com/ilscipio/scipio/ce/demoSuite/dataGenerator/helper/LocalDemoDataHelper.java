package com.ilscipio.scipio.ce.demoSuite.dataGenerator.helper;

import java.util.List;
import java.util.Locale;
import java.util.Map;

import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericEntityException;

public class LocalDemoDataHelper extends DemoDataHelper {

    private Locale locale;

    public LocalDemoDataHelper(Map<String, Object> context) throws Exception {
        super(context, LocalSettings.class);
    }

    public Locale getLocale() {
        return locale;
    }

    public void setLocale(Locale locale) {
        this.locale = locale;
    }

    public static class LocalSettings extends DataGeneratorSettings {
        public LocalSettings(Delegator delegator) throws GenericEntityException {
            super(delegator);
        }

        @Override
        public List<Object> getFields() {
            return null;
        }
    }

}
