package com.ilscipio.scipio.ce.demoSuite.dataGenerator.helper;

import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericEntityException;

import java.util.List;
import java.util.Locale;
import java.util.Map;

public class JFairyDemoDataHelper extends AbstractDemoDataHelper {

    private Locale locale;

    public JFairyDemoDataHelper(Map<String, Object> context) throws Exception {
        super(context, JFairySettings.class);
    }

    public Locale getLocale() {
        return locale;
    }

    public void setLocale(Locale locale) {
        this.locale = locale;
    }

    public boolean generateEmailAddress() {
        return (boolean) getContext().get("generateEmailAddress");
    }
    public boolean generateAddress() {
        return (boolean) getContext().get("generateAddress");
    }

    public boolean generateUserLogin() {
        return (boolean) getContext().get("generateUserLogin");
    }

    public static class JFairySettings extends DataGeneratorSettings {

        public JFairySettings(Delegator delegator) throws GenericEntityException {
            super(delegator);
        }

        @Override
        public List<Object> getFields() {
            return null;
        }

    }

}
