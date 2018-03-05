package com.ilscipio.scipio.ce.demoSuite.dataGenerator;

import java.net.MalformedURLException;
import java.net.URL;
import java.util.HashMap;
import java.util.List;
import java.util.Properties;

import org.ofbiz.base.util.UtilProperties;

import com.ilscipio.scipio.ce.demoSuite.dataGenerator.dataObject.DemoDataObject;

public abstract class ThirdPartyDataGenerator<T extends DemoDataObject> {

    Properties properties = null;

    public ThirdPartyDataGenerator() {
        properties = UtilProperties.getProperties("demosuite.properties");
    }

    protected abstract List<T> retrieveData(Integer count, String api);

    abstract List<T> handleData(String result, String format);

    protected abstract String getDataGeneratorName();

    protected abstract class DataGeneratorSettings extends HashMap<String, Object> {
        private static final long serialVersionUID = -8549187859117423507L;
        private final String method;

        private final URL url;

        public DataGeneratorSettings() {
            try {
                this.url = new URL(properties.getProperty("demosuite.test.data.provider." + getDataGeneratorName() + ".url"));
            } catch (MalformedURLException e) {
                throw new RuntimeException(e);
            }

            this.method = properties.getProperty("demosuite.test.data.provider." + getDataGeneratorName() + ".method");

        }

        public String getMethod() {
            return method;
        }

        public URL getUrl() {
            return url;
        }

        public abstract HashMap<String, Object> getQueryParameters();

        public abstract List<Object> getFields();

    }

}
