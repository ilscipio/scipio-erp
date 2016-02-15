package com.ilscipio.cato.ce.demoSuite.dataGenerator;

import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.ofbiz.base.conversion.ConversionException;
import org.ofbiz.base.conversion.JSONConverters;
import org.ofbiz.base.conversion.JSONConverters.JSONToList;
import org.ofbiz.base.conversion.JSONConverters.ListToJSON;
import org.ofbiz.base.lang.JSON;
import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.HttpClient;
import org.ofbiz.base.util.HttpClientException;

import com.ilscipio.cato.ce.demoSuite.dataGenerator.dataObject.DemoDataObject;

import javolution.util.FastList;
import javolution.util.FastMap;

public class MockarooDataGenerator<T extends DemoDataObject> extends ThirdPartyDataGenerator<T> {
    private static String MOCKAROO_DATA_GENERATOR = "mockaroo";

    // private MockarooDataGenerator<T> mockarooDataGenerator;

    private final Class<T> returnObjectClass;

    public MockarooDataGenerator(Class<T> type) {
        this.returnObjectClass = type;
    }

    @Override
    protected List<T> retrieveData(Integer count, String schema, String format) {

        HttpClient httpClient = new HttpClient();
        MockarooSettings settings = new MockarooSettings(count, schema);
        // HashMap<String, Object> queryParameters =
        // settings.getQueryParameters();
        // httpClient.setParameters(queryParameters);
        String url = properties.get("demosuite.test.data.provider." + getDataGeneratorName() + ".url") + format + "?key="
                + settings.getQueryParameters().get("key");
        httpClient.setContentType("application/json");
        httpClient.setUrl(url + "&count=" + count);
        ListToJSON listJsonConverter = new JSONConverters.ListToJSON();
        try {
            JSON json = listJsonConverter.convert(settings.getFields(schema));
            Debug.log(json.toString());
            httpClient.setRawStream(json.toString());

        } catch (ConversionException e1) {
            Debug.logError(e1.getMessage(), "");
        }

        try {
            String r = httpClient.sendHttpRequest(settings.getMethod());
            try {
                Debug.log("json result ===========> " + JSON.from(r).toString());
                JSONToList jsonListConverter = new JSONConverters.JSONToList();
                List<T> resultList = FastList.newInstance();
                for (Object o : jsonListConverter.convert(JSON.from(r))) {
                    resultList.add((T) JSON.from(o).toObject(returnObjectClass));
                }
                return resultList;

            } catch (ConversionException e) {
                Debug.logError(e.getMessage(), "");
            } catch (IOException e) {
                Debug.logError(e.getMessage(), "");
            }

        } catch (HttpClientException e) {
            throw new RuntimeException(e);
        }
        return null;
    }

    // @Override
    // public MockarooDataGenerator<T> create(Class<T> returnObjectClass) {
    // if (mockarooDataGenerator == null)
    // mockarooDataGenerator = new MockarooDataGenerator<T>(returnObjectClass);
    // return mockarooDataGenerator;
    // }

    @Override
    protected String getDataGeneratorName() {
        return MOCKAROO_DATA_GENERATOR;
    }

    public class MockarooSettings extends DataGeneratorSettings {
        private static final long serialVersionUID = 5626474670087711771L;

        private HashMap<String, Object> queryParameters = new HashMap<String, Object>();

        public MockarooSettings(Integer count, String schema) {
            queryParameters.put("count", count);
            // queryParameters.put("schema", schema);
            queryParameters.put("key", properties.get("demosuite.test.data.provider." + getDataGeneratorName() + ".key"));
            // queryParameters.put("fields",
            // properties.get("demosuite.test.data.provider." +
            // getDataGeneratorName() + ".schema." + schema));

        }

        @Override
        public HashMap<String, Object> getQueryParameters() {
            // demosuite.test.data.provider.mockaroo.queryParams = key,
            // callback, count, array, include_header, schema, delimiter, fields
            return queryParameters;
        }

        public List<Object> getFields(String schema) {
            List<Object> fields = new ArrayList<Object>();
            if (schema.equals("address")) {
                Map<String, Object> country = FastMap.newInstance();
                country.put("name", "country");
                country.put("type", "Country");

                Map<String, Object> state = FastMap.newInstance();
                state.put("name", "state");
                state.put("type", "State");

                Map<String, Object> city = FastMap.newInstance();
                city.put("name", "city");
                city.put("type", "City");

                Map<String, Object> street = FastMap.newInstance();
                street.put("name", "street");
                street.put("type", "Street Name");

                Map<String, Object> zip = FastMap.newInstance();
                zip.put("name", "zip");
                zip.put("type", "Postal Code");

                fields.add(country);
                fields.add(state);
                fields.add(city);
                fields.add(street);
                fields.add(zip);
            } else if (schema.equals("product")) {

            }

            return fields;
        }

    }

}
