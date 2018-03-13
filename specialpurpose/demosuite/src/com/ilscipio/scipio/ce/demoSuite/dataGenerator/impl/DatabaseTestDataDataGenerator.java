package com.ilscipio.scipio.ce.demoSuite.dataGenerator.impl;

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

import com.ilscipio.scipio.ce.demoSuite.dataGenerator.ThirdPartyDataGenerator;
import com.ilscipio.scipio.ce.demoSuite.dataGenerator.dataObject.DemoDataAddress;
import com.ilscipio.scipio.ce.demoSuite.dataGenerator.dataObject.DemoDataObject;
import com.ilscipio.scipio.ce.demoSuite.dataGenerator.dataObject.DemoDataProduct;
import com.ilscipio.scipio.ce.demoSuite.dataGenerator.helper.DemoDataHelper;

import javolution.util.FastList;
import javolution.util.FastMap;

@Deprecated
/**
 * This service doesn't has a proper API to interact with (ie: REST) so it's
 * very hard to get the results. Must be dropped or replaced by something else.
 * 
 * @author jsoto
 *
 * @param <T>
 */
public class DatabaseTestDataDataGenerator extends ThirdPartyDataGenerator {
	private static String DATABASETESTDATA_DATA_GENERATOR = "databaseTestData";

	public DatabaseTestDataDataGenerator(DemoDataHelper helper) {
		super(helper);
	}

	@Override
	protected String getDataGeneratorName() {
		return DATABASETESTDATA_DATA_GENERATOR;
	}

	@Override
	protected List<? extends DemoDataObject> retrieveData() {
		HttpClient httpClient = new HttpClient();
		DatabaseTestDataSettings settings = new DatabaseTestDataSettings();
		String format = properties
				.getProperty("demosuite.test.data.provider." + getDataGeneratorName() + ".exportFormat");
		String url = properties.getProperty("demosuite.test.data.provider." + getDataGeneratorName() + ".url") + "?";
		httpClient.setContentType("application/json");
		httpClient.setUrl(url + "&x__format=" + format + "&x__no_entries=" + helper.getCount());

		JSONToList jsonListConverter = new JSONConverters.JSONToList();
		List<DemoDataObject> resultList = FastList.newInstance();
		
		try {
			String r = httpClient.sendHttpRequest(settings.getMethod());
			for (Object o : jsonListConverter.convert(JSON.from(r))) {
				resultList.add(handleData(o, format));
			}
		} catch (HttpClientException e) {			
		} catch (ConversionException e) {			
		}
		return resultList;
	}

	@Override
	protected DemoDataObject handleData(Object result, String format) {
		return null;
	}

	public class DatabaseTestDataSettings extends DataGeneratorSettings {
		private static final long serialVersionUID = 5626474670087711771L;

		private HashMap<String, Object> queryParameters = new HashMap<String, Object>();

		public DatabaseTestDataSettings() {
			queryParameters.put("key",
					properties.get("demosuite.test.data.provider." + getDataGeneratorName() + ".key"));
		}

		// @Override
		public HashMap<String, Object> getQueryParameters() {
			return queryParameters;
		}

		@Override
		public List<Object> getFields() {
			List<Object> fields = new ArrayList<Object>();
			if (returnObjectClass.equals(DemoDataAddress.class)) {
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
			} else if (returnObjectClass.equals(DemoDataProduct.class)) {
				Map<String, Object> id = FastMap.newInstance();
				id.put("name", "id");
				id.put("type", "Row Number");

				Map<String, Object> name = FastMap.newInstance();
				name.put("name", "name");
				name.put("type", "Words");
				name.put("min", 1);
				name.put("max", 3);

				Map<String, Object> description = FastMap.newInstance();
				description.put("name", "description");
				description.put("type", "Sentences");

				Map<String, Object> price = FastMap.newInstance();
				price.put("name", "price");
				price.put("type", "Money");

				fields.add(id);
				fields.add(name);
				fields.add(description);
				fields.add(price);
			}
			return fields;
		}

	}

}
