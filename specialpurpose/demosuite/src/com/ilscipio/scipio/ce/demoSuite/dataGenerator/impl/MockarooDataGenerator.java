package com.ilscipio.scipio.ce.demoSuite.dataGenerator.impl;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.ofbiz.base.conversion.JSONConverters;
import org.ofbiz.base.conversion.JSONConverters.JSONToList;
import org.ofbiz.base.lang.JSON;
import org.ofbiz.base.util.HttpClient;
import org.ofbiz.base.util.HttpClientException;

import com.ilscipio.scipio.ce.demoSuite.dataGenerator.ThirdPartyDataGenerator;
import com.ilscipio.scipio.ce.demoSuite.dataGenerator.dataObject.DemoDataAddress;
import com.ilscipio.scipio.ce.demoSuite.dataGenerator.dataObject.DemoDataObject;
import com.ilscipio.scipio.ce.demoSuite.dataGenerator.dataObject.DemoDataPerson;
import com.ilscipio.scipio.ce.demoSuite.dataGenerator.dataObject.DemoDataProduct;
import com.ilscipio.scipio.ce.demoSuite.dataGenerator.dataObject.DemoDataUserLogin;
import com.ilscipio.scipio.ce.demoSuite.dataGenerator.helper.DemoDataHelper;

import javolution.util.FastList;
import javolution.util.FastMap;

public class MockarooDataGenerator extends ThirdPartyDataGenerator {
	private static String MOCKAROO_DATA_GENERATOR = "mockaroo";

	public MockarooDataGenerator(DemoDataHelper helper) {
		super(helper);
	}

	@Override
	protected List<? extends DemoDataObject> retrieveData() throws Exception {
		// if (UtilValidate.isEmpty(args))
		// throw new Exception("Invalid arguments. This engine requires one
		// argument to be passed that represents the api");
		// String api = args[0];
		// FIXME: Find a way to pass api properly
		String api = "";
		HttpClient httpClient = new HttpClient();
		MockarooSettings settings = new MockarooSettings();
		String format = properties
				.getProperty("demosuite.test.data.provider." + getDataGeneratorName() + ".exportFormat");
		StringBuilder url = new StringBuilder(settings.getUrl().toString());
		url.append(properties.getProperty("demosuite.test.data.provider." + getDataGeneratorName() + ".api." + api));
		url.append("." + format);
		url.append("?key=" + settings.getQueryParameters().get("key"));
		url.append("&count=" + helper.getCount() + "&array=true");
		httpClient.setContentType("application/json");
		httpClient.setUrl(url.toString());
		try {
			httpClient.setAllowUntrusted(true);
			String r = httpClient.sendHttpRequest(settings.getMethod());
			
			JSONToList jsonListConverter = new JSONConverters.JSONToList();
			List<DemoDataObject> resultList = FastList.newInstance();

			for (Object o : jsonListConverter.convert(JSON.from(r))) {
				resultList.add(handleData(o, format));			}
			
			return resultList;

		} catch (HttpClientException e) {
			throw new RuntimeException(e);
		}
	}

	@Override
	protected String getDataGeneratorName() {
		return MOCKAROO_DATA_GENERATOR;
	}

	public class MockarooSettings extends DataGeneratorSettings {
		private static final long serialVersionUID = 5626474670087711771L;

		private final HashMap<String, Object> queryParameters = new HashMap<String, Object>();
		// private final Map<String, Map<String, Object>> schemas;

		public MockarooSettings() {
			queryParameters.put("key",
					properties.get("demosuite.test.data.provider." + getDataGeneratorName() + ".key"));
			// Map<String, Map<String, Object>> schemas = new HashMap<>();
			// UtilProperties.extractPropertiesWithPrefixAndId(schemas,
			// properties, "demosuite.test.data.provider." +
			// getDataGeneratorName() + ".api.");
			// this.schemas = schemas;
		}

		// @Override
		public HashMap<String, Object> getQueryParameters() {
			// demosuite.test.data.provider.mockaroo.queryParams = key,
			// callback, count, array, include_header, schema, delimiter, fields
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
				description.put("type", "Words");
				description.put("min", 5);
				description.put("max", 15);

				Map<String, Object> longDescription = FastMap.newInstance();
				longDescription.put("name", "longDescription");
				longDescription.put("type", "Sentences");
				longDescription.put("min", 1);
				longDescription.put("max", 3);

				Map<String, Object> price = FastMap.newInstance();
				price.put("name", "price");
				price.put("type", "Money");

				fields.add(id);
				fields.add(name);
				fields.add(description);
				fields.add(longDescription);
				fields.add(price);
			} else if (returnObjectClass.equals(DemoDataPerson.class)) {
				Map<String, Object> title = FastMap.newInstance();
				title.put("name", "title");
				title.put("type", "Title");

				Map<String, Object> firstName = FastMap.newInstance();
				firstName.put("name", "firstName");
				firstName.put("type", "First Name");

				Map<String, Object> lastName = FastMap.newInstance();
				lastName.put("name", "lastName");
				lastName.put("type", "Last Name");

				Map<String, Object> gender = FastMap.newInstance();
				gender.put("name", "gender");
				gender.put("type", "Gender");

				fields.add(title);
				fields.add(firstName);
				fields.add(lastName);
				fields.add(gender);
			} else if (returnObjectClass.equals(DemoDataUserLogin.class)) {
				Map<String, Object> userLoginId = FastMap.newInstance();
				userLoginId.put("name", "userLoginId");
				userLoginId.put("type", "Username");

				Map<String, Object> password = FastMap.newInstance();
				password.put("name", "currentPassword");
				password.put("type", "Password");

				fields.add(userLoginId);
				fields.add(password);
			}
			return fields;
		}

		// public Map<String, Map<String, Object>> getSchemas() {
		// return schemas;
		// }

	}

}
