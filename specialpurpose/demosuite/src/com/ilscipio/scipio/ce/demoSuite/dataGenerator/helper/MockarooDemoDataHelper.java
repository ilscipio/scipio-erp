package com.ilscipio.scipio.ce.demoSuite.dataGenerator.helper;

import java.util.ArrayList;
import java.util.List;
import java.util.Locale;
import java.util.Map;

import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericEntityException;
import org.ofbiz.entity.GenericValue;

public class MockarooDemoDataHelper extends DemoDataHelper {

	private Locale locale;

	public MockarooDemoDataHelper(Map<String, Object> context) throws Exception {
		super(context, (Class<? extends DemoDataHelper.DataGeneratorSettings>) MockarooSettings.class);
	}

	public boolean generateAddress() {
		if (!getContext().containsKey("generateAddress"))
			return false;
		return (boolean) getContext().get("generateAddress");
	}

	public boolean generateUserLogin() {
		if (!getContext().containsKey("generateUserLogin"))
			return false;
		return (boolean) getContext().get("generateUserLogin");
	}

	public Locale getLocale() {
		return locale;
	}

	public void setLocale(Locale locale) {
		this.locale = locale;
	}

	public static class MockarooSettings extends DataGeneratorSettings {
		private final Map<String, Object> queryParameters;
		private final String api;
		private final String url;
		private final String method;
		private final String key;
		private final String exportFormat;

		public MockarooSettings(Delegator delegator) throws Exception {
			super(delegator);
			try {
				GenericValue mockarooSettings = delegator.findOne("MockarooDataGeneratorProvider",
						UtilMisc.toMap("", ""), true);
				this.queryParameters = UtilMisc.toMap("");
				this.url = mockarooSettings.getString("url");
				this.method = mockarooSettings.getString("method");
				this.key = mockarooSettings.getString("mockarooKey");
				this.exportFormat = mockarooSettings.getString("exportFormat");
				this.api = mockarooSettings.getString("api");
			} catch (GenericEntityException e) {
				throw new Exception(e);
			}

			// queryParameters.put("key",
			// getProperties().get("demosuite.test.data.provider." +
			// getDataGeneratorName() + ".key"));
			// Map<String, Map<String, Object>> schemas = new HashMap<>();
			// UtilProperties.extractPropertiesWithPrefixAndId(schemas,
			// properties, "demosuite.test.data.provider." +
			// getDataGeneratorName() + ".api.");
			// this.schemas = schemas;
		}

		public String getApi() {			
			return api;
		}

		public String getUrl() {
			return url;
		}

		public String getMethod() {
			return method;
		}

		public String getKey() {
			return key;
		}

		public String getExportFormat() {
			return exportFormat;
		}

		@Override
		public List<Object> getFields() {
			List<Object> fields = new ArrayList<Object>();
			// if (returnObjectClass.equals(DemoDataAddress.class)) {
			// Map<String, Object> country = UtilMisc.newMap();
			// country.put("name", "country");
			// country.put("type", "Country");
			//
			// Map<String, Object> state = UtilMisc.newMap();
			// state.put("name", "state");
			// state.put("type", "State");
			//
			// Map<String, Object> city = UtilMisc.newMap();
			// city.put("name", "city");
			// city.put("type", "City");
			//
			// Map<String, Object> street = UtilMisc.newMap();
			// street.put("name", "street");
			// street.put("type", "Street Name");
			//
			// Map<String, Object> zip = UtilMisc.newMap();
			// zip.put("name", "zip");
			// zip.put("type", "Postal Code");
			//
			// fields.add(country);
			// fields.add(state);
			// fields.add(city);
			// fields.add(street);
			// fields.add(zip);
			// } else if (returnObjectClass.equals(DemoDataProduct.class)) {
			// Map<String, Object> id = UtilMisc.newMap();
			// id.put("name", "id");
			// id.put("type", "Row Number");
			//
			// Map<String, Object> name = UtilMisc.newMap();
			// name.put("name", "name");
			// name.put("type", "Words");
			// name.put("min", 1);
			// name.put("max", 3);
			//
			// Map<String, Object> description = UtilMisc.newMap();
			// description.put("name", "description");
			// description.put("type", "Words");
			// description.put("min", 5);
			// description.put("max", 15);
			//
			// Map<String, Object> longDescription = UtilMisc.newMap();
			// longDescription.put("name", "longDescription");
			// longDescription.put("type", "Sentences");
			// longDescription.put("min", 1);
			// longDescription.put("max", 3);
			//
			// Map<String, Object> price = UtilMisc.newMap();
			// price.put("name", "price");
			// price.put("type", "Money");
			//
			// fields.add(id);
			// fields.add(name);
			// fields.add(description);
			// fields.add(longDescription);
			// fields.add(price);
			// } else if (returnObjectClass.equals(DemoDataPerson.class)) {
			// Map<String, Object> title = UtilMisc.newMap();
			// title.put("name", "title");
			// title.put("type", "Title");
			//
			// Map<String, Object> firstName = UtilMisc.newMap();
			// firstName.put("name", "firstName");
			// firstName.put("type", "First Name");
			//
			// Map<String, Object> lastName = UtilMisc.newMap();
			// lastName.put("name", "lastName");
			// lastName.put("type", "Last Name");
			//
			// Map<String, Object> gender = UtilMisc.newMap();
			// gender.put("name", "gender");
			// gender.put("type", "Gender");
			//
			// fields.add(title);
			// fields.add(firstName);
			// fields.add(lastName);
			// fields.add(gender);
			// } else if (returnObjectClass.equals(DemoDataUserLogin.class)) {
			// Map<String, Object> userLoginId = UtilMisc.newMap();
			// userLoginId.put("name", "userLoginId");
			// userLoginId.put("type", "Username");
			//
			// Map<String, Object> password = UtilMisc.newMap();
			// password.put("name", "currentPassword");
			// password.put("type", "Password");
			//
			// fields.add(userLoginId);
			// fields.add(password);
			// }
			return fields;
		}		

		// public Map<String, Map<String, Object>> getSchemas() {
		// return schemas;
		// }

	}

	@Override
	public MockarooSettings getSettings() {
		// TODO Auto-generated method stub
		return (MockarooSettings) super.getSettings();
	}

}
