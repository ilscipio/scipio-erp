package com.ilscipio.scipio.ce.demoSuite.dataGenerator.helper;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;

import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericEntityException;

import com.ilscipio.scipio.ce.demoSuite.dataGenerator.dataObject.DemoDataObject;
import com.ilscipio.scipio.ce.demoSuite.dataGenerator.dataObject.party.DemoDataParty;

public class DatabaseTestDemoDataHelper extends DemoDataHelper {

	private Locale locale;

	public DatabaseTestDemoDataHelper(Map<String, Object> context) throws Exception {
		super(context, DatabaseTestSettings.class);
	}

	@Override
	public Class<? extends DemoDataObject> getReturnObjectClass() throws UnsupportedOperationException {
		if (getDataType().equals("party"))
			return DemoDataParty.class;
		else
			throw new UnsupportedOperationException("Data type [" + getDataType() + "] not supported");
	}

	public boolean generateAddress() {
		return (boolean) getContext().get("generateAddress");
	}

	public boolean generateUserLogin() {
		return (boolean) getContext().get("generateUserLogin");
	}

	public Locale getLocale() {
		return locale;
	}

	public void setLocale(Locale locale) {
		this.locale = locale;
	}
	
	@Override
	public DatabaseTestSettings getSettings() {
		return (DatabaseTestSettings) getSettings();
	}

	public class DatabaseTestSettings extends DataGeneratorSettings {
		private final Map<String, Object> queryParameters;
		// private final Map<String, Map<String, Object>> schemas;

		public DatabaseTestSettings(Delegator delegator) throws GenericEntityException  {
			super(delegator);
			this.queryParameters = new HashMap<String, Object>();
		}

		// @Override
		public Map<String, Object> getQueryParameters() {
			// demosuite.test.data.provider.mockaroo.queryParams = key,
			// callback, count, array, include_header, schema, delimiter, fields
			return queryParameters;
		}

		@Override
		public List<Object> getFields() {
			List<Object> fields = new ArrayList<Object>();

			return fields;
		}
	}
}
