package com.ilscipio.scipio.ce.demoSuite.dataGenerator.helper;

import java.util.List;
import java.util.Locale;
import java.util.Map;

import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericEntityException;

public class JFairyDemoDataHelper extends DemoDataHelper {

	private Locale locale;

	public JFairyDemoDataHelper(Map<String, Object> context) throws Exception {
		super(context, JFairySettings.class);
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
