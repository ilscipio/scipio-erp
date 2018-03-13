package com.ilscipio.scipio.ce.demoSuite.dataGenerator.helper.jfairy;

import java.util.Locale;
import java.util.Map;

import com.ilscipio.scipio.ce.demoSuite.dataGenerator.dataObject.DemoDataObject;
import com.ilscipio.scipio.ce.demoSuite.dataGenerator.dataObject.party.DemoDataParty;
import com.ilscipio.scipio.ce.demoSuite.dataGenerator.helper.DemoDataHelper;

public class JFairyDemoDataHelper extends DemoDataHelper {
	
	private Locale locale;

	public JFairyDemoDataHelper(Map<String, Object> context) throws Exception {
		super(context);

	}

	@Override
	public Class<? extends DemoDataObject> getReturnObjectClass() throws UnsupportedOperationException {
		if (dataType.equals("party"))
			return DemoDataParty.class;
		else 
			throw new UnsupportedOperationException("Data type [" +  dataType + "] not supported");
	}
	
	public boolean generateAddress() {
		return (boolean) context.get("generateAddress");
	}
	
	public boolean generateUserLogin() {
		return (boolean) context.get("generateUserLogin");
	}

	public Locale getLocale() {
		return locale;
	}

	public void setLocale(Locale locale) {
		this.locale = locale;
	}
}
