package com.ilscipio.scipio.ce.demoSuite.dataGenerator;

import java.util.List;
import java.util.Properties;

import org.ofbiz.base.util.UtilProperties;

import com.ilscipio.scipio.ce.demoSuite.dataGenerator.dataObject.DemoDataObject;
import com.ilscipio.scipio.ce.demoSuite.dataGenerator.helper.DemoDataHelper;

public abstract class DataGenerator {

	protected final Class<? extends DemoDataObject> returnObjectClass;
	protected final Properties properties;
	protected final DemoDataHelper helper;

	public DataGenerator(DemoDataHelper helper) {
		this.returnObjectClass = helper.getReturnObjectClass();
		this.properties = UtilProperties.getProperties("demosuite.properties");
		this.helper = helper;
	}

	protected abstract List<? extends DemoDataObject> retrieveData() throws Exception;

	protected DemoDataObject handleData(Object result) {
		return handleData(result, null);
	}

	protected abstract DemoDataObject handleData(Object result, String format);

	protected abstract String getDataGeneratorName();

	public DemoDataHelper getHelper() {
		return helper;
	}

}
