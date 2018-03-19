package com.ilscipio.scipio.ce.demoSuite.dataGenerator;

import java.util.List;

import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.base.util.UtilValidate;

import com.ilscipio.scipio.ce.demoSuite.dataGenerator.dataObject.DemoDataObject;
import com.ilscipio.scipio.ce.demoSuite.dataGenerator.helper.DemoDataHelper;

public abstract class DataGenerator {

	private final DemoDataHelper helper;

	public DataGenerator(DemoDataHelper helper) {
		this.helper = helper;
	}

	public abstract List<? extends DemoDataObject> retrieveData() throws Exception;

	public DemoDataObject handleData(Object result) {
		return handleData(result, null);
	}

	public abstract DemoDataObject handleData(Object result, String format);

	public abstract String getDataGeneratorName();

	public DemoDataHelper getHelper() {
		return helper;
	}

	public abstract class DataGeneratorSettings {
		public DataGeneratorSettings() {
		}

		public List<Object> getFields(String dataType) throws UnsupportedOperationException {
			String fields = null;
			fields = helper.getProperties()
					.getProperty("demosuite.test.data.provider." + getDataGeneratorName() + ".fields." + dataType);
			if (UtilValidate.isNotEmpty(fields)) {
				return UtilMisc.toListArray(fields.split(",\\s{0,1}"));
			}
			return null;
		}
	}

}
