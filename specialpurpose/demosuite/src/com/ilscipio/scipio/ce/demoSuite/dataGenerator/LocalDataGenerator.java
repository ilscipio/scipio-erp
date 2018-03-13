package com.ilscipio.scipio.ce.demoSuite.dataGenerator;

import java.util.List;

import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.base.util.UtilValidate;

import com.ilscipio.scipio.ce.demoSuite.dataGenerator.helper.DemoDataHelper;

public abstract class LocalDataGenerator extends DataGenerator {

	public LocalDataGenerator(DemoDataHelper helper) {
		super(helper);		
	}

	protected abstract class LocalDataGeneratorSettings {		

		public LocalDataGeneratorSettings() {
			
		}		

		public List<String> getFields(String dataType) throws UnsupportedOperationException {
			String fields = null;
			fields = properties
					.getProperty("demosuite.test.data.provider." + getDataGeneratorName() + ".fields." + dataType);
			if (UtilValidate.isNotEmpty(fields)) {
				return UtilMisc.toListArray(fields.split(",\\s{0,1}"));
			}
			return null;
		}

	}	

}
