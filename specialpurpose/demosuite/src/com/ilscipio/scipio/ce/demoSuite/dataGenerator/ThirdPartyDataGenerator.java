package com.ilscipio.scipio.ce.demoSuite.dataGenerator;

import java.io.IOException;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.HashMap;
import java.util.List;

import org.ofbiz.base.lang.JSON;
import org.ofbiz.base.util.Debug;

import com.ilscipio.scipio.ce.demoSuite.dataGenerator.dataObject.DemoDataObject;
import com.ilscipio.scipio.ce.demoSuite.dataGenerator.helper.DemoDataHelper;

public abstract class ThirdPartyDataGenerator extends DataGenerator {

	protected ThirdPartyDataGenerator(DemoDataHelper helper) {
		super(helper);
	}

	protected abstract class DataGeneratorSettings extends HashMap<String, Object> {
		private static final long serialVersionUID = -8549187859117423507L;
		private final String method;

		private final URL url;

		public DataGeneratorSettings() {
			try {
				this.url = new URL(
						properties.getProperty("demosuite.test.data.provider." + getDataGeneratorName() + ".url"));
			} catch (MalformedURLException e) {
				throw new RuntimeException(e);
			}

			this.method = properties.getProperty("demosuite.test.data.provider." + getDataGeneratorName() + ".method");

		}

		public String getMethod() {
			return method;
		}

		public URL getUrl() {
			return url;
		}

		public abstract HashMap<String, Object> getQueryParameters();

		public abstract List<Object> getFields();

	}

	@Override
	protected DemoDataObject handleData(Object result, String format) {
		if (format.equals("json")) {
			try {
				Object o = JSON.from(result);
				return (DemoDataObject) JSON.from(o).toObject(returnObjectClass);
			} catch (IOException e) {
				Debug.logError(e.getMessage(), "");
			}
		} else {
			throw new UnsupportedOperationException("Export format " + format + " currently not supported");
		}
		return null;
	}

}
