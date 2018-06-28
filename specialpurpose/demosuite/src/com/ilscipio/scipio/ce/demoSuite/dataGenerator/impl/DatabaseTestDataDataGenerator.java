package com.ilscipio.scipio.ce.demoSuite.dataGenerator.impl;

import java.util.ArrayList;
import java.util.List;

import org.ofbiz.base.util.HttpClient;

import com.ilscipio.scipio.ce.demoSuite.dataGenerator.DataGenerator;
import com.ilscipio.scipio.ce.demoSuite.dataGenerator.dataObject.DemoDataObject;
import com.ilscipio.scipio.ce.demoSuite.dataGenerator.helper.DatabaseTestDemoDataHelper;
import com.ilscipio.scipio.ce.demoSuite.dataGenerator.helper.DemoDataHelper;

@Deprecated
/**
 * This service doesn't has a proper API to interact with (ie: REST) so it's
 * very hard to get the results. Must be dropped or replaced by something else.
 * 
 * @author jsoto
 *
 * @param <T>
 */
public class DatabaseTestDataDataGenerator extends DataGenerator {
	private static String DATABASETESTDATA_DATA_GENERATOR = "databaseTestData";

	DatabaseTestDemoDataHelper helper;

	public DatabaseTestDataDataGenerator(DemoDataHelper helper) {
		super(helper);
		this.helper = (DatabaseTestDemoDataHelper) helper;
	}

	@Override
	public String getDataGeneratorName() {
		return DATABASETESTDATA_DATA_GENERATOR;
	}

	@Override
	public List<? extends DemoDataObject> retrieveData() {
		HttpClient httpClient = new HttpClient();
		//DatabaseTestSettings settings = helper.getSettings();
		String format = helper.getProperties()
				.getProperty("demosuite.test.data.provider." + getDataGeneratorName() + ".exportFormat");
		String url = helper.getProperties()
				.getProperty("demosuite.test.data.provider." + getDataGeneratorName() + ".url") + "?";
		httpClient.setContentType("application/json");
		httpClient.setUrl(url + "&x__format=" + format + "&x__no_entries=" + helper.getCount());

		//JSONToList jsonListConverter = new JSONConverters.JSONToList();
		List<DemoDataObject> resultList = new ArrayList<>();

		// try {
		// String r =
		// httpClient.sendHttpRequest(helper.getProperties().getSettings().getMethod());
		// for (Object o : jsonListConverter.convert(JSON.from(r))) {
		// resultList.add(handleData(o, format));
		// }
		// } catch (HttpClientException e) {
		// } catch (ConversionException e) {
		// }
		return resultList;
	}

	@Override
	public DemoDataObject handleData(Object result, String format) {
		return null;
	}

}
