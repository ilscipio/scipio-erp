package com.ilscipio.scipio.ce.demoSuite.dataGenerator.helper;

import java.util.Map;

import com.ilscipio.scipio.ce.demoSuite.dataGenerator.dataObject.DemoDataObject;

public abstract class DemoDataHelper {

	protected final Map<String, Object> context;
	protected final String dataType;
	protected final Integer count;

	public DemoDataHelper(Map<String, Object> context) throws Exception {
		this.context = context;
		if (!context.containsKey("dataType"))
			throw new Exception("dataType not found");
		this.dataType = (String) context.get("dataType");
		this.count = (Integer) context.get("num");
	}

	public abstract Class<? extends DemoDataObject> getReturnObjectClass();

	public String getDataType() {
		return dataType;
	}

	public Integer getCount() {
		return count;
	}

}
