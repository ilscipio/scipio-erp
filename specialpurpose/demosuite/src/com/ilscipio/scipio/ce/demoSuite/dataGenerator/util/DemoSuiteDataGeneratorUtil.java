package com.ilscipio.scipio.ce.demoSuite.dataGenerator.util;

import java.util.ArrayList;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.base.util.UtilRandom;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.service.DispatchContext;
import org.ofbiz.service.GenericServiceException;
import org.ofbiz.service.LocalDispatcher;
import org.ofbiz.service.ModelService;
import org.ofbiz.service.ModelServiceIface;

import javolution.util.FastMap;

public class DemoSuiteDataGeneratorUtil {

	private static final String DEMO_DATA_GENERATOR_SERVICE_INTERFACE_NAME = "demoDataGenerator";

	public static String module = DemoSuiteDataGeneratorUtil.class.getName();

	/**
	 * NOTE:
	 * 
	 * @author jsoto
	 *
	 */
	public enum LocaleClasses {
		DE(Locale.forLanguageTag("de")), ES(Locale.forLanguageTag("es")), PL(Locale.forLanguageTag("pl")), SV(
				Locale.forLanguageTag("sv")), EN(
						Locale.forLanguageTag("en")), KA(Locale.forLanguageTag("ka")), ZH(Locale.forLanguageTag("zh"));
		private final Locale locale;

		private String name;

		LocaleClasses(Locale locale) {
			this.locale = locale;
		}

		public static Locale getRandom() {
			int i = UtilRandom.random(UtilMisc.toListArray(LocaleClasses.values()));
			try {
				return LocaleClasses.values()[i].locale;
			} catch (Exception e) {
				Debug.log(e);
				return null;
			}
		}

		public String toString() {
			return name;
		}
	}

	public static List<Map<String, Object>> getDemoDataServices(LocalDispatcher dispatcher)
			throws GenericServiceException {
		List<Map<String, Object>> servicesList = new ArrayList<Map<String, Object>>();
		DispatchContext dispatchCtx = dispatcher.getDispatchContext();
		Set<String> serviceNames = dispatchCtx.getAllServiceNames();
		for (String serviceName : serviceNames) {
			ModelService curServiceModel = dispatchCtx.getModelService(serviceName);
			if (curServiceModel != null) {
				for (ModelServiceIface implService : curServiceModel.implServices) {
					if (implService.getService().equals(DEMO_DATA_GENERATOR_SERVICE_INTERFACE_NAME)) {
						Map<String, Object> curServiceMap = FastMap.newInstance();
						String engineName = (UtilValidate.isNotEmpty(curServiceModel.engineName))
								? curServiceModel.engineName : "NA";
						String defaultEntityName = (UtilValidate.isNotEmpty(curServiceModel.defaultEntityName))
								? curServiceModel.defaultEntityName : "NA";
						String invoke = (UtilValidate.isNotEmpty(curServiceModel.invoke)) ? curServiceModel.invoke
								: "NA";
						String location = (UtilValidate.isNotEmpty(curServiceModel.location)) ? curServiceModel.location
								: "NA";
						Boolean requireNewTransaction = curServiceModel.requireNewTransaction;

						curServiceMap.put("serviceName", serviceName);
						curServiceMap.put("engineName", engineName);
						curServiceMap.put("defaultEntityName", defaultEntityName);
						curServiceMap.put("invoke", invoke);
						curServiceMap.put("location", location);
						curServiceMap.put("definitionLocation", curServiceModel.definitionLocation
								.replaceFirst("file:/" + System.getProperty("ofbiz.home") + "/", ""));
						curServiceMap.put("requireNewTransaction", requireNewTransaction);

						servicesList.add(curServiceMap);
					}
				}
			}
		}
		return servicesList;
	}

}
