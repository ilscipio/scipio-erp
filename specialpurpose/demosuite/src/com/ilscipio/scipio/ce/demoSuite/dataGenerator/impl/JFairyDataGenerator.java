package com.ilscipio.scipio.ce.demoSuite.dataGenerator.impl;

import java.util.ArrayList;
import java.util.List;
import java.util.Locale;

import org.ofbiz.base.util.UtilValidate;

import com.ilscipio.scipio.ce.demoSuite.dataGenerator.DataGenerator;
import com.ilscipio.scipio.ce.demoSuite.dataGenerator.dataObject.DemoDataAddress;
import com.ilscipio.scipio.ce.demoSuite.dataGenerator.dataObject.DemoDataObject;
import com.ilscipio.scipio.ce.demoSuite.dataGenerator.dataObject.DemoDataPerson;
import com.ilscipio.scipio.ce.demoSuite.dataGenerator.dataObject.DemoDataProduct;
import com.ilscipio.scipio.ce.demoSuite.dataGenerator.dataObject.DemoDataUserLogin;
import com.ilscipio.scipio.ce.demoSuite.dataGenerator.dataObject.party.DemoDataParty;
import com.ilscipio.scipio.ce.demoSuite.dataGenerator.helper.DemoDataHelper;
import com.ilscipio.scipio.ce.demoSuite.dataGenerator.helper.JFairyDemoDataHelper;
import com.ilscipio.scipio.ce.demoSuite.dataGenerator.util.DemoSuiteDataGeneratorUtil;

import io.codearte.jfairy.Fairy;
import io.codearte.jfairy.producer.person.Person;

public class JFairyDataGenerator extends DataGenerator {
	private final static String JFAIRY_DATA_GENERATOR = "jfairy";
	//private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

	private final JFairyDemoDataHelper helper;

	public JFairyDataGenerator(DemoDataHelper helper) {
		super(helper);
		this.helper = (JFairyDemoDataHelper) helper;
	}	

	@Override
	public List<? extends DemoDataObject> retrieveData() throws Exception {
		List<DemoDataObject> result = new ArrayList<>(helper.getCount());
		try {			
			for (int i = 0; i < helper.getCount(); i++) {
				Locale locale = DemoSuiteDataGeneratorUtil.LocaleClasses.getRandom();
				helper.setLocale(locale);
				Fairy fairy = Fairy.create(locale);
				Object o = null;
				if (helper.getReturnObjectClass().equals(DemoDataParty.class)) {
					o = fairy.person();					
				} else if (helper.getReturnObjectClass().equals(DemoDataProduct.class)) {
					throw new UnsupportedOperationException("Product demo data is not supported");
				}
				if (UtilValidate.isNotEmpty(o)) {
					result.add(handleData(o));
				}
			}
		} catch (Exception e) {
			throw new RuntimeException(e);
		}

		return result;
	}

	@SuppressWarnings("unchecked")
	@Override
	public DemoDataObject handleData(Object result, String format) throws UnsupportedOperationException {
		DemoDataObject handledData = null;
		// FIXME: Make sure this makes sense for all data generators
//		JFairySettings settings = new JFairySettings();
//		List<String> fields = settings.getFields(helper.getDataType());

		if (helper.getReturnObjectClass().equals(DemoDataParty.class) && result instanceof Person) {
			DemoDataParty demoDataParty = new DemoDataParty();
			
			Person person = (Person) result;
			DemoDataPerson demoDataPerson = new DemoDataPerson();
			demoDataPerson.setFirstName(person.getFirstName());
			demoDataPerson.setLastName(person.getLastName());
			demoDataPerson.setGender(person.getSex().name());
			demoDataParty.setPerson(demoDataPerson);

			if (helper.generateAddress()) {
				DemoDataAddress demoDataAddress = new DemoDataAddress();
				demoDataAddress.setCountry(helper.getLocale().getCountry());
				demoDataAddress.setCity(person.getAddress().getCity());
				demoDataAddress.setStreet(person.getAddress().getAddressLine1());
				demoDataAddress.setZip(person.getAddress().getPostalCode());
				demoDataParty.setAddress(demoDataAddress);
				// address.getStreet();
				// address.getStreetNumber();
				// address.getApartmentNumber();
				// address.getStreetNumber();
			}

			if (helper.generateUserLogin()) {
				DemoDataUserLogin demoDataUserLogin = new DemoDataUserLogin();
				demoDataUserLogin.setUserLoginId(person.getUsername());
				demoDataUserLogin.setCurrentPassword(person.getPassword());
				demoDataParty.setUserLogin(demoDataUserLogin);
			}
			
			handledData = demoDataParty;

		} else {
			throw new UnsupportedOperationException();
		}

		return handledData;

	}

	@Override
	public String getDataGeneratorName() {
		return JFAIRY_DATA_GENERATOR;
	}

}
