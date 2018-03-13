package com.ilscipio.scipio.ce.demoSuite.dataGenerator.impl;

import java.util.List;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilValidate;

import com.ilscipio.scipio.ce.demoSuite.dataGenerator.LocalDataGenerator;
import com.ilscipio.scipio.ce.demoSuite.dataGenerator.dataObject.DemoDataAddress;
import com.ilscipio.scipio.ce.demoSuite.dataGenerator.dataObject.DemoDataObject;
import com.ilscipio.scipio.ce.demoSuite.dataGenerator.dataObject.DemoDataPerson;
import com.ilscipio.scipio.ce.demoSuite.dataGenerator.dataObject.DemoDataProduct;
import com.ilscipio.scipio.ce.demoSuite.dataGenerator.dataObject.DemoDataUserLogin;
import com.ilscipio.scipio.ce.demoSuite.dataGenerator.dataObject.party.DemoDataParty;
import com.ilscipio.scipio.ce.demoSuite.dataGenerator.helper.DemoDataHelper;
import com.ilscipio.scipio.ce.demoSuite.dataGenerator.helper.jfairy.JFairyDemoDataHelper;

import io.codearte.jfairy.Fairy;
import io.codearte.jfairy.producer.person.Person;
import javolution.util.FastList;

public class JFairyDataGenerator extends LocalDataGenerator {
	private final static String JFAIRY_DATA_GENERATOR = "jfairy";
	private final static String module = JFairyDataGenerator.class.getName();

	private final JFairyDemoDataHelper helper;

	public JFairyDataGenerator(DemoDataHelper helper) {
		super(helper);
		this.helper = (JFairyDemoDataHelper) helper;
	}

	public class JFairySettings extends LocalDataGeneratorSettings {
		public JFairySettings() {
		}
	}

	@Override
	protected List<? extends DemoDataObject> retrieveData() throws Exception {
		List<DemoDataObject> result = FastList.newInstance();
		try {
			Fairy fairy = Fairy.create();
			for (int i = 0; i <= helper.getCount(); i++) {
				Object o = null;
				if (returnObjectClass.equals(DemoDataParty.class)) {
					o = fairy.person();
					Debug.log("person [" + i + "]: " + o);
				} else if (returnObjectClass.equals(DemoDataProduct.class)) {
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
	protected DemoDataObject handleData(Object result, String format) throws UnsupportedOperationException {
		DemoDataObject handledData = null;
		JFairySettings settings = new JFairySettings();
		// TODO: Apply fields while creating the DemoData* objects
		List<String> fields = settings.getFields(helper.getDataType());

		if (returnObjectClass.equals(DemoDataParty.class) && result instanceof Person) {
			DemoDataParty demoDataParty = new DemoDataParty();
			
			Person person = (Person) result;
			DemoDataPerson demoDataPerson = new DemoDataPerson();
			demoDataPerson.setFirstName(person.getFirstName());
			demoDataPerson.setLastName(person.getLastName());
			demoDataPerson.setGender(person.getSex().name());
			demoDataParty.setPerson(demoDataPerson);

			if (helper.generateAddress()) {
				DemoDataAddress demoDataAddress = new DemoDataAddress();
				demoDataAddress.setCountry("");
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
	protected String getDataGeneratorName() {
		return JFAIRY_DATA_GENERATOR;
	}

}
