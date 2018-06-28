package com.ilscipio.scipio.ce.demoSuite.dataGenerator.dataObject.party;

import com.ilscipio.scipio.ce.demoSuite.dataGenerator.dataObject.DemoDataAddress;
import com.ilscipio.scipio.ce.demoSuite.dataGenerator.dataObject.DemoDataObject;
import com.ilscipio.scipio.ce.demoSuite.dataGenerator.dataObject.DemoDataPerson;
import com.ilscipio.scipio.ce.demoSuite.dataGenerator.dataObject.DemoDataUserLogin;

public class DemoDataParty implements DemoDataObject {

	DemoDataAddress address;
	DemoDataPerson person;
	DemoDataUserLogin userLogin;

	public DemoDataAddress getAddress() {
		return address;
	}

	public void setAddress(DemoDataAddress address) {
		this.address = address;
	}

	public DemoDataPerson getPerson() {
		return person;
	}

	public void setPerson(DemoDataPerson person) {
		this.person = person;
	}

	public DemoDataUserLogin getUserLogin() {
		return userLogin;
	}

	public void setUserLogin(DemoDataUserLogin userLogin) {
		this.userLogin = userLogin;
	}

}
