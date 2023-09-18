package com.ilscipio.scipio.ce.demoSuite.dataGenerator.dataObject.party;

import com.ilscipio.scipio.ce.demoSuite.dataGenerator.dataObject.DemoDataAddress;
import com.ilscipio.scipio.ce.demoSuite.dataGenerator.dataObject.AbstractDataObject;
import com.ilscipio.scipio.ce.demoSuite.dataGenerator.dataObject.DemoDataEmailAddress;
import com.ilscipio.scipio.ce.demoSuite.dataGenerator.dataObject.DemoDataPerson;
import com.ilscipio.scipio.ce.demoSuite.dataGenerator.dataObject.DemoDataUserLogin;

public class DemoDataParty implements AbstractDataObject {

    DemoDataAddress address;
    DemoDataPerson person;
    DemoDataUserLogin userLogin;
    DemoDataEmailAddress emailAddress;

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

    public DemoDataEmailAddress getEmailAddress() {
        return emailAddress;
    }

    public void setEmailAddress(DemoDataEmailAddress emailAddress) {
        this.emailAddress = emailAddress;
    }
}
