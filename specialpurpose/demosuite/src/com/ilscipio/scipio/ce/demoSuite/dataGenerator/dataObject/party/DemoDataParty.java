/*
 * Scipio Commerce
 * Copyright (C) Ilscipio GmbH
 *
 * This file is part of Scipio Commerce. Scipio Commerce is free software: you
 * can redistribute it and modify it under the terms of the GNU Affero General
 * Public License, version 3, as published by the Free Software Foundation.
 * Scipio Commerce is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License
 * for more details. You should have received a copy of the license with this
 * work (file LICENSE). If not, see <https://www.gnu.org/licenses/agpl-3.0.html>.
 * A commercial license is available from Ilscipio GmbH.
 *
 * SPDX-License-Identifier: AGPL-3.0-only
 */
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
