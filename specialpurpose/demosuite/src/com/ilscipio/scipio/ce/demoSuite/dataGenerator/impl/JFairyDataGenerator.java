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
package com.ilscipio.scipio.ce.demoSuite.dataGenerator.impl;

import com.ilscipio.scipio.ce.demoSuite.dataGenerator.AbstractDataGenerator;
import com.ilscipio.scipio.ce.demoSuite.dataGenerator.dataObject.AbstractDataObject;
import com.ilscipio.scipio.ce.demoSuite.dataGenerator.dataObject.DemoDataAddress;
import com.ilscipio.scipio.ce.demoSuite.dataGenerator.dataObject.DemoDataEmailAddress;
import com.ilscipio.scipio.ce.demoSuite.dataGenerator.dataObject.DemoDataOrder;
import com.ilscipio.scipio.ce.demoSuite.dataGenerator.dataObject.DemoDataPerson;
import com.ilscipio.scipio.ce.demoSuite.dataGenerator.dataObject.DemoDataProduct;
import com.ilscipio.scipio.ce.demoSuite.dataGenerator.dataObject.DemoDataUserLogin;
import com.ilscipio.scipio.ce.demoSuite.dataGenerator.dataObject.party.DemoDataParty;
import com.ilscipio.scipio.ce.demoSuite.dataGenerator.helper.AbstractDemoDataHelper;
import com.ilscipio.scipio.ce.demoSuite.dataGenerator.helper.JFairyDemoDataHelper;
import com.ilscipio.scipio.ce.demoSuite.dataGenerator.util.DemoSuiteDataGeneratorUtil;
import io.codearte.jfairy.Fairy;
import io.codearte.jfairy.producer.person.Person;
import org.ofbiz.base.util.UtilValidate;

import java.util.ArrayList;
import java.util.List;
import java.util.Locale;

public class JFairyDataGenerator extends AbstractDataGenerator {
    private final static String JFAIRY_DATA_GENERATOR = "jfairy";
    // private static final Debug.OfbizLogger module =
    // Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    private final JFairyDemoDataHelper helper;

    public JFairyDataGenerator(AbstractDemoDataHelper helper) {
        super(helper);
        this.helper = (JFairyDemoDataHelper) helper;
    }

    @Override
    public List<? extends AbstractDataObject> retrieveData() throws Exception {
        List<AbstractDataObject> result = new ArrayList<>(helper.getCount());
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
                } else if (helper.getReturnObjectClass().equals(DemoDataOrder.class)) {
                    throw new UnsupportedOperationException("Order demo data is not supported");
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

    @Override
    public AbstractDataObject handleData(Object result, String format) throws UnsupportedOperationException {
        AbstractDataObject handledData = null;
        // FIXME: Make sure this makes sense for all data generators
        // JFairySettings settings = new JFairySettings();
        // List<String> fields = settings.getFields(helper.getDataType());

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

            if (helper.generateEmailAddress()) {
                DemoDataEmailAddress demoDataEmailAddress = new DemoDataEmailAddress();
                demoDataEmailAddress.setEmailAddress(person.getEmail());
                demoDataParty.setEmailAddress(demoDataEmailAddress);
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
