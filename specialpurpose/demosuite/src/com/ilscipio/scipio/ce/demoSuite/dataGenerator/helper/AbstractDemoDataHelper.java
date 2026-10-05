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
package com.ilscipio.scipio.ce.demoSuite.dataGenerator.helper;

import java.util.List;
import java.util.Map;
import java.util.Properties;

import org.ofbiz.base.util.UtilProperties;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.entity.Delegator;

import com.ilscipio.scipio.ce.demoSuite.dataGenerator.dataObject.AbstractDataObject;
import com.ilscipio.scipio.ce.demoSuite.dataGenerator.dataObject.DemoDataOrder;
import com.ilscipio.scipio.ce.demoSuite.dataGenerator.dataObject.DemoDataProduct;
import com.ilscipio.scipio.ce.demoSuite.dataGenerator.dataObject.DemoDataTransaction;
import com.ilscipio.scipio.ce.demoSuite.dataGenerator.dataObject.DemoDataWorkEffort;
import com.ilscipio.scipio.ce.demoSuite.dataGenerator.dataObject.party.DemoDataParty;

public abstract class AbstractDemoDataHelper {
    private static final Integer DATA_GENERATOR_MAX_RECORDS = UtilProperties.getPropertyAsInteger("demosuite", "demosuite.test.data.max.records", 50);

    public enum DataTypeEnum {
        PARTY(DemoDataParty.class), PRODUCT(DemoDataProduct.class), ORDER(DemoDataOrder.class), TRANSACTION(DemoDataTransaction.class), WORKEFFORT(DemoDataWorkEffort.class);

        private final Class<? extends AbstractDataObject> demoDataObject;

        DataTypeEnum(Class<? extends AbstractDataObject> o) {
            this.demoDataObject = o;
        }

        Class<? extends AbstractDataObject> getReturnObjectClass() {
            return demoDataObject;
        }
    }

    private final Map<String, Object> context;
    private final Delegator delegator;
    private final String dataType;
    private final Integer count;
    private final Properties properties;
    private DataGeneratorSettings settings;

    public AbstractDemoDataHelper(Map<String, Object> context, Class<? extends DataGeneratorSettings> settingsClass) throws Exception {
        this.delegator = (Delegator) context.get("delegator");
        this.context = context;
        if (!context.containsKey("dataType"))
            throw new Exception("dataType not found");
        this.dataType = (String) context.get("dataType");
        if (UtilValidate.isEmpty(DataTypeEnum.valueOf(dataType))) {
            throw new Exception("dataType not valid");
        }
        if (context.containsKey("num") && UtilValidate.isNotEmpty(context.get("num"))) {
            this.count = (Integer) context.get("num");
        } else {
            this.count = DATA_GENERATOR_MAX_RECORDS;
        }
        this.settings = settingsClass.getConstructor(Delegator.class).newInstance(delegator);
        this.properties = UtilProperties.getProperties("demosuite");
    }

    public Class<? extends AbstractDataObject> getReturnObjectClass() {
        return DataTypeEnum.valueOf(dataType).getReturnObjectClass();
    }

    public String getDataType() {
        return dataType;
    }

    public Integer getCount() {
        return count;
    }

    public DataGeneratorSettings getSettings() {
        return settings;
    }

    public Map<String, Object> getContext() {
        return context;
    }

    public Properties getProperties() {
        return properties;
    }

    public Delegator getDelegator() {
        return delegator;
    }

    public static abstract class DataGeneratorSettings {
        public DataGeneratorSettings(Delegator delegator) {
        }

        public abstract List<Object> getFields();
    }

}
