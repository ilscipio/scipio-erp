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
package com.ilscipio.scipio.ce.demoSuite.dataGenerator.service;

import java.util.ArrayList;
import java.util.List;

import org.ofbiz.entity.GenericValue;

public class DataGeneratorStat {
    private String entityName;
    private int stored;
    private int failed;
    private List<GenericValue> generatedValues;

    DataGeneratorStat(String entityName) {
        this.entityName = entityName;
        this.setGeneratedValues(new ArrayList<>());
    }

    public String getEntityName() {
        return entityName;
    }

    public int getStored() {
        return stored;
    }

    public void setStored(int stored) {
        this.stored = stored;
    }

    public int getFailed() {
        return failed;
    }

    public void setFailed(int failed) {
        this.failed = failed;
    }

    public List<GenericValue> getGeneratedValues() {
        return generatedValues;
    }

    public void setGeneratedValues(List<GenericValue> generatedValues) {
        this.generatedValues = generatedValues;
    }

}
