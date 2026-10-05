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
package com.ilscipio.scipio.accounting.external.datev.category;

import java.util.Map;

import org.ofbiz.entity.Delegator;

import com.ilscipio.scipio.accounting.external.BaseOperationResults;
import com.ilscipio.scipio.accounting.external.BaseOperationStats;
import com.ilscipio.scipio.accounting.external.datev.DatevException;
import com.ilscipio.scipio.accounting.external.datev.DatevHelper;

public class DatevDiverseAdressen extends AbstractDatevDataCategory {

    public DatevDiverseAdressen(Delegator delegator, DatevHelper datevHelper) throws DatevException {
        super(delegator, datevHelper);
        // TODO Auto-generated constructor stub
    }

    @Override
    public void processRecord(int index, Map<String, String> recordMap) throws DatevException {
        // TODO Auto-generated method stub
    }

    @Override
    public boolean validateField(String fieldName, String value) throws DatevException {
        // TODO Auto-generated method stub
        return false;
    }

    @Override
    public boolean validateField(int position, String value) throws DatevException {
        // TODO Auto-generated method stub
        return false;
    }

    @Override
    public Class<? extends BaseOperationStats> getOperationStatsClass() throws DatevException {
        // TODO Auto-generated method stub
        return null;
    }

    @Override
    public Class<? extends BaseOperationResults> getOperationResultsClass() throws DatevException {
        // TODO Auto-generated method stub
        return null;
    }

}
