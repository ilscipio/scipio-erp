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
import java.util.Locale;
import java.util.Map;

import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericEntityException;

public class LocalDemoDataHelper extends AbstractDemoDataHelper {

    private Locale locale;

    public LocalDemoDataHelper(Map<String, Object> context) throws Exception {
        super(context, LocalSettings.class);
    }

    public Locale getLocale() {
        return locale;
    }

    public void setLocale(Locale locale) {
        this.locale = locale;
    }

    public static class LocalSettings extends DataGeneratorSettings {
        public LocalSettings(Delegator delegator) throws GenericEntityException {
            super(delegator);
        }

        @Override
        public List<Object> getFields() {
            return null;
        }
    }

}
