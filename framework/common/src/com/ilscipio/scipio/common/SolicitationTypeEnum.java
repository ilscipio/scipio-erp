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
package com.ilscipio.scipio.common;

public enum SolicitationTypeEnum {

    EMAIL(new String[]{"CUSTOMER_EMAIL_ALLOW_SOL", "USER_EMAIL_ALLOW_SOL", "MAILEON_EMAIL_SOLICITATION"}),
    ADDRESS(new String[]{"CUSTOMER_ADDRESS_ALLOW_SOL", "USER_ADDRESS_ALLOW_SOL"}),
    WORK_PHONE(new String[]{"CUSTOMER_WORK_ALLOW_SOL", "USER_WORK_ALLOW_SOL"}),
    HOME_PHONE(new String[]{"CUSTOMER_HOME_ALLOW_SOL", "USER_HOME_ALLOW_SOL"}),
    MOBILE_PHONE(new String[]{"CUSTOMER_MOBILE_ALLOW_SOL", "USER_MOBILE_ALLOW_SOL"}),
    FAX(new String[]{"CUSTOMER_FAX_ALLOW_SOL", "USER_FAX_ALLOW_SOL"});

    private String[] parameterNames;

    SolicitationTypeEnum(String[] parameterNames) {
        this.parameterNames = parameterNames;
    }

    public String[] getParameterNames() {
        return parameterNames;
    }

}
