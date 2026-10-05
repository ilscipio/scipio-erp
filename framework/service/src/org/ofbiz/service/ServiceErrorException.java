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
package org.ofbiz.service;

import java.util.List;
import java.util.Map;

import org.ofbiz.base.util.GeneralException;

/**
 * SCIPIO: Thrown by some methods when a service or service-like function returned an error requiring an exception.
 * <p>
 * Small exception class to pass back service results without requiring the method
 * result to be a service result.
 */
@SuppressWarnings("serial")
public class ServiceErrorException extends GeneralException {

    private final Map<String, Object> serviceResult;

    public ServiceErrorException(String exceptionMessage, Map<String, Object> serviceResult) {
        super(exceptionMessage);
        this.serviceResult = serviceResult;
    }

    public ServiceErrorException(String exceptionMessage, List<String> serviceErrorMessageList) {
        super(exceptionMessage);
        this.serviceResult = ServiceUtil.returnError(serviceErrorMessageList);
    }

    public ServiceErrorException(String exceptionMessage, String serviceErrorMessage) {
        super(exceptionMessage);
        this.serviceResult = ServiceUtil.returnError(serviceErrorMessage);
    }

    public Map<String, Object> getServiceResult() {
        return serviceResult;
    }

}