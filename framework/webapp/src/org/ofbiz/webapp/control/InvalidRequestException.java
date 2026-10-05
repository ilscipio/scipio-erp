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
package org.ofbiz.webapp.control;

import org.ofbiz.base.util.PropertyMessage;

import java.util.Collection;

/**
 * Thrown when a request is missing or generally invalid.
 *
 * <p>SCIPIO: 3.0.0: Added to decrease logging verbosity.</p>
 */
public class InvalidRequestException extends RequestHandlerException {

    public InvalidRequestException() {
    }

    public InvalidRequestException(String msg) {
        super(msg);
    }

    public InvalidRequestException(String msg, Throwable nested) {
        super(msg, nested);
    }

    public InvalidRequestException(Throwable nested) {
        super(nested);
    }

    public InvalidRequestException(String msg, Collection<?> messageList) {
        super(msg, messageList);
    }

    public InvalidRequestException(String msg, Collection<?> messageList, Throwable nested) {
        super(msg, messageList, nested);
    }

    public InvalidRequestException(Collection<?> messageList, Throwable nested) {
        super(messageList, nested);
    }

    public InvalidRequestException(Collection<?> messageList) {
        super(messageList);
    }

    public InvalidRequestException(PropertyMessage propMsg) {
        super(propMsg);
    }

    public InvalidRequestException(PropertyMessage propMsg, Throwable nested) {
        super(propMsg, nested);
    }

}
