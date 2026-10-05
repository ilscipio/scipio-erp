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
package com.ilscipio.scipio.cms;

import org.ofbiz.base.util.PropertyMessage;

import java.util.Collection;

/**
 * Exception thrown to pass around input errors.
 */
@SuppressWarnings("serial")
public class CmsInputException extends CmsException {

    public CmsInputException() {
    }

    public CmsInputException(String msg) {
        super(msg);
    }

    public CmsInputException(String msg, Throwable nested) {
        super(msg, nested);
    }

    public CmsInputException(Throwable nested) {
        super(nested);
    }

    public CmsInputException(String msg, Collection<?> messageList) {
        super(msg, messageList);
    }

    public CmsInputException(String msg, Collection<?> messageList, Throwable nested) {
        super(msg, messageList, nested);
    }

    public CmsInputException(Collection<?> messageList, Throwable nested) {
        super(messageList, nested);
    }

    public CmsInputException(Collection<?> messageList) {
        super(messageList);
    }

    public CmsInputException(PropertyMessage propMsg) {
        super(propMsg);
    }

    public CmsInputException(PropertyMessage propMsg, Throwable nested) {
        super(propMsg, nested);
    }

}
