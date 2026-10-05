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

import java.util.Collection;
import java.util.List;
import java.util.Locale;

import org.ofbiz.base.util.GeneralRuntimeException;
import org.ofbiz.base.util.PropertyMessage;
import org.ofbiz.base.util.PropertyMessageEx.SettablePropertyMessageEx;
import org.ofbiz.base.util.PropertyMessageExUtil;

/**
 * Base CMS exception.
 * <p>
 * This is essentially an unchecked version of {@link org.ofbiz.base.util.GeneralException},
 * and some of the subclasses are analogous.
 * <p>
 * 2016: Supports PropertyMessage for delayed localization of error message;
 * code that catches can call {@link #getPropertyMessage} and
 * pass it a locale to localize the message.
 */
@SuppressWarnings("serial")
public class CmsException extends GeneralRuntimeException implements SettablePropertyMessageEx {

    public CmsException() {
    }

    public CmsException(String msg) {
        super(msg);
    }

    public CmsException(String msg, Throwable nested) {
        super(msg, nested);
    }

    public CmsException(Throwable nested) {
        super(nested);
    }

    public CmsException(String msg, Collection<?> messageList) {
        super(msg, messageList);
    }

    public CmsException(String msg, Collection<?> messageList, Throwable nested) {
        super(msg, messageList, nested);
    }

    public CmsException(Collection<?> messageList, Throwable nested) {
        super(messageList, nested);
    }

    public CmsException(Collection<?> messageList) {
        super(messageList);
    }

    public CmsException(PropertyMessage propMsg) {
        super(propMsg);
    }

    public CmsException(PropertyMessage propMsg, Throwable nested) {
        super(propMsg, nested);
    }

}
