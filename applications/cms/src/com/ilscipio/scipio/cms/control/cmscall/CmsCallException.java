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
package com.ilscipio.scipio.cms.control.cmscall;

import org.ofbiz.base.util.PropertyMessage;

import com.ilscipio.scipio.cms.CmsException;

import java.util.Collection;

/**
 * Exceptions thrown during or related to CMS invocations.
 */
@SuppressWarnings("serial")
public class CmsCallException extends CmsException {

    public CmsCallException() {
    }

    public CmsCallException(String msg) {
        super(msg);
    }

    public CmsCallException(String msg, Throwable nested) {
        super(msg, nested);
    }

    public CmsCallException(Throwable nested) {
        super(nested);
    }

    public CmsCallException(String msg, Collection<?> messageList) {
        super(msg, messageList);
    }

    public CmsCallException(String msg, Collection<?> messageList, Throwable nested) {
        super(msg, messageList, nested);
    }

    public CmsCallException(Collection<?> messageList, Throwable nested) {
        super(messageList, nested);
    }

    public CmsCallException(Collection<?> messageList) {
        super(messageList);
    }

    public CmsCallException(PropertyMessage propMsg) {
        super(propMsg);
    }

    public CmsCallException(PropertyMessage propMsg, Throwable nested) {
        super(propMsg, nested);
    }

}
