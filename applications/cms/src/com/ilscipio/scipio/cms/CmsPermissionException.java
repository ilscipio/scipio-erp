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

@SuppressWarnings("serial")
public class CmsPermissionException extends CmsException {

    public CmsPermissionException() {
    }

    public CmsPermissionException(String msg) {
        super(msg);
    }

    public CmsPermissionException(String msg, Throwable nested) {
        super(msg, nested);
    }

    public CmsPermissionException(Throwable nested) {
        super(nested);
    }

    public CmsPermissionException(String msg, Collection<?> messageList) {
        super(msg, messageList);
    }

    public CmsPermissionException(String msg, Collection<?> messageList, Throwable nested) {
        super(msg, messageList, nested);
    }

    public CmsPermissionException(Collection<?> messageList, Throwable nested) {
        super(messageList, nested);
    }

    public CmsPermissionException(Collection<?> messageList) {
        super(messageList);
    }

    public CmsPermissionException(PropertyMessage propMsg) {
        super(propMsg);
    }

    public CmsPermissionException(PropertyMessage propMsg, Throwable nested) {
        super(propMsg, nested);
    }

}
