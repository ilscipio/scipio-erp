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
package com.ilscipio.scipio.cms.data;

import org.ofbiz.base.util.PropertyMessage;

import com.ilscipio.scipio.cms.CmsException;

import java.util.Collection;

/**
 * CMS data exception.
 */
@SuppressWarnings("serial")
public class CmsDataException extends CmsException {

    public CmsDataException() {
    }

    public CmsDataException(String msg) {
        super(msg);
    }

    public CmsDataException(String msg, Throwable nested) {
        super(msg, nested);
    }

    public CmsDataException(Throwable nested) {
        super(nested);
    }

    public CmsDataException(String msg, Collection<?> messageList) {
        super(msg, messageList);
    }

    public CmsDataException(String msg, Collection<?> messageList, Throwable nested) {
        super(msg, messageList, nested);
    }

    public CmsDataException(Collection<?> messageList, Throwable nested) {
        super(messageList, nested);
    }

    public CmsDataException(Collection<?> messageList) {
        super(messageList);
    }

    public CmsDataException(PropertyMessage propMsg) {
        super(propMsg);
    }

    public CmsDataException(PropertyMessage propMsg, Throwable nested) {
        super(propMsg, nested);
    }

    /**
     * Thrown when candidate keys clash which are not enforced otherwise in schema.
     */
    public static class CmsUniqueDataException extends CmsDataException {

        private static final long serialVersionUID = 4868713020052920312L;

        public CmsUniqueDataException() {
        }

        public CmsUniqueDataException(String msg) {
            super(msg);
        }

        public CmsUniqueDataException(String msg, Throwable nested) {
            super(msg, nested);
        }

        public CmsUniqueDataException(Throwable nested) {
            super(nested);
        }

        public CmsUniqueDataException(String msg, Collection<?> messageList) {
            super(msg, messageList);
        }

        public CmsUniqueDataException(String msg, Collection<?> messageList, Throwable nested) {
            super(msg, messageList, nested);
        }

        public CmsUniqueDataException(Collection<?> messageList, Throwable nested) {
            super(messageList, nested);
        }

        public CmsUniqueDataException(Collection<?> messageList) {
            super(messageList);
        }

        public CmsUniqueDataException(PropertyMessage propMsg) {
            super(propMsg);
        }

        public CmsUniqueDataException(PropertyMessage propMsg, Throwable nested) {
            super(propMsg, nested);
        }
    }
}
