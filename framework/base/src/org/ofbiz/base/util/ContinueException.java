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
package org.ofbiz.base.util;

import java.util.List;

/** General operation continue or recoverable exception (SCIPIO). */
public class ContinueException extends ControlException {
    public ContinueException() {
    }

    public ContinueException(String msg) {
        super(msg);
    }

    public ContinueException(String msg, Throwable nested) {
        super(msg, nested);
    }

    public ContinueException(Throwable nested) {
        super(nested);
    }

    public ContinueException(String msg, List<?> messages) {
        super(msg, messages);
    }

    public ContinueException(String msg, List<?> messages, Throwable nested) {
        super(msg, messages, nested);
    }

    public ContinueException(List<?> messages, Throwable nested) {
        super(messages, nested);
    }

    public ContinueException(List<?> messages) {
        super(messages);
    }

    public ContinueException(PropertyMessage propMsg) {
        super(propMsg);
    }

    public ContinueException(PropertyMessage propMsg, Throwable nested) {
        super(propMsg, nested);
    }
}
