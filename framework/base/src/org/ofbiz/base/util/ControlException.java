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

/**
 * Control operation (SCIPIO).
 * @see AbortException
 * @see ContinueException
 */
public class ControlException extends GeneralException {
    public ControlException() {
    }

    public ControlException(String msg) {
        super(msg);
    }

    public ControlException(String msg, Throwable nested) {
        super(msg, nested);
    }

    public ControlException(Throwable nested) {
        super(nested);
    }

    public ControlException(String msg, List<?> messages) {
        super(msg, messages);
    }

    public ControlException(String msg, List<?> messages, Throwable nested) {
        super(msg, messages, nested);
    }

    public ControlException(List<?> messages, Throwable nested) {
        super(messages, nested);
    }

    public ControlException(List<?> messages) {
        super(messages);
    }

    public ControlException(PropertyMessage propMsg) {
        super(propMsg);
    }

    public ControlException(PropertyMessage propMsg, Throwable nested) {
        super(propMsg, nested);
    }
}
