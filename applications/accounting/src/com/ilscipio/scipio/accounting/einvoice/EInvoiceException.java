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
package com.ilscipio.scipio.accounting.einvoice;

import java.util.Collections;
import java.util.List;

/** An e-invoice cannot be made or read; {@link #getMessages()} holds the reasons for the user. */
@SuppressWarnings("serial")
public class EInvoiceException extends Exception {

    private final List<String> messages;

    public EInvoiceException(String message) {
        this(message, (Throwable) null);
    }

    public EInvoiceException(String message, Throwable cause) {
        super(message, cause);
        this.messages = Collections.singletonList(message);
    }

    public EInvoiceException(List<String> messages) {
        super(String.join("; ", messages));
        this.messages = Collections.unmodifiableList(messages);
    }

    public List<String> getMessages() {
        return messages;
    }
}
