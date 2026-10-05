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
package com.ilscipio.scipio.accounting.external.datev;

public class DatevException extends Exception {
    private static final long serialVersionUID = -5965872197826715315L;

    final String message;

    public DatevException() {
        super();
        this.message = null;
    }

    public DatevException(String message) {
        super(message);
        this.message = message;
    }

    public DatevException(String message, Throwable t) {
        super(message, t);
        this.message = message;
    }

    @Override
    public String getMessage() {
        return message;
    }

}
