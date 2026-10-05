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
package com.ilscipio.scipio.product.seo;

import java.util.List;

import org.ofbiz.base.util.GeneralException;

@SuppressWarnings("serial")
public class SeoCatalogException extends GeneralException {

    public SeoCatalogException() {
        super();
    }

    public SeoCatalogException(List<?> messages, Throwable nested) {
        super(messages, nested);
    }

    public SeoCatalogException(List<?> messages) {
        super(messages);
    }

    public SeoCatalogException(String msg, List<?> messages, Throwable nested) {
        super(msg, messages, nested);
    }

    public SeoCatalogException(String msg, List<?> messages) {
        super(msg, messages);
    }

    public SeoCatalogException(String msg, Throwable nested) {
        super(msg, nested);
    }

    public SeoCatalogException(String msg) {
        super(msg);
    }

    public SeoCatalogException(Throwable nested) {
        super(nested);
    }

}