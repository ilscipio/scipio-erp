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
package com.ilscipio.scipio.product.category;

import java.util.List;

import org.ofbiz.base.util.GeneralException;

@SuppressWarnings("serial")
public class CatalogTraversalException extends GeneralException {
    public CatalogTraversalException() { super(); }
    public CatalogTraversalException(List<?> messages, Throwable nested) { super(messages, nested); }
    public CatalogTraversalException(List<?> messages) { super(messages); }
    public CatalogTraversalException(String msg, List<?> messages, Throwable nested) { super(msg, messages, nested); }
    public CatalogTraversalException(String msg, List<?> messages) { super(msg, messages); }
    public CatalogTraversalException(String msg, Throwable nested) { super(msg, nested); }
    public CatalogTraversalException(String msg) { super(msg); }
    public CatalogTraversalException(Throwable nested) { super(nested); }

    /**
     * CLEAN STOP - Visitor or Traverser may call this to request a clean stop to traversal; not an error; not logged.
     */
    public static class StopCatalogTraversalException extends CatalogTraversalException {
        public StopCatalogTraversalException() { super(); }
        public StopCatalogTraversalException(List<?> messages) { super(messages); }
        public StopCatalogTraversalException(String msg, List<?> messages) { super(msg, messages); }
        public StopCatalogTraversalException(String msg) { super(msg); }
    }

}