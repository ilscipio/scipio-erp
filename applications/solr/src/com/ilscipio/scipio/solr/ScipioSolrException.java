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
package com.ilscipio.scipio.solr;

/**
 * Scipio Solr Exception.
 * <p>
 * NOTE: 2018-02-21: This is not currently in use much.
 */
@SuppressWarnings("serial")
public class ScipioSolrException extends Exception {

    private boolean lightweight = false;

    public ScipioSolrException() {
    }

    public ScipioSolrException(String message) {
        super(message);
    }

    public ScipioSolrException(Throwable cause) {
        super(cause);
    }

    public ScipioSolrException(String message, Throwable cause) {
        super(message, cause);
    }

    public boolean isLightweight() {
        return lightweight;
    }

    public ScipioSolrException setLightweight(boolean lightweight) {
        this.lightweight = lightweight;
        return this;
    }
}
