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
package com.ilscipio.scipio.cms.webapp;


/**
 * Exception thrown when an otherwise valid webapp ID of any sort is used to and
 * fails to resolve to a webapp.
 */
@SuppressWarnings("serial")
public class InvalidWebappException extends WebappException {

    public InvalidWebappException(String message) {
        super(message);
    }

    public InvalidWebappException(Throwable cause) {
        super(cause);
    }

    public InvalidWebappException(String message, Throwable cause) {
        super(message, cause);
    }

}
