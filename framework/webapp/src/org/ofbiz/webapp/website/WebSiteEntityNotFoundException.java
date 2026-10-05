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
package org.ofbiz.webapp.website;

import org.ofbiz.entity.GenericEntityException;

public class WebSiteEntityNotFoundException extends GenericEntityException {

    private static final long serialVersionUID = -5601192509173423208L;

    private final String webSiteId;

    public WebSiteEntityNotFoundException(String webSiteId) {
        this.webSiteId = webSiteId;
    }

    public WebSiteEntityNotFoundException(String message, String webSiteId) {
        super(message);
        this.webSiteId = webSiteId;
    }

    public WebSiteEntityNotFoundException(String message, Throwable cause, String webSiteId) {
        super(message, cause);
        this.webSiteId = webSiteId;
    }

    public WebSiteEntityNotFoundException(Throwable cause, String webSiteId) {
        super(cause);
        this.webSiteId = webSiteId;
    }

    public String getWebSiteId() {
        return webSiteId;
    }

}
