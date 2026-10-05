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
package com.ilscipio.scipio.cms.control.cmscall;

public enum CmsCallType {
    // PAGE_INFO,

    OFBIZ_RENDER,
    OFBIZ_PREVIEW, // new 2016
    CMS_ADMIN_APP,
    CMS_STATIC_RENDER;

    public boolean cachingAllowed() {
        // 2016: MUST NOT CACHE PREVIEWS!
        return this != OFBIZ_PREVIEW;
    }

    public boolean isPreview() {
        return this == OFBIZ_PREVIEW;
    }

    public boolean representCaller() {
        if (this == OFBIZ_RENDER || this == OFBIZ_PREVIEW) {
            return true;
        } else {
            return false;
        }
    }

    public boolean requiresServerRequest() {
        if (this == OFBIZ_RENDER || this == OFBIZ_PREVIEW || this == CMS_STATIC_RENDER) {
            return true;
        } else {
            return false;
        }
    }

    public boolean supportsServerRequest() {
        if (this == OFBIZ_RENDER || this == OFBIZ_PREVIEW || this == CMS_STATIC_RENDER) {
            return true;
        } else {
            return false;
        }
    }

    public boolean requiresCallParams() {
        if (this == OFBIZ_RENDER || this == OFBIZ_PREVIEW) {
            return true;
        } else {
            return false;
        }
    }
}
