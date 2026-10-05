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
package com.ilscipio.scipio.cms.content;

/**
 * Bean-like in-memory representation of a Cms page.
 *
 * @see CmsPage
 */
public class CmsPageInfo {

    //private String webSiteId;
    private String pageId;

    public CmsPageInfo(CmsPage mapping) {
        //this.webSiteId = mapping.getWebSiteId();
        this.pageId = mapping.getId();
    }

    public CmsPageInfo(String pageId) {
        super();
        this.pageId = pageId;
    }

    public String getPageId() {
        return pageId;
    }

//    public String getWebSiteId() {
//        return webSiteId;
//    }

    public String getLogIdRepr() {
        return CmsPage.getLogIdRepr(pageId, null);
    }

    public String getLogIdReprTargetPage() {
        return CmsPage.getLogIdRepr(pageId, null);
    }

}
