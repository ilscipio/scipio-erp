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
package com.ilscipio.scipio.cms.control.cmscall.render;

import java.io.Writer;

import javax.servlet.ServletContext;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import com.ilscipio.scipio.cms.content.CmsPage;
import com.ilscipio.scipio.cms.control.CmsView;
import com.ilscipio.scipio.cms.control.cmscall.CmsCallException;
import com.ilscipio.scipio.cms.control.cmscall.CmsCallType;
import com.ilscipio.scipio.cms.control.cmscall.CmsInvoker;
import org.ofbiz.webapp.view.ViewHandler;

public abstract class RenderInvoker extends CmsInvoker {

    //private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    protected RenderInvoker(ServletContext servletCtx) {
        super(servletCtx);
    }

    public static RenderInvoker getRenderInvoker(ServletContext servletCtx) {
        // Using local CMS page
        return LocalCmsPageRenderInvoker.getRenderInvoker(servletCtx);
    }

    // 2016: better to pass CmsPage for local
//    public abstract void invokeCmsRendering(HttpServletRequest request, HttpServletResponse response,
//            ServletContext servletCtx, CmsPageInfo page, CmsView view, String webSiteId) throws CmsCallException, Exception;

    public abstract void invokeCmsRendering(HttpServletRequest request, HttpServletResponse response,
                                            ServletContext servletCtx, CmsPage cmsPage, CmsView view, String webSiteId,
                                            CmsCallType renderMode, Writer writer) throws CmsCallException, Exception;

    public void invokeCmsRendering(ViewHandler.ViewRenderContext vrctx, CmsPage cmsPage, CmsView view, String webSiteId,
                                   CmsCallType renderMode) throws CmsCallException, Exception {
        invokeCmsRendering(vrctx.request(), vrctx.response(), vrctx.request().getServletContext(), cmsPage, view, webSiteId, renderMode, vrctx.renderWriter());
    }

}
