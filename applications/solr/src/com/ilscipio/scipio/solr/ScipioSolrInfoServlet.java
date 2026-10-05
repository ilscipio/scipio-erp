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

import javax.servlet.ServletConfig;
import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;

import org.ofbiz.base.util.Debug;

/**
 * Scipio Solr info/helper servlet.
 * <p>
 * WARN: 2018-05-22: This should no longer be used to check Solr initialization state;
 * use {@link com.ilscipio.scipio.solr.SolrUtil#isSystemInitialized()} instead.
 */
@SuppressWarnings("serial")
public class ScipioSolrInfoServlet extends HttpServlet {

    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    /**
     * Used to deduce whether the solr webapp initialization step was reached
     * during Ofbiz initialization.
     */
    private static volatile boolean servletInitStatusReached = false;

    @Override
    public void init(ServletConfig config) throws ServletException {
        super.init(config);
        boolean firstInit = setServletInitStatusReached();
        if (!firstInit) {
            Debug.logInfo("Solr: ScipioSolrInfoServlet: (Non-first) servlet init executed", module);
        }
    }

    private static boolean setServletInitStatusReached() {
        if (!isServletInitStatusReached()) {
            synchronized(ScipioSolrInfoServlet.class) {
                if (!isServletInitStatusReached()) {
                    Debug.logInfo("Solr: ScipioSolrInfoServlet: First servlet init executed", module);
                    servletInitStatusReached = true;
                    return true;
                }
            }
        }
        return false;
    }

    /**
     * Checks if the init method was called for any instance of this servlet.
     * <p>
     * WARN: 2018-05-22: This should no longer be used to check Solr initialization state;
     * use {@link com.ilscipio.scipio.solr.SolrUtil#isSystemInitialized()} instead.
     */
    public static boolean isServletInitStatusReached() {
        return servletInitStatusReached;
    }

}
