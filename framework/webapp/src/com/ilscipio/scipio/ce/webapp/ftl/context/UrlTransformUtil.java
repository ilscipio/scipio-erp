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
package com.ilscipio.scipio.ce.webapp.ftl.context;

import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.webapp.FullWebappInfo;
import org.ofbiz.webapp.renderer.RenderEnvType;

import com.ilscipio.scipio.ce.webapp.ftl.template.TemplateFtlUtil;

import freemarker.core.Environment;
import freemarker.template.TemplateModelException;

public abstract class UrlTransformUtil {

    protected UrlTransformUtil() {
    }

    /**
     * Tries to determine the fullPath Boolean value to use when generating the link,
     * and whether it should be forced or not, based on the rendering context.
     */
    public static Boolean determineFullPath(Boolean fullPathArg, RenderEnvType renderEnvType, Environment env) throws TemplateModelException {
        return determineFullPath(fullPathArg, renderEnvType);
    }

    /**
     * Tries to determine the fullPath Boolean value to use when generating the link,
     * and whether it should be forced or not, based on the rendering context.
     */
    public static Boolean determineFullPath(Boolean fullPathArg, RenderEnvType renderEnvType) {
        return renderEnvType.isStatic() ? Boolean.TRUE : fullPathArg;
    }

    /**
     * Determines what webSiteId arg should be used for link building.
     * <p>
     * NOTE: This only fishes the webSiteId from context if we're in static render context,
     * because in webapp context we need to leave the webSiteId arg empty.
     * <p>
     * @see org.ofbiz.common.email.NotificationServices#setBaseUrl
     */
    public static String determineWebSiteId(String webSiteIdArg, RenderEnvType renderEnvType, FullWebappInfo currentWebappInfo, Environment env) throws TemplateModelException {
        if (renderEnvType.isStatic() && UtilValidate.isEmpty(webSiteIdArg) && currentWebappInfo != null) {
            webSiteIdArg = currentWebappInfo.getWebSiteId();
        }
        return webSiteIdArg;
    }

    /**
     * Escapes a URL built by a transform such as ofbizUrl IF a language is specified.
     * <p>
     * WARN/FIXME?: 2016-10-19: THE STRICT BOOLEAN IS CURRENTLY IGNORED HERE BECAUSE THERE ARE TOO
     * MANY ESCAPED AMPERSANDS THROUGHOUT ALL OF OFBIZ AND TEMPLATES.
     */
    public static String escapeGeneratedUrl(String value, String lang, boolean strict, Environment env) throws TemplateModelException {
        //return TemplateFtlUtil.escapeFullUrl(value, lang, strict, env); // TODO/FIXME?
        return TemplateFtlUtil.escapeFullUrl(value, lang, null, env);
    }
}
