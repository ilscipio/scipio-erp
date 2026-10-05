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
package com.ilscipio.scipio.cms.menu;
import java.util.Collections;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.collections.MapStack;
import org.ofbiz.base.util.template.FreeMarkerWorker;
import org.ofbiz.entity.Delegator;

import com.ilscipio.scipio.cms.content.CmsPageContext;
import com.ilscipio.scipio.cms.template.CmsRenderUtil;

import freemarker.core.Environment;
import freemarker.template.TemplateModelException;

/**
 * SCIPIO: CmsMenuUtil
 */
public abstract class CmsMenuUtil {
    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    /**
     * WARN: This overload only works from FTL rendering.
     * FIXME?: This is sketchy could break in future, should use a transform to ensure Environment can be fetched...
     */
    public static CmsMenu getMenuById(Delegator delegator, String menuId) throws IllegalStateException, TemplateModelException {
        return getMenuById(delegator, menuId, FreeMarkerWorker.getCurrentEnvironment());
    }

    public static CmsMenu getMenuById(Delegator delegator, String menuId, Environment env) throws IllegalStateException, TemplateModelException {
        MapStack<String> context = CmsRenderUtil.getRenderContextAlways(env);
        CmsPageContext pageContext = CmsPageContext.getOrMakeFromContext(context);
        // for live renders we will use the cache here.
        final boolean useCache = !pageContext.isPreview();
        return getMenuById(delegator, menuId, useCache);
    }

    public static CmsMenu getMenuById(Delegator delegator, String menuId, boolean useCache) {
        try {
            return CmsMenu.getWorker().findById(delegator, menuId, useCache);
        } catch(Exception e) {
            Debug.logError(e, "Could not get CmsMenu '" + menuId + "'", module);
            return null;
        }
//        GenericValue value;
//        CmsMenu cmsMenu = null;
//        try {
//            value = delegator.findOne("CmsMenu", true, UtilMisc.toMap("menuId", menuId));
//            if(value!=null){
//                cmsMenu = new CmsMenu(value);
//            }
//        } catch (GenericEntityException e) {
//            Debug.logError(e, module);
//        }
//        return cmsMenu;
    }

    /**
     * WARN: This overload only works from FTL rendering.
     * FIXME?: This is sketchy could break in future, should use a transform to ensure Environment can be fetched...
     */
    public static Object getMenuJsonById(Delegator delegator, String menuId) throws IllegalStateException, TemplateModelException {
        return getMenuJsonById(delegator, menuId, FreeMarkerWorker.getCurrentEnvironment());
    }
 
    public static Object getMenuJsonById(Delegator delegator, String menuId, Environment env) throws IllegalStateException, TemplateModelException {
        MapStack<String> context = CmsRenderUtil.getRenderContextAlways(env);
        CmsPageContext pageContext = CmsPageContext.getOrMakeFromContext(context);
        // for live renders we will use the cache here.
        final boolean useCache = !pageContext.isPreview();
        return getMenuJsonById(delegator, menuId, useCache);
    }

    public static Object getMenuJsonById(Delegator delegator, String menuId, boolean useCache) {
        try {
            CmsMenu cmsMenu = CmsMenu.getWorker().findById(delegator, menuId, useCache);
            if (cmsMenu != null) {
                // this is much too inefficient for live use, must be cached
                //JSON json = JSON.from(cmsMenu.getEntityMenuJson());
                //return json.toObject(LinkedList.class);
                return cmsMenu.getParsedMenuJsonList();
            }
        } catch(Exception e) {
            Debug.logError(e, "Could not get CmsMenu '" + menuId + "'", module);
        }
        return Collections.<Object>emptyList();
        // too slow for production use
//        GenericValue value;
//        Object returnObj = new ArrayList<>();
//        try {
//            value = delegator.findOne("CmsMenu", true, UtilMisc.toMap("menuId", menuId));
//            if(value!=null){
//                CmsMenu cmsMenu = new CmsMenu(value);
//                JSON json = JSON.from(cmsMenu.getEntityMenuJson());
//                returnObj = json.toObject(LinkedList.class);
//            }
//        } catch (GenericEntityException e) {
//            Debug.logError(e, module);
//        } catch (IOException e) {
//            Debug.logError(e, module);
//        }
//        return returnObj;
    }
}
