/*******************************************************************************
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements.  See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership.  The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License.  You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations
 * under the License.
 *******************************************************************************/
package org.ofbiz.product.product;

import java.io.IOException;
import java.io.StringWriter;
import java.io.Writer;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;

import javax.servlet.http.HttpServletRequest;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.GeneralException;
import org.ofbiz.base.util.GeneralRuntimeException;
import org.ofbiz.base.util.UtilCodec;
import org.ofbiz.base.util.UtilHttp;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.base.util.cache.UtilCache;
import org.ofbiz.content.content.CommonContentWrapper;
import org.ofbiz.content.content.ContentLangUtil;
import org.ofbiz.content.content.ContentWorker;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.entity.condition.EntityCondition;
import org.ofbiz.entity.condition.EntityExpr;
import org.ofbiz.entity.condition.EntityOperator;
import org.ofbiz.entity.model.ModelEntity;
import org.ofbiz.entity.model.ModelUtil;
import org.ofbiz.entity.util.EntityQuery;
import org.ofbiz.entity.util.EntityUtil;
import org.ofbiz.service.LocalDispatcher;

/**
 * Product Promo Content Worker: gets product promo content to display
 * <p>
 * SCIPIO: NOTE: 2017: This ContentWrapper is heavily updated from stock for localization behavior, caching, and other fixes.
 */
@SuppressWarnings("serial")
public class ProductPromoContentWrapper extends CommonContentWrapper {

    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());
    public static final String SEPARATOR = "::";    // cache key separator

    private static final UtilCache<String, String> productPromoContentCache = UtilCache.createUtilCache("product.promo.content.rendered", true);

    public static ProductPromoContentWrapper makeProductPromoContentWrapper(GenericValue productPromo, HttpServletRequest request) {
        return new ProductPromoContentWrapper(productPromo, request);
    }

    public ProductPromoContentWrapper(GenericValue entityValue, HttpServletRequest request, boolean useCache) {
        super(entityValue, request, useCache);
    }

    public ProductPromoContentWrapper(GenericValue entityValue, HttpServletRequest request) {
        super(entityValue, request);
    }

    public ProductPromoContentWrapper(LocalDispatcher dispatcher, GenericValue entityValue, Locale locale,
            String mimeTypeId, boolean useCache) {
        super(dispatcher, entityValue, locale, mimeTypeId, useCache);
    }

    public ProductPromoContentWrapper(LocalDispatcher dispatcher, GenericValue entityValue, Locale locale,
            String mimeTypeId) {
        super(dispatcher, entityValue, locale, mimeTypeId);
    }

    @Override
    protected String getImpl(String contentTypeId, boolean useCache, String contentLang) {
        return getProductPromoContentAsText(getEntityValue(), contentTypeId, getLocale(), getMimeTypeId(), null, null, getDelegator(), getDispatcher(), useCache, contentLang);
    }

    public static String getProductPromoContentAsText(GenericValue productPromo, String productPromoContentTypeId, HttpServletRequest request, String encoderType) {
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        return getProductPromoContentAsText(productPromo, productPromoContentTypeId, UtilHttp.getLocale(request), "text/html", null, null, productPromo.getDelegator(), dispatcher, encoderType);
    }

    public static String getProductContentAsText(GenericValue productPromo, String productPromoContentTypeId, Locale locale, LocalDispatcher dispatcher, String encoderType) {
        return getProductPromoContentAsText(productPromo, productPromoContentTypeId, locale, null, null, null, null, dispatcher, encoderType);
    }

    /**
     * SCIPIO: Gets content as text, with option to bypass wrapper cache.
     */
    public static String getProductContentAsText(GenericValue productPromo, String productPromoContentTypeId, Locale locale, LocalDispatcher dispatcher, boolean useCache, String encoderType) {
        return getProductPromoContentAsText(productPromo, productPromoContentTypeId, locale, null, null, null, null, dispatcher, useCache, encoderType);
    }

    /**
     * Gets content as text, with wrapper cache enabled.
     * SCIPIO: delegating.
     */
    public static String getProductPromoContentAsText(GenericValue productPromo, String productPromoContentTypeId, Locale locale, String mimeTypeId, String partyId, String roleTypeId, Delegator delegator, LocalDispatcher dispatcher, String encoderType) {
        return getProductPromoContentAsText(productPromo, productPromoContentTypeId, locale, mimeTypeId, partyId, roleTypeId, delegator, dispatcher, true, encoderType);
    }

    /**
     * SCIPIO: Gets content as text, with option to bypass wrapper cache.
     */
    public static String getProductPromoContentAsText(GenericValue productPromo, String productPromoContentTypeId, Locale locale, String mimeTypeId, String partyId, String roleTypeId, Delegator delegator, LocalDispatcher dispatcher, boolean useCache, String encoderType) {
        if (UtilValidate.isEmpty(productPromo)) {
            return null;
        }

        UtilCodec.SimpleEncoder encoder = ContentLangUtil.getContentWrapperSanitizer(encoderType);
        String candidateFieldName = ModelUtil.dbNameToVarName(productPromoContentTypeId);
        /* caching: there is one cache created, "product.promo.content"  Each productPromo's content is cached with a key of
         * contentTypeId::locale::mimeType::productPromoId, or whatever the SEPARATOR is defined above to be.
         */
        String cacheKey = (useCache) ? productPromoContentTypeId + SEPARATOR + locale + SEPARATOR + mimeTypeId + SEPARATOR + productPromo.get("productPromoId") + SEPARATOR + encoder.getLang() + SEPARATOR + delegator : null;
        try {
            if (useCache) {
                String cachedValue = productPromoContentCache.get(cacheKey);
                if (cachedValue != null) {
                    return cachedValue;
                }
            }

            Writer outWriter = new StringWriter();
            getProductPromoContentAsText(null, productPromo, productPromoContentTypeId, locale, mimeTypeId, partyId, roleTypeId, delegator, dispatcher, outWriter);
            String outString = outWriter.toString();
            if (UtilValidate.isEmpty(outString)) {
                outString = productPromo.getModelEntity().isField(candidateFieldName) ? productPromo.getString(candidateFieldName): "";
                outString = outString == null? "" : outString;
            }
            outString = encoder.sanitize(outString);
            if (useCache && productPromoContentCache != null) {
                productPromoContentCache.put(cacheKey, outString);
            }
            return outString;
        } catch (GeneralException e) {
            Debug.logError(e, "Error rendering ProductPromoContent, inserting empty String", module);
            String candidateOut = productPromo.getModelEntity().isField(candidateFieldName) ? productPromo.getString(candidateFieldName): "";
            return candidateOut == null? "" : encoder.sanitize(candidateOut);
        } catch (IOException e) {
            Debug.logError(e, "Error rendering ProductPromoContent, inserting empty String", module);
            String candidateOut = productPromo.getModelEntity().isField(candidateFieldName) ? productPromo.getString(candidateFieldName): "";
            return candidateOut == null? "" : encoder.sanitize(candidateOut);
        }
    }

    public static void getProductPromoContentAsText(String productPromoId, GenericValue productPromo, String productPromoContentTypeId, Locale locale, String mimeTypeId, String partyId, String roleTypeId, Delegator delegator, LocalDispatcher dispatcher, Writer outWriter) throws GeneralException, IOException {
        getProductPromoContentAsText(productPromoId, productPromo, productPromoContentTypeId, locale, mimeTypeId, partyId, roleTypeId, delegator, dispatcher, outWriter, true);
    }

    public static void getProductPromoContentAsText(String productPromoId, GenericValue productPromo, String productPromoContentTypeId, Locale locale, String mimeTypeId, String partyId, String roleTypeId, Delegator delegator, LocalDispatcher dispatcher, Writer outWriter, boolean cache) throws GeneralException, IOException {
        if (UtilValidate.isEmpty(productPromoId) && productPromo != null) {
            productPromoId = productPromo.getString("productPromoId");
        }

        if (UtilValidate.isEmpty(delegator) && productPromo != null) {
            delegator = productPromo.getDelegator();
        }

        if (UtilValidate.isEmpty(mimeTypeId)) {
            mimeTypeId = "text/html";
        }

        if (UtilValidate.isEmpty(delegator)) {
            throw new GeneralRuntimeException("Unable to find a delegator to use!");
        }

        List<EntityExpr> exprs = new ArrayList<EntityExpr>();
        exprs.add(EntityCondition.makeCondition("productPromoId", EntityOperator.EQUALS, productPromoId));
        exprs.add(EntityCondition.makeCondition("productPromoContentTypeId", EntityOperator.EQUALS, productPromoContentTypeId));

        List<GenericValue> productPromoContentList = EntityQuery.use(delegator).from("ProductPromoContent").where(EntityCondition.makeCondition(exprs, EntityOperator.AND)).orderBy("-fromDate").cache(cache).filterByDate().queryList(); // SCIPIO: added filterByDate
        // SCIPIO: covered by query and EntityUtil
        //GenericValue productPromoContent = null;
        //if (UtilValidate.isNotEmpty(productPromoContentList)) {
        //    productPromoContent = EntityUtil.getFirst(EntityUtil.filterByDate(productPromoContentList));
        //}
        GenericValue productPromoContent = EntityUtil.getFirst(productPromoContentList);

        if (productPromoContent != null) {
            // when rendering the product promo content, always include the ProductPromo and ProductPromoContent records that this comes from
            Map<String, Object> inContext = new HashMap<>();
            inContext.put("productPromo", productPromo);
            inContext.put("productPromoContent", productPromoContent);
            ContentWorker.renderContentAsText(dispatcher, delegator, productPromoContent.getString("contentId"), outWriter, inContext, locale, mimeTypeId, partyId, roleTypeId, cache);
            return;
        }

        String candidateFieldName = ModelUtil.dbNameToVarName(productPromoContentTypeId);
        ModelEntity productModel = delegator.getModelEntity("ProductPromo");
        if (productModel.isField(candidateFieldName)) {
            if (productPromo == null) {
                productPromo = EntityQuery.use(delegator).from("ProductPromo").where("productPromoId", productPromoId).cache(cache).queryOne();
            }
            if (productPromo != null) {
                String candidateValue = productPromo.getString(candidateFieldName);
                if (UtilValidate.isNotEmpty(candidateValue)) {
                    outWriter.write(candidateValue);
                    return;
                }
            }
        }
    }
}
