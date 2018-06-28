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
package org.ofbiz.product.config;

import java.io.IOException;
import java.io.Serializable;
import java.io.StringWriter;
import java.io.Writer;
import java.util.HashMap;
import java.util.Locale;
import java.util.Map;

import javax.servlet.http.HttpServletRequest;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.GeneralException;
import org.ofbiz.base.util.StringUtil;
import org.ofbiz.base.util.StringUtil.StringWrapper;
import org.ofbiz.base.util.UtilCodec;
import org.ofbiz.base.util.UtilHttp;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.base.util.cache.UtilCache;
import org.ofbiz.content.content.CommonContentWrapper;
import org.ofbiz.content.content.ContentLangUtil;
import org.ofbiz.content.content.ContentWorker;
import org.ofbiz.content.content.ContentWrapper;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.DelegatorFactory;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.entity.model.ModelEntity;
import org.ofbiz.entity.model.ModelUtil;
import org.ofbiz.entity.util.EntityQuery;
import org.ofbiz.entity.util.EntityUtil;
import org.ofbiz.service.LocalDispatcher;
import org.ofbiz.service.ServiceContainer;

/**
 * Product Config Item Content Worker: gets product content to display
 * <p>
 * SCIPIO: NOTE: 2017: This ContentWrapper is heavily updated from stock for localization behavior, caching, and other fixes.
 */
@SuppressWarnings("serial")
public class ProductConfigItemContentWrapper extends CommonContentWrapper {

    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());
    public static final String SEPARATOR = "::";    // cache key separator
    private static final UtilCache<String, String> configItemContentCache = UtilCache.createUtilCache("configItem.content", true); // use soft reference to free up memory if needed

    public static ProductConfigItemContentWrapper makeProductConfigItemContentWrapper(GenericValue productConfigItem, HttpServletRequest request) {
        return new ProductConfigItemContentWrapper(productConfigItem, request);
    }

    public ProductConfigItemContentWrapper(GenericValue entityValue, HttpServletRequest request, boolean useCache) {
        super(entityValue, request, useCache);
    }

    public ProductConfigItemContentWrapper(GenericValue entityValue, HttpServletRequest request) {
        super(entityValue, request);
    }

    public ProductConfigItemContentWrapper(LocalDispatcher dispatcher, GenericValue entityValue, Locale locale,
            String mimeTypeId, boolean useCache) {
        super(dispatcher, entityValue, locale, mimeTypeId, useCache);
    }

    public ProductConfigItemContentWrapper(LocalDispatcher dispatcher, GenericValue entityValue, Locale locale,
            String mimeTypeId) {
        super(dispatcher, entityValue, locale, mimeTypeId);
    }

    @Override
    protected String getImpl(String contentTypeId, boolean useCache, String contentLang) {
        return getProductConfigItemContentAsText(getEntityValue(), contentTypeId, getLocale(), getMimeTypeId(), getDelegator(), getDispatcher(), useCache, contentLang);
    }

    public static String getProductConfigItemContentAsText(GenericValue productConfigItem, String confItemContentTypeId, HttpServletRequest request, String encoderType) {
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        return getProductConfigItemContentAsText(productConfigItem, confItemContentTypeId, UtilHttp.getLocale(request), "text/html", productConfigItem.getDelegator(), dispatcher, encoderType);
    }

    public static String getProductConfigItemContentAsText(GenericValue productConfigItem, String confItemContentTypeId, Locale locale, LocalDispatcher dispatcher, String encoderType) {
        return getProductConfigItemContentAsText(productConfigItem, confItemContentTypeId, locale, null, null, dispatcher, encoderType);
    }
    
    /**
     * SCIPIO: Gets content as text, with option to bypass wrapper cache.
     */
    public static String getProductConfigItemContentAsText(GenericValue productConfigItem, String confItemContentTypeId, Locale locale, LocalDispatcher dispatcher, boolean useCache, String encoderType) {
        return getProductConfigItemContentAsText(productConfigItem, confItemContentTypeId, locale, null, null, dispatcher, useCache, encoderType);
    }

    /**
     * Gets content as text, with wrapper cache enabled.
     * SCIPIO: delegating.
     */
    public static String getProductConfigItemContentAsText(GenericValue productConfigItem, String confItemContentTypeId, Locale locale, String mimeTypeId, Delegator delegator, LocalDispatcher dispatcher, String encoderType) {
        return getProductConfigItemContentAsText(productConfigItem, confItemContentTypeId, locale, mimeTypeId, delegator, dispatcher, true, encoderType);
    }
    
    /**
     * SCIPIO: Gets content as text, with option to bypass wrapper cache.
     */
    public static String getProductConfigItemContentAsText(GenericValue productConfigItem, String confItemContentTypeId, Locale locale, String mimeTypeId, Delegator delegator, LocalDispatcher dispatcher, boolean useCache, String encoderType) {
        UtilCodec.SimpleEncoder encoder = ContentLangUtil.getContentWrapperSanitizer(encoderType);
        String candidateFieldName = ModelUtil.dbNameToVarName(confItemContentTypeId);
        String cacheKey = (useCache) ? confItemContentTypeId + SEPARATOR + locale + SEPARATOR + mimeTypeId + SEPARATOR + productConfigItem.get("configItemId") + SEPARATOR + encoder.getLang() + SEPARATOR + delegator : null;
        try {
            if (useCache) {
                String cachedValue = configItemContentCache.get(cacheKey);
                if (cachedValue != null) {
                    return cachedValue;
                }
            }
            Writer outWriter = new StringWriter();
            getProductConfigItemContentAsText(null, productConfigItem, confItemContentTypeId, locale, mimeTypeId, delegator, dispatcher, outWriter, false);
            String outString = outWriter.toString();
            if (UtilValidate.isEmpty(outString)) {
                outString = productConfigItem.getModelEntity().isField(candidateFieldName) ? productConfigItem.getString(candidateFieldName): "";
                outString = outString == null? "" : outString;
            }
            outString = encoder.sanitize(outString);
            if (useCache && configItemContentCache != null) {
                configItemContentCache.put(cacheKey, outString);
            }
            return outString;
        } catch (GeneralException e) {
            Debug.logError(e, "Error rendering ProdConfItemContent, inserting empty String", module);
            String candidateOut = productConfigItem.getModelEntity().isField(candidateFieldName) ? productConfigItem.getString(candidateFieldName): "";
            return candidateOut == null? "" : encoder.sanitize(candidateOut);
        } catch (IOException e) {
            Debug.logError(e, "Error rendering ProdConfItemContent, inserting empty String", module);
            String candidateOut = productConfigItem.getModelEntity().isField(candidateFieldName) ? productConfigItem.getString(candidateFieldName): "";
            return candidateOut == null? "" : encoder.sanitize(candidateOut);
        }
    }

    public static void getProductConfigItemContentAsText(String configItemId, GenericValue productConfigItem, String confItemContentTypeId, Locale locale, String mimeTypeId, Delegator delegator, LocalDispatcher dispatcher, Writer outWriter) throws GeneralException, IOException {
        getProductConfigItemContentAsText(configItemId, productConfigItem, confItemContentTypeId, locale, mimeTypeId, delegator, dispatcher, outWriter, true);
    }

    public static void getProductConfigItemContentAsText(String configItemId, GenericValue productConfigItem, String confItemContentTypeId, Locale locale, String mimeTypeId, Delegator delegator, LocalDispatcher dispatcher, Writer outWriter, boolean cache) throws GeneralException, IOException {
        if (configItemId == null && productConfigItem != null) {
            configItemId = productConfigItem.getString("configItemId");
        }

        if (delegator == null && productConfigItem != null) {
            delegator = productConfigItem.getDelegator();
        }

        if (UtilValidate.isEmpty(mimeTypeId)) {
            mimeTypeId = "text/html";
        }

        GenericValue productConfigItemContent = EntityQuery.use(delegator).from("ProdConfItemContent")
                .where("configItemId", configItemId, "confItemContentTypeId", confItemContentTypeId)
                .orderBy("-fromDate")
                .cache(cache)
                .filterByDate()
                .queryFirst();
        if (productConfigItemContent != null) {
            // when rendering the product config item content, always include the ProductConfigItem and ProdConfItemContent records that this comes from
            Map<String, Object> inContext = new HashMap<>();
            inContext.put("productConfigItem", productConfigItem);
            inContext.put("productConfigItemContent", productConfigItemContent);
            ContentWorker.renderContentAsText(dispatcher, delegator, productConfigItemContent.getString("contentId"), outWriter, inContext, locale, mimeTypeId, null, null, cache);
            return;
        }
        
        String candidateFieldName = ModelUtil.dbNameToVarName(confItemContentTypeId);
        ModelEntity productConfigItemModel = delegator.getModelEntity("ProductConfigItem");
        if (productConfigItemModel.isField(candidateFieldName)) {
            if (productConfigItem == null) {
                productConfigItem = EntityQuery.use(delegator).from("ProductConfigItem").where("configItemId", configItemId).cache(cache).queryOne();
            }
            if (productConfigItem != null) {
                String candidateValue = productConfigItem.getString(candidateFieldName);
                if (UtilValidate.isNotEmpty(candidateValue)) {
                    outWriter.write(candidateValue);
                    return;
                }
            }
        }
    }
}

