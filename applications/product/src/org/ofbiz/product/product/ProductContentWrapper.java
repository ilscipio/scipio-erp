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
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;

import javax.servlet.http.HttpServletRequest;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.GeneralException;
import org.ofbiz.base.util.GeneralRuntimeException;
import org.ofbiz.base.util.StringUtil;
import org.ofbiz.base.util.UtilCodec;
import org.ofbiz.base.util.UtilHttp;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.base.util.cache.UtilCache;
import org.ofbiz.content.content.CommonContentWrapper;
import org.ofbiz.content.content.ContentLangUtil;
import org.ofbiz.content.content.ContentLangUtil.ContentSanitizer;
import org.ofbiz.content.content.ContentWorker;
import org.ofbiz.content.content.ContentWrapper;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.entity.model.ModelEntity;
import org.ofbiz.entity.model.ModelUtil;
import org.ofbiz.entity.util.EntityQuery;
import org.ofbiz.entity.util.EntityUtil;
import org.ofbiz.service.LocalDispatcher;

/**
 * Product Content Worker: gets product content to display
 * <p>
 * SCIPIO: NOTE: 2017: This ContentWrapper is heavily updated from stock for localization behavior, caching, and other fixes.
 */
@SuppressWarnings("serial")
public class ProductContentWrapper extends CommonContentWrapper {

    public static final String module = ProductContentWrapper.class.getName();
    public static final String SEPARATOR = "::";    // cache key separator

    private static final UtilCache<String, String> productContentCache = UtilCache.createUtilCache("product.content.rendered", true);

    public static ProductContentWrapper makeProductContentWrapper(GenericValue product, HttpServletRequest request) {
        return new ProductContentWrapper(product, request);
    }

    public ProductContentWrapper(GenericValue entityValue, HttpServletRequest request, boolean useCache) {
        super(entityValue, request, useCache);
    }

    public ProductContentWrapper(GenericValue entityValue, HttpServletRequest request) {
        super(entityValue, request);
    }
    
    public ProductContentWrapper(LocalDispatcher dispatcher, GenericValue entityValue, Locale locale, String mimeTypeId,
            boolean useCache) {
        super(dispatcher, entityValue, locale, mimeTypeId, useCache);
    }

    public ProductContentWrapper(LocalDispatcher dispatcher, GenericValue entityValue, Locale locale,
            String mimeTypeId) {
        super(dispatcher, entityValue, locale, mimeTypeId);
    }

    @Override
    protected String getImpl(String contentTypeId, boolean useCache, String contentLang) {
        return getProductContentAsText(getEntityValue(), contentTypeId, getLocale(), getMimeTypeId(), null, null, getDelegator(), getDispatcher(), useCache, contentLang);
    }

    public static String getProductContentAsText(GenericValue product, String productContentTypeId, HttpServletRequest request, String encoderType) {
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        return getProductContentAsText(product, productContentTypeId, UtilHttp.getLocale(request), "text/html", null, null, product.getDelegator(), dispatcher, encoderType);
    }

    public static String getProductContentAsText(GenericValue product, String productContentTypeId, Locale locale, LocalDispatcher dispatcher, String encoderType) {
        return getProductContentAsText(product, productContentTypeId, locale, null, null, null, null, dispatcher, encoderType);
    }
    
    /**
     * SCIPIO: Gets content as text, with option to bypass wrapper cache.
     */
    public static String getProductContentAsText(GenericValue product, String productContentTypeId, Locale locale, LocalDispatcher dispatcher, boolean useCache, String encoderType) {
        return getProductContentAsText(product, productContentTypeId, locale, null, null, null, null, dispatcher, useCache, encoderType);
    }

    /**
     * Gets content as text, with wrapper cache enabled.
     * SCIPIO: delegating.
     */
    public static String getProductContentAsText(GenericValue product, String productContentTypeId, Locale locale, String mimeTypeId, String partyId, 
            String roleTypeId, Delegator delegator, LocalDispatcher dispatcher, String encoderType) {
        return getProductContentAsText(product, productContentTypeId, locale, mimeTypeId, partyId, roleTypeId, delegator, dispatcher, true, encoderType);
    }
    
    /**
     * SCIPIO: Gets content as text, with option to bypass wrapper cache.
     */
    public static String getProductContentAsText(GenericValue product, String productContentTypeId, Locale locale, String mimeTypeId, String partyId, 
            String roleTypeId, Delegator delegator, LocalDispatcher dispatcher, boolean useCache, String encoderType) {
        if (product == null) {
            return null;
        }

        ContentSanitizer encoder = ContentLangUtil.getContentWrapperSanitizer(encoderType);
        String candidateFieldName = ModelUtil.dbNameToVarName(productContentTypeId);
        /* caching: there is one cache created, "product.content"  Each product's content is cached with a key of
         * contentTypeId::locale::mimeType::productId, or whatever the SEPARATOR is defined above to be.
         */
        String cacheKey = (useCache) ? productContentTypeId + SEPARATOR + locale + SEPARATOR + mimeTypeId + SEPARATOR + product.get("productId") + SEPARATOR + encoder.getLang() + SEPARATOR + delegator : "";
        try {
            if (useCache) {
                String cachedValue = productContentCache.get(cacheKey);
                if (cachedValue != null) {
                    return cachedValue;
                }
            }

            Writer outWriter = new StringWriter();
            getProductContentAsText(null, product, productContentTypeId, locale, mimeTypeId, partyId, roleTypeId, delegator, dispatcher, outWriter, false);
            String outString = outWriter.toString();
            if (UtilValidate.isEmpty(outString)) {
                outString = product.getModelEntity().isField(candidateFieldName) ? product.getString(candidateFieldName): "";
                outString = outString == null? "" : outString;
            }
            outString = encoder.encode(outString);
            if (useCache && productContentCache != null) {
                productContentCache.put(cacheKey, outString);
            }
            return outString;
        } catch (GeneralException e) {
            Debug.logError(e, "Error rendering ProductContent, inserting empty String", module);
            String candidateOut = product.getModelEntity().isField(candidateFieldName) ? product.getString(candidateFieldName): "";
            return candidateOut == null? "" : encoder.encode(candidateOut);
        } catch (IOException e) {
            Debug.logError(e, "Error rendering ProductContent, inserting empty String", module);
            String candidateOut = product.getModelEntity().isField(candidateFieldName) ? product.getString(candidateFieldName): "";
            return candidateOut == null? "" : encoder.encode(candidateOut);
        }
    }

    public static void getProductContentAsText(String productId, GenericValue product, String productContentTypeId, Locale locale, String mimeTypeId, String partyId, String roleTypeId, Delegator delegator, LocalDispatcher dispatcher, Writer outWriter) throws GeneralException, IOException {
        getProductContentAsText(productId, product, productContentTypeId, locale, mimeTypeId, partyId, roleTypeId, delegator, dispatcher, outWriter, true);
    }
    
    public static void getProductContentAsText(String productId, GenericValue product, String productContentTypeId, Locale locale, String mimeTypeId, String partyId, String roleTypeId, Delegator delegator, LocalDispatcher dispatcher, Writer outWriter, boolean cache) throws GeneralException, IOException {
        if (productId == null && product != null) {
            productId = product.getString("productId");
        }

        if (delegator == null && product != null) {
            delegator = product.getDelegator();
        }

        if (UtilValidate.isEmpty(mimeTypeId)) {
            mimeTypeId = "text/html";
        }

        if (delegator == null) {
            throw new GeneralRuntimeException("Unable to find a delegator to use!");
        }

        List<GenericValue> productContentList = EntityQuery.use(delegator).from("ProductContent").where("productId", productId, "productContentTypeId", productContentTypeId).orderBy("-fromDate").cache(cache).filterByDate().queryList();
        if (UtilValidate.isEmpty(productContentList) && ("Y".equals(product.getString("isVariant")))) {
            GenericValue parent = ProductWorker.getParentProduct(productId, delegator);
            if (parent != null) {
                productContentList = EntityQuery.use(delegator).from("ProductContent").where("productId", parent.get("productId"), "productContentTypeId", productContentTypeId).orderBy("-fromDate").cache(cache).filterByDate().queryList();
            }
        }
        GenericValue productContent = EntityUtil.getFirst(productContentList);
        if (productContent != null) {
            // when rendering the product content, always include the Product and ProductContent records that this comes from
            Map<String, Object> inContext = new HashMap<>();
            inContext.put("product", product);
            inContext.put("productContent", productContent);
            ContentWorker.renderContentAsText(dispatcher, delegator, productContent.getString("contentId"), outWriter, inContext, locale, mimeTypeId, partyId, roleTypeId, cache);
            return;
        }
        
        String candidateFieldName = ModelUtil.dbNameToVarName(productContentTypeId);
        ModelEntity productModel = delegator.getModelEntity("Product");
        if (product == null) {
            product = EntityQuery.use(delegator).from("Product").where("productId", productId).cache(cache).queryOne();
        }
        if (UtilValidate.isEmpty(product)) {
            Debug.logWarning("No Product entity found for productId: " + productId, module);
            return;
        }
        
        if (productModel.isField(candidateFieldName)) {
                String candidateValue = product.getString(candidateFieldName);
                if (UtilValidate.isNotEmpty(candidateValue)) {
                    outWriter.write(candidateValue);
                    return;
                } else if ("Y".equals(product.getString("isVariant"))) {
                    // look up the virtual product
                    GenericValue parent = ProductWorker.getParentProduct(productId, delegator, cache);
                    if (parent != null) {
                        candidateValue = parent.getString(candidateFieldName);
                        if (UtilValidate.isNotEmpty(candidateValue)) {
                            outWriter.write(candidateValue);
                            return;
                        }
                    }
                }
        }

    }

    /**
     * SCIPIO: Gets the entity field value corresponding to the given productContentTypeId.
     * DO NOT USE FROM TEMPLATES - NOT CACHED - intended for code that must replicate ProductContentWrapper behavior.
     * DEV NOTE: LOGIC DUPLICATED FROM getProductContentAsText ABOVE - PLEASE KEEP IN SYNC.
     * Added 2017-09-05.
     */
    public static String getEntityFieldValue(GenericValue product, String productContentTypeId, Delegator delegator, LocalDispatcher dispatcher, boolean cache) throws GeneralException, IOException {
        String productId = product.getString("productId");

        if (delegator == null) {
            delegator = product.getDelegator();
        }

        if (delegator == null) {
            throw new GeneralRuntimeException("Unable to find a delegator to use!");
        }

        String candidateFieldName = ModelUtil.dbNameToVarName(productContentTypeId);
        ModelEntity productModel = delegator.getModelEntity("Product");
        
        if (productModel.isField(candidateFieldName)) {
                String candidateValue = product.getString(candidateFieldName);
                if (UtilValidate.isNotEmpty(candidateValue)) {
                    return candidateValue;
                } else if ("Y".equals(product.getString("isVariant"))) {
                    // look up the virtual product
                    GenericValue parent = ProductWorker.getParentProduct(productId, delegator, cache);
                    if (parent != null) {
                        candidateValue = parent.getString(candidateFieldName);
                        if (UtilValidate.isNotEmpty(candidateValue)) {
                            return candidateValue;
                        }
                    }
                }
        }
        return null;
    }

}
