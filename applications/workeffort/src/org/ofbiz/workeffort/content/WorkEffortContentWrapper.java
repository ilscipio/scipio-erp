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
package org.ofbiz.workeffort.content;

import java.io.IOException;
import java.io.StringWriter;
import java.io.Writer;
import java.sql.Timestamp;
import java.util.HashMap;
import java.util.LinkedList;
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
import org.ofbiz.service.LocalDispatcher;

/**
 * WorkEffortContentWrapper; gets work effort content for display
 * <p>
 * SCIPIO: NOTE: 2017: This ContentWrapper is heavily updated from stock for localization behavior, caching, and other fixes.
 */
@SuppressWarnings("serial")
public class WorkEffortContentWrapper extends CommonContentWrapper {

    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());
    public static final String CACHE_KEY_SEPARATOR = "::";

    private static final UtilCache<String, String> workEffortContentCache = UtilCache.createUtilCache("workeffort.content.rendered", true);

    public WorkEffortContentWrapper(GenericValue entityValue, HttpServletRequest request, boolean useCache) {
        super(entityValue, request, useCache);
    }

    public WorkEffortContentWrapper(GenericValue entityValue, HttpServletRequest request) {
        super(entityValue, request);
    }

    public WorkEffortContentWrapper(LocalDispatcher dispatcher, GenericValue entityValue, Locale locale,
            String mimeTypeId, boolean useCache) {
        super(dispatcher, entityValue, locale, mimeTypeId, useCache);
    }

    public WorkEffortContentWrapper(LocalDispatcher dispatcher, GenericValue entityValue, Locale locale,
            String mimeTypeId) {
        super(dispatcher, entityValue, locale, mimeTypeId);
    }
    
    @Override
    protected String getImpl(String contentTypeId, boolean useCache, String contentLang) {
        return getWorkEffortContentAsText(getEntityValue(), contentTypeId, getLocale(), getMimeTypeId(), getDelegator(), getDispatcher(), useCache, contentLang);
    }

    /**
     * Get the ID from the most current content data by the defined type
     * @param contentTypeId Type of content to return
     * @return String containing the contentId
     */
    public String getContentId(String contentTypeId) {
        GenericValue workEffortContent = getFirstWorkEffortContentByType(null, getEntityValue(), contentTypeId, getDelegator(), isUseCache());
        if (workEffortContent != null) {
            return workEffortContent.getString("contentId");
        } else {
            return null;
        }
    }

    /**
     * Get the name of the most current content data by the defined type
     * @param contentTypeId Type of content to return
     * @return String containing the name of the content record
     */
    public String getContentName(String contentTypeId) {
        GenericValue workEffortContent = getFirstWorkEffortContentByType(null, getEntityValue(), contentTypeId, getDelegator(), isUseCache());
        if (workEffortContent != null) {
            GenericValue content;
            try {
                content = workEffortContent.getRelatedOne("Content", false);
            } catch (GeneralException e) {
                Debug.logError(e, module);
                return null;
            }

            if (content != null) {
                return content.getString("contentName");
            }
        }

        return null;
    }

    /**
     * Get the fromDate from teh most current content data by the defined type
     * @param contentTypeId Type of content to return
     * @return Timestamp of the fromDate field for this content type
     */
    public Timestamp getFromDate(String contentTypeId) {
        GenericValue workEffortContent = getFirstWorkEffortContentByType(null, getEntityValue(), contentTypeId, getDelegator(), isUseCache());
        if (workEffortContent != null) {
            return workEffortContent.getTimestamp("fromDate");
        } else {
            return null;
        }
    }

    public String getDataResourceId(String contentTypeId) {
        GenericValue workEffortContent = getFirstWorkEffortContentByType(null, getEntityValue(), contentTypeId, getDelegator(), isUseCache());
        if (workEffortContent != null) {
            GenericValue content;
            try {
                content = workEffortContent.getRelatedOne("Content", false);
            } catch (GeneralException e) {
                Debug.logError(e, module);
                return null;
            }
            if (content != null) {
                GenericValue dataResource;
                try {
                    dataResource = content.getRelatedOne("DataResource", false);
                } catch (GeneralException e) {
                    Debug.logError(e, module);
                    return null;
                }
                if (dataResource != null) {
                    return dataResource.getString("dataResourceId");
                }
            }
        }

        return null;
    }

    public List<String> getList(String contentTypeId) {
        try {
            return getWorkEffortContentTextList(getEntityValue(), contentTypeId, getLocale(), getMimeTypeId(), getDelegator(), getDispatcher(), isUseCache());
        } catch (Exception e) {
            Debug.logError(e, module);
            return null;
        }
    }

    public String getTypeDescription(String contentTypeId) {
        Delegator delegator = getDelegator(); // SCIPIO: simplified by base class

        if (delegator != null) {
            GenericValue contentType = null;
            try {
                contentType = EntityQuery.use(delegator).from("WorkEffortContentType").where("workEffortContentTypeId", contentTypeId).cache().queryOne();
            } catch (GeneralException e) {
                Debug.logError(e, module);
            }

            if (contentType != null) {
                return contentType.getString("description");
            }
        }

        return null;
    }

    public String getContent(String contentId, boolean useCache, String encoderType) {
        return getWorkEffortContentAsText(getEntityValue(), contentId, null, getLocale(), getMimeTypeId(), getDelegator(), getDispatcher(), useCache, encoderType);
    }

    public String getContent(String contentId, String encoderType) {
        return getContent(contentId, isUseCache(), encoderType);
    }

    // static method helpers
     public static String getWorkEffortContentAsText(GenericValue workEffort, String workEffortContentTypeId, HttpServletRequest request, String encoderType) {
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        return getWorkEffortContentAsText(workEffort, workEffortContentTypeId, UtilHttp.getLocale(request), "text/html", workEffort.getDelegator(), dispatcher, true, encoderType);
    }

    public static String getWorkEffortContentAsText(GenericValue workEffort, String workEffortContentTypeId, Locale locale, LocalDispatcher dispatcher, String encoderType) {
        return getWorkEffortContentAsText(workEffort, workEffortContentTypeId, locale, null, null, dispatcher, true, encoderType);
    }

    public static String getWorkEffortContentAsText(GenericValue workEffort, String workEffortContentTypeId,
            Locale locale, String mimeTypeId, Delegator delegator, LocalDispatcher dispatcher, boolean useCache, String encoderType) {
        return getWorkEffortContentAsText(workEffort, null, workEffortContentTypeId, locale, mimeTypeId, delegator, dispatcher, useCache, encoderType);
    }

    public static String getWorkEffortContentAsText(GenericValue workEffort, String contentId, String workEffortContentTypeId,
            Locale locale, String mimeTypeId, Delegator delegator, LocalDispatcher dispatcher, boolean useCache, String encoderType) {
        if (workEffort == null) {
            return null;
        }

        ContentSanitizer encoder = ContentLangUtil.getContentWrapperSanitizer(encoderType);
        String candidateFieldName = ModelUtil.dbNameToVarName(workEffortContentTypeId);
        String cacheKey = null;
        if (useCache) { // SCIPIO: don't build cache key if disabled
            if (contentId != null) {
                cacheKey = contentId + CACHE_KEY_SEPARATOR + locale + CACHE_KEY_SEPARATOR + mimeTypeId +
                        CACHE_KEY_SEPARATOR + workEffort.get("workEffortId") + CACHE_KEY_SEPARATOR + encoder.getLang(); // SCIPIO: added encoder
            } else {
                cacheKey = workEffortContentTypeId + CACHE_KEY_SEPARATOR + locale + CACHE_KEY_SEPARATOR + mimeTypeId +
                        CACHE_KEY_SEPARATOR + workEffort.get("workEffortId") + CACHE_KEY_SEPARATOR + encoder.getLang(); // SCIPIO: added encoder
            }
        }
        
        try {
            if (useCache) {
                String cachedValue = workEffortContentCache.get(cacheKey);
                if (cachedValue != null) {
                    return cachedValue;
                }
            }

            Writer outWriter = new StringWriter();
            getWorkEffortContentAsText(contentId, null, workEffort, workEffortContentTypeId, locale, mimeTypeId, delegator, dispatcher, outWriter, false);
            String outString = outWriter.toString();
            if (UtilValidate.isEmpty(outString)) {
                outString = workEffort.getModelEntity().isField(candidateFieldName) ? workEffort.getString(candidateFieldName): "";
                outString = outString == null? "" : outString;
            }
            outString = encoder.sanitize(outString);
            if (useCache && workEffortContentCache != null) {
                workEffortContentCache.put(cacheKey, outString);
            }
            return outString;
        } catch (GeneralException e) {
            Debug.logError(e, "Error rendering WorkEffortContent, inserting empty String", module);
            String candidateOut = workEffort.getModelEntity().isField(candidateFieldName) ? workEffort.getString(candidateFieldName): "";
            return candidateOut == null? "" : encoder.sanitize(candidateOut);
        } catch (IOException e) {
            Debug.logError(e, "Error rendering WorkEffortContent, inserting empty String", module);
            String candidateOut = workEffort.getModelEntity().isField(candidateFieldName) ? workEffort.getString(candidateFieldName): "";
            return candidateOut == null? "" : encoder.sanitize(candidateOut);
        }
    }

    public static void getWorkEffortContentAsText(String contentId, String workEffortId, GenericValue workEffort, String workEffortContentTypeId, Locale locale, String mimeTypeId, Delegator delegator, LocalDispatcher dispatcher, Writer outWriter) throws GeneralException, IOException {
        getWorkEffortContentAsText(contentId, null, workEffort, workEffortContentTypeId, locale, mimeTypeId, delegator, dispatcher, outWriter, true);
    }

    public static void getWorkEffortContentAsText(String contentId, String workEffortId, GenericValue workEffort, String workEffortContentTypeId, Locale locale, String mimeTypeId, Delegator delegator, LocalDispatcher dispatcher, Writer outWriter, boolean cache) throws GeneralException, IOException {
        if (workEffortId == null && workEffort != null) {
            workEffortId = workEffort.getString("workEffortId");
        }

        if (delegator == null && workEffort != null) {
            delegator = workEffort.getDelegator();
        }

        if (UtilValidate.isEmpty(mimeTypeId)) {
            mimeTypeId = "text/html";
        }

        if (delegator == null) {
            throw new GeneralRuntimeException("Unable to find a delegator to use!");
        }

        // Honor work effort content over WorkEffort entity fields.
        GenericValue workEffortContent;
        if (contentId != null) {
            workEffortContent = EntityQuery.use(delegator).from("WorkEffortContent").where("workEffortId", workEffortId, "contentId", contentId).cache(cache).queryOne();
        } else {
            workEffortContent = getFirstWorkEffortContentByType(workEffortId, workEffort, workEffortContentTypeId, delegator, cache);
        }
        if (workEffortContent != null) {
            // when rendering the product content, always include the Product and ProductContent records that this comes from
            Map<String, Object> inContext = new HashMap<>();
            inContext.put("workEffort", workEffort);
            inContext.put("workEffortContent", workEffortContent);
            ContentWorker.renderContentAsText(dispatcher, delegator, workEffortContent.getString("contentId"), outWriter, inContext, locale, mimeTypeId, null, null, cache);
            return;
        }
        
        // check for workeffort field
        String candidateFieldName = ModelUtil.dbNameToVarName(workEffortContentTypeId);
        ModelEntity workEffortModel = delegator.getModelEntity("WorkEffort");
        if (workEffortModel != null && workEffortModel.isField(candidateFieldName)) {
            if (workEffort == null) {
                workEffort = EntityQuery.use(delegator).from("WorkEffort").where("workEffortId", workEffortId).cache(cache).queryOne();
            }
            if (workEffort != null) {
                String candidateValue = workEffort.getString(candidateFieldName);
                if (UtilValidate.isNotEmpty(candidateValue)) {
                    outWriter.write(candidateValue);
                    return;
                }
            }
        }
    }

    public static List<String> getWorkEffortContentTextList(GenericValue workEffort, String workEffortContentTypeId, Locale locale, String mimeTypeId, Delegator delegator, LocalDispatcher dispatcher) throws GeneralException, IOException {
        // SCIPIO: delegating
        return getWorkEffortContentTextList(workEffort, workEffortContentTypeId, locale, mimeTypeId, delegator, dispatcher, true);
    }
    
    public static List<String> getWorkEffortContentTextList(GenericValue workEffort, String workEffortContentTypeId, Locale locale, String mimeTypeId, Delegator delegator, LocalDispatcher dispatcher, boolean cache) throws GeneralException, IOException {
        List<GenericValue> partyContentList = EntityQuery.use(delegator).from("WorkEffortContent")
                .where("workEffortId", workEffort.getString("partyId"), "workEffortContentTypeId", workEffortContentTypeId)
                .orderBy("-fromDate")
                .cache(cache)
                .filterByDate()
                .queryList();

        List<String> contentList = new LinkedList<>();
        if (partyContentList != null) {
            for (GenericValue workEffortContent: partyContentList) {
                StringWriter outWriter = new StringWriter();
                Map<String, Object> inContext = new HashMap<>();
                inContext.put("workEffort", workEffort);
                inContext.put("workEffortContent", workEffortContent);
                ContentWorker.renderContentAsText(dispatcher, delegator, workEffortContent.getString("contentId"), outWriter, inContext, locale, mimeTypeId, null, null, cache);
                contentList.add(outWriter.toString());
            }
        }

        return contentList;
    }

    public static GenericValue getFirstWorkEffortContentByType(String workEffortId, GenericValue workEffort, String workEffortContentTypeId, Delegator delegator, boolean cache) {
        if (workEffortId == null && workEffort != null) {
            workEffortId = workEffort.getString("workEffortId");
        }

        if (delegator == null && workEffort != null) {
            delegator = workEffort.getDelegator();
        }

        if (delegator == null) {
            throw new IllegalArgumentException("Delegator missing");
        }

        GenericValue workEffortContent = null;
        try {
            workEffortContent = EntityQuery.use(delegator).from("WorkEffortContent")
                                    .where("workEffortId", workEffortId, "workEffortContentTypeId", workEffortContentTypeId)
                                    .orderBy("-fromDate")
                                    .filterByDate()
                                    .cache(cache)
                                    .queryFirst();
        } catch (GeneralException e) {
            Debug.logError(e, module);
        }
        return workEffortContent;
    }
    
    // SCIPIO: backward-compat
    public static GenericValue getFirstWorkEffortContentByType(String workEffortId, GenericValue workEffort, String workEffortContentTypeId, Delegator delegator) {
        return getFirstWorkEffortContentByType(workEffortId, workEffort, workEffortContentTypeId, delegator, true);
    }

    public static WorkEffortContentWrapper makeWorkEffortContentWrapper(GenericValue workEffort, HttpServletRequest request) {
        return new WorkEffortContentWrapper(workEffort, request);
    }
}
