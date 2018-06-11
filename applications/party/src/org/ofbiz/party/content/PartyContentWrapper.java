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

package org.ofbiz.party.content;

import java.io.IOException;
import java.io.StringWriter;
import java.io.Writer;
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
 * WorkEffortContentWrapper; gets work effort content for display
 * <p>
 * SCIPIO: NOTE: 2017: This ContentWrapper is heavily updated from stock for localization behavior, caching, and other fixes.
 */
@SuppressWarnings("serial")
public class PartyContentWrapper extends CommonContentWrapper {

    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());
    public static final String CACHE_KEY_SEPARATOR = "::";

    private static final UtilCache<String, String> partyContentCache = UtilCache.createUtilCache("party.content.rendered", true);

    public PartyContentWrapper(GenericValue entityValue, HttpServletRequest request, boolean useCache) {
        super(entityValue, request, useCache);
    }

    public PartyContentWrapper(GenericValue entityValue, HttpServletRequest request) {
        super(entityValue, request);
    }

    public PartyContentWrapper(LocalDispatcher dispatcher, GenericValue entityValue, Locale locale, String mimeTypeId,
            boolean useCache) {
        super(dispatcher, entityValue, locale, mimeTypeId, useCache);
    }

    public PartyContentWrapper(LocalDispatcher dispatcher, GenericValue entityValue, Locale locale, String mimeTypeId) {
        super(dispatcher, entityValue, locale, mimeTypeId);
    }

    @Override
    protected String getImpl(String contentTypeId, boolean useCache, String contentLang) {
        return getPartyContentAsText(getEntityValue(), contentTypeId, getLocale(), getMimeTypeId(), getDelegator(), getDispatcher(), useCache, contentLang);
    }

    public String getId(String contentTypeId) {
        GenericValue partyContent = getFirstPartyContentByType(null, getEntityValue(), contentTypeId, getDelegator());
        if (partyContent != null) {
            return partyContent.getString("contentId");
        } else {
            return null;
        }
    }

    public List<String> getList(String contentTypeId) {
        try {
            return getPartyContentTextList(getEntityValue(), contentTypeId, getLocale(), getMimeTypeId(), getDelegator(), getDispatcher());
        } catch (Exception e) {
            Debug.logError(e, module);
            return null;
        }
    }

    public String getContent(String contentId, boolean useCache, String encoderType) {
        return getPartyContentAsText(getEntityValue(), contentId, null, getLocale(), getMimeTypeId(), getDelegator(), getDispatcher(), useCache, encoderType);
    }

    public String getContent(String contentId, String encoderType) {
        return getContent(contentId, isUseCache(), encoderType);
    }

    // static methods
    public static String getPartyContentAsText(GenericValue party, String partyContentId, HttpServletRequest request, String encoderType) {
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        return getPartyContentAsText(party, partyContentId, null, UtilHttp.getLocale(request), "text/html", party.getDelegator(), dispatcher, true, encoderType);
    }

    public static String getPartyContentAsText(GenericValue party, String partyContentId, Locale locale, LocalDispatcher dispatcher, String encoderType) {
        return getPartyContentAsText(party, partyContentId, null, locale, null, null, dispatcher, true, encoderType);
    }
    
    /**
     * SCIPIO: Gets content as text, with option to bypass wrapper cache.
     */
    public static String getPartyContentAsText(GenericValue party, String partyContentId, Locale locale, LocalDispatcher dispatcher, boolean useCache, String encoderType) {
        return getPartyContentAsText(party, partyContentId, null, locale, null, null, dispatcher, useCache, encoderType);
    }

    public static String getPartyContentAsText(GenericValue party, String partyContentTypeId,
            Locale locale, String mimeTypeId, Delegator delegator, LocalDispatcher dispatcher, boolean useCache, String encoderType) {
        return getPartyContentAsText(party, null, partyContentTypeId, locale, mimeTypeId, delegator, dispatcher, useCache, encoderType);
    }

    public static String getPartyContentAsText(GenericValue party, String contentId, String partyContentTypeId,
            Locale locale, String mimeTypeId, Delegator delegator, LocalDispatcher dispatcher, boolean useCache, String encoderType) {
        if (party == null) {
            return null;
        }
        
        UtilCodec.SimpleEncoder encoder = ContentLangUtil.getContentWrapperSanitizer(encoderType);
        String candidateFieldName = ModelUtil.dbNameToVarName(partyContentTypeId);
        String cacheKey = null;
        if (useCache) { // SCIPIO: don't build cache key if disabled
            if (contentId != null) {
                cacheKey = contentId + CACHE_KEY_SEPARATOR + locale + CACHE_KEY_SEPARATOR + mimeTypeId +
                        CACHE_KEY_SEPARATOR + party.get("partyId") + CACHE_KEY_SEPARATOR + encoder.getLang(); // SCIPIO: added encoder
            } else {
                cacheKey = partyContentTypeId + CACHE_KEY_SEPARATOR + locale + CACHE_KEY_SEPARATOR + mimeTypeId +
                        CACHE_KEY_SEPARATOR + party.get("partyId") + CACHE_KEY_SEPARATOR + encoder.getLang(); // SCIPIO: added encoder
            }
        }

        try {
            if (useCache) {
                String cachedValue = partyContentCache.get(cacheKey);
                if (cachedValue != null) {
                    return cachedValue;
                }
            }

            Writer outWriter = new StringWriter();
            getPartyContentAsText(contentId, party.getString("partyId"), party, partyContentTypeId, locale, mimeTypeId, delegator, dispatcher, outWriter, false);

            String outString = outWriter.toString();
            if (UtilValidate.isEmpty(outString)) {
                outString = party.getModelEntity().isField(candidateFieldName) ? party.getString(candidateFieldName): "";
                outString = outString == null? "" : outString;
            }
            outString = encoder.sanitize(outString);
            if (useCache && partyContentCache != null) {
                partyContentCache.put(cacheKey, outString);
            }
            return outString;
        } catch (GeneralException e) {
            Debug.logError(e, "Error rendering PartyContent, inserting empty String", module);
            String candidateOut = party.getModelEntity().isField(candidateFieldName) ? party.getString(candidateFieldName): "";
            return candidateOut == null? "" : encoder.sanitize(candidateOut);
        } catch (IOException e) {
            Debug.logError(e, "Error rendering PartyContent, inserting empty String", module);
            String candidateOut = party.getModelEntity().isField(candidateFieldName) ? party.getString(candidateFieldName): "";
            return candidateOut == null? "" : encoder.sanitize(candidateOut);
        }
    }

    public static void getPartyContentAsText(String contentId, String partyId, GenericValue party, String partyContentTypeId, Locale locale, String mimeTypeId, Delegator delegator, LocalDispatcher dispatcher, Writer outWriter) throws GeneralException, IOException {
        getPartyContentAsText(contentId, partyId, party, partyContentTypeId, locale, mimeTypeId, delegator, dispatcher, outWriter, true);
    }

    public static void getPartyContentAsText(String contentId, String partyId, GenericValue party, String partyContentTypeId, Locale locale, String mimeTypeId, Delegator delegator, LocalDispatcher dispatcher, Writer outWriter, boolean cache) throws GeneralException, IOException {
        if (partyId == null && party != null) {
            partyId = party.getString("partyId");
        }

        if (delegator == null && party != null) {
            delegator = party.getDelegator();
        }

        if (UtilValidate.isEmpty(mimeTypeId)) {
            mimeTypeId = "text/html";
        }

        if (delegator == null) {
            throw new GeneralRuntimeException("Unable to find a delegator to use!");
        }

        // Honor party content over Party entity fields.
        GenericValue partyContent;
        if (contentId != null) {
            partyContent = EntityQuery.use(delegator).from("PartyContent").where("partyId", partyId, "contentId", contentId).cache(cache).queryOne();
        } else {
            partyContent = getFirstPartyContentByType(partyId, party, partyContentTypeId, delegator, cache);
        }
        if (partyContent != null) {
            // when rendering the product content, always include the Product and ProductContent records that this comes from
            Map<String, Object> inContext = new HashMap<>();
            inContext.put("party", party);
            inContext.put("partyContent", partyContent);
            ContentWorker.renderContentAsText(dispatcher, delegator, partyContent.getString("contentId"), outWriter, inContext, locale, mimeTypeId, null, null, cache);
            return;
        }
        
        if (partyContentTypeId != null) {
            String candidateFieldName = ModelUtil.dbNameToVarName(partyContentTypeId);

            // first check for a person field
            ModelEntity partyPersonModel = delegator.getModelEntity("PartyAndPerson");
            if (partyPersonModel != null && partyPersonModel.isField(candidateFieldName)) {
                if (party == null) {
                    party = EntityQuery.use(delegator).from("PartyAndPerson").where("partyId", partyId).cache(cache).queryOne();
                }
                if (party != null) {
                    String candidateValue = party.getString(candidateFieldName);
                    if (UtilValidate.isNotEmpty(candidateValue)) {
                        outWriter.write(candidateValue);
                        return;
                    }
                }
            }

            // next check for group field
            ModelEntity partyGroupModel = delegator.getModelEntity("PartyAndGroup");
            if (partyGroupModel != null && partyGroupModel.isField(candidateFieldName)) {
                if (party == null) {
                    party = EntityQuery.use(delegator).from("PartyAndGroup").where("partyId", partyId).cache(cache).queryOne();
                }
                if (party != null) {
                    String candidateValue = party.getString(candidateFieldName);
                    if (UtilValidate.isNotEmpty(candidateValue)) {
                        outWriter.write(candidateValue);
                        return;
                    }
                }
            }
        }
    }

    public static List<String> getPartyContentTextList(GenericValue party, String partyContentTypeId, Locale locale, String mimeTypeId, Delegator delegator, LocalDispatcher dispatcher) throws GeneralException, IOException { // SCIPIO: added cache flag
        // SCIPIO: delegating
        return getPartyContentTextList(party, partyContentTypeId, locale, mimeTypeId, delegator, dispatcher, true);
    }

    public static List<String> getPartyContentTextList(GenericValue party, String partyContentTypeId, Locale locale, String mimeTypeId, Delegator delegator, LocalDispatcher dispatcher, boolean cache) throws GeneralException, IOException { // SCIPIO: added cache flag
        List<GenericValue> partyContentList = EntityQuery.use(delegator).from("PartyContent")
                .where("partyId", party.getString("partyId"), "partyContentTypeId", partyContentTypeId)
                .orderBy("-fromDate")
                .cache(cache) // SCIPIO: cache flag
                .filterByDate()
                .queryList();

        List<String> contentList = new LinkedList<>();
        if (partyContentList != null) {
            for (GenericValue partyContent: partyContentList) {
                StringWriter outWriter = new StringWriter();
                Map<String, Object> inContext = new HashMap<>();
                inContext.put("party", party);
                inContext.put("partyContent", partyContent);
                ContentWorker.renderContentAsText(dispatcher, delegator, partyContent.getString("contentId"), outWriter, inContext, locale, mimeTypeId, null, null, cache); // SCIPIO: cache flag
                contentList.add(outWriter.toString());
            }
        }

        return contentList;
    }

    public static GenericValue getFirstPartyContentByType(String partyId, GenericValue party, String partyContentTypeId, Delegator delegator) {
        // SCIPIO: delegating
        return getFirstPartyContentByType(partyId, party, partyContentTypeId, delegator, true);
    }

    public static GenericValue getFirstPartyContentByType(String partyId, GenericValue party, String partyContentTypeId, Delegator delegator, boolean cache) { // SCIPIO: added cache flag
        if (partyId == null && party != null) {
            partyId = party.getString("partyId");
        }

        if (delegator == null && party != null) {
            delegator = party.getDelegator();
        }

        if (delegator == null) {
            throw new IllegalArgumentException("Delegator missing");
        }

        List<GenericValue> partyContentList = null;
        try {
            partyContentList = EntityQuery.use(delegator).from("PartyContent")
                    .where("partyId", partyId, "partyContentTypeId", partyContentTypeId)
                    .orderBy("-fromDate")
                    .cache(cache)
                    .filterByDate() // SCIPIO: added
                    .queryList();
        } catch (GeneralException e) {
            Debug.logError(e, module);
        }

        // SCIPIO: covered above
        //if (partyContentList != null) {
        //    partyContentList = EntityUtil.filterByDate(partyContentList);
        //    return EntityUtil.getFirst(partyContentList);
        //} else {
        //    return null;
        //}
        return EntityUtil.getFirst(partyContentList); 
    }

    public static PartyContentWrapper makePartyContentWrapper(GenericValue party, HttpServletRequest request) {
        return new PartyContentWrapper(party, request);
    }
}
