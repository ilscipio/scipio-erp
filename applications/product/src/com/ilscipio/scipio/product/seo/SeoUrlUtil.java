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
package com.ilscipio.scipio.product.seo;

import java.util.ArrayList;
import java.util.List;
import java.util.Locale;
import java.util.Map;

import org.apache.commons.lang.StringUtils;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.service.ServiceUtil;

/**
 * SCIPIO: SEO Catalog URL util
 */
public class SeoUrlUtil {

    public static String replaceSpecialCharsUrl(String url, Map<String, String> charFilters) {
        if (charFilters == null) return url;
        if (UtilValidate.isEmpty(url)) {
            url = "";
        }
        for (String characterPattern : charFilters.keySet()) {
            url = url.replaceAll(characterPattern, charFilters.get(characterPattern));
        }
        return url;
    }
    
    /**
     * @deprecated tied to static SeoConfigUtil config (FIXME: future)
     */
    @Deprecated
    public static String replaceSpecialCharsUrl(String url) {
        return replaceSpecialCharsUrl(url, SeoConfig.getDefaultConfig().getCharFilters());
    }

    /**
     * @deprecated FIXME: BROKEN - MISSING DELIMITERS
     */
    @Deprecated
    public static String removeContextPath(String uri, String contextPath) {
        if (UtilValidate.isEmpty(contextPath) || UtilValidate.isEmpty(uri)) {
            return uri;
        }
        if (uri.length() > contextPath.length() && uri.startsWith(contextPath)) {
            return uri.substring(contextPath.length());
        }
        return uri;
    }
    
    /**
     * Stats for product/category iterating services.
     * TODO: LOCALIZE
     */
    public static class UrlGenStats {
        public final boolean doProducts;
        public final boolean doCategory;
        public final boolean useSkipped;
        
        public int productSuccess = 0;
        public int productSkipped = 0;
        public int productError = 0;
        
        public int categorySuccess = 0;
        public int categoryError = 0;
        public int categorySkipped = 0;

        public UrlGenStats(boolean doProducts, boolean doCategory, boolean useSkipped) {
            this.doProducts = doProducts;
            this.doCategory = doCategory;
            this.useSkipped = useSkipped;
        }
        
        public UrlGenStats(boolean doProducts, boolean doCategory) {
            this(doProducts, doCategory, true);
        }

        public boolean hasError() {
            return productError > 0 || categoryError > 0;
        }
        
        public void toMsgLists(Locale locale, List<String> msgList, List<String> errMsgList) {
            if (doProducts) {
                msgList.add("Products updated: " + productSuccess);
                if (useSkipped) msgList.add("Products skipped: " + productSkipped);
                if (productError > 0) errMsgList.add("Products failed: " + productError);
            }
            
            if (doCategory) {
                msgList.add("Categories updated: " + categorySuccess);
                if (useSkipped) msgList.add("Categories skipped: " + categorySkipped);
                if (categoryError > 0) errMsgList.add("Categories failed: " + categoryError);
            }
        }
        
        public String toMsg(Locale locale) {
            List<String> msgList = new ArrayList<>();
            List<String> errMsgList = new ArrayList<>();
            toMsgLists(locale, msgList, errMsgList);
            
            List<String> allMsgs = new ArrayList<>();
            allMsgs.addAll(msgList);
            allMsgs.addAll(errMsgList);
            return StringUtils.join(allMsgs, "; ");
        }
        
        public Map<String, Object> toServiceResultSuccessFailure(String msg) {
            return hasError() ? ServiceUtil.returnFailure(msg) : ServiceUtil.returnSuccess(msg);
        }

    }
}
