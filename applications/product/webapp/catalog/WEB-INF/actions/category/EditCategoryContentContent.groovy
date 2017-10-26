/*
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
 */

import org.ofbiz.base.util.*
import org.ofbiz.entity.*
import org.ofbiz.entity.util.*

uiLabelMap = UtilProperties.getResourceBundleMap("ProductUiLabels", locale);

// Show update form
// SCIPIO: invalid for groovy: requestAttributes.contentId
if (!request.getAttribute("contentId") && (parameters.contentId && parameters.productCategoryId && parameters.prodCatContentTypeId && parameters.fromDate)) {
    fromDate = UtilDateTime.stringToTimeStamp(parameters.fromDate, "yyyy-MM-dd HH:mm:ss.S", timeZone, locale)    
    prodCatContentTypeId = parameters.prodCatContentTypeId;
    productCategoryContent = from("ProductCategoryContent").
            where(["contentId" : parameters.contentId, "productCategoryId" : parameters.productCategoryId, "prodCatContentTypeId" : parameters.prodCatContentTypeId, "fromDate" : fromDate]).queryOne();    
    if (productCategoryContent) {
        context.productCategoryContent = productCategoryContent;
        context.contentFormName = "EditCategoryContentSimpleText";
        context.contentFormAction = "updateSimpleTextContentForCategory";
        if (("PAGE_TITLE".equals(prodCatContentTypeId))||("META_KEYWORD".equals(prodCatContentTypeId))||("META_DESCRIPTION".equals(prodCatContentTypeId))) {
            context.contentFormName = "EditCategoryContentSEO";
            context.contentFormAction = "updateContentSEOForCategory";
        } else if ("RELATED_URL".equals(prodCatContentTypeId)) {
            context.contentFormName = "EditCategoryContentRelatedUrl";
            context.contentFormAction = "updateRelatedUrlContentForCategory";           
        } else if ("VIDEO".equals(prodCatContentTypeId) || "CATEGORY_IMAGE".equals(prodCatContentTypeId)) {
            context.contentFormName = "EditCategoryContentDownload";
            context.contentFormAction = "updateDownloadContentForCategory";
            if("CATEGORY_IMAGE".equals(prodCatContentTypeId)){
                context.dataResourceTypeId = "IMAGE_OBJECT";
            }else{
                context.dataResourceTypeId = "VIDEO_OBJECT";
            }
        }
        
        if ("RELATED_URL".equals(prodCatContentTypeId) || "VIDEO".equals(prodCatContentTypeId) || "CATEGORY_IMAGE".equals(prodCatContentTypeId)) {
            contentList = from("ContentDataResourceView").where("contentId", parameters.contentId).queryList();
            if (contentList) {
                context.contentDataResourceView = contentList.get(0);
            }
        }
        
        content = productCategoryContent.getRelatedOne("Content", false);
        context.content = content;
        context.textDataMap = delegator.findOne("ElectronicText", ["dataResourceId" : content.dataResourceId], false);
        context.prodCatContentTypeId = prodCatContentTypeId;
    }
    // Show create form
} else if (!request.getAttribute("contentId")) { // SCIPIO: invalid for groovy: requestAttributes.contentId 
    prodCatContentTypeId = parameters.prodCatContentTypeId;
    context.contentFormName = "EditCategoryContentSimpleText";
    context.contentFormAction = "createSimpleTextContentForCategory";
    if (("PAGE_TITLE".equals(prodCatContentTypeId))||("META_KEYWORD".equals(prodCatContentTypeId))||("META_DESCRIPTION".equals(prodCatContentTypeId))) {
        context.contentFormName = "EditCategoryContentSEO";
        context.contentFormAction = "updateContentSEOForCategory";
    } else if ("RELATED_URL".equals(prodCatContentTypeId)) {
        context.contentFormName = "EditCategoryContentRelatedUrl";
        context.contentFormAction = "createRelatedUrlContentForCategory";
    } else if ("VIDEO".equals(prodCatContentTypeId) || "CATEGORY_IMAGE".equals(prodCatContentTypeId)) {
        context.contentFormName = "EditCategoryContentDownload";
        context.contentFormAction = "createDownloadContentForCategory";
    }
    context.prodCatContentTypeId = prodCatContentTypeId;
}
//  context.contentFormTitle = "${uiLabelMap.ProductUpdateSEOContentCategory}";
//  context.contentFormTitle = "${uiLabelMap.ProductUpdateRelatedURLContentCategory}";
//  context.contentFormTitle = "${uiLabelMap.ProductUpdateSimpleTextContentCategory}";
//  context.contentFormTitle = "${uiLabelMap.ProductUpdateDownloadContentCategory}";