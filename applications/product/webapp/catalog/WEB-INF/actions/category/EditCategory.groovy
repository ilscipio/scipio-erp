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
import org.ofbiz.base.util.string.*
import org.ofbiz.entity.util.EntityUtilProperties

if (productCategory) {
    context.productCategoryType = productCategory.getRelatedOne("ProductCategoryType", false);
}

primaryParentCategory = null;
primParentCatIdParam = request.getParameter("primaryParentCategoryId");
if (productCategory) {
    primaryParentCategory = productCategory.getRelatedOne("PrimaryParentProductCategory", false);
} else if (primParentCatIdParam) {
    primaryParentCategory = from("ProductCategory").where("productCategoryId", primParentCatIdParam).queryOne();
}
context.primaryParentCategory = primaryParentCategory;


// make the image file formats
context.tenantId = delegator.getDelegatorTenantId();
imageFilenameFormat = EntityUtilProperties.getPropertyValue("catalog", "image.filename.format", delegator);
imageServerPath = FlexibleStringExpander.expandString(EntityUtilProperties.getPropertyValue("catalog", "image.server.path", delegator), context);
imageUrlPrefix = FlexibleStringExpander.expandString(EntityUtilProperties.getPropertyValue("catalog", "image.url.prefix",delegator), context);
imageServerPath = imageServerPath.endsWith("/") ? imageServerPath.substring(0, imageServerPath.length()-1) : imageServerPath;
imageUrlPrefix = imageUrlPrefix.endsWith("/") ? imageUrlPrefix.substring(0, imageUrlPrefix.length()-1) : imageUrlPrefix;
context.imageFilenameFormat = imageFilenameFormat;
context.imageServerPath = imageServerPath;
context.imageUrlPrefix = imageUrlPrefix;

filenameExpander = FlexibleStringExpander.getInstance(imageFilenameFormat, false);
context.imageNameCategory = imageUrlPrefix + "/" + filenameExpander.expandString([location : "categories", type : "category", id : productCategoryId]);
context.imageNameLinkOne  = imageUrlPrefix + "/" + filenameExpander.expandString([location : "categories", type : "linkOne", id : productCategoryId]);
context.imageNameLinkTwo  = imageUrlPrefix + "/" + filenameExpander.expandString([location : "categories", type : "linkTwo", id : productCategoryId]);