<#--
Scipio Commerce
Copyright (C) Ilscipio GmbH

This file is part of Scipio Commerce. Scipio Commerce is free software: you
can redistribute it and modify it under the terms of the GNU Affero General
Public License, version 3, as published by the Free Software Foundation.
Scipio Commerce is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License
for more details. You should have received a copy of the license with this
work (file LICENSE). If not, see <https://www.gnu.org/licenses/agpl-3.0.html>.
A commercial license is available from Ilscipio GmbH.

SPDX-License-Identifier: AGPL-3.0-only
-->
<#include "component://shop/webapp/shop/catalog/catalogcommon.ftl">

<#assign productCategoryLink = requestAttributes.productCategoryLink!/>
<#if productCategoryLink?has_content>
<#if productCategoryLink.detailSubScreen?has_content>
    <@render resource=productCategoryLink.detailSubScreen />
<#else>
    <#assign titleText = productCategoryLink.titleText!/>
    <#assign imageUrl = productCategoryLink.imageUrl!/>
    <#assign detailText = productCategoryLink.detailText!/>

    <#if productCategoryLink.linkTypeEnumId == "PCLT_SEARCH_PARAM">
      <#assign linkUrl = requestAttributes._REQUEST_HANDLER_.makeLink(request, response, "search?" + productCategoryLink.linkInfo)/>
    <#elseif productCategoryLink.linkTypeEnumId == "PCLT_ABS_URL">
      <#assign linkUrl = productCategoryLink.linkInfo!/>
    <#elseif productCategoryLink.linkTypeEnumId == "PCLT_CAT_ID">
      <#assign linkUrl = requestAttributes._REQUEST_HANDLER_.makeLink(request, response, "category/~category_id=" + productCategoryLink.linkInfo) + "/~pcategory=" + productCategoryId/>
      <#assign linkProductCategory = delegator.findOne("ProductCategory", {"productCategoryId":productCategoryLink.linkInfo}, true)/>
      <#assign linkCategoryContentWrapper = Static["org.ofbiz.product.category.CategoryContentWrapper"].makeCategoryContentWrapper(linkProductCategory, request)/>
      <#assign titleText = productCategoryLink.titleText!(linkCategoryContentWrapper.get("CATEGORY_NAME"))!/>
      <#assign imageUrl = productCategoryLink.imageUrl!(linkCategoryContentWrapper.get("CATEGORY_IMAGE_URL", "url"))!/>
      <#assign detailText = productCategoryLink.detailText!(linkCategoryContentWrapper.get("DESCRIPTION"))!/>
    </#if>

    <div class="productcategorylink">
      <#if imageUrl?string?has_content>
        <div class="smallimage"><a href="${linkUrl}"><img src="<@contentUrl ctxPrefix=true>${imageUrl}</@contentUrl>" alt="${titleText!"Link Image"}"/></a></div>
      </#if>
      <#if titleText?has_content>
        <a href="${linkUrl}" class="${styles.link_nav_info_name!}">${titleText}</a>
      </#if>
      <#if detailText?has_content>
        <div>${detailText}</div>
      </#if>
    </div>
</#if>
</#if>
