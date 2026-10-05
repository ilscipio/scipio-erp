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

<#if productCategoryList?has_content>
    <@heading level=1>Popular Categories</@heading>
    <div class="productsummary-container matrix">
       <@table type="generic">
           <@tbody>

            <#list productCategoryList as childCategoryList>
                   <@tr open=true close=false />
                   <#assign cateCount = 0/>
                   <#list childCategoryList as productCategory>
                       <#if (cateCount > 2)>
                            <@tr open=true close=false />
                            <#assign cateCount = 0/>
                       </#if>
                       <#assign productCategoryId = productCategory.productCategoryId/>
                       <#assign categoryImageUrl = "/images/defaultImage.jpg">
                       <#assign productCategoryMembers = delegator.findByAnd("ProductCategoryAndMember", {"productCategoryId":productCategoryId}, UtilMisc.toList("-quantity"), false)>
                       <#if productCategory.categoryImageUrl?has_content>
                            <#assign categoryImageUrl = productCategory.categoryImageUrl/>
                       <#elseif productCategoryMembers?has_content>
                            <#assign productCategoryMember = Static["org.ofbiz.entity.util.EntityUtil"].getFirst(productCategoryMembers)/>
                            <#assign product = delegator.findOne("Product", {"productId":productCategoryMember.productId}, false)/>
                            <#if product.smallImageUrl?has_content>
                                <#assign categoryImageUrl = product.smallImageUrl/>
                            </#if>
                       </#if>
                        <@td>
                            <div class="productsummary">
                                <div class="smallimage">
                                    <#-- SCIPIO: NOTE: category link changed from @catalogAltUrl to @catalogUrl due to possible loss of browsing information by CatalogUrlFilter and consistency -->
                                    <a href="<@catalogUrl productCategoryId=productCategoryId/>">
                                        <span class="popup_link"><img alt="Small Image" src="${categoryImageUrl}"/></span>
                                    </a>
                                </div>
                                <div class="productbuy">
                                    <#-- SCIPIO: NOTE: category link changed from @catalogAltUrl to @catalogUrl due to possible loss of browsing information by CatalogUrlFilter and consistency -->
                                    <a class="${styles.link_nav_info_name!}"  style="font-size:12px" href="<@catalogUrl productCategoryId=productCategoryId/>">${productCategory.categoryName!productCategoryId}</a>
                                </div>
                                <div class="productinfo">
                                    <ul>
                                    <#if productCategoryMembers??>
                                        <#assign i = 0/>
                                        <#list productCategoryMembers as productCategoryMember>
                                            <#if (i > 2)>
                                                <#if productCategoryMembers[i]?has_content>
                                                    <#-- SCIPIO: NOTE: category link changed from @catalogAltUrl to @catalogUrl due to possible loss of browsing information by CatalogUrlFilter and consistency -->
                                                    <a class="${styles.link_nav!} ${styles.action_view!}" href="<@catalogUrl productCategoryId=productCategoryId/>">
                                                        <span>More...</span>
                                                    </a>
                                                </#if>
                                                <#break>
                                            </#if>
                                            <#if productCategoryMember?has_content>
                                                <#assign product = delegator.findOne("Product", {"productId":productCategoryMember.productId}, false)>
                                                <li class="browsecategorytext">
                                                    <a class="${styles.link_nav_info_name!}" href="<@catalogAltUrl productCategoryId="PROMOTIONS" productId="${product.productId}"/>">
                                                        ${product.productName!product.productId}
                                                    </a>
                                                </li>
                                            </#if>
                                            <#assign i = i+1/>
                                        </#list>
                                    </#if>
                                    </ul>
                                </div>
                            </div>
                        </@td>
                        <#assign cateCount = cateCount + 1/>
                 </#list>
               <@tr close=true open=false />
            </#list>
        </@tbody>
      </@table>
    </div>
</#if>
