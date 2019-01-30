<#--
This file is subject to the terms and conditions defined in the
files 'LICENSE' and 'NOTICE', which are part of this source
code package.
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
                       <#assign productCategoryMembers = delegator.findByAnd("ProductCategoryAndMember", {"productCategoryId":productCategoryId}, Static["org.ofbiz.base.util.UtilMisc"].toList("-quantity"), false)>
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
