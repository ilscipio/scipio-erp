<#--
Licensed to the Apache Software Foundation (ASF) under one
or more contributor license agreements.  See the NOTICE file
distributed with this work for additional information
regarding copyright ownership.  The ASF licenses this file
to you under the Apache License, Version 2.0 (the
"License"); you may not use this file except in compliance
with the License.  You may obtain a copy of the License at

http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing,
software distributed under the License is distributed on an
"AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
KIND, either express or implied.  See the License for the
specific language governing permissions and limitations
under the License.
-->
<@table type="fields" class="+${styles.table_spacing_tiny_hint!}"> <#-- orig: class="" --> <#-- orig: cellspacing="" -->
<#-- Header row, contains product small image, product name, price -->
    <@tr>
        <@td>&nbsp;</@td>
<#list compareList as product>
    <#assign tdWidth = 100/compareList?size />
    <#assign productData = productDataMap[product.productId]/>
    <#assign productContentWrapper = productData.productContentWrapper/>
    <#assign price = productData.priceMap/>
    <#assign productUrl><@ofbizCatalogAltUrl productId=product.productId/></#assign>
    <#assign smallImageUrl = productContentWrapper.get("SMALL_IMAGE_URL", "url")!/>
    <#if !smallImageUrl?has_content>
        <#assign smallImageUrl = "/images/defaultImage.jpg"/>
    </#if>
        <@td style="width:${tdWidth?c}%;">
            <img src="<@ofbizContentUrl ctxPrefix=true>${smallImageUrl}</@ofbizContentUrl>" alt="Small Image"/><br />
            ${productContentWrapper.get("PRODUCT_NAME")!}<br />
    <#if totalPrice??>
            <div>${uiLabelMap.ProductAggregatedPrice}: <span class="basePrice"><@ofbizCurrency amount=totalPrice isoCode=totalPrice.currencyUsed/></span></div>
    <#else>
        <#if price.isSale?? && price.isSale>
            <#assign priceStyle = "salePrice">
        <#else>
            <#assign priceStyle = "regularPrice">
        </#if>

        <#if ((price.price!0) > 0) && ((product.requireAmount!"N") == "N")>
                <#if "Y" == (product.isVirtual!)> ${uiLabelMap.CommonFrom} </#if><span class="${priceStyle}"><@ofbizCurrency amount=price.price isoCode=price.currencyUsed/></span>
        </#if>
    </#if>
            <div class="productbuy">
    <#-- check to see if introductionDate hasn't passed yet -->
    <#if product.introductionDate?? && nowTimestamp.before(product.introductionDate)>
                <div class="${styles.text_color_alert!}">${uiLabelMap.ProductNotYetAvailable}</div>
    <#-- check to see if salesDiscontinuationDate has passed -->
    <#elseif product.salesDiscontinuationDate?? && nowTimestamp.after(product.salesDiscontinuationDate)/>
                <div class="${styles.text_color_alert!}">${uiLabelMap.ProductNoLongerAvailable}</div>
    <#-- check to see if it is a rental item; will enter parameters on the detail screen-->
    <#elseif (product.productTypeId!) == "ASSET_USAGE"/>
                <a href="javascript:doGetViaParent('${productUrl}');" class="${styles.link_nav!} ${styles.action_add!}">${uiLabelMap.OrderMakeBooking}...</a>
    <#elseif (product.productTypeId!) == "ASSET_USAGE_OUT_IN"/>
                <a href="javascript:doGetViaParent('${productUrl}');" class="${styles.link_nav!} ${styles.action_add!}">${uiLabelMap.OrderRent}...</a>
    <#-- check to see if it is an aggregated or configurable product; will enter parameters on the detail screen-->
    <#elseif (product.productTypeId!) == "AGGREGATED" || (product.productTypeId!) == "AGGREGATED_SERVICE"/>
                <a href="javascript:doGetViaParent('${productUrl}');" class="${styles.link_nav!} ${styles.action_configure!}">${uiLabelMap.OrderConfigure}...</a>
    <#-- check to see if the product is a virtual product -->
    <#elseif product.isVirtual?? && product.isVirtual == "Y"/>
                <a href="javascript:doGetViaParent('${productUrl}');" class="${styles.link_nav!} ${styles.action_select!}">${uiLabelMap.OrderChooseVariations}...</a>
    <#-- check to see if the product requires an amount -->
    <#elseif product.requireAmount?? && product.requireAmount == "Y"/>
                <a href="javascript:doGetViaParent('${productUrl}');" class="${styles.link_nav!} ${styles.action_select!}">${uiLabelMap.OrderChooseAmount}...</a>
    <#else>
                <form method="post" action="<@ofbizUrl>additem</@ofbizUrl>" name="compareFormAdd${product_index}">
                    <input type="hidden" name="add_product_id" value="${product.productId}"/>
                    <input type="text" size="5" name="quantity" value="1"/>
                    <input type="hidden" name="clearSearch" value="N"/>
                </form>
                <a href="javascript:doPostViaParent('compareFormAdd${product_index}');" class="${styles.link_run_session!} ${styles.action_add!}">${uiLabelMap.OrderAddToCart}</a>

        <#if prodCatMem?? && prodCatMem.quantity?? && 0.00 < prodCatMem.quantity?double>
                <form method="post" action="<@ofbizUrl>additem</@ofbizUrl>" name="compareFormAddDefault${product_index}">
                    <input type="hidden" name="add_product_id" value="${prodCatMem.productId!}"/>
                    <input type="hidden" name="quantity" value="${prodCatMem.quantity!}"/>
                    <input type="hidden" name="clearSearch" value="N"/>
                </form>
                <a href="javascript:doPostViaParent('compareFormAddDefault${product_index}');" class="${styles.link_run_session!} ${styles.action_add!}">${uiLabelMap.CommonAddDefault} (${prodCatMem.quantity?string.number}) ${uiLabelMap.OrderToCart}</a>
        </#if>
    </#if>
            </div>
        </@td>
</#list>
    </@tr>
    <#-- Brand name -->
    <@tr>
        <@td>${uiLabelMap.ProductBrandName}</@td>
<#list compareList as product>
        <@td>${product.brandName?default("&nbsp;")}</@td>
</#list>
    </@tr>
    <#-- Description -->
    <@tr>
        <@td>${uiLabelMap.ProductProductDescription}</@td>
<#list compareList as product>
    <#assign productData = productDataMap[product.productId]/>
    <#assign productContentWrapper = productData.productContentWrapper/>
        <@td>${productContentWrapper.get("DESCRIPTION")!"&nbsp;"}</@td>
</#list>
    </@tr>
    <#-- Long Description -->
    <@tr>
        <@td>${uiLabelMap.ProductLongDescription}</@td>
<#list compareList as product>
    <#assign productData = productDataMap[product.productId]/>
    <#assign productContentWrapper = productData.productContentWrapper/>
        <@td>${escapeVal(productContentWrapper.get("LONG_DESCRIPTION")!, 'htmlmarkup', {"allow":"internal"})}</@td>
</#list>
    </@tr>
<#list productFeatureTypeIds as productFeatureTypeId>
    <#assign productFeatureType = productFeatureTypeMap[productFeatureTypeId]/>
    <@tr>
        <@td>${productFeatureType.get("description", locale)}</@td>
    <#list compareList as product>
        <#assign productData = productDataMap[product.productId]/>
        <#assign applMap = productData[productFeatureTypeId]!/>
        <@td>
        <#if applMap.STANDARD_FEATURE?has_content>
            <#assign features = applMap.STANDARD_FEATURE/>
            <#list features as feature>
            <div>${feature.get("description", locale)}</div>
            </#list>
        </#if>
        <#if applMap.DISTINGUISHING_FEAT?has_content>
            <#assign features = applMap.DISTINGUISHING_FEAT/>
            <#list features as feature>
            <div>${feature.get("description", locale)}</div>
            </#list>
        </#if>
        <#if applMap.SELECTABLE_FEATURE?has_content>
            <#assign features = applMap.SELECTABLE_FEATURE/>
            <div>Available Options:</div>
            <ul>
            <#list features as feature>
                <li>${feature.get("description", locale)}</li>
            </#list>
            </ul>
        </#if>
        </@td>
    </#list>
    </@tr>
</#list>
    <@tr>
        <@td>&nbsp;</@td>
<#list compareList as product>
        <@td>
            <div class="productbuy">
    <#-- check to see if introductionDate hasn't passed yet -->
    <#if product.introductionDate?? && nowTimestamp.before(product.introductionDate)>
                <div class="${styles.text_color_alert!}">${uiLabelMap.ProductNotYetAvailable}</div>
    <#-- check to see if salesDiscontinuationDate has passed -->
    <#elseif product.salesDiscontinuationDate?? && nowTimestamp.after(product.salesDiscontinuationDate)/>
                <div class="${styles.text_color_alert!}">${uiLabelMap.ProductNoLongerAvailable}</div>
    <#-- check to see if it is a rental item; will enter parameters on the detail screen-->
    <#elseif (product.productTypeId!) == "ASSET_USAGE"/>
                <a href="javascript:doGetViaParent('${productUrl}');" class="${styles.link_nav!} ${styles.action_add!}">${uiLabelMap.OrderMakeBooking}...</a>
    <#elseif (product.productTypeId!) == "ASSET_USAGE_OUT_IN"/>
                <a href="javascript:doGetViaParent('${productUrl}');" class="${styles.link_nav!} ${styles.action_add!}">${uiLabelMap.OrderRent}...</a>
    <#-- check to see if it is an aggregated or configurable product; will enter parameters on the detail screen-->
    <#elseif (product.productTypeId!) == "AGGREGATED" || (product.productTypeId!) == "AGGREGATED_SERVICE"/>
                <a href="javascript:doGetViaParent('${productUrl}');" class="${styles.link_nav!} ${styles.action_configure!}">${uiLabelMap.OrderConfigure}...</a>
    <#-- check to see if the product is a virtual product -->
    <#elseif product.isVirtual?? && product.isVirtual == "Y"/>
                <a href="javascript:doGetViaParent('${productUrl}');" class="${styles.link_nav!} ${styles.action_select!}">${uiLabelMap.OrderChooseVariations}...</a>
    <#-- check to see if the product requires an amount -->
    <#elseif product.requireAmount?? && product.requireAmount == "Y"/>
                <a href="javascript:doGetViaParent('${productUrl}');" class="${styles.link_nav!} ${styles.action_select!}">${uiLabelMap.OrderChooseAmount}...</a>
    <#else>
                <form method="post" action="<@ofbizUrl>additem</@ofbizUrl>" name="compare2FormAdd${product_index}">
                    <input type="hidden" name="add_product_id" value="${product.productId}"/>
                    <input type="text" size="5" name="quantity" value="1"/>
                    <input type="hidden" name="clearSearch" value="N"/>
                </form>
                <a href="javascript:doPostViaParent('compare2FormAdd${product_index}');" class="${styles.link_run_session!} ${styles.action_add!}">${uiLabelMap.OrderAddToCart}</a>
        <#if prodCatMem?? && prodCatMem.quantity?? && (0.00 < prodCatMem.quantity?double)>
                <a href="javascript:doPostViaParent('compareFormAddDefault${product_index}');" class="${styles.link_run_session!} ${styles.action_add!}">${uiLabelMap.CommonAddDefault} (${prodCatMem.quantity?string.number}) ${uiLabelMap.OrderToCart}</a>
        </#if>
    </#if>
            </div>
        </@td>
</#list>
    </@tr>
</@table>
