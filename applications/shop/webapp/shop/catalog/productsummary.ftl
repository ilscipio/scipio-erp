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

<#if requestAttributes.solrProduct??>
    <#assign solrProduct = requestAttributes.solrProduct>
</#if>

<#if product?? || solrProduct??>
    <#-- variable setup -->
    <#assign targetRequestName = "product">
    <#if requestAttributes.targetRequestName?has_content>
        <#assign targetRequestName = requestAttributes.targetRequestName>
    </#if>    
    
    <#if solrProduct?has_content && solrProduct.mediumImage??>    
        <#assign smallImageUrl = raw(solrProduct.mediumImage)?trim>
    <#elseif solrProduct?has_content && solrProduct.smallImage??>
        <#assign smallImageUrl = raw(solrProduct.smallImage)?trim>
    <#elseif productContentWrapper?? && productContentWrapper.get("SMALL_IMAGE_URL","url")?has_content>
        <#assign smallImageUrl = productContentWrapper.get("SMALL_IMAGE_URL","url")>        
    </#if>
    
    <#assign isPromotional = false>
    <#if requestAttributes.isPromotional??>
        <#assign isPromotional = requestAttributes.isPromotional>
    </#if>
    
    <#assign class = "product-associated-item" />
    <#if requestAttributes.class??>
        <#assign class = requestAttributes.class />
    </#if>

    <#-- Product Information -->
    <#if solrProduct?? && title??>
        <#assign productName = title>
    <#elseif productContentWrapper?? && productContentWrapper.get("PRODUCT_NAME")?has_content>
        <#assign productName = productContentWrapper.get("PRODUCT_NAME")>
    <#elseif !productName??>
        <#assign productName = "">
    </#if>
    <#assign productTitle = productName/>

    <#if smallImageUrl?has_content>
        <#assign imgSrc = makeContentCtxPrefixUrl(smallImageUrl)>
    <#else>
        <#assign imgSrc = "https://placehold.co/300x100"/>
    </#if>
    <#assign imgLink><@catalogAltUrl rawParams=true productCategoryId=categoryId productId=productId/></#assign>
    <#assign productImage><@img src=imgSrc type="contain" link=imgLink width="100%" height="100px"/></#assign>

    <#assign productDescription>
        <#if solrProduct?? && description??>
            ${description}<#t>
        <#elseif productContentWrapper??>
            ${productContentWrapper.get("DESCRIPTION")!}<#--<#if daysToShip??></#if>--><#t>
        </#if>
    </#assign>

    <#assign productPrice>
        <#if hasProduct>
            <#-- SCIPIO: 4.0.0: totalPrice belongs to a configurable (AGGREGATED) product only; on the page of a configurable
                product the page's own total must not reach the cards of other products -->
            <#if totalPrice?? && (product.productTypeId!"")?starts_with("AGGREGATED")>
                <@ofbizCurrency amount=totalPrice isoCode=price.currencyUsed/>
            <#else>
                <#if ((price.price!0) > 0) && ((requireAmount!"N") == "N")>
                    <@ofbizCurrency amount=price.price isoCode=price.currencyUsed/>
                <#elseif price.listPrice??>
                    <@ofbizCurrency amount=price.listPrice isoCode=price.currencyUsed/>
                <#else>
                    -
                </#if>
                <#-- SCIPIO: 4.0.0: with the compliance component the saving follows the store's rules (EU: 30-day prior price) -->
                <#if scpCompliance && product?? && price.price??>
                    <#assign scpPriceRef = compliance.priceReference(product, price.price, price.currencyUsed, price.listPrice!0)>
                    <#if (scpPriceRef.percent!0) gt 0><sup><small>(-${scpPriceRef.percent}%)</small></sup></#if>
                <#elseif price.listPrice?? && price.price?? && (price.price?double < price.listPrice?double)>
                    <#assign priceSaved = price.listPrice?double - price.price?double>
                    <#assign percentSaved = (priceSaved?double / price.listPrice?double) * 100>
                    <#--<@ofbizCurrency amount=priceSaved isoCode=price.currencyUsed/>--> 
                    <#if (percentSaved?int > 0)><sup><small>(-${percentSaved?int}%)</small></sup></#if>
                </#if>
            </#if>
            <#if showPriceDetails?? && (showPriceDetails!"N") == "Y">
                <#if price.orderItemPriceInfos??>
                    <#list price.orderItemPriceInfos as orderItemPriceInfo>
                        ${orderItemPriceInfo.description!}
                    </#list>
                </#if>
            </#if>
        </#if>
    </#assign>

     <@pul title=productTitle>
        <#if price.isSale?? && price.isSale><@pli type="ribbon">${uiLabelMap.OrderOnSale}!</@pli></#if>
        <@pli>
           ${productImage}
        </@pli>
        <@pli class="+${styles.text_center}">
            <@ratingAsStars rating=averageRating!0 />
        </@pli>
        <#if productDescription?has_content>
        <@pli type="description">
            ${productDescription!""}       
        </@pli>
        </#if>
        <@pli type="price">
            ${productPrice}
        </@pli>
        <@pli type="button">
            <a href="<@catalogAltUrl productCategoryId=categoryId productId=productId/>" class="${styles.link_nav!} ${styles.action_view!}">${uiLabelMap.CommonDetail}</a>           
        </@pli>
    </@pul>   

</#if>
