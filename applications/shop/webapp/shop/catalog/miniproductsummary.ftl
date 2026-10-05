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

<#if miniProduct??>
    <#if solrProduct?has_content && solrProduct.mediumImage??>    
        <#assign smallImageUrl = solrProduct.mediumImage>
    <#elseif solrProduct?has_content && solrProduct.smallImage??>
        <#assign smallImageUrl = solrProduct.smallImage>        
    <#elseif miniProductContentWrapper?? && miniProductContentWrapper.get("SMALL_IMAGE_URL","url")?has_content>
        <#assign smallImageUrl = miniProductContentWrapper.get("SMALL_IMAGE_URL","url")>        
    </#if>

    <#if smallImageUrl?has_content>
        <#assign imgSrc = makeContentUrl(smallImageUrl) />
    <#else>
        <#assign imgSrc = "https://placehold.co/300x100"/>
    </#if>
    <#assign imgLink><@catalogAltUrl rawParams=true productCategoryId=requestParameters.category_id!"" productId=miniProduct.productId/></#assign>

    <#assign productImage>
        <#-- has no effect: (imgSrc!"https://placehold.co/300x100") -->
        <@img src=imgSrc type="contain" link=imgLink!"" width="100%" height="100px"/>
    </#assign>
    <@pul>
        <#if priceResult.isSale?? && priceResult.isSale><@pli type="ribbon">${uiLabelMap.OrderOnSale}!</@pli></#if>
            <@pli>
               ${productImage}
            </@pli>
        <@pli type="description">
            <#-- SCIPIO: TODO: Localize -->
            ${miniProductContentWrapper.get("PRODUCT_NAME")!"No Name Available"}
         </@pli>

         <@pli>
           <#if ((priceResult.price!0) > 0) && ((miniProduct.requireAmount!"N") == "N")>
            <#if totalPrice?? && (miniProduct.productTypeId!"")?starts_with("AGGREGATED")>
                <@ofbizCurrency amount=totalPrice isoCode=priceResult.currencyUsed/>
            <#else>
                <@ofbizCurrency amount=priceResult.price isoCode=priceResult.currencyUsed/>
            </#if>
          </#if>
          <a href="<@catalogAltUrl productCategoryId=requestParameters.category_id!"" productId=miniProduct.productId/>"><i class="${styles.icon} ${styles.icon_prefix}magnifying-glass ${styles.icon_prefix!}search"></i></a>
        </@pli>
        </@pul>

</#if>
