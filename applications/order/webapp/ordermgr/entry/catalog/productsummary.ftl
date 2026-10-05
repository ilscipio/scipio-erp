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
<#--
Changes to this file: Copyright (C) Ilscipio GmbH. The changes are licensed
under the GNU Affero General Public License, version 3, or a commercial
license from Ilscipio GmbH (file LICENSE). The original code stays under
the Apache License, version 2.0, as stated above.
-->

<#include "component://order/webapp/ordermgr/entry/catalog/catalogcommon.ftl">

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
        <#assign smallImageUrl = solrProduct.mediumImage?trim>
    <#elseif solrProduct?has_content && solrProduct.smallImage??>
        <#assign smallImageUrl = solrProduct.smallImage?trim>        
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
    <#assign imgLink><@pageUrl uri="product?product_id="+escapeVal(product.productId, 'url')/></#assign>
    <#assign productImage><@img src=imgSrc type="contain" link=imgLink width="100%" height="100px"/></#assign>

    <#assign productDescription>
        <#if solrProduct?? && description??>
            ${description}<#t>
        <#elseif productContentWrapper??>
            ${productContentWrapper.get("DESCRIPTION")!}<#--<#if daysToShip??></#if>--><#t>
        </#if>
    </#assign>

    <#assign productPrice>
        <#if product??>
            <#if totalPrice??>
                <@ofbizCurrency amount=totalPrice isoCode=price.currencyUsed/>
            <#else>
                <#if ((price.price!0) > 0) && ((product.requireAmount!"N") == "N")>
                    <@ofbizCurrency amount=price.price isoCode=price.currencyUsed/>
                <#elseif price.listPrice??>
                    <@ofbizCurrency amount=price.listPrice isoCode=price.currencyUsed/>
                <#else>
                    -
                </#if>
                <#if price.listPrice?? && price.price?? && (price.price?double < price.listPrice?double)>
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
        <#-- FIXME: PRICE CANNOT WORK PROPERLY WITHOUT CURRENCY UOM (STORED + TARGET)!
            don't even try to display until this is resolved, because wrong value is more confusing
            than no value
        <#elseif solrProduct??>
            
             
            <#if solrProduct.listPrice??>
                <@ofbizCurrency amount=solrProduct.listPrice />          
            <#elseif solrProduct.defaultPrice??>                    
                <@ofbizCurrency amount=solrProduct.defaultPrice />
            </#if>
        -->
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
            <a href="<@pageUrl uri="product?product_id="+escapeVal(product.productId, 'url')/>" class="${styles.link_nav!} ${styles.action_view!}">${uiLabelMap.CommonDetail}</a>           
        </@pli>
    </@pul>   

</#if>