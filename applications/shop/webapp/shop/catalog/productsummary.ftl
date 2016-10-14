<#include "catalogcommon.ftl">

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
    <#elseif productContentWrapper?? && productContentWrapper.get("SMALL_IMAGE_URL","url")!?string?has_content>
        <#assign smallImageUrl = productContentWrapper.get("SMALL_IMAGE_URL","url")?string?trim>        
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
    <#assign productTitle>
        <#if solrProduct?? && title??>
            <#assign productName = title>
        <#elseif productContentWrapper?? && productContentWrapper.get("PRODUCT_NAME","html")!?string?has_content>
            <#assign productName = productContentWrapper.get("PRODUCT_NAME","html")?string>
        <#elseif !productName??>
            <#assign productName = "">
        </#if>
        ${productName}
    </#assign>


    <#if smallImageUrl?has_content>
        <#assign imgSrc><@ofbizContentUrl>${smallImageUrl}</@ofbizContentUrl></#assign>
    <#else>
        <#assign imgSrc="https://placehold.it/300x100"/>    
    </#if>
    <#assign imgLink><@ofbizCatalogAltUrl productCategoryId=categoryId productId=product.productId/></#assign>
    <#assign productImage><@img src=imgSrc type="contain" link=imgLink width="100%" height="100px"/></#assign>

    <#assign productDescription>
        <#if solrProduct?? && description??>
            ${description}<#t>
        <#elseif productContentWrapper??>
            ${productContentWrapper.get("DESCRIPTION","html")!}<#--<#if daysToShip??></#if>--><#t>
        </#if>
    </#assign>

    <#assign productPrice>
        <#if solrProduct??>                   
            <#if solrProduct.listPrice??>
                <@ofbizCurrency amount=solrProduct.listPrice />          
            <#elseif solrProduct.defaultPrice??>                    
                <@ofbizCurrency amount=solrProduct.defaultPrice />
            </#if>
        <#elseif product??>
            <#if totalPrice??>
                <@ofbizCurrency amount=totalPrice isoCode=totalPrice.currencyUsed/>
            <#else>
                <#if ((price.price!0) > 0) && ((product.requireAmount!"N") == "N")>
                    <@ofbizCurrency amount=price.price isoCode=price.currencyUsed/>
                <#else>
                    <@ofbizCurrency amount=price.listPrice isoCode=price.currencyUsed/>
                </#if>
                <#if price.listPrice?? && price.price?? && price.price?double < price.listPrice?double>
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

     <@pul title=wrapAsRaw(productTitle!"", 'htmlmarkup')><#-- FIXME?: the wrapping already done by the content wrapper -->
        <#if price.isSale?? && price.isSale><li class="ribbon"><span>${uiLabelMap.OrderOnSale}!</span></li></#if>
        <@pli>
           ${productImage!""}
        </@pli>
        <#if productDescription?has_content>
        <@pli type="description">
            ${productDescription!""}       
        </@pli>
        </#if>
        <@pli type="price">
            ${productPrice!""}
        </@pli>
        <@pli type="button">
            <a href="<@ofbizCatalogAltUrl productCategoryId=categoryId productId=product.productId/>" class="${styles.link_nav!} ${styles.action_view!}">${uiLabelMap.CommonDetail}</a>           
        </@pli>
    </@pul>   

</#if>