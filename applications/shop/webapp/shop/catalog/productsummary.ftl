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
        <#assign imgSrc = makeOfbizContentCtxPrefixUrl(smallImageUrl)>
    <#else>
        <#assign imgSrc = "https://placehold.it/300x100"/>    
    </#if>
    <#assign imgLink><@ofbizCatalogAltUrl rawParams=true productCategoryId=categoryId productId=product.productId/></#assign>
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
                <@ofbizCurrency amount=totalPrice isoCode=totalPrice.currencyUsed/>
            <#else>
                <#if ((price.price!0) > 0) && ((product.requireAmount!"N") == "N")>
                    <@ofbizCurrency amount=price.price isoCode=price.currencyUsed/>
                <#else>
                    <@ofbizCurrency amount=price.listPrice isoCode=price.currencyUsed/>
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
        <#if productDescription?has_content>
        <@pli type="description">
            ${productDescription!""}       
        </@pli>
        </#if>
        <@pli type="price">
            ${productPrice}
        </@pli>
        <@pli type="button">
            <a href="<@ofbizCatalogAltUrl productCategoryId=categoryId productId=product.productId/>" class="${styles.link_nav!} ${styles.action_view!}">${uiLabelMap.CommonDetail}</a>           
        </@pli>
    </@pul>   

</#if>