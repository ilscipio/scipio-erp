<#if requestAttributes.solrProduct?exists>
    <#assign solrProduct = requestAttributes.solrProduct>
</#if>

<#if product?exists || solrProduct?exists>
    <#-- variable setup -->
    <#assign targetRequestName = "product">
    <#if requestAttributes.targetRequestName?has_content>
        <#assign targetRequestName = requestAttributes.targetRequestName>
    </#if>    
    
    <#if solrProduct?has_content && solrProduct.mediumImage?exists>    
        <#assign smallImageUrl = solrProduct.mediumImage>
    <#elseif solrProduct?has_content && solrProduct.smallImage?exists>
        <#assign smallImageUrl = solrProduct.smallImage>        
    <#elseif productContentWrapper?exists && productContentWrapper.get("LARGE_IMAGE_URL","html")?has_content>
        <#assign smallImageUrl = productContentWrapper.get("LARGE_IMAGE_URL","html")?if_exists>        
    </#if>
    

    <#assign isPromotional = false>
    <#if requestAttributes.isPromotional?exists>
        <#assign isPromotional = requestAttributes.isPromotional>
    </#if>
    
    <#assign class = "product-associated-item" />
    <#if requestAttributes.class?exists>
        <#assign class = requestAttributes.class />
    </#if>

    <#-- Product Information -->
    <#assign productTitle>
        <#if solrProduct?exists && title?exists>
            <#assign productName = title>
        <#elseif productContentWrapper?exists && productContentWrapper.get("PRODUCT_NAME","html")?has_content>
            <#assign productName = productContentWrapper.get("PRODUCT_NAME","html")>
        </#if>
        ${productName}
    </#assign>

    <#assign productImage>
        <#assign imgSrc><@ofbizContentUrl>${smallImageUrl}</@ofbizContentUrl></#assign>
        <#assign imgLink><@ofbizCatalogAltUrl productCategoryId=categoryId productId=product.productId/></#assign>
        <@img src=imgSrc!"" type="contain" link=link!"" width="100%" height="100px"/>
    </#assign>

    <#assign productDescription>
        <#if solrProduct?exists && description?exists>
            ${description}
        <#elseif productContentWrapper?exists>
            ${productContentWrapper.get("DESCRIPTION","html")?if_exists}<#if daysToShip?exists></#if>
        </#if>
    </#assign>

    <#assign productPrice>
        <#if solrProduct?exists>                   
            <#if solrProduct.listPrice?exists>
                <@ofbizCurrency amount=solrProduct.listPrice />          
            <#elseif solrProduct.defaultPrice?exists>                    
                <@ofbizCurrency amount=solrProduct.defaultPrice />
            </#if>
        <#elseif product?exists>
            <#if totalPrice?exists>
                <@ofbizCurrency amount=totalPrice isoCode=totalPrice.currencyUsed/>
            <#else>
                <#if (price.price?default(0) > 0 && product.requireAmount?default("N") == "N")>
                        <@ofbizCurrency amount=price.price isoCode=price.currencyUsed/>
                    <#else>
                        <@ofbizCurrency amount=price.listPrice isoCode=price.currencyUsed/>
                </#if>
                <#if price.listPrice?exists && price.price?exists && price.price?double < price.listPrice?double>
                    <#assign priceSaved = price.listPrice?double - price.price?double>
                    <#assign percentSaved = (priceSaved?double / price.listPrice?double) * 100>
                    <#--<@ofbizCurrency amount=priceSaved isoCode=price.currencyUsed/>--> 
                    <#if percentSaved?int &gt; 0><sup><small>(-${percentSaved?int}%)</small><sup></#if>
                </#if>
            </#if>
            <#if (showPriceDetails?exists && showPriceDetails?default("N") == "Y")>
                <#if price.orderItemPriceInfos?exists>
                    <#list price.orderItemPriceInfos as orderItemPriceInfo>
                        ${orderItemPriceInfo.description?if_exists}
                    </#list>
                </#if>
            </#if>

        </#if>
    </#assign>

     <@pul title=productTitle!"">
        <#if smallImageUrl?has_content>
            <@pli>
               ${productImage!""}
            </@pli>
        </#if>
        <#if productDescription?has_content>
        <@pli type="description">
            ${productDescription!""}       
         </@pli>
         </#if>
         <@pli type="price">
            ${productPrice!""}
        </@pli>
        <#if price.isSale?exists && price.isSale>
            <@pli>${uiLabelMap.OrderOnSale}!</@pli>
        </#if>
        <@pli type="button">
            <a href="<@ofbizCatalogAltUrl productCategoryId=categoryId productId=product.productId/>" class="${styles.button_default!}">${uiLabelMap.CommonDetail}</a>           
        </@pli>
    </@pul>   

</#if>