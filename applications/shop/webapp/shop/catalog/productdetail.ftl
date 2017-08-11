
<#include "component://shop/webapp/shop/catalog/catalogcommon.ftl">

<#-- variable setup -->
<#assign price = priceMap! />
<#-- end variable setup -->


<@script>

    var featureCount = 0; <#-- NOTE: This is overridden further below -->
    var featureIdList = [];
    var variantProductInfoMap = {};
    var baseProductInfo = { <#-- TODO: move map making to groovy -->
        "productId" : "${escapeVal(product.productId, 'js')}",
        "requireAmount" : "${escapeVal(product.requireAmount!'N', 'js')}"
    };

    <#if variantTree?has_content>
    
        <#-- SCIPIO: Function to select a product variant  -->
        var variantTree = <@objectAsScript lang="js" object=variantTree />;
        var currentNode = [];
        variantProductInfoMap = <@objectAsScript lang="js" object=(variantProductInfoMap!{}) />;

        <#-- Product Variant - Option Updater
             The following script takes into account that there may be a diverse selection of configuration options.
             Each option is validated against the tree (the following select boxes invalidated) and reactivated, once the options
             have been updated according to the variantTree. -->
        function updateVariants(type, variantId, index){
            if(variantId){
                <#-- This cannot be relied on; now using a variable
                var selectNum = $('[id^=FT_]').length; // get number of select boxes -->
                var selectNum = featureCount;
                
                currentNode.splice(index); // reset the node information & remove anything beyond the current selection
                for(i = currentNode.length+1; i <= selectNum; i++){
                    var featureId = featureIdList[i];
                    $('#FT_'+featureId).prop( "disabled", true ); // invalidate all select boxes beyond the current selected one
                }
                currentNode.push(variantId); // update the node path
         
                //get data from variantTree
                var dataObject = variantTree;
                var value ="";
                currentNode.forEach(function (node,index) {
                    dataObject = dataObject[node]; // traverse down the tree, based on our path
                });
                
                if((index+1) >= selectNum){
                    var productId;
                    if (typeof dataObject === 'string' || dataObject instanceof String) {
                        productId = dataObject;
                    } else {
                        productId = dataObject[0];
                    }
                    if (productId) {
                        <#-- set the variant price -->
                        setVariantPrice(productId);
    
                        <#-- check for amount box -->
                        toggleAmt(checkAmtReq(productId));
                        
                        $('#add_product_id').val(productId);
                    }
                }else{
                    $('#add_product_id').val('NULL'); <#-- make sure to reset the add_product_id! otherwise issues when starting over -->
                    var nextIndex = index+1;
                    var options = [];
                    var nextFeatureId = featureIdList[nextIndex]; 
                    $('#FT_'+nextFeatureId).empty();
                    $('#FT_'+nextFeatureId).append('<option value="">${escapeVal(uiLabelMap.EcommerceSelectOption, 'js')}</option>');
                    $.each(dataObject,function(object) { 
                        $('#FT_'+nextFeatureId).append('<option value="'+object+'">'+object+'</option>');
                    });                    
                    $('#FT_'+nextFeatureId).prop( "disabled", false ); // activate next option
  
                    <#-- set the variant price
                        SCIPIO: TODO?: Currently can't do this here, at least not by productId...
                            so if we changed any box, just reset the price to the original virtual for now...
                    setVariantPrice(productId); -->
                    setVariantPriceSpec(baseCurrentPrice);

                    <#-- check for amount box
                    <#-- SCIPIO: For now we'll only set this at the last step, because don't have a tangible product ID until then
                    toggleAmt(checkAmtReq(productId)); -->
                    toggleAmt('N');
                }
            } else {
                setVariantPriceSpec("${escapeVal(uiLabelMap.OrderChooseVariations, 'js')}...");
            }         
        }
        
    </#if>

    function setVariantPrice(productId) {
        console.log("setting variant price for productId: " + productId);       
        var productInfo = variantProductInfoMap[productId];
        if (productInfo) {
            var price = productInfo.priceFormatted;
            if (price == null)
                price = productInfo.price;
            if (price != null) {
                setVariantPriceSpec(price);
            }
        }
    }
    
    function setVariantPriceSpec(price) {
        <#-- SCIPIO: TODO -->
        $("#product-price strong").text(price);
    }    
  
    function checkAmtReq(productId) {
        if (productId === baseProductInfo.productId) {
            var requireAmount = baseProductInfo.requireAmount;
            if (requireAmount) {
                return requireAmount;
            }       
        } else {
            var productInfo = variantProductInfoMap[productId];
            if (productInfo) {
                var requireAmount = productInfo.requireAmount;
                if (requireAmount) {
                    return requireAmount;
                }
            }
        }
        return 'N'; <#-- SCIPIO: hide it by default -->
    }
    
    <#-- SCIPIO: Handles final addItem submit -->
    function addItem() {
        var productId = jQuery('#add_product_id').val();
        <#-- Ensure we have a product -->
        if (!productId || productId === 'NULL') {
            showErrorAlert("${escapeVal(uiLabelMap.CommonErrorMessage2, 'js')}","${escapeVal(uiLabelMap.CommonPleaseSelectAllRequiredOptions, 'js')}");
            return;
        } 
        
        <#-- basic check (only): ensure all required features are selected.
            This is most important for VV_FEATURETREE (and REQUIRED_FEATURE, but not implemented in this template)
            We also run for VV_VARIANTTREE as a sanity check.
            FIXME?: OPTIONAL_FEATURE would require additional checks. -->
        <#--<#if (product.virtualVariantMethodEnum!) == "VV_FEATURETREE" && featureLists?has_content>-->
        if (featureCount > 0) {
            for(var i=0; i < featureIdList.length; i++) {
                var id = featureIdList[i];
                var val = jQuery('#FT_' + id).val();
                if (!val) {
                    showErrorAlert("${escapeVal(uiLabelMap.CommonErrorMessage2, 'js')}","${escapeVal(uiLabelMap.CommonPleaseSelectAllRequiredOptions, 'js')}");
                    return;
                }
            }
        }
        <#--</#if>-->
        
        <#-- verify quantity -->
        <#assign qtyErrorLabel = getLabel('cart.quantity_not_positive_number', 'OrderErrorUiLabels')>
        if (verifyQty('quantity', '${escapeVal(qtyErrorLabel, 'js')}') == null) {
            return;
        }
        
        <#-- verify amount (if applicable) -->
        if (checkAmtReq(productId) == 'Y') {
            <#assign amtErrorLabel = getLabel('AccountingFinAccountMustBePositive', 'AccountingErrorUiLabels')>
            if (verifyQty('add_amount', '${escapeVal(amtErrorLabel, 'js')}') == null) {
                return;
            }
        }

        <#-- SCIPIO: TODO?: This appears to be a sanity check to prevent trying to add a virtual product, which would be good,
             except it's not friendly to have this check in additem submit... isVirtual method is gone...
             server _should_ do this check anyway...
        if (isVirtual(addProductId)) {
            document.location = '<@ofbizUrl>product?category_id=${escapeVal(categoryId!, 'js')}&amp;product_id=</@ofbizUrl>' + addProductId;
            return;
        }
        -->
        document.addform.submit();
    }        
    
    function verifyQty(fieldId, qtyErrorLabel) {
        var quantity = jQuery('#' + fieldId).val();
        if (!quantity) {
            showErrorAlert("${escapeVal(uiLabelMap.CommonErrorMessage2, 'js')}", qtyErrorLabel);
            return null;
        }
        quantity = quantity.trim();
        jQuery('#' + fieldId).val(quantity);
        if (!quantity) {
            showErrorAlert("${escapeVal(uiLabelMap.CommonErrorMessage2, 'js')}", qtyErrorLabel);
            return null;
        }
        if (!quantity.match(/^[0-9.,]+$/)) {
            showErrorAlert("${escapeVal(uiLabelMap.CommonErrorMessage2, 'js')}", qtyErrorLabel);
            return null;
        }
        quantity = parseFloat(quantity);
        if (isNaN(quantity)) {
            showErrorAlert("${escapeVal(uiLabelMap.CommonErrorMessage2, 'js')}", qtyErrorLabel);
            return null;
        }
        if (quantity <= 0) {
            showErrorAlert("${escapeVal(uiLabelMap.CommonErrorMessage2, 'js')}", qtyErrorLabel);
            return null;
        }
        return quantity;
    }
    
    function toggleAmt(toggle) {
        if (toggle == 'Y') {
            jQuery("#add_amount_container").show();
        }

        if (toggle == 'N') {
            jQuery("#add_amount_container").hide();
        }
    }

</@script>

<div id="productdetail">

<@section>
    <@row>
        <@cell columns=8>
          <@productDetailImages />
        </@cell>
        <@cell columns=4>
            <@panel>
            <div id="product-info"> 
              <#assign hasDesc = productContentWrapper.get("DESCRIPTION")?has_content>
              <#if hasDesc><p></#if>
                <#if hasDesc>${productContentWrapper.get("DESCRIPTION")}</#if>
                <#-- for prices:
                    - if price < competitivePrice, show competitive or "Compare At" price
                    - if price < listPrice, show list price
                    - if price < defaultPrice and defaultPrice < listPrice, show default
                    - if isSale show price with salePrice style and print "On Sale!"
                -->
                <#-- SCIPIO: These are alternative prices that are not really commonly used 
                <#if price.competitivePrice?? && price.price?? && (price.price < price.competitivePrice)>
                    ${uiLabelMap.ProductCompareAtPrice}: <span class="product-price"><@ofbizCurrency amount=price.competitivePrice isoCode=price.currencyUsed /></span>
                </#if>
                -->
                <#-- Asset Usage price calculation -->
                <#-- SCIPIO: TODO?: styling for this detail info -->
                <#if (product.productTypeId!) == "ASSET_USAGE" || (product.productTypeId!) == "ASSET_USAGE_OUT_IN">
                    <#if product.reserv2ndPPPerc?? && product.reserv2ndPPPerc != 0><br />${uiLabelMap.ProductReserv2ndPPPerc}<#if !product.reservNthPPPerc?? || product.reservNthPPPerc == 0> ${uiLabelMap.CommonUntil} ${product.reservMaxPersons!}</#if><#rt/>
                        <#lt/> <span class="product-price"><@ofbizCurrency amount=(product.reserv2ndPPPerc*price.price/100) isoCode=price.currencyUsed /></span></#if>
                    <#if product.reservNthPPPerc?? && product.reservNthPPPerc != 0><br />${uiLabelMap.ProductReservNthPPPerc}<#if !product.reserv2ndPPPerc?? || product.reserv2ndPPPerc == 0> ${uiLabelMap.ProductReservSecond}<#else> ${uiLabelMap.ProductReservThird}</#if><#rt/>
                        <#lt/> ${uiLabelMap.CommonUntil} ${product.reservMaxPersons!}, ${uiLabelMap.ProductEach}: <span class="product-price"><@ofbizCurrency amount=(product.reservNthPPPerc*price.price/100) isoCode=price.currencyUsed /></span></#if>
                    <#if (!product.reserv2ndPPPerc?? || product.reserv2ndPPPerc == 0) && (!product.reservNthPPPerc?? || product.reservNthPPPerc == 0)><br />${uiLabelMap.ProductMaximum} ${product.reservMaxPersons!} ${uiLabelMap.ProductPersons}.</#if>
                </#if>
                <#--
                <#if price.specialPromoPrice?has_content>
                    <#assign currentPrice = price.specialPromoPrice/>
                <#if>
                -->
              <#if hasDesc></p></#if>

                <#if price.listPrice?has_content>
                    <#assign oldPrice = price.listPrice/>
                <#elseif price.defaultPrice?has_content>
                    <#assign oldPrice = price.defaultPrice/>
                </#if>

                <#if price.price?has_content>
                    <#assign currentPrice = price.price/>                    
                <#else>
                    <#assign currentPrice = oldPrice/>
                </#if>               

                <#-- SCIPIO: Uncomment to mark a product that is on sale
                <#if price.isSale?? && price.isSale>
                    <p>${uiLabelMap.OrderOnSale}!</p>
                </#if>-->
                
                <p>
                <#-- Only show the "old" price if the current price is lower (otherwise, bad advertisement) -->
                <#if oldPrice?has_content && currentPrice?has_content && (oldPrice?double > currentPrice?double)>
                    <span id="product-price_old"><del><@ofbizCurrency amount=oldPrice isoCode=price.currencyUsed /></del></span>
                </#if>
                 
                <#if ((product.isVirtual?has_content && product.isVirtual!?upper_case == "Y"))>
                    <span id="product-price"><strong>${uiLabelMap.OrderChooseVariations}...</strong></span>
                <#elseif currentPrice?has_content>
                    <span id="product-price"><strong><@ofbizCurrency amount=currentPrice isoCode=price.currencyUsed /></strong></span>
                </#if>
                    <@script>
                        var baseCurrentPrice = "${currentPrice}";
                    </@script>
                </p>
                
                <#-- SCIPIO: Uncomment to display how much a user is saving by buying this product
                <#if price.listPrice?? && price.price?? && (price.price < price.listPrice)>
                    <span id="product-saved"><sup>
                        <#assign priceSaved = oldPrice - currentPrice />
                        <#assign percentSaved = (priceSaved / oldPrice) * 100 />
                        ${uiLabelMap.OrderSave}: <@ofbizCurrency amount=priceSaved isoCode=price.currencyUsed /> (${percentSaved?int}%)
                        </sup>
                    </span>
                </#if>
                -->
                
                <#-- European VAT support (VAT included) -->
                <#if productStore?has_content && productStore.showPricesWithVatTax?has_content && productStore.showPricesWithVatTax =="Y">
                    <p><small>* ${getLabel("OrderSalesTaxIncluded")}</small></p>
                </#if>

                <#-- show price details ("showPriceDetails" field can be set in the screen definition) -->
                <#if (showPriceDetails?? && (showPriceDetails!"N") == "Y")>
                    <#if price.orderItemPriceInfos??>
                        <#list price.orderItemPriceInfos as orderItemPriceInfo>
                            <p>${orderItemPriceInfo.description!}</p>
                        </#list>
                    </#if>
                </#if>
            </div>
            
            <div id="product-add-cart">
              <#-- onePageCheckout-->
              <form method="post" action="<@ofbizUrl>additem</@ofbizUrl>" name="addform">
                <input type="hidden" name="goToOnePageCheckout" value="true" />
                    <#assign urlFile = Static["org.ofbiz.product.product.ProductContentWrapper"].getProductContentAsText(product, "URL_FILE", request,"html") />                    
                    <#assign inStock = true />
                    
                    <#-- SCIPIO: TODO: We currently have no client-side check for incompatible (FEATURE_IACTN_INCOMP) and dependent features.
                        See org.ofbiz.product.product.ProductWorker.getVariantFromFeatureTree(String, List<String>, Delegator).
                        The check only happens server-side currently. -->
                    
                    <#-- SCIPIO: TODO: There is another case not currently handled properly: ALTERNATIVE_PACKAGE - see CDR-1111-BX2 -->
                    
                    <#-- SCIPIO: This field should generally not be present! It is only for product that support/require an amount
                        in ADDITION to the quantity field. You also cannot know this for sure beforehand because for virtuals it is set on the
                        specific variant, so it has to be updated through JS lookups (the initial is only for the virtual product, but each variant can be different).  -->
                    <#macro amountField>
                        <#local fieldStyle = "">
                        <#if (product.requireAmount!"N") != "Y">
                            <#-- SCIPIO: Issues with css
                            <#assign hiddenStyle = styles.hidden!/>-->
                            <#local fieldStyle = "display: none;">
                        </#if>
                        <@field type="input" size="5" name="add_amount" id="add_amount" containerStyle=fieldStyle value="" label=uiLabelMap.CommonAmount /> <#-- containerClass=("+"+hiddenStyle) -->
                    </#macro>
                    
                    <#-- Variant Selection -->
                    <#if (product.isVirtual!?upper_case) == "Y">
                        <#if (product.virtualVariantMethodEnum!) == "VV_FEATURETREE" && featureLists?has_content>
                            <input type="hidden" name="add_product_id" id="add_product_id" value="${product.productId}" />
                            <#list featureLists as featureList>
                                <@field type="select" id="FT_${featureList.productFeatureTypeId}" name="FT${featureList.productFeatureTypeId}" label=(featureList.description!"")>
                                    <option value="">${uiLabelMap.EcommerceSelectOption}</option>
                                    <#list featureList.features as feature>
                                        <option value="${feature.productFeatureId}">${feature.description} <#if feature.price??>(+ <@ofbizCurrency amount=feature.price?string isoCode=feature.currencyUomId />)</#if></option>
                                    </#list>
                                </@field>
                            </#list>
                            <@field type="text" name="quantity" id="quantity" value="1" size="4" maxLength="4" label=uiLabelMap.CommonQuantity/>
                            <@amountField />
                            <@script>
                                featureCount = ${featureLists?size};
                                featureIdList = [<#list featureLists as featureList>"${escapeVal(featureList.productFeatureTypeId, 'js')}"<#if featureList_has_next>,</#if></#list>];
                                jQuery(document).ready(function() {
                                  <#-- SCIPIO: FIXME?: This forces the select to return to their empty values upon page refresh.
                                      We don't really want this, but otherwise values after browser refresh are currently too inconsistent -->
                                  <#list featureLists as featureList>
                                    jQuery('#FT_${escapeVal(featureList.productFeatureTypeId, 'js')}').val('');
                                  </#list>
                                });
                            </@script>
                        </#if>
                        
                        <#-- SCIPIO: It is possible to have a limited amount of variant combination. 
                                   Therefore the available options are only displayed for the first variant and updated for the next based on the selected type. -->
                        <#if !product.virtualVariantMethodEnum?? || product.virtualVariantMethodEnum == "VV_VARIANTTREE">                            
                            <#if variantTree?? && (variantTree.size() > 0)>
                                <#list featureSet as currentType>
                                    <#if currentType_index == 0>
                                        <@field type="select" id="FT_${currentType}" name="FT${currentType}" label=(featureTypes.get(currentType)!"") onChange="javascript:updateVariants(this.name,this.value,${currentType_index});">
                                            <option value="">${uiLabelMap.EcommerceSelectOption}</option>
                                            <#list variantTree.keySet() as variant>
                                                <option value="${variant}">${variant}</option>
                                            </#list>
                                        </@field>
                                    <#else>
                                        <@field type="select" id="FT_${currentType}" name="FT${currentType}" label=(featureTypes.get(currentType)!"") onChange="javascript:updateVariants(this.name,this.value,${currentType_index});" disabled=true />
                                    </#if>
                                </#list>
                                <input type="hidden" name="add_product_id" id="add_product_id" value="NULL"/>
                                <@script>
                                    featureCount = ${featureSet?size};
                                    featureIdList = <@objectAsScript lang="js" object=featureSet />;
                                    
                                    jQuery(document).ready(function() {
                                      <#-- SCIPIO: FIXME?: This forces the select to return to their empty values upon page refresh.
                                          We don't really want this, but otherwise values after browser refresh are currently too inconsistent -->
                                      <#list featureSet as currentType>
                                        jQuery('#FT_${escapeVal(currentType, 'js')}').val('');
                                      </#list>
                                    });
                                </@script>
                            <#else>
                                <input type="hidden" name="add_product_id" id="add_product_id" value="NULL"/>
                                <#assign inStock = false />
                            </#if>
                        </#if>
                    <#else>
                        <#-- SCIPIO: This is a sanity check, leave here for debugging, will do no harm -->
                        <#assign selFeatureTypes=toSimpleMap(selFeatureTypes!{})>
                        <#if selFeatureTypes?has_content>
                          <p>
                            <strong>WARN: </strong> Product has selectable features 
                              [<#list mapKeys(selFeatureTypes) as typeId>${escapeVal(selFeatureTypes[typeId]!typeId!, 'html')}<#if typeId_has_next>, </#if></#list>]
                              but is not virtual - not currently handled
                          </p>
                        </#if>
                    
                        <input type="hidden" name="add_product_id" id="add_product_id" value="${product.productId}" />
                        <#if (availableInventory??) && (availableInventory <= 0)>
                            <#assign inStock = false />
                        </#if>
                    </#if>
    
                    <#-- check to see if introductionDate hasnt passed yet -->
                    <#if product.introductionDate?? && nowTimestamp.before(product.introductionDate)>
                        <@alert type="info">${uiLabelMap.ProductProductNotYetMadeAvailable}.</@alert>
                        <#-- check to see if salesDiscontinuationDate has passed -->
                    <#elseif product.salesDiscontinuationDate?? && nowTimestamp.after(product.salesDiscontinuationDate)>
                        <@alert type="info">${uiLabelMap.ProductProductNoLongerAvailable}.</@alert>
                        <#-- check to see if the product requires inventory check and has inventory -->                        
                    <#elseif (product.virtualVariantMethodEnum!) != "VV_FEATURETREE">
                        <#if inStock>
                            <#if (product.productTypeId!) == "ASSET_USAGE" || (product.productTypeId!) == "ASSET_USAGE_OUT_IN">
                                <#-- SCIPIO: NOTE: here the actual dateType sent to server is yyyy-MM-dd -->
                                <@field type="datetime" dateType="date" name="reservStart" maxlength=10 value=(parameters.reservStart!(earliestReservStartDate?string?substring(0, 10))) label=uiLabelMap.EcommerceStartdate />

                                <#-- SCIPIO: TODO?: Consolidate these two inputs?... I think reservLength is more important... reservEnd is not included on cart
                                <@field type="datetime" dateType="date" name="reservEnd" maxlength=10 value=(parameters.reservEnd!) label=uiLabelMap.CommonEndDate /> -->
                                <@field type="input" size="4" name="reservLength" value=(parameters.reservLength!) label=uiLabelMap.CommonDays />

                                <@field type="input" size="4" name="reservPersons" value=(parameters.reservPersons!"1") label=uiLabelMap.CommonPersons/>
                                <#-- SCIPIO: "Rooms" is too specific without checking product config: label=uiLabelMap.ProductRooms -->
                                <@field type="input" size="4" name="quantity" id="quantity" value=(parameters.quantity!"1") maxLength="4" label=uiLabelMap.CommonQuantity/>
                            <#else> 
                                <@field name="quantity" id="quantity" label=uiLabelMap.CommonQuantity value=(parameters.quantity!"1") size="4" maxLength="4" type="input" /> <#-- there's no need to handle this: disabled=((product.isVirtual!?upper_case) == "Y") -->
                            </#if>
                            <@amountField />
                            <@field type="submit" submitType="link" id="addToCart" name="addToCart" href="javascript:addItem();" text=uiLabelMap.OrderAddToCart class="+${styles.grid_columns_12} ${styles.link_run_session!} ${styles.action_add!}"/> 
                        <#else>
                            <#if productStore??>
                                <#if productStore.requireInventory?? && productStore.requireInventory == "N">
                                    <#if product.inventoryMessage?has_content><div>${product.inventoryMessage}</div></#if>
                                    <@field name="quantity" id="quantity" label=uiLabelMap.CommonQuantity value="1" size="4" maxLength="4" type="input" /> <#-- there's no need to handle this: disabled=((product.isVirtual!?upper_case) == "Y") -->
                                    <@amountField />
                                    <@field type="submit" submitType="link" id="addToCart" name="addToCart" href="javascript:addItem();" text=uiLabelMap.OrderAddToCart class="+${styles.grid_columns_12} ${styles.link_run_session!} ${styles.action_add!}"/> 
                                <#else>
                                    <input name="quantity" id="quantity" value="1" type="hidden"/>
                                    <div>${uiLabelMap.ProductItemOutOfStock}<#if product.inventoryMessage?has_content>&mdash; ${product.inventoryMessage}</#if></div>
                                </#if>
                            </#if>
                        </#if>
                    <#elseif (product.virtualVariantMethodEnum!) == "VV_FEATURETREE">
                        <#-- SCIPIO: NOTE: All stuff for VV_FEATURETREE should already be included above (except button) -->
                        <@field type="submit" submitType="link" id="addToCart" name="addToCart" href="javascript:addItem();" text=uiLabelMap.OrderAddToCart class="+${styles.grid_columns_12} ${styles.link_run_session!} ${styles.action_add!}"/> 
                    </#if>                 

              </form>
                <#-- SCIPIO: Review 
                    <#if variantPriceList??>
                            <#list variantPriceList as vpricing>
                                <#assign variantName = vpricing.get("variantName")!>
                                <#assign secondVariantName = vpricing.get("secondVariantName")!>
                                <#assign minimumQuantity = vpricing.get("minimumQuantity")>
                                <#if (minimumQuantity > 0)>
                                    <div>minimum order quantity for ${secondVariantName!} ${variantName!} is ${minimumQuantity!}</div>
                                </#if>
                            </#list>
                        <#elseif minimumQuantity?? && minimumQuantity?has_content && (minimumQuantity > 0)>
                            <div>minimum order quantity for ${productContentWrapper.get("PRODUCT_NAME")!} is ${minimumQuantity!}</div>
                        </#if>
                -->
            </div>
            </@panel>
            <#-- SCIPIO: Shopping list functionality - disabled for now
            <div id="product-shopping-list">
                <#if userHasAccount>
                    <form name="addToShoppingList" method="post" action="<@ofbizUrl>addItemToShoppingList<#if requestAttributes._CURRENT_VIEW_??>/${requestAttributes._CURRENT_VIEW_}</#if></@ofbizUrl>">
                        <fieldset>
                            <input type="hidden" name="productId" value="${product.productId}" />
                            <input type="hidden" name="product_id" value="${product.productId}" />
                            <input type="hidden" name="productStoreId" value="${productStoreId}" />
                            <input type="hidden" name="reservStart" value= "" />
                            <select name="shoppingListId">
                                <#if shoppingLists?has_content>
                                    <#list shoppingLists as shoppingList>
                                        <option value="${shoppingList.shoppingListId}">${shoppingList.listName}</option>
                                    </#list>
                                </#if>
                                <option value=""> </option>
                                <option value="">${uiLabelMap.OrderNewShoppingList}</option>
                            </select>
                            &nbsp;&nbsp;
                            <#if (product.productTypeId!) == "ASSET_USAGE" || (product.productTypeId!) == "ASSET_USAGE_OUT_IN">
                                &nbsp;${uiLabelMap.CommonStartDate} (yyyy-mm-dd)<input type="text" size="10" name="reservStartStr" />Number of&nbsp;days<input type="text" size="4" name="reservLength" />&nbsp;Number of&nbsp;persons<input type="text" size="4" name="reservPersons" value="1" />Qty&nbsp;<input type="text" size="5" name="quantity" value="1" />
                            <#else>
                                <input type="text" size="5" name="quantity" value="1" />
                                <input type="hidden" name="reservStartStr" value= "" />
                            </#if>
                            <a href="javascript:addShoplistSubmit();" >${uiLabelMap.OrderAddToShoppingList}</a>
                        </fieldset>
                    </form>
                <#else>                
                    ${uiLabelMap.OrderYouMust} <a href="<@ofbizUrl>checkLogin/ShowCart</@ofbizUrl>">${uiLabelMap.CommonBeLogged}</a>
                    ${uiLabelMap.OrderToAddSelectedItemsToShoppingList}.&nbsp;
                </#if>
            </div>
             -->    
         
            <div class="shariff" data-lang="${locale[0..1]!"en"}"></div>

        </@cell>
    </@row>
</@section>        

<#-- SCIPIO: show tell a friend details only in shop application     
<div id="product-tell-a-friend">
    <a href="javascript:popUpSmall('<@ofbizUrl>tellafriend?productId=${product.productId}</@ofbizUrl>','tellafriend');" >${uiLabelMap.CommonTellAFriend}</a>
</div>
-->   
     
<@section>
    <#assign prodLongDescr = escapeVal(productContentWrapper.get("LONG_DESCRIPTION")!, 'htmlmarkup', {"allow":"internal"})/>
    <#if !prodLongDescr?has_content>
      <#assign prodLongDescr = productContentWrapper.get("DESCRIPTION")!?trim/>
    </#if>
    <#assign prodWarnings = escapeVal(productContentWrapper.get("WARNINGS")!, 'htmlmarkup', {"allow":"internal"})/>
    
    <#assign productDetailLongDescContentString><@productDetailLongDescContent /></#assign>
    <#assign productDetailProductAttribContentString><@productDetailProductAttribContent /></#assign>
    
    <#if productDetailLongDescContentString?has_content || productDetailProductAttribContentString?has_content>
        <@tabs>
            <#if productDetailLongDescContentString?trim?has_content>
                <@tab title=uiLabelMap.CommonOverview>
                    ${productDetailLongDescContentString}
                </@tab>
            </#if>
            <#if productDetailProductAttribContentString?trim?has_content>
                <@tab title=uiLabelMap.CommonSpecifications>
                    ${productDetailProductAttribContentString}
                </@tab>
            </#if>
        </@tabs>
    </#if>
    
</@section>
<@section>
        <#-- Prefill first select box (virtual products only)
        <div id="product-virtual-swatch">            
            <#if variantTree?? && (0 < variantTree.size())>
                <script type="text/javascript">eval("list" + "${featureOrderFirst}" + "()");</script>
            </#if>
    
            <#if variantSample?? && (0 < variantSample.size())>
                <#assign imageKeys = variantSample.keySet() />
                <#assign imageMap = variantSample />            
                <#assign maxIndex = 7 />
                <#assign indexer = 0 />
                <#list imageKeys as key>
                    <#assign swatchProduct = imageMap.get(key) />
                    <#if swatchProduct?has_content && (indexer < maxIndex)>
                        <#assign imageUrl = Static["org.ofbiz.product.product.ProductContentWrapper"].getProductContentAsText(swatchProduct, "SMALL_IMAGE_URL", request,"html")! />
                        <#if !imageUrl?string?has_content>
                            <#assign imageUrl = productContentWrapper.get("SMALL_IMAGE_URL","url")! />
                        </#if>

                        <div class="product-virtual-swatch-item">
                           <a href="javascript:getList('FT${featureOrderFirst}','${indexer}',1);"><img src="<@ofbizContentUrl ctxPrefix=true>${imageUrl}</@ofbizContentUrl>" width="60" height="60" alt="" /></a>                        
                           <a href="javascript:getList('FT${featureOrderFirst}','${indexer}',1);" class="linktext">${key}</a>
                           <div class="clear"></div>
                        </div>
                    </#if>
                    <#assign indexer = indexer + 1 />
                </#list>
                <#if (indexer > maxIndex)>
                    <p>${uiLabelMap.ProductMoreOptions}</p>
                </#if>
            </#if>
        </div>
         -->

        <@commonAssociatedProducts productValue=product commonFeatureResultIds=(commonFeatureResultIds!)/>

</@section>

<#-- SCIPIO: uncomment to use unavailableVariants
<#macro showUnavailableVarients>
  <#if unavailableVariants??>
    <ul>
      <#list unavailableVariants as prod>
        <#assign features = prod.getRelated("ProductFeatureAppl")/>
        <li>
          <#list features as feature>
            <em>${feature.getRelatedOne("ProductFeature").description}</em><#if feature_has_next>, </#if>
          </#list>
          <span>${uiLabelMap.ProductItemOutOfStock}</span>
        </li>
      </#list>
    </ul>
  </#if>
</#macro>-->

</div>

