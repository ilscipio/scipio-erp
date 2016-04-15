<#-- variable setup -->
<#assign price = priceMap! />
<#-- end variable setup -->

<#macro associated assocProducts beforeName showName afterName formNamePrefix targetRequestName>
      <#assign pageProduct = product />
      <#assign targetRequest = "product" />
      <#if targetRequestName?has_content>
        <#assign targetRequest = targetRequestName />
      </#if>
      <#if assocProducts?has_content>
        <#assign assocTitle>${beforeName!}<#if showName == "Y">${productContentWrapper.get("PRODUCT_NAME", "html")!}</#if>${afterName!}</#assign>
        <@section title=assocTitle>
            <@grid columns=5>
                <#list assocProducts as productAssoc>
                    <li>
                        <#if productAssoc.productId == product.productId>
                            <#assign assocProductId = productAssoc.productIdTo />
                        <#else>
                            <#assign assocProductId = productAssoc.productId />
                        </#if>
                        <#--
                          <a href="<@ofbizUrl>${targetRequest}/<#if categoryId??>~category_id=${categoryId}/</#if>~product_id=${assocProductId}</@ofbizUrl>" class="${styles.link_nav_info_id!}">
                            ${assocProductId}
                          </a>
                        <#if productAssoc.reason?has_content>
                          - <strong>${productAssoc.reason}</strong>
                        </#if>
                        </div>
                      -->
                      <#assign dummy = setRequestAttribute("optProductId", assocProductId)>
                      <#assign dummy = setRequestAttribute("listIndex", listIndex)>
                      <#assign dummy = setRequestAttribute("formNamePrefix", formNamePrefix)>
                      <#if targetRequestName?has_content>
                        <#assign dummy = setRequestAttribute("targetRequestName", targetRequestName)>
                      </#if>
                      <@render resource="component://shop/widget/CatalogScreens.xml#miniproductsummary" />
                  </li>
                  <#assign product = pageProduct />
                  <#local listIndex = listIndex + 1 />
                </#list>
            </@grid>
            <#assign dummy = setRequestAttribute("optProductId", "")>
            <#assign dummy = setRequestAttribute("formNamePrefix", "")>
            <#assign dummy = setRequestAttribute("targetRequestName", "")>

        </@section>
      </#if>
    </#macro>

<@script>

    var featureCount = 0; <#-- NOTE: This is overridden further below -->
    var featureIdList = [];

    <#if variantTree?has_content>
    
        <#-- CATO: Function to select a product variant  -->
        var variantTree = <@objectAsScript lang="js" object=variantTree />;
        var currentNode = [];
        var variantProductInfoMap = <@objectAsScript lang="js" object=variantProductInfoMap />;

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
                    $('#FT_'+nextFeatureId).append('<option value="">${uiLabelMap.EcommerceSelectOption}</option>');
                    $.each(dataObject,function(object) { 
                        $('#FT_'+nextFeatureId).append('<option value="'+object+'">'+object+'</option>');
                    });                    
                    $('#FT_'+nextFeatureId).prop( "disabled", false ); // activate next option
  
                    <#-- set the variant price
                        Cato: TODO?: Currently can't do this here, at least not by productId...
                            so if we changed any box, just reset the price to the original virtual for now...
                    setVariantPrice(productId); -->
                    setVariantPriceSpec(baseCurrentPrice);

                    <#-- check for amount box
                    <#-- Cato: For now we'll only set this at the last step, because don't have a tangible product ID until then
                    toggleAmt(checkAmtReq(productId)); -->
                    toggleAmt('N');
                }
            }          
        }
        
    </#if>

    function setVariantPrice(productId) {
        var productInfo = variantProductInfoMap[productId];
        if (productInfo) {
            var price = productInfo.price;
            if (price != null) {
                setVariantPriceSpec(price);
            }
        }
    }
    
    function setVariantPriceSpec(price) {
        <#-- Cato: TODO -->
    }    
  
    function checkAmtReq(productId) {
        var productInfo = variantProductInfoMap[productId];
        if (productInfo) {
            var requireAmount = productInfo.requireAmount;
            if (requireAmount) {
                return requireAmount;
            }
        }
        return 'N'; <#-- Cato: hide it by default -->
    }
    
    <#-- Cato: Handles final addItem submit -->
    function addItem() {
        var productId = jQuery('#add_product_id').val();
        <#-- Ensure we have a product -->
        if (!productId || productId === 'NULL') {
            showErrorAlert("${uiLabelMap.CommonErrorMessage2}","${uiLabelMap.CommonPleaseSelectAllRequiredOptions}");
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
                    showErrorAlert("${uiLabelMap.CommonErrorMessage2}","${uiLabelMap.CommonPleaseSelectAllRequiredOptions}");
                    return;
                }
            }
        }
        <#--</#if>-->
        
        <#-- verify quantity -->
        <#assign qtyErrorLabel = getLabel('cart.quantity_not_positive_number', 'OrderErrorUiLabels')>
        if (verifyQty('quantity', '${qtyErrorLabel}') == null) {
            return;
        }
        
        <#-- verify amount (if applicable) -->
        if (checkAmtReq(productId) == 'Y') {
            <#assign amtErrorLabel = getLabel('AccountingFinAccountMustBePositive', 'AccountingErrorUiLabels')>
            if (verifyQty('add_amount', '${amtErrorLabel}') == null) {
                return;
            }
        }

        <#-- Cato: TODO?: This appears to be a sanity check to prevent trying to add a virtual product, which would be good,
             except it's not friendly to have this check in additem submit... isVirtual method is gone...
             server _should_ do this check anyway...
        if (isVirtual(addProductId)) {
            document.location = '<@ofbizUrl>product?category_id=${categoryId!}&amp;product_id=</@ofbizUrl>' + addProductId;
            return;
        }
        -->
        document.addform.submit();
    }        
    
    function verifyQty(fieldId, qtyErrorLabel) {
        var quantity = jQuery('#' + fieldId).val();
        if (!quantity) {
            showErrorAlert("${uiLabelMap.CommonErrorMessage2}", qtyErrorLabel);
            return null;
        }
        quantity = quantity.trim();
        jQuery('#' + fieldId).val(quantity);
        if (!quantity) {
            showErrorAlert("${uiLabelMap.CommonErrorMessage2}", qtyErrorLabel);
            return null;
        }
        if (!quantity.match(/^[0-9.,]+$/)) {
            showErrorAlert("${uiLabelMap.CommonErrorMessage2}", qtyErrorLabel);
            return null;
        }
        quantity = parseFloat(quantity);
        if (isNaN(quantity)) {
            showErrorAlert("${uiLabelMap.CommonErrorMessage2}", qtyErrorLabel);
            return null;
        }
        if (quantity <= 0) {
            showErrorAlert("${uiLabelMap.CommonErrorMessage2}", qtyErrorLabel);
            return null;
        }
        return quantity;
    }
    
    function toggleAmt(toggle) {
        if (toggle == 'Y') {
            jQuery("#add_amount_wrapper").show();
        }

        if (toggle == 'N') {
            jQuery("#add_amount_wrapper").hide();
        }
    }

</@script>


<@section>
    <@row>
        <@cell columns=8>
            <#--
            <#assign productAdditionalSmallImage1 = productContentWrapper.get("XTRA_IMG_1_SMALL","url")!?string?trim />
            <#assign productAdditionalSmallImage2 = productContentWrapper.get("XTRA_IMG_2_SMALL","url")!?string?trim />
            <#assign productAdditionalSmallImage3 = productContentWrapper.get("XTRA_IMG_3_SMALL","url")?string?trim />
            <#assign productAdditionalSmallImage4 = productContentWrapper.get("XTRA_IMG_4_SMALL","url")!?string?trim />-->

            <#assign productAdditionalImage1 = productContentWrapper.get("ADDITIONAL_IMAGE_1","url")!?string?trim />
            <#assign productAdditionalImage2 = productContentWrapper.get("ADDITIONAL_IMAGE_2","url")!?string?trim />
            <#assign productAdditionalImage3 = productContentWrapper.get("ADDITIONAL_IMAGE_3","url")?string?trim />
            <#assign productAdditionalImage4 = productContentWrapper.get("ADDITIONAL_IMAGE_4","url")!?string?trim />
            <#assign productLargeImageUrl = productContentWrapper.get("LARGE_IMAGE_URL","url")!?string?trim /> 
            <#assign productOriginalImage = productContentWrapper.get("ORIGINAL_IMAGE_URL","url")!?string?trim />
            
            <#if firstLargeImage?has_content>
                <#assign productLargeImageUrl = firstLargeImage />
            </#if>
            <#if productLargeImageUrl?string?has_content>
                <#assign largeImage><@ofbizContentUrl>${contentPathPrefix!}${productLargeImageUrl!}</@ofbizContentUrl></#assign>
            </#if>
            <@img src=largeImage!"https://placehold.it/800x300" width="100%" height="300px" type="contain" class="product-image"/>
            <#-- CATO: We are using the clearing mechanism in foundation here. This may be migrated to the grid macro at a later stage. -->
            <#if productAdditionalImage1?has_content>
                <ul class="clearing-thumbs" data-clearing>
                    <#if productAdditionalImage1?has_content>
                        <#assign largeImage><@ofbizContentUrl>${contentPathPrefix!}${productAdditionalImage1!}</@ofbizContentUrl></#assign>
                        <li><@img src=largeImage!"" link=largeImage!"" width="auto" height="80px" type="cover" class=""/></li>
                    </#if>
                    <#if productAdditionalImage2?has_content>
                        <#assign largeImage><@ofbizContentUrl>${contentPathPrefix!}${productAdditionalImage2!}</@ofbizContentUrl></#assign>
                       <li><@img src=largeImage!"" link=largeImage!"" width="auto" height="80px" type="cover"/></li>
                    </#if>
                    <#if productAdditionalImage3?has_content>
                        <#assign largeImage><@ofbizContentUrl>${contentPathPrefix!}${productAdditionalImage3!}</@ofbizContentUrl></#assign>
                        <li><@img src=largeImage!"" link=largeImage!"" width="auto" height="80px" type="cover"/></li>
                    </#if>
                    <#if productAdditionalImage4?has_content>
                        <#assign largeImage><@ofbizContentUrl>${contentPathPrefix!}${productAdditionalImage4!}</@ofbizContentUrl></#assign>
                        <li><@img src=largeImage!"" link=largeImage!"" width="auto" height="80px" type="cover"/></li>
                    </#if>
                </ul>

            </#if>
        </@cell>
        <@cell columns=4>
            <@panel>
            <div id="product-info"> 
                <#if productContentWrapper.get("DESCRIPTION","html")!?string?has_content><p>${productContentWrapper.get("DESCRIPTION","html")!}</p></#if>
                <#-- for prices:
                    - if price < competitivePrice, show competitive or "Compare At" price
                    - if price < listPrice, show list price
                    - if price < defaultPrice and defaultPrice < listPrice, show default
                    - if isSale show price with salePrice style and print "On Sale!"
                -->
                <#-- CATO: These are alternative prices that are not really commonly used 
                <#if price.competitivePrice?? && price.price?? && (price.price < price.competitivePrice)>
                    ${uiLabelMap.ProductCompareAtPrice}: <span class="product-price"><@ofbizCurrency amount=price.competitivePrice isoCode=price.currencyUsed /></span>
                </#if>
                
                Asset Usage price calculation
                <#if (product.productTypeId!) == "ASSET_USAGE">
                    <#if product.reserv2ndPPPerc?? && product.reserv2ndPPPerc != 0><br />${uiLabelMap.ProductReserv2ndPPPerc}<#if !product.reservNthPPPerc?? || product.reservNthPPPerc == 0>${uiLabelMap.CommonUntil} ${product.reservMaxPersons!}</#if> <@ofbizCurrency amount=product.reserv2ndPPPerc*price.price/100 isoCode=price.currencyUsed /></#if>
                    <#if product.reservNthPPPerc?? &&product.reservNthPPPerc != 0><br />${uiLabelMap.ProductReservNthPPPerc} <#if !product.reserv2ndPPPerc?? || product.reserv2ndPPPerc == 0>${uiLabelMap.ProductReservSecond} <#else> ${uiLabelMap.ProductReservThird} </#if> ${uiLabelMap.CommonUntil} ${product.reservMaxPersons!}, ${uiLabelMap.ProductEach}: <span class="product-price"><@ofbizCurrency amount=product.reservNthPPPerc*price.price/100 isoCode=price.currencyUsed /></span></#if>
                    <#if (!product.reserv2ndPPPerc?? || product.reserv2ndPPPerc == 0) && (!product.reservNthPPPerc?? || product.reservNthPPPerc == 0)><br />${uiLabelMap.ProductMaximum} ${product.reservMaxPersons!} ${uiLabelMap.ProductPersons}.</#if>
                </#if>
                
                <#if price.specialPromoPrice?has_content>
                    <#assign currentPrice = price.specialPromoPrice/>
                <#if>
                -->
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

                <#-- CATO: Uncomment to mark a product that is on sale
                <#if price.isSale?? && price.isSale>
                    <p>${uiLabelMap.OrderOnSale}!</p>
                </#if>-->
                
                <p>
                <#-- Only show the "old" price if the current price is lower (otherwise, bad advertisement) -->
                <#if oldPrice?has_content && currentPrice?has_content && (oldPrice?double > currentPrice?double)>
                    <span id="product-price_old"><del><@ofbizCurrency amount=oldPrice isoCode=price.currencyUsed /></del></span>
                </#if>
                 
                <#if currentPrice?has_content>
                    <span id="product-price"><strong><@ofbizCurrency amount=currentPrice isoCode=price.currencyUsed /></strong></span>
                </#if>
                    <@script>
                        var baseCurrentPrice = "${currentPrice}";
                    </@script>
                </p>
                
                <#-- CATO: Uncomment to display how much a user is saving by buying this product
                <#if price.listPrice?? && price.price?? && (price.price < price.listPrice)>
                    <span id="product-saved"><sup>
                        <#assign priceSaved = oldPrice - currentPrice />
                        <#assign percentSaved = (priceSaved / oldPrice) * 100 />
                        ${uiLabelMap.OrderSave}: <@ofbizCurrency amount=priceSaved isoCode=price.currencyUsed /> (${percentSaved?int}%)
                        <sup>
                    </span>
                </#if>
                -->

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
                    
                    <#-- Cato: TODO: We currently have no client-side check for incompatible (FEATURE_IACTN_INCOMP) and dependent features.
                        See org.ofbiz.product.product.ProductWorker.getVariantFromFeatureTree(String, List<String>, Delegator).
                        The check only happens server-side currently. -->
                    
                    <#-- Cato: TODO: There is another case not currently handled properly: ALTERNATIVE_PACKAGE - see CDR-1111-BX2 -->
                    
                    <#-- Cato: This field should generally not be present! It is only for product that support/require an amount
                        in ADDITION to the quantity field. You also cannot know this for sure beforehand because for virtuals it is set on the
                        specific variant, so it has to be updated through JS lookups (the initial is only for the virtual product, but each variant can be different).  -->
                    <#macro amountField>
                        <#if (product.requireAmount!"N") != "Y">
                            <#assign hiddenStyle = styles.hidden!/>
                            <#-- Cato: FIXME: jquery hack to hide the field -->
                            <@script>
                                jQuery(document).ready(function() {
                                    jQuery("#add_amount_wrapper").hide();
                                });
                            </@script>
                        </#if>
                        <@field type="input" size="5" name="add_amount" id="add_amount" value="" label=uiLabelMap.CommonAmount /> <#-- containerClass=("+"+hiddenStyle) -->
                    </#macro>
                    
                    <#-- Variant Selection -->
                    <#if (product.isVirtual!?upper_case) == "Y">
                        <#if (product.virtualVariantMethodEnum!) == "VV_FEATURETREE" && featureLists?has_content>
                            <@script>
                                featureCount = ${featureLists?size};
                                featureIdList = [<#list featureLists as featureList>"${featureList.productFeatureTypeId?js_string}"<#if featureList_has_next>,</#if></#list>];
                            </@script>
                            <input type="hidden" name="add_product_id" id="add_product_id" value="${product.productId}" />
                            <#list featureLists as featureList>
                                <@field type="select" id="FT_${featureList.productFeatureTypeId}" name="FT${featureList.productFeatureTypeId}" label=featureList.description!"">
                                    <option value="">${uiLabelMap.EcommerceSelectOption}</option>
                                    <#list featureList.features as feature>
                                        <option value="${feature.productFeatureId}">${feature.description} <#if feature.price??>(+ <@ofbizCurrency amount=feature.price?string isoCode=feature.currencyUomId />)</#if></option>
                                    </#list>
                                </@field>
                            </#list>
                            <@field type="text" name="quantity" id="quantity" value="1" size="4" maxLength="4" label=uiLabelMap.CommonQuantity/>
                            <@amountField />
                        </#if>
                        
                        <#-- CATO: It is possible to have a limited amount of variant combination. 
                                   Therefore the available options are only displayed for the first variant and updated for the next based on the selected type. -->
                        <#if !product.virtualVariantMethodEnum?? || product.virtualVariantMethodEnum == "VV_VARIANTTREE">
                            <#if variantTree?? && (variantTree.size() > 0)>
                                <#list featureSet as currentType>
                                    <#if currentType_index == 0>
                                        <@field type="select" id="FT_${currentType}" name="FT${currentType}" label=featureTypes.get(currentType)!"" onChange="javascript:updateVariants(this.name,this.value,${currentType_index});">
                                            <option value="">${uiLabelMap.EcommerceSelectOption}</option>
                                            <#list variantTree.keySet() as variant>
                                                <option value="${variant}">${variant}</option>
                                            </#list>
                                        </@field>
                                    <#else>
                                        <@field type="select" id="FT_${currentType}" name="FT${currentType}" label=featureTypes.get(currentType)!"" onChange="javascript:updateVariants(this.name,this.value,${currentType_index});" disabled=true />
                                    </#if>
                                </#list>
                                <input type="hidden" name="add_product_id" id="add_product_id" value="NULL"/>
                                <@script>
                                    featureCount = ${featureSet?size};
                                    featureIdList = <@objectAsScript lang="js" object=featureSet />;
                                </@script>
                            <#else>
                                <input type="hidden" name="add_product_id" id="add_product_id" value="NULL"/>
                                <#assign inStock = false />
                            </#if>
                        </#if>
                    <#else>
                        <#-- Cato: This is a sanity check, leave here for debugging, will do no harm -->
                        <#if selFeatureTypes?has_content>
                          <p>
                            <strong>WARN: </strong> Product has selectable features 
                              [<#list mapKeys(selFeatureTypes) as typeId>${selFeatureTypes[typeId]!typeId!}<#if typeId_has_next>, </#if></#list>]
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
                    <#elseif (product.virtualVariantMethodEnum!) != "VV_FEATURETREE"> <#-- Cato: NOTE: All stuff for VV_FEATURETREE should already be included above -->
                        <#if inStock>
                            <#if (product.productTypeId!) == "ASSET_USAGE">
                                <#-- Cato: TODO: format this -->
                                <div>
                                    <@field type="input" size="10" name="reservStart" label="Start Date(yyyy-mm-dd)"/>
                                    <a href="javascript:call_cal_notime(document.addform.reservStart, '${nowTimestamp.toString().substring(0,10)}');"><img src="<@ofbizContentUrl>/images/cal.gif</@ofbizContentUrl>" width="16" height="16" alt="Calendar" alt="" /></a>
                                    <@field type="input" size="10" name="reservEnd" label="End Date(yyyy-mm-dd)"/>
                                    <a href="javascript:call_cal_notime(document.addform.reservEnd, '${nowTimestamp.toString().substring(0,10)}');"><img src="<@ofbizContentUrl>/images/cal.gif</@ofbizContentUrl>" width="16" height="16" alt="Calendar" alt="" /></a>
                                </div>
                                <div>
                                   <@field type="input" size="4" name="reservLength" value="" label="Number of days" />
                                   <@field type="input" size="4" name="reservPersons" value="2" label="Number of persons"/>
                                   <@field type="input" size="5" name="quantity" id="quantity" value="1" label="Number of rooms"/>
                                </div>
                            <#else> 
                                <@field name="quantity" id="quantity" label=uiLabelMap.CommonQuantity value="1" size="4" maxLength="4" type="input" /> <#-- there's no need to handle this: disabled=((product.isVirtual!?upper_case) == "Y") -->
                            </#if>
                            <@amountField />
                        <#else>
                            <#if productStore??>
                                <#if productStore.requireInventory?? && productStore.requireInventory == "N">
                                    <@field name="quantity" id="quantity" label=uiLabelMap.CommonQuantity value="1" size="4" maxLength="4" type="input" /> <#-- there's no need to handle this: disabled=((product.isVirtual!?upper_case) == "Y") -->
                                    <@amountField />
                                <#else>
                                    <input name="quantity" id="quantity" value="1" type="hidden"/>
                                    <span>${uiLabelMap.ProductItemOutOfStock}<#if product.inventoryMessage??>&mdash; ${product.inventoryMessage}</#if></span>
                                </#if>
                            </#if>
                        </#if>
                    </#if>
                    <@field type="submit" submitType="link" id="addToCart" name="addToCart" href="javascript:addItem();" text=uiLabelMap.OrderAddToCart class="+${styles.grid_columns_12}"/>                  

            </form>
            <#-- CATO: Review 
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
                        <div>minimum order quantity for ${productContentWrapper.get("PRODUCT_NAME","html")!} is ${minimumQuantity!}</div>
                    </#if>
            -->
        </div>
        </@panel>
        <#-- CATO: Shopping list functionality - disabled for now
        <div id="product-shopping-list">
            <#if sessionAttributes.userLogin?has_content && sessionAttributes.userLogin.userLoginId != "anonymous">
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
                        <#if (product.productTypeId!) == "ASSET_USAGE">
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
         -->    <div class="shariff" data-lang="${locale[0..1]!"en"}"></div>

        </@cell>
    </@row>
</@section>        

<#-- CATO: show tell a friend details only in shop application     
<div id="product-tell-a-friend">
    <a href="javascript:popUpSmall('<@ofbizUrl>tellafriend?productId=${product.productId}</@ofbizUrl>','tellafriend');" >${uiLabelMap.CommonTellAFriend}</a>
</div>
-->   
     
<@section>
    <#assign prodLongDescr=productContentWrapper.get("LONG_DESCRIPTION","html")!?string?trim/>
    <#if !prodLongDescr?has_content>
      <#assign prodLongDescr=productContentWrapper.get("DESCRIPTION","html")!?string?trim/>
    </#if>
    <#assign prodWarnings=productContentWrapper.get("WARNINGS","html")!?string?trim/>

    <ul class="tabs" data-tab>
      <li class="tab-title active"><a href="#panel11"><i class="${styles.icon!} ${styles.icon_prefix}pencil"></i> ${uiLabelMap.CommonDescription}</a></li>
      <li class="tab-title"><a href="#panel21"><i class="${styles.icon!} ${styles.icon_prefix}wrench"></i> ${uiLabelMap.CommonInformation}</a></li>
    </ul>
    <div class="tabs-content">
         <div class="content active" id="panel11">
            <#-- Long description of product -->
            <p>${prodLongDescr!""}</p>
            <#if warnings?has_content><@alert type="warning">${prodWarnings!""}</@alert></#if>

            <#-- Digital Download Files Associated with this Product -->
                <#if downloadProductContentAndInfoList?has_content>            
                    <p>${uiLabelMap.OrderDownloadFilesTitle}:</p>
                    <#list downloadProductContentAndInfoList as downloadProductContentAndInfo>
                        <p>${downloadProductContentAndInfo.contentName!}<#if downloadProductContentAndInfo.description?has_content> - ${downloadProductContentAndInfo.description}</#if></p>
                    </#list>
                </#if>

        </div>
            
        <div class="content" id="panel21">
            <#-- Included quantities/pieces -->
            <#if product.piecesIncluded?? && product.piecesIncluded?long != 0>
                <p id="product-specs-pieces-included">
                    ${uiLabelMap.OrderPieces}: ${product.piecesIncluded}
                </p>
            </#if>
            <#if (product.quantityIncluded?? && product.quantityIncluded != 0) || product.quantityUomId?has_content>
                <#assign quantityUom = product.getRelatedOneCache("QuantityUom")! />
                <p id="product-specs-quantity-included">
                    ${uiLabelMap.CommonQuantity}: ${product.quantityIncluded!} ${((quantityUom.abbreviation)?default(product.quantityUomId))!}
                </p>
            </#if>
            <#if (product.weight?? && product.weight != 0) || product.weightUomId?has_content>
                <#assign weightUom = product.getRelatedOneCache("WeightUom")! />
                <p id="product-specs-weight">
                    ${uiLabelMap.CommonWeight}: ${product.weight!} ${((weightUom.abbreviation)?default(product.weightUomId))!}
                </p>
            </#if>
            <#if (product.productHeight?? && product.productHeight != 0) || product.heightUomId?has_content>
                <#assign heightUom = product.getRelatedOneCache("HeightUom")! />
                <p id="product-specs-height">
                    ${uiLabelMap.CommonHeight}: ${product.productHeight!} ${((heightUom.abbreviation)?default(product.heightUomId))!}
                </p>
            </#if>
            <#if (product.productWidth?? && product.productWidth != 0) || product.widthUomId?has_content>
                <#assign widthUom = product.getRelatedOneCache("WidthUom")! />
                <p id="product-specs-width">
                    ${uiLabelMap.CommonWidth}: ${product.productWidth!} ${((widthUom.abbreviation)?default(product.widthUomId))!}
                </p>
            </#if>
            <#if (product.productDepth?? && product.productDepth != 0) || product.depthUomId?has_content>
                <#assign depthUom = product.getRelatedOneCache("DepthUom")! />
                <p id="product-specs-depth">
                    ${uiLabelMap.CommonDepth}: ${product.productDepth!} ${((depthUom.abbreviation)?default(product.depthUomId))!}
                </p>
            </#if>

            <#if daysToShip??>
                <p id="product-specs-days-to-ship">${uiLabelMap.ProductUsuallyShipsIn} ${daysToShip} ${uiLabelMap.CommonDays}!</p>
            </#if>

            <#if disFeatureList?? && (0 < disFeatureList.size())>                
                <#list disFeatureList as currentFeature>
                    <#assign disFeatureType = currentFeature.getRelatedOneCache("ProductFeatureType") />
                    <p>
                        <#if disFeatureType.description??>${disFeatureType.get("description", locale)}<#else>${currentFeature.productFeatureTypeId}</#if>:&nbsp;${currentFeature.description}
                    </p>
                </#list>
            </#if>

            <#-- Cato: Debugging info
            <@heading relLevel=+1>Debugging Info</@heading>
            <p style="font-size:0.7em;">Product ID: ${product.productId}</p>
            <p style="font-size:0.7em;">Product info map: ${product?string}</p>
            -->
        </div>
    </div>
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
                           <a href="javascript:getList('FT${featureOrderFirst}','${indexer}',1);"><img src="<@ofbizContentUrl>${contentPathPrefix!}${imageUrl}</@ofbizContentUrl>" width="60" height="60" alt="" /></a>                        
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
         <#-- special cross/up-sell area using commonFeatureResultIds (from common feature product search) -->

        <#if comsmonFeatureResultIds?has_content>            
            <@section title=uiLabelMap.ProductSimilarProducts>
                <#list commonFeatureResultIds as commonFeatureResultId>
                    <#assign dummy = setRequestAttribute("optProductId", commonFeatureResultId)>
                    <#assign dummy = setRequestAttribute("listIndex", commonFeatureResultId_index)>
                    <#assign dummy = setRequestAttribute("formNamePrefix", "cfeatcssl")>                    
                    <#-- <#assign dummy = setRequestAttribute("targetRequestName", targetRequestName)> -->
                    <@render resource="component://shop/widget/CatalogScreens.xml#miniproductsummary" />
                </#list>
            </@section>
        </#if>

        <#-- Upgrades/Up-Sell/Cross-Sell -->
        <#assign productValue = product />
        <#assign listIndex = 1 />
        <#assign dummy = setRequestAttribute("productValue", productValue)>

        <#-- also bought -->
        <@associated assocProducts=alsoBoughtProducts beforeName="" showName="N" afterName="${uiLabelMap.ProductAlsoBought}" formNamePrefix="albt" targetRequestName="" />
        <#-- obsolete -->
        <@associated assocProducts=obsoleteProducts beforeName="" showName="Y" afterName=" ${uiLabelMap.ProductObsolete}" formNamePrefix="obs" targetRequestName="" />
        <#-- cross sell -->
        <@associated assocProducts=crossSellProducts beforeName="" showName="N" afterName="${uiLabelMap.ProductCrossSell}" formNamePrefix="cssl" targetRequestName="crosssell" />
        <#-- up sell -->
        <@associated assocProducts=upSellProducts beforeName="${uiLabelMap.ProductUpSell} " showName="Y" afterName=":" formNamePrefix="upsl" targetRequestName="upsell" />
        <#-- obsolescence -->
        <@associated assocProducts=obsolenscenseProducts beforeName="" showName="Y" afterName=" ${uiLabelMap.ProductObsolescense}" formNamePrefix="obce" targetRequestName="" />
</@section>

<#-- CATO: uncomment to use unavailableVariants
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
