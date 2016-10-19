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
<#include "catalogcommon.ftl">

<#-- SCIPIO: TODO: reorganize the code sharing with productdetail, very similar -->
<#-- SCIPIO: TODO: does not currently support virtual config products (none in demo data) (only supports virtual options) -->
<#-- SCIPIO: TODO: Rewrite virtual product support/JS and add fields-->
<#-- SCIPIO: NOTE: This template is tied into inlineProductDetail.ftl -->

<#-- variable setup -->
<#assign productContentWrapper = productContentWrapper!>
<#assign price = priceMap!>
<#-- end variable setup -->

<#-- virtual product javascript -->
<#-- SCIPIO: TODO: rewrite virtual JS
    NOTE: there is no virtual config product in demo data, but options can be virtual and 
    do they interact with this?? -->
${virtualJavaScript!}
<@script>
     function setAddProductId(name) {
        document.addform.add_product_id.value = name;
        if (document.addform.quantity == null) return;
        if (name == 'NULL' || isVirtual(name) == true) {
            document.addform.quantity.disabled = true;
        } else {
            document.addform.quantity.disabled = false;
        }
     }
     function isVirtual(product) {
        var isVirtual = false;
        <#-- SCIPIO: TODO: Support for virtual products 
            Leave here until has replacement -->
        <#if virtualJavaScript??>
        for (i = 0; i < VIR.length; i++) {
            if (VIR[i] == product) {
                isVirtual = true;
            }
        }
        </#if>
        return isVirtual;
     }

    function addItem() {
        document.configform.action = document.addform.action;
        document.configform.quantity.value = document.addform.quantity.value;
        document.configform.submit();
    }
    function verifyConfig() {
        document.configform.submit();
    }

    function toggleAmt(toggle) {
        if (toggle == 'Y') {
            jQuery("#add_amount_container").show();
        }

        if (toggle == 'N') {
            jQuery("#add_amount_container").hide();
        }
    }

    function findIndex(name) {
        for (i = 0; i < OPT.length; i++) {
            if (OPT[i] == name) {
                return i;
            }
        }
        return -1;
    }

    function getList(name, index, src) {
        currentFeatureIndex = findIndex(name);

        if (currentFeatureIndex == 0) {
            // set the images for the first selection
            if (IMG[index] != null) {
                if (document.images['mainImage'] != null) {
                    document.images['mainImage'].src = IMG[index];
                    detailImageUrl = DET[index];
                }
            }

            // set the drop down index for swatch selection
            document.forms["addform"].elements[name].selectedIndex = (index*1)+1;
        }

        if (currentFeatureIndex < (OPT.length-1)) {
            // eval the next list if there are more
            var selectedValue = document.forms["addform"].elements[name].options[(index*1)+1].value;
            eval("list" + OPT[(currentFeatureIndex+1)] + selectedValue + "()");

            // set the product ID to NULL to trigger the alerts
            setAddProductId('NULL');
        } else {
            // this is the final selection -- locate the selected index of the last selection
            var indexSelected = document.forms["addform"].elements[name].selectedIndex;

            // using the selected index locate the sku
            var sku = document.forms["addform"].elements[name].options[indexSelected].value;

            // set the product ID
            setAddProductId(sku);

            // check for amount box
            toggleAmt(checkAmtReq(sku));
        }
    }

    jQuery(document).ready(function () {
        jQuery('#configFormId').change(getConfigDetails)
    });
    
    function getConfigDetails(event) {
        jQuery.ajax({
            url: '<@ofbizUrl>getConfigDetailsEvent</@ofbizUrl>',
            type: 'POST',
            data: jQuery('#configFormId').serialize(),
            success: function(data) {
                var totalPrice = data.totalPrice;
                var configId = data.configId;
                document.getElementById('totalPrice').innerHTML = totalPrice;
                <#-- SCIPIO: not yet support
                document.addToShoppingList.configId.value = configId;
                -->
                <#-- SCIPIO: this is invalid
                event.stop();-->
                <#-- SCIPIO: put it here too -->
                baseCurrentPrice = totalPrice;
                event.preventDefault();
            },
            error: function(data) {
                <#-- FIXME: better message -->
                alert("${uiLabelMap.CommonError?js_string}");
                <#-- SCIPIO: prevent connection fail causing weirdness -->
                event.preventDefault();
            }
        });
    }

</@script>

<div id="productdetail">

<#macro productConfigurator>
  <#-- Product Configurator -->
  <#-- SCIPIO: FIXME: view switching bad -->
  <div class="product-configurator">
  <form name="configform" id="configFormId" method="post" action="<@ofbizUrl>product<#if requestAttributes._CURRENT_VIEW_??>/${requestAttributes._CURRENT_VIEW_}</#if></@ofbizUrl>">
    <input type="hidden" name="add_product_id" value="${product.productId}" />
    <input type="hidden" name="add_category_id" value="" />
    <input type="hidden" name="quantity" value="1" />

    <input type="hidden" name="product_id" value="${product.productId}" />

      <#-- SCIPIO: TODO: Preselection using parameters map -->

      <#assign counter = 0>
      <#assign questions = configwrapper.questions>
      <#list questions as question>
        <@section containerClass="+product-config-question">
        <@row>
          <@cell small=4>
              <div><label>${question.question!""}</label></div>
              <#if question.isFirst()>
                <a name="#${question.getConfigItem().getString("configItemId")}"></a>
                <div>${question.description!}</div>
                <#assign instructions = htmlContentString(question.content.get("INSTRUCTIONS")!)>
                <#if instructions?has_content>
                  <#-- SCIPIO: don't understand why this is always "error" message in stock ofbiz. just use a modal and leave out title to keep generic...
                  <a href="javascript:showErrorAlert('${uiLabelMap.CommonErrorMessage2}','${instructions}');" class="${styles.link_run_local_inline!} ${styles.action_view!}">Instructions</a> -->
                  <@modal label=uiLabelMap.OrderInstructions><p>${instructions}</p></@modal>
                </#if>
                <#assign image = question.content.get("IMAGE_URL", "url")!>
                <#if image?has_content>
                  <img src="<@ofbizContentUrl ctxPrefix=true>${image}</@ofbizContentUrl>" vspace="5" hspace="5" class="cssImgXLarge" align="left" alt="" />
                </#if>
              <#else>
                <#-- SCIPIO: FIXME?: this does nothing in ecommerce
                <div><a href="#${question.getConfigItem().getString("configItemId")}" class="${styles.link_nav_inline!} ${styles.action_view!}">${uiLabelMap.CommonDetails}</a></div>
                -->
              </#if>
          </@cell>
          <@cell small=8>
            <@fields type="default-manual-widgetonly" checkboxType="simple-standard">
            <#if question.isStandard()>
              <#-- Standard item: all the options are always included -->
              <#assign options = question.options>
              <#list options as option>
                <div><label>${option.description!}<#if !option.isAvailable()> (*)</#if></label></div>
              </#list>
            <#else>
              <#if question.isSingleChoice()>
                <#-- Single choice question -->
                <#assign options = question.options>
                <#assign selectedOption = question.getSelected()!>
                <#assign selectedPrice = 0.0>
                <#if selectedOption?has_content>
                  <#assign selectedPrice = selectedOption.getPrice()>
                </#if>
                <#-- The single choice input can be implemented with radio buttons or a select field -->
                <#if "Y" == (renderSingleChoiceWithRadioButtons!"")>
                    <#-- This is the radio button implementation -->
                    <#if !question.isMandatory()>
                      <div><@field type="radio" name=counter?string value="checked" checked=(!question.isSelected()) label="No option" />
                    </#if>
                    <#assign optionCounter = 0>
                    <#list options as option>
                      <#assign componentCounter = 0>
                      <#if showOffsetPrice?? && "Y" == showOffsetPrice>
                        <#assign shownPrice = option.price - selectedPrice>
                      <#else>
                        <#assign shownPrice = option.price>
                      </#if>
                        <#-- Render virtual compoennts -->
                        <#if option.hasVirtualComponent()>
                          <div>
                          <#assign inlineCounter = counter+ "_" +optionCounter + "_"+componentCounter>
                            <#assign fieldLabel>${option.description}<#if !option.isAvailable()> (*)</#if> <span id="variant_price_display${inlineCounter}"> </span></#assign>
                            <@field type="radio" name=counter?string id="${counter}_${optionCounter}" value=optionCounter?string
                                onClick="javascript:checkOptionVariants('${counter}_${optionCounter}');" label=wrapAsRaw(fieldLabel, 'htmlmarkup') />
                            
                          <@fields type="default">
                            <#assign components = option.getComponents()>
                            <#list components as component>
                              <#if (option.isVirtualComponent(component))>
                                <@render resource=inlineProductDetailScreen reqAttribs={
                                    "inlineProductId":component.productId, 
                                    "inlineCounter":inlineCounter, 
                                    "addJavaScript":componentCounter,
                                    "ipdIncludePriceDisplay":false,
                                    "ipdPlusMinusPriceDisplay":true } clearValues=true />
                                <#assign componentCounter = componentCounter + 1>
                              </#if>
                            </#list>
                          </@fields>
                          </div>
                        <#else>
                          <div>
                            <#assign fieldLabel>
                              ${option.description!}&nbsp;<#rt/>
                              <#t/><#if (shownPrice > 0)>+<@ofbizCurrency amount=shownPrice isoCode=price.currencyUsed/>&nbsp;</#if>
                              <#t/><#if (shownPrice < 0)>-<@ofbizCurrency amount=(-1*shownPrice) isoCode=price.currencyUsed/>&nbsp;</#if>
                              <#if !option.isAvailable()> (*)</#if>
                            </#assign>
                            <@field type="radio" name=counter?string value=optionCounter?string checked=(option.isSelected() || (!question.isSelected() && optionCounter == 0 && question.isMandatory()))
                                label=wrapAsRaw(fieldLabel, 'htmlmarkup') />
                          </div>
                        </#if>
                      <#assign optionCounter = optionCounter + 1>
                    </#list>
                <#else>
                      <#-- And this is the select box implementation -->
                      <@field type="select" name=counter?string>
                        <#if !question.isMandatory()>
                          <option value=""></option>
                        </#if>
                        <#assign options = question.options>
                        <#assign optionCounter = 0>
                        <#list options as option>
                          <#if "Y" == (showOffsetPrice!)>
                            <#assign shownPrice = option.price - selectedPrice>
                          <#else>
                            <#assign shownPrice = option.price>
                          </#if>
                          <#if option.isSelected()>
                            <#assign optionCounter = optionCounter + 1>
                          </#if>
                          <option value="${optionCounter}"<#if option.isSelected()> selected="selected"</#if>>
                            ${option.description}&nbsp;
                            <#if (shownPrice > 0)>+<@ofbizCurrency amount=shownPrice isoCode=price.currencyUsed/>&nbsp;</#if>
                            <#if (shownPrice < 0)>-<@ofbizCurrency amount=(-1*shownPrice) isoCode=price.currencyUsed/>&nbsp;</#if>
                            <#if !option.isAvailable()> (*)</#if>
                          </option>
                          <#assign optionCounter = optionCounter + 1>
                        </#list>
                      </@field>
                </#if>
              <#else>
                <#-- Multi choice question -->
                <#assign options = question.options>
                <#assign optionCounter = 0>
                <#list options as option>
                    <#assign componentCounter = 0>
                    <#-- Render virtual compoennts -->
                    <#if option.hasVirtualComponent()>
                      <div>
                      <#assign inlineCounter = counter+ "_" +optionCounter + "_"+componentCounter>
                        <#assign fieldLabel>${option.description}<#if !option.isAvailable()> (*)</#if> <span id="variant_price_display${inlineCounter}"> </span></#assign>
                        <@field type="checkbox" name=counter?string id="${counter}_${optionCounter}" value=optionCounter?string onClick="javascript:checkOptionVariants('${counter}_${optionCounter}');" 
                            label=wrapAsRaw(fieldLabel, 'htmlmarkup') />

                     <@fields type="default">
                        <#assign components = option.getComponents()>
                        <#list components as component>
                          <#if (option.isVirtualComponent(component))>
                            <@render resource=inlineProductDetailScreen reqAttribs={
                                "inlineProductId":component.productId, 
                                "inlineCounter":inlineCounter, 
                                "addJavaScript":componentCounter,
                                "ipdIncludePriceDisplay":false,
                                "ipdPlusMinusPriceDisplay":true } clearValues=true />
                            <#assign componentCounter = componentCounter + 1>
                          </#if>
                        </#list>
                      </@fields>
                      </div>
                    <#else>
                    <div>
                      <#assign fieldLabel>${option.description!}&nbsp;+<@ofbizCurrency amount=option.price isoCode=price.currencyUsed/><#if !option.isAvailable()> (*)</#if></#assign>
                      <@field type="checkbox" name=counter?string value=optionCounter?string checked=option.isSelected() label=wrapAsRaw(fieldLabel, 'htmlmarkup') />
                    </div>
                    </#if>
                  <#assign optionCounter = optionCounter + 1>
                </#list>
              </#if>
            </#if>
            </@fields>
          </@cell>
        </@row>
        </@section>
        <#assign counter = counter + 1>
      </#list>

    <#-- SCIPIO: I don't think need this button... probably on a shop you want this to check automatically or on additem
      <@menu type="button">
        <@menuitem type="link" href="javascript:verifyConfig();" class="+${styles.action_run_sys!} ${styles.action_verify!}" text=uiLabelMap.OrderVerifyConfiguration />
      </@menu>
    -->

  </form>
  </div>
</#macro>


<#-- MAIN PAGE -->

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
              <#if hasDesc></p></#if>

              <#-- example of showing a certain type of feature with the product -->
              <#-- Scipio: not now
              <#if sizeProductFeatureAndAppls?has_content>
                <div>
                  <#if (sizeProductFeatureAndAppls?size == 1)>
                    <#- TODO : i18n ->
                    ${uiLabelMap.CommonSize}:
                  <#else>
                    Sizes Available:
                  </#if>
                  <#list sizeProductFeatureAndAppls as sizeProductFeatureAndAppl>
                    ${sizeProductFeatureAndAppl.description!(sizeProductFeatureAndAppl.abbrev!(sizeProductFeatureAndAppl.productFeatureId))}<#if sizeProductFeatureAndAppl_has_next>,</#if>
                  </#list>
                </div>
              </#if>
              -->
        
              <#-- for prices:
                      - if totalPrice is present, use it (totalPrice is the price calculated from the parts)
                      - if price < competitivePrice, show competitive or "Compare At" price
                      - if price < listPrice, show list price
                      - if price < defaultPrice and defaultPrice < listPrice, show default
                      - if isSale show price with salePrice style and print "On Sale!"
              -->
              <#-- SCIPIO: make sure to use totalPrice first for config products, ignore defaultPrice -->
                
                <#if price.listPrice?has_content>
                    <#assign oldPrice = price.listPrice/>
                <#elseif !totalPrice?has_content && price.defaultPrice?has_content>
                    <#assign oldPrice = price.defaultPrice/>
                </#if>

                <#if totalPrice?has_content>
                    <#assign currentPrice = totalPrice>
                <#elseif price.price?has_content>
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
                 
                <#if currentPrice?has_content>
                    <span id="product-price"><strong><span id="totalPrice"><@ofbizCurrency amount=currentPrice isoCode=price.currencyUsed /></span></strong></span>
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

                <#-- show price details ("showPriceDetails" field can be set in the screen definition) -->
                <#if (showPriceDetails?? && (showPriceDetails!"N") == "Y")>
                    <#if price.orderItemPriceInfos??>
                        <#list price.orderItemPriceInfos as orderItemPriceInfo>
                            <p>${orderItemPriceInfo.description!}</p>
                        </#list>
                    </#if>
                </#if>

              <#-- show tell a friend details only in shop application -->
              <#-- SCIPIO: not yet supported
              <div>&nbsp;</div>
              <div>
                <a href="javascript:popUpSmall('<@ofbizUrl>tellafriend?productId=${product.productId}</@ofbizUrl>','tellafriend');" class="${styles.link_nav!} ${styles.action_send!}">${uiLabelMap.CommonTellAFriend}</a>
              </div>
              -->
            </div>
            
            <div id="product-add-cart">
            <#macro amountField>
                <#local fieldStyle = "">
                <#if (product.requireAmount!"N") != "Y">
                    <#-- Scipio: Issues with css
                    <#assign hiddenStyle = styles.hidden!/>-->
                    <#local fieldStyle = "display: none;">
                </#if>
                <@field type="input" size="5" name="add_amount" id="add_amount" containerStyle=fieldStyle value="" label=uiLabelMap.CommonAmount /> <#-- containerClass=("+"+hiddenStyle) -->
            </#macro>

              <#-- SCIPIO: don't need ugly view-switch: <#if requestAttributes._CURRENT_VIEW_??>/${requestAttributes._CURRENT_VIEW_}</#if> -->
              <form method="post" action="<@ofbizUrl>additem</@ofbizUrl>" name="addform">
                <#assign inStock = true>
                <#-- Variant Selection -->
                <#if (product.isVirtual!?upper_case) == "Y">
                  <#-- Scipio: TODO: support for virtual products -->
                  <#if variantTree?? && (0 < variantTree.size())>
                    <#list featureSet as currentType>
                      <@field type="select" name="FT${currentType}" onChange="javascript:getList(this.name, (this.selectedIndex-1), 1);">
                        <option>${featureTypes.get(currentType)}</option>
                      </@field>
                    </#list>
                    <input type="hidden" name="product_id" value="${product.productId}" />
                    <input type="hidden" name="add_product_id" value="NULL" />
                  <#else>
                    <input type="hidden" name="product_id" value="${product.productId}" />
                    <input type="hidden" name="add_product_id" value="NULL" />
                    <div>${uiLabelMap.ProductItemOutOfStock}.</div>
                    <#assign inStock = false>
                  </#if>
                <#else>
                  <input type="hidden" name="product_id" value="${product.productId}" />
                  <input type="hidden" name="add_product_id" value="${product.productId}" />
                  <#if productNotAvailable??>
                    <#assign isStoreInventoryRequired = Static["org.ofbiz.product.store.ProductStoreWorker"].isStoreInventoryRequired(request, product)>
                    <#if isStoreInventoryRequired>
                      <div>${uiLabelMap.ProductItemOutOfStock}<#if product.inventoryMessage?has_content>&mdash; ${product.inventoryMessage}</#if></div>
                      <#assign inStock = false>
                    <#else>
                      <#if product.inventoryMessage?has_content><div>${product.inventoryMessage}</div></#if>
                    </#if>
                  </#if>
                </#if>

        
                <#-- check to see if introductionDate hasn't passed yet -->
                <#if product.introductionDate?? && nowTimestamp.before(product.introductionDate)>
                  <@alert type="info">${uiLabelMap.ProductProductNotYetMadeAvailable}.</@alert>
                <#-- check to see if salesDiscontinuationDate has passed -->
                <#elseif product.salesDiscontinuationDate?? && nowTimestamp.after(product.salesDiscontinuationDate)>
                  <@alert type="info">${uiLabelMap.ProductProductNoLongerAvailable}.</@alert>
                <#-- check to see if the product requires inventory check and has inventory -->
                <#else>
                  <#if inStock>
                    <@amountField />
                    
                    <#if !configwrapper.isCompleted()>
                      <@alert type="info">${uiLabelMap.EcommerceProductNotConfigured}</@alert>
                      <@field type="text" disabled=true name="quantity" id="quantity" value="0" size="5" label=uiLabelMap.CommonQuantity/>
                    <#else>
                      <#assign qtyDefault = 1>
                      <#if minimumQuantity?? && (minimumQuantity > 0)>
                        <#assign qtyDefault = minimumQuantity>
                      </#if>
                      <@field type="text" name="quantity" id="quantity" value=(parameters.quantity!qtyDefault) size="5" label=uiLabelMap.CommonQuantity/>
                      <#if minimumQuantity?? && (minimumQuantity > 0)>
                        <div>${uiLabelMap.ProductMinimumOrderQuantity}: ${minimumQuantity}</div>
                      </#if>

                      <@field type="submit" submitType="link" id="addToCart" name="addToCart" href="javascript:addItem();" text=uiLabelMap.OrderAddToCart class="+${styles.grid_columns_12} ${styles.link_run_session!} ${styles.action_add!}"/>
                    </#if>
                  </#if>
                  <#-- SCIPIO: FIXME?: I think this parameter does more harm than good, but leave until sure can remove -->
                  <#if requestParameters.category_id??>
                    <input type="hidden" name="category_id" value="${requestParameters.category_id}" />
                  </#if>
                </#if>
              </form>
              
              <#-- SCIPIO: Shopping list not supported yet
              <div>
                  <#if userHasAccount>
                    <form name="addToShoppingList" method="post" action="<@ofbizUrl>addItemToShoppingList<#if requestAttributes._CURRENT_VIEW_??>/${requestAttributes._CURRENT_VIEW_}</#if></@ofbizUrl>">
                      <input type="hidden" name="productId" value="${product.productId}" />
                      <input type="hidden" name="product_id" value="${product.productId}" />
                      <input type="hidden" name="configId" value="${configId!}" />
                      <select name="shoppingListId">
                        <#if shoppingLists?has_content>
                          <#list shoppingLists as shoppingList>
                            <option value="${shoppingList.shoppingListId}">${shoppingList.listName}</option>
                          </#list>
                        </#if>
                        <option value="">---</option>
                        <option value="">${uiLabelMap.OrderNewShoppingList}</option>
                      </select>
                      &nbsp;&nbsp;
                      <input type="text" size="5" name="quantity" value="1" />
                      <a href="javascript:document.addToShoppingList.submit();" class="${styles.link_run_sys!} ${styles.action_add!}">[${uiLabelMap.OrderAddToShoppingList}]</a>
                    </form>
                  <#else>
                    <#- SCIPIO: why ever show this?
                    <@commonMsg type="info">${uiLabelMap.OrderYouMust} <a href="<@ofbizUrl>checkLogin/showcart</@ofbizUrl>" class="${styles.link_nav_inline!} ${styles.action_login!}">${uiLabelMap.CommonBeLogged}</a>
                    ${uiLabelMap.OrderToAddSelectedItemsToShoppingList}.</@commonMsg>->
                  </#if>
              </div>
              -->
            </div>
          </@panel>
              
              
              <#-- Prefill first select box (virtual products only) -->
              <#-- SCIPIO: TODO: virtual products 
              <#if variantTree?? && 0 < variantTree.size()>
                <@script>eval("list" + "${featureOrderFirst}" + "()");</@script>
              </#if>
              -->
        
              <#-- Swatches (virtual products only) -->
              <#-- SCIPIO: no swatches for now
              <#if variantSample?? && (0 < variantSample.size())>
                <#assign imageKeys = variantSample.keySet()>
                <#assign imageMap = variantSample>
                <@table type="generic" cellspacing="0" cellpadding="0">
                  <@tr>
                    <#assign maxIndex = 7>
                    <#assign indexer = 0>
                    <#list imageKeys as key>
                      <#assign swatchProduct = imageMap.get(key)>
                      <#if swatchProduct?has_content && indexer < maxIndex>
                        <#assign imageUrl = Static["org.ofbiz.product.product.ProductContentWrapper"].getProductContentAsText(swatchProduct, "SMALL_IMAGE_URL", request, "url")!>
                        <#if !imageUrl?string?has_content>
                          <#assign imageUrl = productContentWrapper.get("SMALL_IMAGE_URL", "url")!>
                        </#if>
                        <#if !imageUrl?string?has_content>
                          <#assign imageUrl = "/images/defaultImage.jpg">
                        </#if>
                        <@td align="center" valign="bottom">
                          <a href="javascript:getList('FT${featureOrderFirst}','${indexer}',1);"><img src="<@ofbizContentUrl ctxPrefix=true>${imageUrl}</@ofbizContentUrl>" class="cssImgSmall" alt="" /></a>
                          <br />
                          <a href="javascript:getList('FT${featureOrderFirst}','${indexer}',1);" class="${styles.link_nav_info_name!}">${key}</a>
                        </@td>
                      </#if>
                      <#assign indexer = indexer + 1>
                    </#list>
                    <#if (indexer > maxIndex)>
                      <div><b>${uiLabelMap.OrderMoreOptionsAvailable}.</b></div>
                    </#if>
                  </@tr>
                </@table>
              </#if>
              -->

        </@cell>
    </@row>
</@section>


<@section>

    <#assign prodLongDescr = htmlContentString(productContentWrapper.get("LONG_DESCRIPTION")!)/>
    <#if !prodLongDescr?has_content>
      <#assign prodLongDescr = productContentWrapper.get("DESCRIPTION")!?trim/>
    </#if>
    <#assign prodWarnings = htmlContentString(productContentWrapper.get("WARNINGS")!)/>

    <ul class="tabs" data-tab>
      <li class="tab-title active"><a href="#panel11"><i class="${styles.icon!} ${styles.icon_prefix}pencil"></i> ${uiLabelMap.CommonOverview}</a></li><#-- ${uiLabelMap.CommonDescription} -->
      <li class="tab-title"><a href="#panel21"><i class="${styles.icon!} ${styles.icon_prefix}list"></i> ${uiLabelMap.CommonSpecifications}</a></li><#-- ${uiLabelMap.CommonInformation} -->

      <#-- "Configuration" is too weird for an iitem like a pizza: ${uiLabelMap.CommonConfiguration} -->
      <li class="tab-title"><a href="#panel31"><i class="${styles.icon!} ${styles.icon_prefix}wrench"></i> ${uiLabelMap.CommonOptions}</a></li>
    </ul>
    <div class="tabs-content">
        <div class="content active" id="panel11">
            <@productDetailLongDescContent />
        </div>
            
        <div class="content" id="panel21">
            <@productDetailProductAttribContent />
        </div>
        
        <div class="content" id="panel31">
            <@productConfigurator />
        </div>
    </div>

</@section>

  <#-- Scipio: Not for now
  <#- Product Reviews ->
  <@tr>
    <@td colspan="2">
      <div>${uiLabelMap.OrderCustomerReviews}:</div>
      <#if averageRating?? && (averageRating > 0) && numRatings?? && (numRatings > 1)>
          <div>${uiLabelMap.OrderAverageRating}: ${averageRating} <#if numRatings??>(${uiLabelMap.CommonFrom} ${numRatings} ${uiLabelMap.OrderRatings})</#if></div>
      </#if>
    </@td>
  </@tr>
  <@tr><@td colspan="2"><hr class="sepbar"/></@td></@tr>
  <#if productReviews?has_content>
    <#list productReviews as productReview>
      <#assign postedUserLogin = productReview.getRelatedOne("UserLogin", false)>
      <#assign postedPerson = postedUserLogin.getRelatedOne("Person", false)!>
      <@tr>
        <@td colspan="2">
          <@table type="generic"> <#- orig: border="0" cellpadding="0" cellspacing="0" ->
            <@tr>
              <@td>${uiLabelMap.CommonBy}: <#if (productReview.postedAnonymous!("N")) == "Y">${uiLabelMap.OrderAnonymous}<#else>${postedPerson.firstName} ${postedPerson.lastName}</#if>
              </@td>
              <@td>${uiLabelMap.CommonOn}: ${productReview.postedDateTime!}
              </@td>
              <@td>${uiLabelMap.OrderRanking}: ${productReview.productRating!?string}
              </@td>
            </@tr>
            <@tr>
              <@td colspan="3">&nbsp;
              </@td>
            </@tr>
            <@tr>
              <@td colspan="3">${productReview.productReview!}
              </@td>
            </@tr>

          </@table>
        </@td>
      </@tr>
    </#list>
    <@tr>
      <@td colspan="2">
        <a href="<@ofbizUrl>reviewProduct?category_id=${categoryId!}&amp;product_id=${product.productId}</@ofbizUrl>" class="${styles.link_nav!} ${styles.action_add!}">${uiLabelMap.ProductReviewThisProduct}!</a>
      </@td>
    </@tr>
  <#else>
    <@tr>
      <@td colspan="2">${uiLabelMap.ProductProductNotReviewedYet}.
      </@td>
    </@tr>
    <@tr>
      <@td colspan="2">
        <a href="<@ofbizUrl>reviewProduct?category_id=${categoryId!}&amp;product_id=${product.productId}</@ofbizUrl>" class="${styles.link_nav!} ${styles.action_add!}">${uiLabelMap.ProductBeTheFirstToReviewThisProduct}</a>
      </@td>
    </@tr>
  </#if>
  -->


<@section>
    <@commonAssociatedProducts productValue=product commonFeatureResultIds=(commonFeatureResultIds!)/>
</@section>

</div>
