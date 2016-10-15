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
<#-- variable setup -->
<#assign productContentWrapper = productContentWrapper!>
<#assign price = priceMap!>
<#-- end variable setup -->

<@script>

<#-- virtual product javascript -->
    ${virtualJavaScript!}

    var detailImageUrl = null;
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

    function popupDetail() {
        var defaultDetailImage = "${firstDetailImage?default(mainDetailImageUrl?default("_NONE_"))}";
        if (defaultDetailImage == null || defaultDetailImage == "null" || defaultDetailImage == "") {
            defaultDetailImage = "_NONE_";
        }

        if (detailImageUrl == null || detailImageUrl == "null") {
            detailImageUrl = defaultDetailImage;
        }

        if (detailImageUrl == "_NONE_") {
            hack = document.createElement('span');
            hack.innerHTML="${uiLabelMap.CommonNoDetailImageAvailableToDisplay}";
            showErrorAlert("${uiLabelMap.CommonErrorMessage2}","${uiLabelMap.CommonNoDetailImageAvailableToDisplay}");
            return;
        }
        detailImageUrl = detailImageUrl.replace(/\&\#47;/g, "/");
        popUp("<@ofbizUrl>detailImage?detail=" + detailImageUrl + "</@ofbizUrl>", 'detailImage', '400', '550');
    }

    function toggleAmt(toggle) {
        if (toggle == 'Y') {
            jQuery('#add_amount').removeClass("${styles.hidden!}");
        }
        else if (toggle == 'N') {
            jQuery('#add_amount').removeClass("${styles.hidden!}").addClass("${styles.hidden!}");
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

jQuery(document).ready( function() {
  jQuery('#configFormId').change(getConfigDetails);
});

function getConfigDetails() {
       var element = jQuery(this);
       if (element.attr("id").is("[id^='comments_']")) {
         //  don't update the price for comment change
         return;
       }

       jQuery.ajax({
           url: '<@ofbizInterWebappUrl>/ordermgr/control/getConfigDetailsEvent</@ofbizInterWebappUrl>',
           data: jQuery('configFormId').serialize(),
           type: "POST",
           success: function(data) {
               var totalPrice = data.totalPrice;
               var configId = data.configId;
               document.getElementById('totalPrice').innerHTML = totalPrice;
               document.addToShoppingList.configId.value = configId;
           },
           error: function(data) {
           }
       });
}
</@script>

<@section id="productdetail">

  <#-- Category next/previous -->
  <#if category??>
    <@row>
      <@cell>
        <#if previousProductId??>
          <a href="<@ofbizUrl>product/~category_id=${categoryId!}/~product_id=${previousProductId!}</@ofbizUrl>" class="${styles.link_nav!}">${uiLabelMap.CommonPrevious}</a>&nbsp;|&nbsp;
        </#if>
        <a href="<@ofbizUrl>category/~category_id=${categoryId!}</@ofbizUrl>" class="${styles.link_nav_info_name!}">${(category.categoryName)?default(category.description)!}</a>
        <#if nextProductId??>
          &nbsp;|&nbsp;<a href="<@ofbizUrl>product/~category_id=${categoryId!}/~product_id=${nextProductId!}</@ofbizUrl>" class="${styles.link_nav!}">${uiLabelMap.CommonNext}</a>
        </#if>
      </@cell>
    </@row>
  </#if>

  <hr class="sepbar"/>

  <#-- Scipio: open form earlier than stock code so don't produce invalid html... -->
  <#assign action><@ofbizUrl>additem<#if requestAttributes._CURRENT_VIEW_??>/${requestAttributes._CURRENT_VIEW_}</#if></@ofbizUrl></#assign>
  <@form method="post" action=action name="addform">

  <#-- Product image/name/price -->
  <@row>
    <@cell columns=4>
      <#assign productLargeImageUrl = productContentWrapper.get("LARGE_IMAGE_URL", "url")!>
      <#-- remove the next two lines to always display the virtual image first (virtual images must exist) -->
      <#if firstLargeImage?has_content>
        <#assign productLargeImageUrl = firstLargeImage>
      </#if>
      <#if productLargeImageUrl?string?has_content>
        <a href="javascript:popupDetail();" class="${styles.link_type_image!} ${styles.action_run_sys!} ${styles.action_view!}"><img src="<@ofbizContentUrl>${contentPathPrefix!}${productLargeImageUrl!}</@ofbizContentUrl>" name="mainImage" vspace="5" hspace="5" class="cssImgLarge" align="left" alt="" /></a>
      </#if>
    </@cell>
    <@cell columns=8>
      <@heading>${productContentWrapper.get("PRODUCT_NAME")!}</@heading>
      <div>${productContentWrapper.get("DESCRIPTION")!}</div>
      <div><b>${product.productId!}</b></div>
      <#-- example of showing a certain type of feature with the product -->
      <#if sizeProductFeatureAndAppls?has_content>
        <div>
          <#if (sizeProductFeatureAndAppls?size == 1)>
            <#-- TODO : i18n -->
            Size:
          <#else>
            Sizes Available:
          </#if>
          <#list sizeProductFeatureAndAppls as sizeProductFeatureAndAppl>
            ${sizeProductFeatureAndAppl.description?default(sizeProductFeatureAndAppl.abbrev?default(sizeProductFeatureAndAppl.productFeatureId))}<#if sizeProductFeatureAndAppl_has_next>,</#if>
          </#list>
        </div>
      </#if>

      <#-- for prices:
              - if totalPrice is present, use it (totalPrice is the price calculated from the parts)
              - if price < competitivePrice, show competitive or "Compare At" price
              - if price < listPrice, show list price
              - if price < defaultPrice and defaultPrice < listPrice, show default
              - if isSale show price with salePrice style and print "On Sale!"
      -->
      <#if totalPrice??>
        <div>${uiLabelMap.ProductAggregatedPrice}: <span id="totalPrice" class="basePrice"><@ofbizCurrency amount=totalPrice isoCode=totalPrice.currencyUsed/></span></div>
      <#else>
      <#if price.competitivePrice?? && price.price?? && price.price?double < price.competitivePrice?double>
        <div>${uiLabelMap.ProductCompareAtPrice}: <span class="basePrice"><@ofbizCurrency amount=price.competitivePrice isoCode=price.currencyUsed/></span></div>
      </#if>
      <#if price.listPrice?? && price.price?? && price.price?double < price.listPrice?double>
        <div>${uiLabelMap.ProductListPrice}: <span class="basePrice"><@ofbizCurrency amount=price.listPrice isoCode=price.currencyUsed/></span></div>
      </#if>
      <#if price.listPrice?? && price.defaultPrice?? && price.price?? && price.price?double < price.defaultPrice?double && price.defaultPrice?double < price.listPrice?double>
        <div>${uiLabelMap.ProductRegularPrice}: <span class="basePrice"><@ofbizCurrency amount=price.defaultPrice isoCode=price.currencyUsed/></span></div>
      </#if>
      <div>
        <b>
          <#if price.isSale?? && price.isSale>
            <span class="salePrice">${uiLabelMap.OrderOnSale}!</span>
            <#assign priceStyle = "salePrice">
          <#else>
            <#assign priceStyle = "regularPrice">
          </#if>
            ${uiLabelMap.OrderYourPrice}: <#if "Y" == (product.isVirtual!)>from </#if><span class="${priceStyle}"><@ofbizCurrency amount=price.price isoCode=price.currencyUsed/></span>
        </b>
      </div>
      <#if price.listPrice?? && price.price?? && price.price?double < price.listPrice?double>
        <#assign priceSaved = price.listPrice?double - price.price?double>
        <#assign percentSaved = (priceSaved?double / price.listPrice?double) * 100>
        <div>${uiLabelMap.OrderSave}: <span class="basePrice"><@ofbizCurrency amount=priceSaved isoCode=price.currencyUsed/> (${percentSaved?int}%)</span></div>
      </#if>
      </#if>

      <#-- Included quantities/pieces -->
      <#if product.quantityIncluded?? && product.quantityIncluded?double != 0>
        <div>${uiLabelMap.OrderIncludes}:
          ${product.quantityIncluded!}
          ${product.quantityUomId!}
        </div>
      </#if>
      <#if product.piecesIncluded?? && product.piecesIncluded?long != 0>
        <div>${uiLabelMap.OrderPieces}:
          ${product.piecesIncluded}
        </div>
      </#if>
      <#if daysToShip??>
        <div><b>${uiLabelMap.ProductUsuallyShipsIn} <span class="${styles.color_alert!}">${daysToShip}</font> ${uiLabelMap.CommonDays}!<b></div>
      </#if>

      <#if disFeatureList?? && 0 < disFeatureList.size()>
        <p>&nbsp;</p>
        <#list disFeatureList as currentFeature>
            <div>
                ${currentFeature.productFeatureTypeId}:&nbsp;${currentFeature.description}
            </div>
        </#list>
            <div>&nbsp;</div>
      </#if>

        <#assign inStock = true>
        <#-- Variant Selection -->
        <#if product.isVirtual?? && product.isVirtual?upper_case == "Y">
          <#if variantTree?? && 0 < variantTree.size()>
            <#list featureSet as currentType>
            <@fields type="default-nolabelarea">
              <@field type="select" name="FT${currentType}" onChange="javascript:getList(this.name, (this.selectedIndex-1), 1);">
                  <option>${featureTypes.get(currentType)}</option>
              </@field>
            </@fields>
            </#list>
            <input type="hidden" name="product_id" value="${product.productId}" />
            <input type="hidden" name="add_product_id" value="NULL" />
          <#else>
            <input type="hidden" name="product_id" value="${product.productId}" />
            <input type="hidden" name="add_product_id" value="NULL" />
            <@alert type="info">${uiLabelMap.ProductItemOutOfStock}.</@alert>
            <#assign inStock = false>
          </#if>
        <#else>
          <input type="hidden" name="product_id" value="${product.productId}" />
          <input type="hidden" name="add_product_id" value="${product.productId}" />
          <#if productNotAvailable??>
            <#assign isStoreInventoryRequired = Static["org.ofbiz.product.store.ProductStoreWorker"].isStoreInventoryRequired(request, product)>
            <#if isStoreInventoryRequired>
              <@alert type="info">${uiLabelMap.ProductItemOutOfStock}.</@alert>
              <#assign inStock = false>
            <#elseif inventoryMessage?has_content>
              <@alert type="info">${product.inventoryMessage!}</@alert>
            </#if>
          </#if>
        </#if>

    </@cell>
  </@row>
  <@row>
    <@cell> <#-- TODO?: float right (as stock template) class="+${styles.text_right!}" -->
      
      <@section> 
        <#-- check to see if introductionDate hasn't passed yet -->
        <#if product.introductionDate?? && nowTimestamp.before(product.introductionDate)>
          <@alert type="warning">${uiLabelMap.ProductProductNotYetMadeAvailable}.</@alert>
        <#-- check to see if salesDiscontinuationDate has passed -->
        <#elseif product.salesDiscontinuationDate?? && nowTimestamp.after(product.salesDiscontinuationDate)>
          <@alert type="warning">${uiLabelMap.ProductProductNoLongerAvailable}.</@alert>
        <#-- check to see if the product requires inventory check and has inventory -->
        <#else>
          <#if inStock>
            <#if (product.requireAmount!"N") == "Y">
              <#assign hiddenStyle = "">
            <#else>
              <#assign hiddenStyle = styles.hidden!>
            </#if>
            <#-- Scipio: NOTE: amount is kg or either; quantity is the number of units buying -->
            <@field type="input" containerId="add_amount" label=uiLabelMap.OrderAmount containerClass="+${hiddenStyle}" size="5" name="add_amount" value="" />
            <#if !configwrapper.isCompleted()>
              <@alert type="info">${uiLabelMap.EcommerceProductNotConfigured}</@alert>
              <@field type="input" label="${rawLabel('ProductQuantity')} (${rawLabel('OrderUnits')})" disabled=true size="5" name="quantity" value="0" disabled="disabled" />
            <#else>
              <@field type="input" label=uiLabelMap.ProductQuantity size="5" name="quantity" value="1" />
              <@field type="submit" submitType="link" href="javascript:addItem()" class="+${styles.link_run_session!} ${styles.action_add!}" text=uiLabelMap.OrderAddToCart />
            </#if>
          </#if>
          <#if requestParameters.category_id??>
            <input type="hidden" name="category_id" value="${requestParameters.category_id}" />
          </#if>
        </#if>
      </@section>
    </@cell>
  </@row>
  </@form>
  
  <@row> 
    <@cell>    
  
      <@section>
      <#if sessionAttributes.userLogin?has_content && sessionAttributes.userLogin.userLoginId != "anonymous">
        <hr />
        <form name="addToShoppingList" method="post" action="<@ofbizUrl>addItemToShoppingList<#if requestAttributes._CURRENT_VIEW_??>/${requestAttributes._CURRENT_VIEW_}</#if></@ofbizUrl>">
          <input type="hidden" name="productId" value="${product.productId}" />
          <input type="hidden" name="product_id" value="${product.productId}" />
          <input type="hidden" name="configId" value="${configId!}" />
          <@field type="select" label=uiLabelMap.PageTitleShoppingList name="shoppingListId">
              <#if shoppingLists?has_content>
                <#list shoppingLists as shoppingList>
                  <option value="${shoppingList.shoppingListId}">${shoppingList.listName}</option>
                </#list>
              </#if>
              <option value="">---</option>
              <option value="">${uiLabelMap.OrderNewShoppingList}</option>
          </@field>
          <@field type="input" label=uiLabelMap.ProductQuantity size="5" name="quantity" value="1" />
          <@field type="submit" submitType="link" href="javascript:document.addToShoppingList.submit();" class="+${styles.link_run_sys!} ${styles.action_add!}" text="[${uiLabelMap.OrderAddToShoppingList}]" />
        </form>
      <#else> 
        ${uiLabelMap.OrderYouMust} <a href="<@ofbizUrl>checkLogin/showcart</@ofbizUrl>" class="${styles.link_nav!} ${styles.action_login!}">${uiLabelMap.CommonBeLogged}</a>
        ${uiLabelMap.OrderToAddSelectedItemsToShoppingList}.&nbsp;
      </#if>
      </@section>
      <#-- Prefill first select box (virtual products only) -->
      <#if variantTree?? && 0 < variantTree.size()>
        <@script>eval("list" + "${featureOrderFirst}" + "()");</@script>
      </#if>

      <#-- Swatches (virtual products only) -->
      <#if variantSample?? && 0 < variantSample.size()>
        <#assign imageKeys = variantSample.keySet()>
        <#assign imageMap = variantSample>

        <@table type="data-complex"> <#-- orig: class="" --> <#-- orig: cellspacing="0" --> <#-- orig: cellpadding="0" -->
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
                  <a href="javascript:getList('FT${featureOrderFirst}','${indexer}',1);" class="${styles.link_type_image!} ${styles.action_run_local!} ${styles.action_select!}">les.<img src="<@ofbizContentUrl>${contentPathPrefix!}${imageUrl}</@ofbizContentUrl>" class="cssImgStandard" alt="" /></a>
                  <br />
                  <a href="javascript:getList('FT${featureOrderFirst}','${indexer}',1);" class="${styles.link_run_local!} ${styles.action_select!}">${key}</a>
                </@td>
              </#if>
              <#assign indexer = indexer + 1>
            </#list>
            <#if (indexer > maxIndex)>
              <@td><strong>${uiLabelMap.OrderMoreOptionsAvailable}.</strong></@td>
            </#if>
          </@tr>
        </@table>
      </#if>
    </@cell>
  </@row>

  <#-- Long description of product -->
  <#assign longDesc = htmlContentString(productContentWrapper.get("LONG_DESCRIPTION")!)>
  <#if longDesc?trim?has_content>
    <hr class="sepbar"/>
    <@row>
      <@cell>${longDesc}</@cell>
    </@row>
  </#if>

  <hr class="sepbar"/>


<@fields type="default-compact">
  <#-- Any attributes/etc may go here -->
  <#-- Product Configurator -->
  <@row>
    <@cell>
      <form name="configform" id="configFormId" method="post" action="<@ofbizUrl>product<#if requestAttributes._CURRENT_VIEW_??>/${requestAttributes._CURRENT_VIEW_}</#if></@ofbizUrl>">
        <input type="hidden" name="add_product_id" value="${product.productId}" />
        <input type="hidden" name="add_category_id" value="" />
        <input type="hidden" name="quantity" value="1" />

        <input type="hidden" name="product_id" value="${product.productId}" />
          <@row>
            <@cell>
                <a href="javascript:verifyConfig();" class="${styles.link_run_sys!} ${styles.action_verify!}">${uiLabelMap.OrderVerifyConfiguration}</a>
            </@cell>
          </@row>
          <hr />
          <#assign counter = 0>
          <#assign questions = configwrapper.questions>
          <#list questions as question>
          <@row>
            <@cell columns=4>${question.question}</@cell>
            <@cell columns=8 class="+${styles.text_right!}">
              <#-- Scipio: NOTE: question is already html-escaped by ConfigItem class; wrapString prevents second escape -->
              <#if question.isFirst()>
                <a name="#${question.getConfigItem().getString("configItemId")}"></a>
                
                <#assign instructions = question.content.get("INSTRUCTIONS")!>
                <#assign instructionsHtml = "">
                <#if instructions?has_content>
                  <#assign instructionsHtml> <a href="javascript:showErrorAlert('${escapePart(uiLabelMap.CommonErrorMessage2, 'js-html')}','${escapePart(instructions, 'js-html')}');" class="${styles.link_nav!} ${styles.action_view!}">Instructions</a></#assign>
                </#if>
                <div>${question.description!}${instructionsHtml}</div>

                <#assign image = question.content.get("IMAGE_URL", "url")!>
                <#if image?has_content>
                  <img src="<@ofbizContentUrl>${contentPathPrefix!}${image!}</@ofbizContentUrl>" vspace="5" hspace="5" class="cssImgSmall" align="left" alt="" />
                </#if>
              <#else>
                <a href="#${question.getConfigItem().getString("configItemId")}" class="${styles.link_nav!} ${styles.action_view!}">Details</a>
              </#if>
            </@cell>
          </@row>
          <@row>
            <@cell>
            <#if question.isStandard()>
              <#-- Standard item: all the options are always included -->
              <#assign options = question.options>
              <#assign optionCounter = 0>
              <#list options as option>
                <@field type="display">
                    ${option.description} <#if !option.isAvailable()> (*)</#if>
                </@field>
                <@field type="text" name="comments_${counter}_${optionCounter}" id="comments_${counter}_${optionCounter}" value=(option.comments!) label=uiLabelMap.CommonComments />
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
                <#if renderSingleChoiceWithRadioButtons?? && "Y" == renderSingleChoiceWithRadioButtons>
                <#-- This is the radio button implementation -->
                <#if !question.isMandatory()>
                  <@field type="radio" name=counter?string value="" checked=(!question.isSelected()) label="No option" />
                </#if>
                <#assign optionComment = "">
                <#assign optionCounter = 0>
                <#list options as option>
                  <#assign componentCounter = 0>
                  <#if showOffsetPrice?? && "Y" == showOffsetPrice>
                    <#assign shownPrice = option.price - selectedPrice>
                  <#else>
                    <#assign shownPrice = option.price>
                  </#if>
                  <#if option.isSelected()>
                    <#assign optionComment = option.getComments()!>
                  </#if>
                    <#-- Render virtual compoennts -->
                    <#if option.hasVirtualComponent()>
                      <@row>
                        <@cell>
                        <#assign fieldLabel>${option.description}<#if !option.isAvailable()> (*)</#if></#assign>
                        <@field type="radio" name=counter?string id="${counter}_${optionCounter}" value=optionCounter onClick="javascript:checkOptionVariants('${counter}_${optionCounter}');" label=wrapAsRaw(fieldLabel, 'htmlmarkup') />
                        <#assign components = option.getComponents()>
                        <#list components as component>
                          <#if (option.isVirtualComponent(component))>
                            <@render resource=inlineProductDetailScreen reqAttribs={"inlineProductId":component.productId, "inlineCounter":counter+ "_" +optionCounter + "_"+componentCounter, "addJavaScript":componentCounter}/>
                            <#assign componentCounter = componentCounter + 1>
                          </#if>
                        </#list>
                        </@cell>
                      </@row>
                    <#else>
                      <@row>
                        <@cell>
                          <#assign fieldLabel>
                            ${option.description!}&nbsp;
                            <#if (shownPrice > 0)>+<@ofbizCurrency amount=shownPrice isoCode=price.currencyUsed/>&nbsp;</#if>
                            <#if (shownPrice < 0)>-<@ofbizCurrency amount=(-1*shownPrice) isoCode=price.currencyUsed/>&nbsp;</#if>
                            <#if !option.isAvailable()> (*)</#if>
                          </#assign>
                          <@field type="radio" name=counter?string value=optionCounter checked=(option.isSelected() || (!question.isSelected() && optionCounter == 0 && question.isMandatory())) label=wrapAsRaw(fieldLabel, 'htmlmarkup') />
                        </@cell>
                      </@row>
                    </#if>
                  <#assign optionCounter = optionCounter + 1>
                </#list>
                  <@field type="input" name="comments_${counter}_0" id="comments_${counter}_0" value=(optionComment!) label=uiLabelMap.CommonComments />
                <#else>
                <#-- And this is the select box implementation -->
                <@field type="select" name=counter?string>
                <#if !question.isMandatory()>
                  <option value="">---</option>
                </#if>
                <#assign options = question.options>
                <#assign optionCounter = 0>
                <#assign optionComment = "">
                <#list options as option>
                  <#if showOffsetPrice?? && "Y" == showOffsetPrice>
                    <#assign shownPrice = option.price - selectedPrice>
                  <#else>
                    <#assign shownPrice = option.price>
                  </#if>
                  <#if option.isSelected()>
                    <#assign optionComment = option.getComments()>
                    <#assign optionCounter = optionCounter + 1>
                  </#if>
                  <option value="${optionCounter}" <#if option.isSelected()>selected="selected"</#if>>
                    ${option.description}&nbsp;
                    <#if (shownPrice > 0)>+<@ofbizCurrency amount=shownPrice isoCode=price.currencyUsed/>&nbsp;</#if>
                    <#if (shownPrice < 0)>-<@ofbizCurrency amount=(-1*shownPrice) isoCode=price.currencyUsed/>&nbsp;</#if>
                    <#if !option.isAvailable()> (*)</#if>
                  </option>
                  <#assign optionCounter = optionCounter + 1>
                </#list>
                </@field>
                <@field type="input" name="comments_${counter}_0" id="comments_${counter}_0" value=(optionComment!) label=uiLabelMap.CommonComments />
                </#if>
              <#else>
                <#-- Multi choice question -->
                <#assign options = question.options>
                <#assign optionCounter = 0>
                <#list options as option>
                    <#assign componentCounter = 0>
                    <#-- Render virtual compoennts -->
                    <#if option.hasVirtualComponent()>
                      <@row>
                        <@cell>
                        <#assign fieldLabel>${option.description}<#if !option.isAvailable()> (*)</#if></#assign>
                        <@field type="checkbox" name=counter?string id="${counter}_${optionCounter}" value=optionCounter onClick="javascript:checkOptionVariants('${counter}_${optionCounter}');" label=wrapAsRaw(fieldLabel, 'htmlmarkup') />

                        <#assign components = option.getComponents()>
                        <#list components as component>
                          <#if (option.isVirtualComponent(component))>
                            <@render resource=inlineProductDetailScreen reqAttribs={"inlineProductId":component.productId, "inlineCounter":counter+ "_" +optionCounter + "_"+componentCounter, "addJavaScript":componentCounter}/>
                            <#assign componentCounter = componentCounter + 1>
                          </#if>
                        </#list>
                        </@cell>
                      </@row>
                    <#else>
                      <#assign fieldLabel>${option.description} +<@ofbizCurrency amount=option.price isoCode=price.currencyUsed/><#if !option.isAvailable()> (*)</#if></#assign>
                      <@field type="checkbox" name=counter?string value=optionCounter checked=option.isSelected() label=wrapAsRaw(fieldLabel, 'htmlmarkup') />
                    </#if>
                    <@field type="input" name="comments_${counter}_${optionCounter}" id="comments_${counter}_${optionCounter}" value=(option.comments!) label=uiLabelMap.CommonComments/>
                  <#assign optionCounter = optionCounter + 1>
                </#list>
              </#if>
            </#if>
            </@cell>
          </@row>
          <#if question_has_next>
            <hr />
          </#if>
          <#assign counter = counter + 1>
        </#list>
      </form>
    </@cell>
  </@row>

  <hr class="sepbar"/>

<#-- Upgrades/Up-Sell/Cross-Sell -->
<#macro associated assocProducts beforeName showName afterName formNamePrefix targetRequestName>
  <#assign targetRequest = "product">
  <#if targetRequestName?has_content>
    <#assign targetRequest = targetRequestName>
  </#if>
  <#if assocProducts?has_content>
  <@row><@cell>&nbsp;</@cell></@row>
  <#assign title>${beforeName}<#if showName == "Y">${productContentWrapper.get("PRODUCT_NAME")!}</#if>${afterName}</#assign>
  <@section title=wrapAsRaw(title, 'htmlmarkup')><#-- FIXME: currently needed due to productContentWrapper escaping -->
    <hr />
    <#list assocProducts as productAssoc>
      <@row>
        <@cell>
          <a href="<@ofbizUrl>${targetRequest}/<#if categoryId??>~category_id=${categoryId}/</#if>~product_id=${productAssoc.productIdTo!}</@ofbizUrl>" class="${styles.link_nav_info_id!}">
            ${productAssoc.productIdTo!}
          </a>
          - <b>${productAssoc.reason!}</b>
        </@cell>
      </@row>
      <#assign dummy = setRequestAttribute("optProductId", productAssoc.productIdTo)>
      <#assign dummy = setRequestAttribute("listIndex", listIndex)>
      <#assign dummy = setRequestAttribute("formNamePrefix", formNamePrefix)>
      <#if targetRequestName?has_content>
        <#assign dummy = setRequestAttribute("targetRequestName", targetRequestName)>
      </#if>
      <@row>
        <@cell>
          <@render resource=productsummaryScreen />
        </@cell>
      </@row>
      <#local listIndex = listIndex + 1>
      <hr />
    </#list>
    <#assign dummy = setRequestAttribute("optProductId", "")>
    <#assign dummy = setRequestAttribute("formNamePrefix", "")>
    <#assign dummy = setRequestAttribute("targetRequestName", "")>
  </@section>
  </#if>
</#macro>
<#assign productValue = product>
<#assign listIndex = 1>
<#assign dummy = setRequestAttribute("productValue", productValue)>

  <#-- obsolete -->
  <@associated assocProducts=obsoleteProducts beforeName="" showName="Y" afterName=" is made obsolete by these products:" formNamePrefix="obs" targetRequestName=""/>
  <#-- cross sell -->
  <@associated assocProducts=crossSellProducts beforeName="" showName="N" afterName="You might be interested in these as well:" formNamePrefix="cssl" targetRequestName="crosssell"/>
  <#-- up sell -->
  <@associated assocProducts=upSellProducts beforeName="Try these instead of " showName="Y" afterName=":" formNamePrefix="upsl" targetRequestName="upsell"/>
  <#-- obsolescence -->
  <@associated assocProducts=obsolenscenseProducts beforeName="" showName="Y" afterName=" makes these products obsolete:" formNamePrefix="obce" targetRequestName=""/>

<#-- special cross/up-sell area using commonFeatureResultIds (from common feature product search) -->
<#if commonFeatureResultIds?has_content>
  <@heading>Similar Products That Might Interest You...</@heading>
  <hr />

  <#list commonFeatureResultIds as commonFeatureResultId>
    <@section>
      <#assign dummy = setRequestAttribute("optProductId", commonFeatureResultId)>
      <#assign dummy = setRequestAttribute("listIndex", commonFeatureResultId_index)>
      <#assign dummy = setRequestAttribute("formNamePrefix", "cfeatcssl")>
      <#-- <#assign dummy = setRequestAttribute("targetRequestName", targetRequestName)> -->
      <@render resource=productsummaryScreen />
    </@section>
    <#if commonFeatureResultId_has_next>
      <hr />
    </#if>
  </#list>
</#if>
</@fields>

</@section>
