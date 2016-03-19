<#-- variable setup -->
<#assign price = priceMap?if_exists />
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
                      ${setRequestAttribute("optProductId", assocProductId)}
                      ${setRequestAttribute("listIndex", listIndex)}
                      ${setRequestAttribute("formNamePrefix", formNamePrefix)}
                      <#if targetRequestName?has_content>
                        ${setRequestAttribute("targetRequestName", targetRequestName)}
                      </#if>
                      ${screens.render("component://shop/widget/CatalogScreens.xml#miniproductsummary")}
                  </li>
                  <#assign product = pageProduct />
                  <#local listIndex = listIndex + 1 />
                </#list>
            </@grid>
            ${setRequestAttribute("optProductId", "")}
            ${setRequestAttribute("formNamePrefix", "")}
            ${setRequestAttribute("targetRequestName", "")}

        </@section>
      </#if>
    </#macro>

<@section title="">
    <@row>
        <@cell columns=8>
            <#--
            <#assign productAdditionalSmallImage1 = productContentWrapper.get("XTRA_IMG_1_SMALL","html")?if_exists />
            <#assign productAdditionalSmallImage2 = productContentWrapper.get("XTRA_IMG_2_SMALL","html")?if_exists />
            <#assign productAdditionalSmallImage3 = productContentWrapper.get("XTRA_IMG_3_SMALL","html")?if_exists />
            <#assign productAdditionalSmallImage4 = productContentWrapper.get("XTRA_IMG_4_SMALL","html")?if_exists />-->

            <#assign productAdditionalImage1 = productContentWrapper.get("ADDITIONAL_IMAGE_1","html")?trim!"" />
            <#assign productAdditionalImage2 = productContentWrapper.get("ADDITIONAL_IMAGE_2","html")?trim!"" />
            <#assign productAdditionalImage3 = productContentWrapper.get("ADDITIONAL_IMAGE_3","html")?trim!"" />
            <#assign productAdditionalImage4 = productContentWrapper.get("ADDITIONAL_IMAGE_4","html")?trim!"" />
            <#assign productLargeImageUrl = productContentWrapper.get("LARGE_IMAGE_URL","html")?trim!"" /> 
            <#assign productOriginalImage = productContentWrapper.get("ORIGINAL_IMAGE_URL","html")?trim!"" />
            
            <#if firstLargeImage?has_content>
                <#assign productLargeImageUrl = firstLargeImage />
            </#if>
            <#if productLargeImageUrl?string?has_content>
                <#assign largeImage><@ofbizContentUrl>${contentPathPrefix?if_exists}${productLargeImageUrl?if_exists}</@ofbizContentUrl></#assign>
            </#if>
            <@img src=largeImage!"https://placehold.it/800x300" width="100%" height="300px" type="contain" class="product-image"/>
            <#-- CATO: We are using the clearing mechanism in foundation here. This may be migrated to the grid macro at a later stage. -->
            <#if productAdditionalImage1?has_content>
                <ul class="clearing-thumbs" data-clearing>
                    <#if productAdditionalImage1?has_content>
                        <#assign largeImage><@ofbizContentUrl>${contentPathPrefix?if_exists}${productAdditionalImage1?if_exists}</@ofbizContentUrl></#assign>
                        <li><@img src=largeImage!"" link=largeImage!"" width="auto" height="80px" type="cover" class=""/></li>
                    </#if>
                    <#if productAdditionalImage2?has_content>
                        <#assign largeImage><@ofbizContentUrl>${contentPathPrefix?if_exists}${productAdditionalImage2?if_exists}</@ofbizContentUrl></#assign>
                       <li><@img src=largeImage!"" link=largeImage!"" width="auto" height="80px" type="cover"/></li>
                    </#if>
                    <#if productAdditionalImage3?has_content>
                        <#assign largeImage><@ofbizContentUrl>${contentPathPrefix?if_exists}${productAdditionalImage3?if_exists}</@ofbizContentUrl></#assign>
                        <li><@img src=largeImage!"" link=largeImage!"" width="auto" height="80px" type="cover"/></li>
                    </#if>
                    <#if productAdditionalImage4?has_content>
                        <#assign largeImage><@ofbizContentUrl>${contentPathPrefix?if_exists}${productAdditionalImage4?if_exists}</@ofbizContentUrl></#assign>
                        <li><@img src=largeImage!"" link=largeImage!"" width="auto" height="80px" type="cover"/></li>
                    </#if>
                </ul>

            </#if>
        </@cell>
        <@cell columns=4>
            <@panel>
            <div id="product-info">               
                <#if productContentWrapper.get("DESCRIPTION","html")?has_content><p>${productContentWrapper.get("DESCRIPTION","html")?if_exists}</p></#if>
                <#-- for prices:
                    - if price < competitivePrice, show competitive or "Compare At" price
                    - if price < listPrice, show list price
                    - if price < defaultPrice and defaultPrice < listPrice, show default
                    - if isSale show price with salePrice style and print "On Sale!"
                -->
                <#-- CATO: These are alternative prices that are not really commonly used 
                <#if price.competitivePrice?exists && price.price?exists && price.price &lt; price.competitivePrice>
                    ${uiLabelMap.ProductCompareAtPrice}: <span class="product-price"><@ofbizCurrency amount=price.competitivePrice isoCode=price.currencyUsed /></span>
                </#if>
                
                Asset Usage price calculation
                <#if product.productTypeId?if_exists == "ASSET_USAGE">
                    <#if product.reserv2ndPPPerc?exists && product.reserv2ndPPPerc != 0><br />${uiLabelMap.ProductReserv2ndPPPerc}<#if !product.reservNthPPPerc?exists || product.reservNthPPPerc == 0>${uiLabelMap.CommonUntil} ${product.reservMaxPersons?if_exists}</#if> <@ofbizCurrency amount=product.reserv2ndPPPerc*price.price/100 isoCode=price.currencyUsed /></#if>
                    <#if product.reservNthPPPerc?exists &&product.reservNthPPPerc != 0><br />${uiLabelMap.ProductReservNthPPPerc} <#if !product.reserv2ndPPPerc?exists || product.reserv2ndPPPerc == 0>${uiLabelMap.ProductReservSecond} <#else> ${uiLabelMap.ProductReservThird} </#if> ${uiLabelMap.CommonUntil} ${product.reservMaxPersons?if_exists}, ${uiLabelMap.ProductEach}: <span class="product-price"><@ofbizCurrency amount=product.reservNthPPPerc*price.price/100 isoCode=price.currencyUsed /></span></#if>
                    <#if (!product.reserv2ndPPPerc?exists || product.reserv2ndPPPerc == 0) && (!product.reservNthPPPerc?exists || product.reservNthPPPerc == 0)><br />${uiLabelMap.ProductMaximum} ${product.reservMaxPersons?if_exists} ${uiLabelMap.ProductPersons}.</#if>
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
                <#if price.isSale?exists && price.isSale>
                    <p>${uiLabelMap.OrderOnSale}!</p>
                </#if>-->
                
                <p>
                <#if oldPrice?has_content>
                    <span id="product-price_old"><del><@ofbizCurrency amount=oldPrice isoCode=price.currencyUsed /></del></span>
                </#if>
                 
                <#if currentPrice?has_content>
                    <span id="product-price"><strong><@ofbizCurrency amount=currentPrice isoCode=price.currencyUsed /></strong><span>
                </#if>
                </p>
                
                <#-- CATO: Uncomment to display how much a user is saving by buying this product
                <#if price.listPrice?exists && price.price?exists && price.price &lt; price.listPrice>
                    <span id="product-saved"><sup>
                        <#assign priceSaved = oldPrice - currentPrice />
                        <#assign percentSaved = (priceSaved / oldPrice) * 100 />
                        ${uiLabelMap.OrderSave}: <@ofbizCurrency amount=priceSaved isoCode=price.currencyUsed /> (${percentSaved?int}%)
                        <sup>
                    </span>
                </#if>
                -->

                <#-- show price details ("showPriceDetails" field can be set in the screen definition) -->
                <#if (showPriceDetails?exists && showPriceDetails?default("N") == "Y")>
                    <#if price.orderItemPriceInfos?exists>
                        <#list price.orderItemPriceInfos as orderItemPriceInfo>
                            <p>${orderItemPriceInfo.description?if_exists}</p>
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
                    
                    <#-- Variant Selection -->
                    <#if product.isVirtual?if_exists?upper_case == "Y">
                        <#if product.virtualVariantMethodEnum?if_exists == "VV_FEATURETREE" && featureLists?has_content>
                            <input type="hidden" name="add_product_id" value="${product.productId}" />
                            <#list featureLists as featureList>
                                <@field type="select" id="FT${feature.productFeatureTypeId}" name="FT${feature.productFeatureTypeId}" label=feature.description!"">
                                    <option value="select" selected="selected"> select option </option>
                                    <#list featureList as feature>
                                        <option value="${feature.productFeatureId}">${feature.description} <#if feature.price?exists>(+ <@ofbizCurrency amount=feature.price?string isoCode=feature.currencyUomId />)</#if></option>
                                    </#list>
                                </@field>
                            </#list>
                            <@field type="text" name="quantity" value="1" size="4" maxLength="4" label=uiLabelMap.CommonQuantity/>
                        </#if>
                        <#if !product.virtualVariantMethodEnum?exists || product.virtualVariantMethodEnum == "VV_VARIANTTREE">
                            <#if variantTree?exists && (variantTree.size() &gt; 0)>
                                <#list featureSet as currentType>
                                    <@field type="select" name="FT${currentType}" label=featureTypes.get(currentType)!"">
                                            <option>${featureTypes.get(currentType)}</option>
                                    </@field>
                                </#list>
                                <input type="hidden" name="add_product_id" value="NULL"/>
                            <#else>
                                <input type="hidden" name="add_product_id" value="NULL"/>
                                <#assign inStock = false />
                            </#if>
                        </#if>
                    <#else>
                        <input type="hidden" name="add_product_id" value="${product.productId}" />
                        <#if (availableInventory?exists) && (availableInventory <= 0)>
                            <#assign inStock = false />
                        </#if>
                    </#if>
    
                    <#-- check to see if introductionDate hasnt passed yet -->
                    <#if product.introductionDate?exists && nowTimestamp.before(product.introductionDate)>
                        <@alert type="info">${uiLabelMap.ProductProductNotYetMadeAvailable}.</@alert>
                        <#-- check to see if salesDiscontinuationDate has passed -->
                    <#elseif product.salesDiscontinuationDate?exists && nowTimestamp.after(product.salesDiscontinuationDate)>
                        <@alert type="info">${uiLabelMap.ProductProductNoLongerAvailable}.</@alert>
                        <#-- check to see if the product requires inventory check and has inventory -->                        
                    <#elseif product.virtualVariantMethodEnum?if_exists != "VV_FEATURETREE">
                        <#if product.productTypeId?if_exists != "FINDIG_GOOD" && product.productTypeId?if_exists != "DIGITAL_GOOD">
                             <#if inStock>
                                 <#if product.requireAmount?default("N") != "Y">
                                     <#assign hiddenStyle = "hidden"/>
                                 </#if>
                                <@field type="text" size="5" name="add_amount" value="1" label=uiLabelMap.CommonAmount />
                                 <#if product.productTypeId?if_exists == "ASSET_USAGE">
                                     <div class="inline">
                                         <label>Start Date(yyyy-mm-dd)</label><input type="text" size="10" name="reservStart"/><a href="javascript:call_cal_notime(document.addform.reservStart, '${nowTimestamp.toString().substring(0,10)}');"><img src="<@ofbizContentUrl>/images/cal.gif</@ofbizContentUrl>" width="16" height="16" alt="Calendar" alt="" /></a>
                                         <label>End Date(yyyy-mm-dd)</label><input type="text" size="10" name="reservEnd"/><a href="javascript:call_cal_notime(document.addform.reservEnd, '${nowTimestamp.toString().substring(0,10)}');"><img src="<@ofbizContentUrl>/images/cal.gif</@ofbizContentUrl>" width="16" height="16" alt="Calendar" alt="" /></a>
                                     </div>
                                     <div>
                                         <#--td nowrap="nowrap" align="right">Number<br />of days<input type="text" size="4" name="reservLength"/>&nbsp;&nbsp;-->
                                         Number of persons<input type="text" size="4" name="reservPersons" value="2"/>
                                         Number of rooms<input type="text" size="5" name="quantity" value="1"/>
                                     </div>
                                 <#else> 
                                     <input name="quantity" id="quantity" value="1" type="hidden"/>
                                     <@field type="submit" id="addToCart" name="addToCart" value=uiLabelMap.OrderAddToCart class="+${styles.grid_columns_12}"/>
                                 </#if>
                             <#else>
                                 <#if productStore?exists>
                                     <#if productStore.requireInventory?exists && productStore.requireInventory == "N">
                                         <input name="quantity" id="quantity" value="1" type="hidden"/>
                                         <@field type="submit" id="addToCart" name="addToCart" value=uiLabelMap.OrderAddToCart class="+${styles.grid_columns_12}"/>
                                     <#else>
                                         <input name="quantity" id="quantity" value="1" type="hidden"/>
                                         <span>${uiLabelMap.ProductItemOutOfStock}<#if product.inventoryMessage?exists>&mdash; ${product.inventoryMessage}</#if></span>
                                     </#if>
                                 </#if>
                             </#if>
                        <#else>
                            <@field type="submit" id="addToCart" name="addToCart" value=uiLabelMap.OrderAddToCart class="+${styles.grid_columns_12}"/>                  
                        </#if>
                    </#if>
            </form>
            <#-- CATO: Review 
                <#if variantPriceList?exists>
                        <#list variantPriceList as vpricing>
                            <#assign variantName = vpricing.get("variantName")?if_exists>
                            <#assign secondVariantName = vpricing.get("secondVariantName")?if_exists>
                            <#assign minimumQuantity = vpricing.get("minimumQuantity")>
                            <#if minimumQuantity &gt; 0>
                                <div>minimum order quantity for ${secondVariantName!} ${variantName!} is ${minimumQuantity!}</div>
                            </#if>
                        </#list>
                    <#elseif minimumQuantity?exists && minimumQuantity?has_content && minimumQuantity &gt; 0>
                        <div>minimum order quantity for ${productContentWrapper.get("PRODUCT_NAME","html")?if_exists} is ${minimumQuantity!}</div>
                    </#if>
            -->
        </div>
        </@panel>
        <#-- CATO: Shopping list functionality - disabled for now
        <div id="product-shopping-list">
            <#if sessionAttributes.userLogin?has_content && sessionAttributes.userLogin.userLoginId != "anonymous">
                <form name="addToShoppingList" method="post" action="<@ofbizUrl>addItemToShoppingList<#if requestAttributes._CURRENT_VIEW_?exists>/${requestAttributes._CURRENT_VIEW_}</#if></@ofbizUrl>">
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
                        <#if product.productTypeId?if_exists == "ASSET_USAGE">
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
    <#assign prodLongDescr=productContentWrapper.get("LONG_DESCRIPTION","html")?trim/>
    <#assign prodWarnings=productContentWrapper.get("WARNINGS","html")?trim/>

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
                        <p>${downloadProductContentAndInfo.contentName?if_exists}<#if downloadProductContentAndInfo.description?has_content> - ${downloadProductContentAndInfo.description}</#if></p>
                    </#list>
                </#if>

        </div>
            
        <div class="content" id="panel21">
            <#-- Included quantities/pieces -->
            <#if product.piecesIncluded?exists && product.piecesIncluded?long != 0>
                <p id="product-specs-pieces-included">
                    ${uiLabelMap.OrderPieces}: ${product.piecesIncluded}
                </p>
            </#if>
            <#if (product.quantityIncluded?exists && product.quantityIncluded != 0) || product.quantityUomId?has_content>
                <#assign quantityUom = product.getRelatedOneCache("QuantityUom")?if_exists />
                <p id="product-specs-quantity-included">
                    ${uiLabelMap.CommonQuantity}: ${product.quantityIncluded?if_exists} ${((quantityUom.abbreviation)?default(product.quantityUomId))?if_exists}
                </p>
            </#if>
            <#if (product.weight?exists && product.weight != 0) || product.weightUomId?has_content>
                <#assign weightUom = product.getRelatedOneCache("WeightUom")?if_exists />
                <p id="product-specs-weight">
                    ${uiLabelMap.CommonWeight}: ${product.weight?if_exists} ${((weightUom.abbreviation)?default(product.weightUomId))?if_exists}
                </p>
            </#if>
            <#if (product.productHeight?exists && product.productHeight != 0) || product.heightUomId?has_content>
                <#assign heightUom = product.getRelatedOneCache("HeightUom")?if_exists />
                <p id="product-specs-height">
                    ${uiLabelMap.CommonHeight}: ${product.productHeight?if_exists} ${((heightUom.abbreviation)?default(product.heightUomId))?if_exists}
                </p>
            </#if>
            <#if (product.productWidth?exists && product.productWidth != 0) || product.widthUomId?has_content>
                <#assign widthUom = product.getRelatedOneCache("WidthUom")?if_exists />
                <p id="product-specs-width">
                    ${uiLabelMap.CommonWidth}: ${product.productWidth?if_exists} ${((widthUom.abbreviation)?default(product.widthUomId))?if_exists}
                </p>
            </#if>
            <#if (product.productDepth?exists && product.productDepth != 0) || product.depthUomId?has_content>
                <#assign depthUom = product.getRelatedOneCache("DepthUom")?if_exists />
                <p id="product-specs-depth">
                    ${uiLabelMap.CommonDepth}: ${product.productDepth?if_exists} ${((depthUom.abbreviation)?default(product.depthUomId))?if_exists}
                </p>
            </#if>

            <#if daysToShip?exists>
                <p id="product-specs-days-to-ship">${uiLabelMap.ProductUsuallyShipsIn} ${daysToShip} ${uiLabelMap.CommonDays}!</p>
            </#if>

            <#if disFeatureList?exists && 0 &lt; disFeatureList.size()>                
                <#list disFeatureList as currentFeature>
                    <#assign disFeatureType = currentFeature.getRelatedOneCache("ProductFeatureType") />
                    <p>
                        <#if disFeatureType.description?exists>${disFeatureType.get("description", locale)}<#else>${currentFeature.productFeatureTypeId}</#if>:&nbsp;${currentFeature.description}
                    </p>
                </#list>
            </#if>
        </div>
    </div>
</@section>
<@section>
        <#-- Prefill first select box (virtual products only)
        <div id="product-virtual-swatch">            
            <#if variantTree?exists && 0 &lt; variantTree.size()>
                <script type="text/javascript">eval("list" + "${featureOrderFirst}" + "()");</script>
            </#if>
    
            <#if variantSample?exists && 0 &lt; variantSample.size()>
                <#assign imageKeys = variantSample.keySet() />
                <#assign imageMap = variantSample />            
                <#assign maxIndex = 7 />
                <#assign indexer = 0 />
                <#list imageKeys as key>
                    <#assign swatchProduct = imageMap.get(key) />
                    <#if swatchProduct?has_content && indexer &lt; maxIndex>
                        <#assign imageUrl = Static["org.ofbiz.product.product.ProductContentWrapper"].getProductContentAsText(swatchProduct, "SMALL_IMAGE_URL", request,"html")?if_exists />
                        <#if !imageUrl?string?has_content>
                            <#assign imageUrl = productContentWrapper.get("SMALL_IMAGE_URL","html")?if_exists />
                        </#if>

                        <div class="product-virtual-swatch-item">
                           <a href="javascript:getList('FT${featureOrderFirst}','${indexer}',1);"><img src="<@ofbizContentUrl>${contentPathPrefix?if_exists}${imageUrl}</@ofbizContentUrl>" width="60" height="60" alt="" /></a>                        
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
                    ${setRequestAttribute("optProductId", commonFeatureResultId)}
                    ${setRequestAttribute("listIndex", commonFeatureResultId_index)}
                    ${setRequestAttribute("formNamePrefix", "cfeatcssl")}                    
                    <#-- ${setRequestAttribute("targetRequestName", targetRequestName)} -->
                    ${screens.render("component://shop/widget/CatalogScreens.xml#miniproductsummary")}
                </#list>
            </@section>
        </#if>

        <#-- Upgrades/Up-Sell/Cross-Sell -->
        <#assign productValue = product />
        <#assign listIndex = 1 />
        ${setRequestAttribute("productValue", productValue)}

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


<#-- CATO: disabled JAVASCRIPT
${virtualJavaScript?if_exists}
<script type="text/javascript">
//<![CDATA[
    var detailImageUrl = null;
    function setAddProductId(name) {
        document.addform.add_product_id.value = name;
        if (document.addform.quantity == null) return;
        if (name == '' || name == 'NULL' || isVirtual(name) == true) {
            document.addform.quantity.disabled = true;
            var elem = document.getElementById('product_id_display');
            var txt = document.createTextNode('');
            if(elem.hasChildNodes()) {
                elem.replaceChild(txt, elem.firstChild);
            } else {
                elem.appendChild(txt);
            }
        } else {
            document.addform.quantity.disabled = false;
            var elem = document.getElementById('product_id_display');
            var txt = document.createTextNode(name);
            if(elem.hasChildNodes()) {
                elem.replaceChild(txt, elem.firstChild);
            } else {
                elem.appendChild(txt);
            }
        }
    }
    function setVariantPrice(sku) {
        if (sku == '' || sku == 'NULL' || isVirtual(sku) == true) {
            var elem = document.getElementById('variant_price_display');
            var txt = document.createTextNode('');
            if(elem.hasChildNodes()) {
                elem.replaceChild(txt, elem.firstChild);
            } else {
                elem.appendChild(txt);
            }
        }
        else {
            var elem = document.getElementById('variant_price_display');
            var price = getVariantPrice(sku);
            var txt = document.createTextNode(price);
            if(elem.hasChildNodes()) {
                elem.replaceChild(txt, elem.firstChild);
            } else {
                elem.appendChild(txt);
            }
        }
    }
    function isVirtual(product) {
        var isVirtual = false;
        <#if virtualJavaScript?exists>
        for (i = 0; i < VIR.length; i++) {
            if (VIR[i] == product) {
                isVirtual = true;
            }
        }
        </#if>
        return isVirtual;
    }
    function addItem() {
       if (document.addform.add_product_id.value == 'NULL') {
           alert("Please select all of the required options.");
           return;
       } else {
           if (isVirtual(document.addform.add_product_id.value)) {
               document.location = '<@ofbizUrl>product?category_id=${categoryId?if_exists}&amp;product_id=</@ofbizUrl>' + document.addform.add_product_id.value;
               return;
           } else {
               document.addform.submit();
           }
       }
    }

    function toggleAmt(toggle) {
        if (toggle == 'Y') {
            changeObjectVisibility("add_amount", "visible");
        }

        if (toggle == 'N') {
            changeObjectVisibility("add_amount", "hidden");
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
            if (index == -1) {
              <#if featureOrderFirst?exists>
                var Variable1 = eval("list" + "${featureOrderFirst}" + "()");
              </#if>
            } else {
                var Variable1 = eval("list" + OPT[(currentFeatureIndex+1)] + selectedValue + "()");
            }
            // set the product ID to NULL to trigger the alerts
            setAddProductId('NULL');

            // set the variant price to NULL
            setVariantPrice('NULL');
        } else {
            // this is the final selection -- locate the selected index of the last selection
            var indexSelected = document.forms["addform"].elements[name].selectedIndex;

            // using the selected index locate the sku
            var sku = document.forms["addform"].elements[name].options[indexSelected].value;

            // set the product ID
            setAddProductId(sku);

            // set the variant price
            setVariantPrice(sku);

            // check for amount box
            toggleAmt(checkAmtReq(sku));
        }
    }

    function validate(x){
        var msg=new Array();
        msg[0]="Please use correct date format [yyyy-mm-dd]";

        var y=x.split("-");
        if(y.length!=3){ alert(msg[0]);return false; }
        if((y[2].length>2)||(parseInt(y[2])>31)) { alert(msg[0]); return false; }
        if(y[2].length==1){ y[2]="0"+y[2]; }
        if((y[1].length>2)||(parseInt(y[1])>12)){ alert(msg[0]); return false; }
        if(y[1].length==1){ y[1]="0"+y[1]; }
        if(y[0].length>4){ alert(msg[0]); return false; }
        if(y[0].length<4) {
            if(y[0].length==2) {
                y[0]="20"+y[0];
            } else {
                alert(msg[0]);
                return false;
            }
        }
        return (y[0]+"-"+y[1]+"-"+y[2]);
    }

    function additemSubmit(){
        <#if product.productTypeId?if_exists == "ASSET_USAGE">
        newdatevalue = validate(document.addform.reservStart.value);
        if (newdatevalue == false) {
            document.addform.reservStart.focus();
        } else {
            document.addform.reservStart.value = newdatevalue;
            document.addform.submit();
        }
        <#else>
        document.addform.submit();
        </#if>
    }

    function addShoplistSubmit(){
        <#if product.productTypeId?if_exists == "ASSET_USAGE">
        if (document.addToShoppingList.reservStartStr.value == "") {
            document.addToShoppingList.submit();
        } else {
            newdatevalue = validate(document.addToShoppingList.reservStartStr.value);
            if (newdatevalue == false) {
                document.addToShoppingList.reservStartStr.focus();
            } else {
                document.addToShoppingList.reservStartStr.value = newdatevalue;
                // document.addToShoppingList.reservStart.value = ;
                document.addToShoppingList.reservStartStr.value.slice(0,9)+" 00:00:00.000000000";
                document.addToShoppingList.submit();
            }
        }
        <#else>
        document.addToShoppingList.submit();
        </#if>
    }

    <#if product.virtualVariantMethodEnum?if_exists == "VV_FEATURETREE" && featureLists?has_content>
        function checkRadioButton() {
            var block1 = document.getElementById("addCart1");
            var block2 = document.getElementById("addCart2");
            <#list featureLists as featureList>
                <#list featureList as feature>
                    <#if feature_index == 0>
                        var myList = document.getElementById("FT${feature.productFeatureTypeId}");
                         if (myList.options[0].selected == true){
                             block1.style.display = "none";
                             block2.style.display = "block";
                             return;
                         }
                        <#break>
                    </#if>
                </#list>
            </#list>
            block1.style.display = "block";
            block2.style.display = "none";
        }
    </#if>
//]]>
 </script>

<#macro showUnavailableVarients>
  <#if unavailableVariants?exists>
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