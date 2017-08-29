
<#-- SCIPIO: common shop catalog definitions and macros -->

<#include "component://shop/webapp/shop/common/common.ftl">

<#-- TODO: params instead of context vars -->
<#macro productDetailImages>
    <#-- FIXME: these ?trim should be removed and handled elsewhere or differently because
        they cause coercion to string -->
    <#--
    <#assign productAdditionalSmallImage1 = productContentWrapper.get("XTRA_IMG_1_SMALL","url")! />
    <#assign productAdditionalSmallImage2 = productContentWrapper.get("XTRA_IMG_2_SMALL","url")! />
    <#assign productAdditionalSmallImage3 = productContentWrapper.get("XTRA_IMG_3_SMALL","url")! />
    <#assign productAdditionalSmallImage4 = productContentWrapper.get("XTRA_IMG_4_SMALL","url")! />-->

    <#assign productAdditionalImage1 = productContentWrapper.get("ADDITIONAL_IMAGE_1","url")! />
    <#assign productAdditionalImage2 = productContentWrapper.get("ADDITIONAL_IMAGE_2","url")! />
    <#assign productAdditionalImage3 = productContentWrapper.get("ADDITIONAL_IMAGE_3","url")! />
    <#assign productAdditionalImage4 = productContentWrapper.get("ADDITIONAL_IMAGE_4","url")! />
    <#assign productLargeImageUrl = productContentWrapper.get("LARGE_IMAGE_URL","url")! /> 
    <#assign productOriginalImage = productContentWrapper.get("ORIGINAL_IMAGE_URL","url")! />
    
    <#if firstLargeImage?has_content>
        <#assign productLargeImageUrl = firstLargeImage />
    </#if>
    <#if productLargeImageUrl?has_content>
        <#assign largeImage = makeOfbizContentCtxPrefixUrl(productLargeImageUrl)/>
    <#else>
        <#assign largeImage = "https://placehold.it/800x300">
    </#if>
    <@img src=largeImage width="100%" height="300px" type="contain" class="product-image"/>
    
    <#-- SCIPIO: We are using the clearing mechanism in foundation here. This may be migrated to the grid macro at a later stage. -->
    <#if productAdditionalImage1?has_content>
      <div class="product-image-thumbs">
        <ul class="clearing-thumbs" data-clearing>
            <#if productAdditionalImage1?has_content>
                <#assign largeImage = makeOfbizContentCtxPrefixUrl(productAdditionalImage1)/>
                <li><@img src=largeImage link=largeImage width="auto" height="80px" type="cover" class=""/></li>
            </#if>
            <#if productAdditionalImage2?has_content>
                <#assign largeImage = makeOfbizContentCtxPrefixUrl(productAdditionalImage2)/>
               <li><@img src=largeImage link=largeImage width="auto" height="80px" type="cover"/></li>
            </#if>
            <#if productAdditionalImage3?has_content>
                <#assign largeImage = makeOfbizContentCtxPrefixUrl(productAdditionalImage3)/>
                <li><@img src=largeImage link=largeImage width="auto" height="80px" type="cover"/></li>
            </#if>
            <#if productAdditionalImage4?has_content>
                <#assign largeImage = makeOfbizContentCtxPrefixUrl(productAdditionalImage4)/>
                <li><@img src=largeImage link=largeImage width="auto" height="80px" type="cover"/></li>
            </#if>
        </ul>
      </div>
    </#if>
</#macro>

<#macro productDetailLongDescContent>
    <#-- Long description of product -->
    <p>${prodLongDescr!""}</p>
    <#if prodWarnings?has_content><@alert type="warning">${prodWarnings!""}</@alert></#if>

    <#-- Digital Download Files Associated with this Product -->
    <#if downloadProductContentAndInfoList?has_content>            
        <@heading relLevel=+2>${uiLabelMap.OrderDownloadableFileTitles}:</@heading><#--${uiLabelMap.OrderDownloadFilesTitle}-->
        <ol>
          <#list downloadProductContentAndInfoList as downloadProductContentAndInfo>
            <li><i>${downloadProductContentAndInfo.contentName!}</i><#if downloadProductContentAndInfo.description?has_content> - ${downloadProductContentAndInfo.description}</#if></li>
          </#list>
        </ol>
    </#if>
</#macro>

<#macro productDetailProductAttribContent>
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

    <#-- SCIPIO: Debugging info
    <@heading relLevel=+1>Debugging Info</@heading>
    <p style="font-size:0.7em;">Product ID: ${product.productId}</p>
    <p style="font-size:0.7em;">Product info map: ${product?string}</p>
    -->
</#macro>






<#-- migrated from productdetail.ftl -->
<#macro associatedProducts assocProducts beforeName showName afterName formNamePrefix targetRequestName>
      <#assign pageProduct = product />
      <#assign targetRequest = "product" />
      <#if targetRequestName?has_content>
        <#assign targetRequest = targetRequestName />
      </#if>
      <#if assocProducts?has_content>
        <#assign assocTitle>${rawString(beforeName)}<#if showName == "Y">${rawString(productContentWrapper.get("PRODUCT_NAME")!)}</#if>${rawString(afterName)}</#assign>
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
                      <#else>
                        <#assign dummy = setRequestAttribute("targetRequestName", "")>
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

<#-- migrated from productdetail.ftl -->
<#macro commonAssociatedProducts productValue commonFeatureResultIds=[]>

    <#-- special cross/up-sell area using commonFeatureResultIds (from common feature product search) -->

    <#if commonFeatureResultIds?has_content>            
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
    <#assign listIndex = 1 />
    <#assign dummy = setRequestAttribute("productValue", productValue)>

    <#-- also bought -->
    <@associatedProducts assocProducts=alsoBoughtProducts beforeName="" showName="N" afterName="${rawLabel('ProductAlsoBought')}" formNamePrefix="albt" targetRequestName="" />
    <#-- obsolete -->
    <@associatedProducts assocProducts=obsoleteProducts beforeName="" showName="Y" afterName=" ${rawLabel('ProductObsolete')}" formNamePrefix="obs" targetRequestName="" />
    <#-- cross sell -->
    <@associatedProducts assocProducts=crossSellProducts beforeName="" showName="N" afterName="${rawLabel('ProductCrossSell')}" formNamePrefix="cssl" targetRequestName="crosssell" />
    <#-- up sell -->
    <@associatedProducts assocProducts=upSellProducts beforeName="${rawLabel('ProductUpSell')} " showName="Y" afterName=":" formNamePrefix="upsl" targetRequestName="upsell" />
    <#-- obsolescence -->
    <@associatedProducts assocProducts=obsolenscenseProducts beforeName="" showName="Y" afterName=" ${rawLabel('ProductObsolescense')}" formNamePrefix="obce" targetRequestName="" />
</#macro>

<#-- Makes options for insert within <@field type="select">. Used by keywordsearch & advancedsearch. WARN: uses context fields. -->
<#macro productSortOrderSelectOptions sortOrder sortAscending showAdv="" showAdvDef=false extraArgs...>
    <#if !showAdv?is_boolean>
      <#local showAdv = showAdvFields!showAdvDef><#-- CONTEXT field -->
    </#if>
    <#local sortOrder = rawString(sortOrder)>
    <#-- SCIPIO: NOTE: removed the high-to-low checkbox in favor of dedicated options, because the box
        is not used in all sorting methods and it takes space -->
    <option value="SortKeywordRelevancy"<#if sortOrder == "SortKeywordRelevancy"> selected="selected"</#if>><#if showAdv>${uiLabelMap.ProductKeywordRelevancy}<#else>${uiLabelMap.ProductRelevance}</#if></option>
    <option value="SortProductField:productName"<#if sortOrder == "SortProductField:productName"> selected="selected"</#if>>${uiLabelMap.ProductProductName}</option>
  <#-- TODO/FIXME: 2017-08-18: search can't currently honor this; should be fixed in future...
    <option value="SortProductField:totalQuantityOrdered">${uiLabelMap.ProductPopularityByOrders}</option>
    <option value="SortProductField:totalTimesViewed">${uiLabelMap.ProductPopularityByViews}</option>
    <option value="SortProductField:averageCustomerRating">${uiLabelMap.ProductCustomerRating}</option>
  -->
    <#-- NOTE: shorter label if showAdvFields==false -->
  <#if showAdv>
    <option value="SortProductPrice:LIST_PRICE#ASC"<#if sortOrder == "SortProductPrice:LIST_PRICE" && sortAscending> selected="selected"</#if>><#if showAdv>${uiLabelMap.ProductListPrice}<#else>${uiLabelMap.ProductPrice}</#if>: ${uiLabelMap.EcommerceLowToHigh}</option>
    <option value="SortProductPrice:LIST_PRICE#DESC"<#if sortOrder == "SortProductPrice:LIST_PRICE" && !sortAscending> selected="selected"</#if>><#if showAdv>${uiLabelMap.ProductListPrice}<#else>${uiLabelMap.ProductPrice}</#if>: ${uiLabelMap.EcommerceHighToLow}</option>
  </#if>
    <option value="SortProductPrice:DEFAULT_PRICE#ASC"<#if sortOrder == "SortProductPrice:DEFAULT_PRICE" && sortAscending> selected="selected"</#if>><#if showAdv>${uiLabelMap.ProductDefaultPrice}<#else>${uiLabelMap.ProductPrice}</#if>: ${uiLabelMap.EcommerceLowToHigh}</option>
    <option value="SortProductPrice:DEFAULT_PRICE#DESC"<#if sortOrder == "SortProductPrice:DEFAULT_PRICE" && !sortAscending> selected="selected"</#if>><#if showAdv>${uiLabelMap.ProductDefaultPrice}<#else>${uiLabelMap.ProductPrice}</#if>: ${uiLabelMap.EcommerceHighToLow}</option>
  <#-- TODO/FIXME: 2017-08-18: search can't currently honor this; should be fixed in future...
    <#if productFeatureTypes?? && productFeatureTypes?has_content>
      <#list productFeatureTypes as productFeatureType>
        <option value="SortProductFeature:${productFeatureType.productFeatureTypeId}">${productFeatureType.description!productFeatureType.productFeatureTypeId}</option>
      </#list>
    </#if>
  -->
</#macro>
<#macro productSortOrderSelectScript id formId submitForm=true extraArgs...>
    <@script>
        jQuery(document).ready(function() {
            var endsWith = function(str, suffix) {
                return str.indexOf(suffix, str.length - suffix.length) !== -1;
            };
            var sortOrderChange = function(elem, submit) {
                elem = jQuery(elem);
                var sortOrder = elem.val();
                var asc = true;
                if (endsWith(sortOrder, "#ASC")) {
                    sortOrder = sortOrder.substring(0, sortOrder.length-4);
                } else if (endsWith(sortOrder, "#DESC")) {
                    sortOrder = sortOrder.substring(0, sortOrder.length-5);
                    asc = false;
                }
                var formElem = jQuery('#${escapeVal(formId, 'js')}');
                jQuery('input[name=sortOrder]', formElem).val(sortOrder);
                jQuery('input[name=sortAscending]', formElem).val(asc ? "Y" : "N");
              <#if submitForm>
                if (submit) {
                    formElem.submit();
                }
              </#if>
            };
            var sortOrderElem = jQuery('#${escapeVal(id, 'js')}');
            sortOrderChange(sortOrderElem, false);
            sortOrderElem.change(function() { sortOrderChange(this, true); });
        });
    </@script>
</#macro>

<#function getProductCategoryDisplayName cat>
    <#if isObjectType("string", cat)>
      <#local cat = delegator.findOne("ProductCategory", {"productCategoryId":cat}, true)!>
      <#if !cat?has_content>
        <#return cat>
      </#if>
    </#if>
    <#local catName = Static["org.ofbiz.product.category.CategoryContentWrapper"].getProductCategoryContentAsText(cat, "CATEGORY_NAME", locale, dispatcher, "raw")!>
    <#if !catName?has_content>
        <#local catName = Static["org.ofbiz.product.category.CategoryContentWrapper"].getProductCategoryContentAsText(cat, "DESCRIPTION", locale, dispatcher, "raw")!>
        <#if !catName?has_content>
           <#local catName = cat.productCategoryId!>
        </#if>
    </#if>
    <#return catName>
</#function>
