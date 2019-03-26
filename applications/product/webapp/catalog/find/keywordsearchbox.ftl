<#--
This file is subject to the terms and conditions defined in the
files 'LICENSE' and 'NOTICE', which are part of this source
code package.
-->
<#if (requestAttributes.uiLabelMap)??><#assign uiLabelMap = requestAttributes.uiLabelMap></#if>

<@script>
     function changeCategory() {
         document.forms["keywordsearchform"].elements["SEARCH_CATEGORY_ID"].value=document.forms["advancedsearchform"].elements["DUMMYCAT"].value;
         document.forms["advancedsearchform"].elements["SEARCH_CATEGORY_ID"].value=document.forms["advancedsearchform"].elements["DUMMYCAT"].value;
     }
     function submitProductJump(that) {
         jQuery('#productJumpForm input[name=productId]').val(jQuery('#productJumpForm input[name=productId]').val().replace(" ",""));
         jQuery('#productJumpForm').attr('action', jQuery('#dummyPage').val());
         jQuery('#productJumpForm').submit();
     }
</@script>

<form name="keywordsearchform" id="keywordSearchForm" method="post" action="<@pageUrl>keywordsearch?VIEW_SIZE=25&amp;PAGING=Y</@pageUrl>">
  <@fields type="default-compact">
  <fieldset>
    <@field type="input" label=uiLabelMap.ProductKeywords name="SEARCH_STRING" id="keywordSearchString" size="20" maxlength="50" value=(requestParameters.SEARCH_STRING!) />
    <@field type="lookup" label=uiLabelMap.ProductCategoryId value=(requestParameters.SEARCH_CATEGORY_ID!) formName="keywordsearchform" name="SEARCH_CATEGORY_ID" id="keywordSearchCategoryId" fieldFormName="LookupProductCategory"/>

    <@field type="generic">
      <@field type="checkbox" name="SEARCH_CONTAINS" id="keywordSearchCointains" value="N" checked=((requestParameters.SEARCH_CONTAINS!) == "N") label=uiLabelMap.CommonNoContains />
      <@field type="radio" name="SEARCH_OPERATOR" id="keywordSearchOperatorOr" value="OR" checked=((requestParameters.SEARCH_OPERATOR!) != "AND") label=uiLabelMap.CommonAny />
      <@field type="radio" name="SEARCH_OPERATOR" id="keywordSearchOperatorAnd" value="AND" checked=((requestParameters.SEARCH_OPERATOR!) == "AND") label=uiLabelMap.CommonAll />
    </@field>

    <@field type="submit" name="find" text=uiLabelMap.CommonFind class="${styles.link_run_sys!} ${styles.action_find!}" />
  </fieldset>
  </@fields>
</form>
<form name="advancedsearchform" id="advancedSearchForm" method="post" action="<@pageUrl>advancedsearch</@pageUrl>">
  <@fields type="default-compact">
  <fieldset>
    <@field type="lookup" label=uiLabelMap.ProductCategoryId value=(requestParameters.SEARCH_CATEGORY_ID!) formName="advancedsearchform" name="SEARCH_CATEGORY_ID" id="searchCategoryId" fieldFormName="LookupProductCategory"/>
    <@field type="submit" submitType="link" href="javascript:document.getElementById('advancedSearchForm').submit()" class="${styles.link_run_sys!} ${styles.action_find!}" text=uiLabelMap.ProductAdvancedSearch />
  </fieldset>
  </@fields>
</form>
<form name="productjumpform" id="productJumpForm" method="post" action="<@pageUrl>ViewProduct</@pageUrl>">
  <@fields type="default-compact">
  <fieldset>
    <input type="hidden" name="viewSize" value="20" />
    <input type="hidden" name="viewIndex" value="1" />
    <@field type="lookup" value=(requestParameters.productId!) formName="productjumpform" name="productId" id="productJumpFormProductId" fieldFormName="LookupProduct"/>
    <@field type="select" name="DUMMYPAGE" id="dummyPage" onChange="submitProductJump()">
        <option value="<@pageUrl>ViewProduct</@pageUrl>">-${uiLabelMap.ProductProductJump}-</option>
        <#--<option value="<@pageUrl>EditProductQuickAdmin</@pageUrl>">${uiLabelMap.ProductQuickAdmin}</option>-->
        <option value="<@pageUrl>EditProduct</@pageUrl>">${uiLabelMap.ProductProduct}</option>
        <option value="<@pageUrl>EditProductPrices</@pageUrl>">${uiLabelMap.ProductPrices}</option>
        <option value="<@pageUrl>EditProductContent</@pageUrl>">${uiLabelMap.ProductContent}</option>
        <option value="<@pageUrl>EditProductGeos</@pageUrl>">${uiLabelMap.ProductGeos}</option>        
        <option value="<@pageUrl>EditProductGoodIdentifications</@pageUrl>">${uiLabelMap.CommonIds}</option>
        <option value="<@pageUrl>EditProductCategories</@pageUrl>">${uiLabelMap.ProductCategories}</option>
        <#--<option value="<@pageUrl>EditProductKeyword</@pageUrl>">${uiLabelMap.ProductKeywords}</option>-->
        <option value="<@pageUrl>EditProductAssoc</@pageUrl>">${uiLabelMap.ProductAssociations}</option>
        <#--<option value="<@pageUrl>ViewProductManufacturing</@pageUrl>">${uiLabelMap.ProductManufacturing}</option>-->
        <option value="<@pageUrl>EditProductCosts</@pageUrl>">${uiLabelMap.ProductCosts}</option>
        <option value="<@pageUrl>EditProductAttributes</@pageUrl>">${uiLabelMap.ProductAttributes}</option>
        <option value="<@pageUrl>EditProductFeatures</@pageUrl>">${uiLabelMap.ProductFeatures}</option>
        <option value="<@pageUrl>EditProductFacilities</@pageUrl>">${uiLabelMap.ProductFacilities}</option>
        <#--<option value="<@pageUrl>EditProductFacilityLocations</@pageUrl>">${uiLabelMap.ProductLocations}</option>-->
        <#--<option value="<@pageUrl>EditProductInventoryItems</@pageUrl>">${uiLabelMap.ProductInventory}</option>-->
        <option value="<@pageUrl>EditProductSuppliers</@pageUrl>">${uiLabelMap.ProductSuppliers}</option>
        <option value="<@pageUrl>ViewProductAgreements</@pageUrl>">${uiLabelMap.ProductAgreements}</option>
        <#--<option value="<@pageUrl>EditProductGlAccounts</@pageUrl>">${uiLabelMap.ProductAccounts}</option>-->
        <option value="<@pageUrl>EditProductPaymentMethodTypes</@pageUrl>">${uiLabelMap.ProductPaymentTypes}</option>
        <option value="<@pageUrl>EditProductMaints</@pageUrl>">${uiLabelMap.ProductMaintenance}</option>
        <#--<option value="<@pageUrl>EditProductMeters</@pageUrl>">${uiLabelMap.ProductMeters}</option>-->
        <option value="<@pageUrl>EditProductSubscriptionResources</@pageUrl>">${uiLabelMap.ProductSubscriptionResources}</option>
        <#--<option value="<@pageUrl>QuickAddVariants</@pageUrl>">${uiLabelMap.ProductVariants}</option>-->
        <option value="<@pageUrl>EditProductConfigs</@pageUrl>">${uiLabelMap.ProductConfigs}</option>
        <#--<option value="<@pageUrl>viewProductOrder</@pageUrl>">${uiLabelMap.OrderOrders}</option>-->
        <option value="<@pageUrl>EditProductCommunicationEvents</@pageUrl>">${uiLabelMap.PartyCommunications}</option>
    </@field>
  </fieldset>
  </@fields>
</form>