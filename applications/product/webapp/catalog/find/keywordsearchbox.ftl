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

<form name="keywordsearchform" id="keywordSearchForm" method="post" action="<@ofbizUrl>keywordsearch?VIEW_SIZE=25&amp;PAGING=Y</@ofbizUrl>">
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
<form name="advancedsearchform" id="advancedSearchForm" method="post" action="<@ofbizUrl>advancedsearch</@ofbizUrl>">
  <@fields type="default-compact">
  <fieldset>
    <@field type="lookup" label=uiLabelMap.ProductCategoryId value=(requestParameters.SEARCH_CATEGORY_ID!) formName="advancedsearchform" name="SEARCH_CATEGORY_ID" id="searchCategoryId" fieldFormName="LookupProductCategory"/>
    <@field type="submit" submitType="link" href="javascript:document.getElementById('advancedSearchForm').submit()" class="${styles.link_run_sys!} ${styles.action_find!}" text=uiLabelMap.ProductAdvancedSearch />
  </fieldset>
  </@fields>
</form>
<form name="productjumpform" id="productJumpForm" method="post" action="<@ofbizUrl>ViewProduct</@ofbizUrl>">
  <@fields type="default-compact">
  <fieldset>
    <input type="hidden" name="viewSize" value="20" />
    <input type="hidden" name="viewIndex" value="1" />
    <@field type="lookup" value=(requestParameters.productId!) formName="productjumpform" name="productId" id="productJumpFormProductId" fieldFormName="LookupProduct"/>
    <@field type="select" name="DUMMYPAGE" id="dummyPage" onChange="submitProductJump()">
        <option value="<@ofbizUrl>ViewProduct</@ofbizUrl>">-${uiLabelMap.ProductProductJump}-</option>
        <#--<option value="<@ofbizUrl>EditProductQuickAdmin</@ofbizUrl>">${uiLabelMap.ProductQuickAdmin}</option>-->
        <option value="<@ofbizUrl>EditProduct</@ofbizUrl>">${uiLabelMap.ProductProduct}</option>
        <option value="<@ofbizUrl>EditProductPrices</@ofbizUrl>">${uiLabelMap.ProductPrices}</option>
        <option value="<@ofbizUrl>EditProductContent</@ofbizUrl>">${uiLabelMap.ProductContent}</option>
        <option value="<@ofbizUrl>EditProductGeos</@ofbizUrl>">${uiLabelMap.ProductGeos}</option>        
        <option value="<@ofbizUrl>EditProductGoodIdentifications</@ofbizUrl>">${uiLabelMap.CommonIds}</option>
        <option value="<@ofbizUrl>EditProductCategories</@ofbizUrl>">${uiLabelMap.ProductCategories}</option>
        <#--<option value="<@ofbizUrl>EditProductKeyword</@ofbizUrl>">${uiLabelMap.ProductKeywords}</option>-->
        <option value="<@ofbizUrl>EditProductAssoc</@ofbizUrl>">${uiLabelMap.ProductAssociations}</option>
        <#--<option value="<@ofbizUrl>ViewProductManufacturing</@ofbizUrl>">${uiLabelMap.ProductManufacturing}</option>-->
        <option value="<@ofbizUrl>EditProductCosts</@ofbizUrl>">${uiLabelMap.ProductCosts}</option>
        <option value="<@ofbizUrl>EditProductAttributes</@ofbizUrl>">${uiLabelMap.ProductAttributes}</option>
        <option value="<@ofbizUrl>EditProductFeatures</@ofbizUrl>">${uiLabelMap.ProductFeatures}</option>
        <option value="<@ofbizUrl>EditProductFacilities</@ofbizUrl>">${uiLabelMap.ProductFacilities}</option>
        <#--<option value="<@ofbizUrl>EditProductFacilityLocations</@ofbizUrl>">${uiLabelMap.ProductLocations}</option>-->
        <#--<option value="<@ofbizUrl>EditProductInventoryItems</@ofbizUrl>">${uiLabelMap.ProductInventory}</option>-->
        <option value="<@ofbizUrl>EditProductSuppliers</@ofbizUrl>">${uiLabelMap.ProductSuppliers}</option>
        <option value="<@ofbizUrl>ViewProductAgreements</@ofbizUrl>">${uiLabelMap.ProductAgreements}</option>
        <#--<option value="<@ofbizUrl>EditProductGlAccounts</@ofbizUrl>">${uiLabelMap.ProductAccounts}</option>-->
        <option value="<@ofbizUrl>EditProductPaymentMethodTypes</@ofbizUrl>">${uiLabelMap.ProductPaymentTypes}</option>
        <option value="<@ofbizUrl>EditProductMaints</@ofbizUrl>">${uiLabelMap.ProductMaintenance}</option>
        <#--<option value="<@ofbizUrl>EditProductMeters</@ofbizUrl>">${uiLabelMap.ProductMeters}</option>-->
        <option value="<@ofbizUrl>EditProductSubscriptionResources</@ofbizUrl>">${uiLabelMap.ProductSubscriptionResources}</option>
        <#--<option value="<@ofbizUrl>QuickAddVariants</@ofbizUrl>">${uiLabelMap.ProductVariants}</option>-->
        <option value="<@ofbizUrl>EditProductConfigs</@ofbizUrl>">${uiLabelMap.ProductConfigs}</option>
        <#--<option value="<@ofbizUrl>viewProductOrder</@ofbizUrl>">${uiLabelMap.OrderOrders}</option>-->
        <option value="<@ofbizUrl>EditProductCommunicationEvents</@ofbizUrl>">${uiLabelMap.PartyCommunications}</option>
    </@field>
  </fieldset>
  </@fields>
</form>