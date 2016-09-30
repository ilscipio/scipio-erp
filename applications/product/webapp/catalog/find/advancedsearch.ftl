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
<@section title=uiLabelMap.ProductAdvancedSearchInCategory>
    <form name="advtokeywordsearchform" method="post" action="<@ofbizUrl>keywordsearch</@ofbizUrl>">
      <#-- Scipio: don't hardcode these
      <input type="hidden" name="VIEW_SIZE" value="25"/>
      <input type="hidden" name="PAGING" value="Y"/>-->
      <input type="hidden" name="noConditionFind" value="Y"/>
      <#if searchCategory?has_content>
          <input type="hidden" name="SEARCH_CATEGORY_ID" value="${searchCategoryId!}"/>
      </#if>
      
        <#if searchCategory?has_content>
            <@field type="generic" label=uiLabelMap.ProductCategory>
                <b>"${(searchCategory.description)!}" [${(searchCategory.productCategoryId)!}]</b> ${uiLabelMap.ProductIncludeSubCategories}
                <@field type="radio" name="SEARCH_SUB_CATEGORIES" value="Y" checked=true label=uiLabelMap.CommonYes/>
                <@field type="radio" name="SEARCH_SUB_CATEGORIES" value="N" label=uiLabelMap.CommonNo/>
            </@field>
        <#else>
            <@field type="select" label=uiLabelMap.ProductCatalog name="SEARCH_CATALOG_ID">
              <option value="">- ${uiLabelMap.ProductAnyCatalog} -</option>
              <#list prodCatalogs as prodCatalog>
                <#assign displayDesc = prodCatalog.catalogName?default("${uiLabelMap.ProductNoDescription}")>
                  <#if 18 < displayDesc?length>
                    <#assign displayDesc = displayDesc[0..15] + "...">
                  </#if>
                  <option value="${prodCatalog.prodCatalogId}">${displayDesc} [${prodCatalog.prodCatalogId}]</option>
              </#list>
            </@field>
            <@field type="generic" label=uiLabelMap.ProductCategory>
                <@field type="datetime" value=(requestParameters.SEARCH_CATEGORY_ID!) formName="advtokeywordsearchform" name="SEARCH_CATEGORY_ID" id="SEARCH_CATEGORY_ID" fieldFormName="LookupProductCategory"/>
                ${uiLabelMap.ProductIncludeSubCategories}
                <@field type="radio" name="SEARCH_SUB_CATEGORIES" value="Y" checked=true label=uiLabelMap.CommonYes/>
                <@field type="radio" name="SEARCH_SUB_CATEGORIES" value="N" label=uiLabelMap.CommonNo/>
                <@field type="radio" name="SEARCH_CATEGORY_EXC" value="" checked=true label=uiLabelMap.CommonInclude/>
                <@field type="radio" name="SEARCH_CATEGORY_EXC" value="Y" label=uiLabelMap.CommonExclude/>
                <@field type="radio" name="SEARCH_CATEGORY_EXC" value="N" label=uiLabelMap.CommonAlwaysInclude/>
            </@field>
        </#if>
        <@field type="input" label=uiLabelMap.ProductProductName name="SEARCH_PRODUCT_NAME" size="20" value=(requestParameters.SEARCH_PRODUCT_NAME!)/>
        <@field type="input" label=uiLabelMap.ProductInternalName name="SEARCH_INTERNAL_PROD_NAME" size="20" value=(requestParameters.SEARCH_INTERNAL_PROD_NAME!)/>
        <@field type="generic" label=uiLabelMap.ProductKeywords>
            <@field type="input" name="SEARCH_STRING" size="40" value=(requestParameters.SEARCH_STRING!)/>&nbsp;
            <@field type="radio" name="SEARCH_OPERATOR" value="OR" checked=(searchOperator == "OR") label=uiLabelMap.CommonAny/>
            <@field type="radio" name="SEARCH_OPERATOR" value="AND" checked=(searchOperator == "AND") label=uiLabelMap.CommonAll/>
        </@field>
        <@field type="generic" label="${rawString(uiLabelMap.ProductFeatureCategory)} ${rawString(uiLabelMap.CommonIds)}">
            <div>
              <@field type="input" name="SEARCH_PROD_FEAT_CAT1" size="15" value=(requestParameters.SEARCH_PROD_FEAT_CAT1!)/>&nbsp;
              <@field type="radio" name="SEARCH_PROD_FEAT_CAT_EXC1" value="" checked=true label=uiLabelMap.CommonInclude/>
              <@field type="radio" name="SEARCH_PROD_FEAT_CAT_EXC1" value="Y" label=uiLabelMap.CommonExclude/>
              <@field type="radio" name="SEARCH_PROD_FEAT_CAT_EXC1" value="N" label=uiLabelMap.CommonAlwaysInclude/>
            </div>
            <div>
              <@field type="input" name="SEARCH_PROD_FEAT_CAT2" size="15" value=(requestParameters.SEARCH_PROD_FEAT_CAT2!)/>&nbsp;
              <@field type="radio" name="SEARCH_PROD_FEAT_CAT_EXC2" value="" checked=true label=uiLabelMap.CommonInclude/>
              <@field type="radio" name="SEARCH_PROD_FEAT_CAT_EXC2" value="Y" label=uiLabelMap.CommonExclude/>
              <@field type="radio" name="SEARCH_PROD_FEAT_CAT_EXC2" value="N" label=uiLabelMap.CommonAlwaysInclude/>
            </div>
            <div>
              <@field type="input" name="SEARCH_PROD_FEAT_CAT3" size="15" value=(requestParameters.SEARCH_PROD_FEAT_CAT3!)/>&nbsp;
              <@field type="radio" name="SEARCH_PROD_FEAT_CAT_EXC3" value="" checked=true label=uiLabelMap.CommonInclude/>
              <@field type="radio" name="SEARCH_PROD_FEAT_CAT_EXC3" value="Y" label=uiLabelMap.CommonExclude/>
              <@field type="radio" name="SEARCH_PROD_FEAT_CAT_EXC3" value="N" label=uiLabelMap.CommonAlwaysInclude/>
            </div>
        </@field>
        <@field type="generic" label="${rawString(uiLabelMap.ProductFeatureGroup)} ${rawString(uiLabelMap.CommonIds)}">
            <div>
              <@field type="input" name="SEARCH_PROD_FEAT_GRP1" size="15" value=(requestParameters.SEARCH_PROD_FEAT_GRP1!)/>&nbsp;
              <@field type="radio" name="SEARCH_PROD_FEAT_GRP_EXC1" value="" checked=true label=uiLabelMap.CommonInclude/>
              <@field type="radio" name="SEARCH_PROD_FEAT_GRP_EXC1" value="Y" label=uiLabelMap.CommonExclude/>
              <@field type="radio" name="SEARCH_PROD_FEAT_GRP_EXC1" value="N" label=uiLabelMap.CommonAlwaysInclude/>
            </div>
            <div>
              <@field type="input" name="SEARCH_PROD_FEAT_GRP2" size="15" value=(requestParameters.SEARCH_PROD_FEAT_GRP2!)/>&nbsp;
              <@field type="radio" name="SEARCH_PROD_FEAT_GRP_EXC2" value="" checked=true label=uiLabelMap.CommonInclude/>
              <@field type="radio" name="SEARCH_PROD_FEAT_GRP_EXC2" value="Y" label=uiLabelMap.CommonExclude/>
              <@field type="radio" name="SEARCH_PROD_FEAT_GRP_EXC2" value="N" label=uiLabelMap.CommonAlwaysInclude/>
            </div>
            <div>
              <@field type="input" name="SEARCH_PROD_FEAT_GRP3" size="15" value=(requestParameters.SEARCH_PROD_FEAT_GRP3!)/>&nbsp;
              <@field type="radio" name="SEARCH_PROD_FEAT_GRP_EXC3" value="" checked=true label=uiLabelMap.CommonInclude/>
              <@field type="radio" name="SEARCH_PROD_FEAT_GRP_EXC3" value="Y" label=uiLabelMap.CommonExclude/>
              <@field type="radio" name="SEARCH_PROD_FEAT_GRP_EXC3" value="N" label=uiLabelMap.CommonAlwaysInclude/>
            </div>
        </@field>

        <@field type="generic" label="${rawString(uiLabelMap.ProductFeatures)} ${rawString(uiLabelMap.CommonIds)}">
            <div>
              <@field type="input" name="SEARCH_FEAT1" size="15" value=(requestParameters.SEARCH_FEAT1!)/>&nbsp;
              <@field type="radio" name="SEARCH_FEAT_EXC1" value="" checked=true label=uiLabelMap.CommonInclude/>
              <@field type="radio" name="SEARCH_FEAT_EXC1" value="Y" label=uiLabelMap.CommonExclude/>
              <@field type="radio" name="SEARCH_FEAT_EXC1" value="N" label=uiLabelMap.CommonAlwaysInclude/>
            </div>
            <div>
              <@field type="input" name="SEARCH_FEAT2" size="15" value=(requestParameters.SEARCH_FEAT2!)/>&nbsp;
              <@field type="radio" name="SEARCH_FEAT_EXC2" value="" checked=true label=uiLabelMap.CommonInclude/>
              <@field type="radio" name="SEARCH_FEAT_EXC2" value="Y" label=uiLabelMap.CommonExclude/>
              <@field type="radio" name="SEARCH_FEAT_EXC2" value="N" label=uiLabelMap.CommonAlwaysInclude/>
            </div>
            <div>
              <@field type="input" name="SEARCH_FEAT3" size="15" value=(requestParameters.SEARCH_FEAT3!)/>&nbsp;
              <@field type="radio" name="SEARCH_FEAT_EXC3" value="" checked=true label=uiLabelMap.CommonInclude/>
              <@field type="radio" name="SEARCH_FEAT_EXC3" value="Y" label=uiLabelMap.CommonExclude/>
              <@field type="radio" name="SEARCH_FEAT_EXC3" value="N" label=uiLabelMap.CommonAlwaysInclude/>
            </div>
        </@field>
        <@field type="generic" label=uiLabelMap.ProductListPriceRange>
            <@field type="input" name="LIST_PRICE_LOW" size="8" value=(requestParameters.LIST_PRICE_LOW!)/>&nbsp;
            <@field type="input" name="LIST_PRICE_HIGH" size="8" value=(requestParameters.LIST_PRICE_HIGH!)/>&nbsp;
        </@field>
        <#list productFeatureTypeIdsOrdered as productFeatureTypeId>
          <#assign findPftMap = {"productFeatureTypeId":productFeatureTypeId}>
          <#assign productFeatureType = delegator.findOne("ProductFeatureType", findPftMap, true)>
          <#assign productFeatures = productFeaturesByTypeMap[productFeatureTypeId]>
          <@field type="select" label=((productFeatureType.get('description',locale))!) name="pft_${productFeatureTypeId}">
              <option value="">- ${uiLabelMap.CommonSelectAny} -</option>
              <#list productFeatures as productFeature>
              <option value="${productFeature.productFeatureId}">${productFeature.description?default("${uiLabelMap.ProductNoDescription}")} [${productFeature.productFeatureId}]</option>
              </#list>
          </@field>
        </#list>
        <@field type="select" label=uiLabelMap.ProductSupplier name="SEARCH_SUPPLIER_ID">
            <option value="">- ${uiLabelMap.CommonSelectAny} -</option>
            <#list supplerPartyRoleAndPartyDetails as supplerPartyRoleAndPartyDetail>
              <option value="${supplerPartyRoleAndPartyDetail.partyId}">${supplerPartyRoleAndPartyDetail.groupName!} ${supplerPartyRoleAndPartyDetail.firstName!} ${supplerPartyRoleAndPartyDetail.lastName!} [${supplerPartyRoleAndPartyDetail.partyId}]</option>
            </#list>
        </@field>
        <@field type="generic" label=uiLabelMap.CommonSortedBy>
            <@field type="select" name="sortOrder">
                <option value="SortKeywordRelevancy">${uiLabelMap.ProductKeywordRelevancy}</option>
                <option value="SortProductField:productName">${uiLabelMap.ProductProductName}</option>
                <option value="SortProductField:internalName">${uiLabelMap.ProductInternalName}</option>
                <option value="SortProductField:totalQuantityOrdered">${uiLabelMap.ProductPopularityByOrders}</option>
                <option value="SortProductField:totalTimesViewed">${uiLabelMap.ProductPopularityByViews}</option>
                <option value="SortProductField:averageCustomerRating">${uiLabelMap.ProductCustomerRating}</option>
                <option value="SortProductPrice:LIST_PRICE">${uiLabelMap.ProductListPrice}</option>
                <option value="SortProductPrice:DEFAULT_PRICE">${uiLabelMap.ProductDefaultPrice}</option>
                <option value="SortProductPrice:AVERAGE_COST">${uiLabelMap.ProductAverageCost}</option>
                <option value="SortProductPrice:MINIMUM_PRICE">${uiLabelMap.ProductMinimumPrice}</option>
                <option value="SortProductPrice:MAXIMUM_PRICE">${uiLabelMap.ProductMaximumPrice}</option>
            </@field>
            <@field type="radio" name="sortAscending" value="Y" checked=true  label=uiLabelMap.ProductLowToHigh/>
            <@field type="radio" name="sortAscending" value="N" label=uiLabelMap.ProductHighToLow/>
        </@field>
        <@field type="lookup" label=uiLabelMap.ProductPrioritizeProductsInCategory value=(requestParameters.PRIORITIZE_CATEGORY_ID!) formName="advtokeywordsearchform" name="PRIORITIZE_CATEGORY_ID" id="PRIORITIZE_CATEGORY_ID" fieldFormName="LookupProductCategory"/>
        <@field type="select" label=uiLabelMap.ProductGoodIdentificationType name="SEARCH_GOOD_IDENTIFICATION_TYPE">
            <option value="">- ${uiLabelMap.CommonSelectAny} -</option>
            <#list goodIdentificationTypes as goodIdentificationType>
              <option value="${goodIdentificationType.goodIdentificationTypeId}">${goodIdentificationType.get("description")!}</option>
            </#list>
        </@field>
        <@field type="generic" label=uiLabelMap.ProductGoodIdentificationValue>
            <@field type="input" name="SEARCH_GOOD_IDENTIFICATION_VALUE" size="60" maxlength="60" value=(requestParameters.SEARCH_GOOD_IDENTIFICATION_VALUE!)/>
            <@field type="radio" name="SEARCH_GOOD_IDENTIFICATION_INCL" value="Y" checked=true label=uiLabelMap.CommonInclude/>
            <@field type="radio" name="SEARCH_GOOD_IDENTIFICATION_INCL" value="N" label=uiLabelMap.CommonExclude/>
        </@field>
        <#if searchConstraintStrings?has_content>
          <@field type="generic" label=uiLabelMap.ProductLastSearch>
            <#list searchConstraintStrings as searchConstraintString>
                <div>&nbsp;-&nbsp;${searchConstraintString}</div>
            </#list>
            <span>${uiLabelMap.CommonSortedBy}:</span>${searchSortOrderString}
            <div>
              <@field type="radio" name="clearSearch" value="Y" checked=true label=uiLabelMap.ProductNewSearch/>
              <@field type="radio" name="clearSearch" value="N" label=uiLabelMap.CommonRefineSearch/>
            </div>
          </@field>
        </#if>
        
        <@field type="submit" submitType="link" href="javascript:document.advtokeywordsearchform.submit()" class="+${styles.link_run_sys!} ${styles.action_find!}" text=uiLabelMap.CommonFind />
    </form>
</@section>
