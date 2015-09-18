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
<@section title="${uiLabelMap.ProductAdvancedSearchInCategory}">
    <form name="advtokeywordsearchform" method="post" action="<@ofbizUrl>keywordsearch</@ofbizUrl>" style="margin: 0;">
      <input type="hidden" name="VIEW_SIZE" value="25"/>
      <input type="hidden" name="PAGING" value="Y"/>
      <input type="hidden" name="noConditionFind" value="Y"/>
      <#if searchCategory?has_content>
          <input type="hidden" name="SEARCH_CATEGORY_ID" value="${searchCategoryId!}"/>
      </#if>
      
        <#if searchCategory?has_content>
            <@field type="generic" label="${uiLabelMap.ProductCategory}">
                <b>"${(searchCategory.description)!}" [${(searchCategory.productCategoryId)!}]</b> ${uiLabelMap.ProductIncludeSubCategories}
                  ${uiLabelMap.CommonYes}<input type="radio" name="SEARCH_SUB_CATEGORIES" value="Y" checked="checked" />
                  ${uiLabelMap.CommonNo}<input type="radio" name="SEARCH_SUB_CATEGORIES" value="N"/>
            </@field>
        <#else>
            <@field type="generic" label="${uiLabelMap.ProductCatalog}">
                <select name="SEARCH_CATALOG_ID">
                      <option value="">- ${uiLabelMap.ProductAnyCatalog} -</option>
                      <#list prodCatalogs as prodCatalog>
                        <#assign displayDesc = prodCatalog.catalogName?default("${uiLabelMap.ProductNoDescription}")>
                          <#if 18 < displayDesc?length>
                            <#assign displayDesc = displayDesc[0..15] + "...">
                          </#if>
                          <option value="${prodCatalog.prodCatalogId}">${displayDesc} [${prodCatalog.prodCatalogId}]</option>
                      </#list>
                    </select>
            </@field>
            <@field type="generic" label="${uiLabelMap.ProductCategory}">
                <@htmlTemplate.lookupField value="${requestParameters.SEARCH_CATEGORY_ID!}" formName="advtokeywordsearchform" name="SEARCH_CATEGORY_ID" id="SEARCH_CATEGORY_ID" fieldFormName="LookupProductCategory"/>
                  ${uiLabelMap.ProductIncludeSubCategories}
                  ${uiLabelMap.CommonYes}<input type="radio" name="SEARCH_SUB_CATEGORIES" value="Y" checked="checked"/>
                  ${uiLabelMap.CommonNo}<input type="radio" name="SEARCH_SUB_CATEGORIES" value="N"/>
                  ${uiLabelMap.CommonInclude}<input type="radio" name="SEARCH_CATEGORY_EXC" value="" checked="checked"/>
                  ${uiLabelMap.CommonExclude}<input type="radio" name="SEARCH_CATEGORY_EXC" value="Y"/>
                  ${uiLabelMap.CommonAlwaysInclude}<input type="radio" name="SEARCH_CATEGORY_EXC" value="N"/>
            </@field>
        </#if>
        <@field type="generic" label="${uiLabelMap.ProductProductName}">
            <input type="text" name="SEARCH_PRODUCT_NAME" size="20" value="${requestParameters.SEARCH_PRODUCT_NAME!}"/>
        </@field>
        <@field type="generic" label="${uiLabelMap.ProductInternalName}">
            <input type="text" name="SEARCH_INTERNAL_PROD_NAME" size="20" value="${requestParameters.SEARCH_INTERNAL_PROD_NAME!}"/>
        </@field>
        <@field type="generic" label="${uiLabelMap.ProductKeywords}">
            <input type="text" name="SEARCH_STRING" size="40" value="${requestParameters.SEARCH_STRING!}"/>&nbsp;
              ${uiLabelMap.CommonAny}<input type="radio" name="SEARCH_OPERATOR" value="OR" <#if searchOperator == "OR">checked="checked"</#if>/>
              ${uiLabelMap.CommonAll}<input type="radio" name="SEARCH_OPERATOR" value="AND" <#if searchOperator == "AND">checked="checked"</#if>/>
        </@field>
        <@field type="generic" label="${uiLabelMap.ProductFeatureCategory} ${uiLabelMap.CommonIds}">
            <div>
              <input type="text" name="SEARCH_PROD_FEAT_CAT1" size="15" value="${requestParameters.SEARCH_PROD_FEAT_CAT1!}"/>&nbsp;
                  ${uiLabelMap.CommonInclude}<input type="radio" name="SEARCH_PROD_FEAT_CAT_EXC1" value="" checked="checked"/>
                  ${uiLabelMap.CommonExclude}<input type="radio" name="SEARCH_PROD_FEAT_CAT_EXC1" value="Y"/>
                  ${uiLabelMap.CommonAlwaysInclude}<input type="radio" name="SEARCH_PROD_FEAT_CAT_EXC1" value="N"/>
            </div>
            <div>
              <input type="text" name="SEARCH_PROD_FEAT_CAT2" size="15" value="${requestParameters.SEARCH_PROD_FEAT_CAT2!}"/>&nbsp;
                  ${uiLabelMap.CommonInclude}<input type="radio" name="SEARCH_PROD_FEAT_CAT_EXC2" value="" checked="checked"/>
                  ${uiLabelMap.CommonExclude}<input type="radio" name="SEARCH_PROD_FEAT_CAT_EXC2" value="Y"/>
                  ${uiLabelMap.CommonAlwaysInclude}<input type="radio" name="SEARCH_PROD_FEAT_CAT_EXC2" value="N"/>
            </div>
            <div>
              <input type="text" name="SEARCH_PROD_FEAT_CAT3" size="15" value="${requestParameters.SEARCH_PROD_FEAT_CAT3!}"/>&nbsp;
                  ${uiLabelMap.CommonInclude}<input type="radio" name="SEARCH_PROD_FEAT_CAT_EXC3" value="" checked="checked"/>
                  ${uiLabelMap.CommonExclude}<input type="radio" name="SEARCH_PROD_FEAT_CAT_EXC3" value="Y"/>
                  ${uiLabelMap.CommonAlwaysInclude}<input type="radio" name="SEARCH_PROD_FEAT_CAT_EXC3" value="N"/>
            </div>
        </@field>
        <@field type="generic" label="${uiLabelMap.ProductFeatureGroup} ${uiLabelMap.CommonIds}">
            <div>
              <input type="text" name="SEARCH_PROD_FEAT_GRP1" size="15" value="${requestParameters.SEARCH_PROD_FEAT_GRP1!}"/>&nbsp;
                  ${uiLabelMap.CommonInclude}<input type="radio" name="SEARCH_PROD_FEAT_GRP_EXC1" value="" checked="checked"/>
                  ${uiLabelMap.CommonExclude}<input type="radio" name="SEARCH_PROD_FEAT_GRP_EXC1" value="Y"/>
                  ${uiLabelMap.CommonAlwaysInclude}<input type="radio" name="SEARCH_PROD_FEAT_GRP_EXC1" value="N"/>
            </div>
            <div>
              <input type="text" name="SEARCH_PROD_FEAT_GRP2" size="15" value="${requestParameters.SEARCH_PROD_FEAT_GRP2!}"/>&nbsp;
                  ${uiLabelMap.CommonInclude}<input type="radio" name="SEARCH_PROD_FEAT_GRP_EXC2" value="" checked="checked"/>
                  ${uiLabelMap.CommonExclude}<input type="radio" name="SEARCH_PROD_FEAT_GRP_EXC2" value="Y"/>
                  ${uiLabelMap.CommonAlwaysInclude}<input type="radio" name="SEARCH_PROD_FEAT_GRP_EXC2" value="N"/>
            </div>
            <div>
              <input type="text" name="SEARCH_PROD_FEAT_GRP3" size="15" value="${requestParameters.SEARCH_PROD_FEAT_GRP3!}"/>&nbsp;
                  ${uiLabelMap.CommonInclude}<input type="radio" name="SEARCH_PROD_FEAT_GRP_EXC3" value="" checked="checked"/>
                  ${uiLabelMap.CommonExclude}<input type="radio" name="SEARCH_PROD_FEAT_GRP_EXC3" value="Y"/>
                  ${uiLabelMap.CommonAlwaysInclude}<input type="radio" name="SEARCH_PROD_FEAT_GRP_EXC3" value="N"/>
            </div>
        </@field>

        <@field type="generic" label="${uiLabelMap.ProductFeatures} ${uiLabelMap.CommonIds}">
            <div>
              <input type="text" name="SEARCH_FEAT1" size="15" value="${requestParameters.SEARCH_FEAT1!}"/>&nbsp;
                  ${uiLabelMap.CommonInclude}<input type="radio" name="SEARCH_FEAT_EXC1" value="" checked="checked"/>
                  ${uiLabelMap.CommonExclude}<input type="radio" name="SEARCH_FEAT_EXC1" value="Y"/>
                  ${uiLabelMap.CommonAlwaysInclude}<input type="radio" name="SEARCH_FEAT_EXC1" value="N"/>
            </div>
            <div>
              <input type="text" name="SEARCH_FEAT2" size="15" value="${requestParameters.SEARCH_FEAT2!}"/>&nbsp;
                  ${uiLabelMap.CommonInclude}<input type="radio" name="SEARCH_FEAT_EXC2" value="" checked="checked"/>
                  ${uiLabelMap.CommonExclude}<input type="radio" name="SEARCH_FEAT_EXC2" value="Y"/>
                  ${uiLabelMap.CommonAlwaysInclude}<input type="radio" name="SEARCH_FEAT_EXC2" value="N"/>
            </div>
            <div>
              <input type="text" name="SEARCH_FEAT3" size="15" value="${requestParameters.SEARCH_FEAT3!}"/>&nbsp;
                  ${uiLabelMap.CommonInclude}<input type="radio" name="SEARCH_FEAT_EXC3" value="" checked="checked"/>
                  ${uiLabelMap.CommonExclude}<input type="radio" name="SEARCH_FEAT_EXC3" value="Y"/>
                  ${uiLabelMap.CommonAlwaysInclude}<input type="radio" name="SEARCH_FEAT_EXC3" value="N"/>
            </div>
        </@field>
        <@field type="generic" label="${uiLabelMap.ProductListPriceRange}">
            <input type="text" name="LIST_PRICE_LOW" size="8" value="${requestParameters.LIST_PRICE_LOW!}"/>&nbsp;
              <input type="text" name="LIST_PRICE_HIGH" size="8" value="${requestParameters.LIST_PRICE_HIGH!}"/>&nbsp;
        </@field>
        <#list productFeatureTypeIdsOrdered as productFeatureTypeId>
          <#assign findPftMap = Static["org.ofbiz.base.util.UtilMisc"].toMap("productFeatureTypeId", productFeatureTypeId)>
          <#assign productFeatureType = delegator.findOne("ProductFeatureType", findPftMap, true)>
          <#assign productFeatures = productFeaturesByTypeMap[productFeatureTypeId]>
          <@field type="generic" label="${(productFeatureType.get('description',locale))!}">
              <select name="pft_${productFeatureTypeId}">
                  <option value="">- ${uiLabelMap.CommonSelectAny} -</option>
                  <#list productFeatures as productFeature>
                  <option value="${productFeature.productFeatureId}">${productFeature.description?default("${uiLabelMap.ProductNoDescription}")} [${productFeature.productFeatureId}]</option>
                  </#list>
                </select>
          </@field>
        </#list>
        <@field type="generic" label="${uiLabelMap.ProductSupplier}">
            <select name="SEARCH_SUPPLIER_ID">
                <option value="">- ${uiLabelMap.CommonSelectAny} -</option>
                <#list supplerPartyRoleAndPartyDetails as supplerPartyRoleAndPartyDetail>
                  <option value="${supplerPartyRoleAndPartyDetail.partyId}">${supplerPartyRoleAndPartyDetail.groupName!} ${supplerPartyRoleAndPartyDetail.firstName!} ${supplerPartyRoleAndPartyDetail.lastName!} [${supplerPartyRoleAndPartyDetail.partyId}]</option>
                </#list>
              </select>
        </@field>
        <@field type="generic" label="${uiLabelMap.CommonSortedBy}">
            <select name="sortOrder">
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
              </select>
              ${uiLabelMap.ProductLowToHigh}<input type="radio" name="sortAscending" value="Y" checked="checked" />
              ${uiLabelMap.ProductHighToLow}<input type="radio" name="sortAscending" value="N"/>
        </@field>
        <@field type="generic" label="${uiLabelMap.ProductPrioritizeProductsInCategory}">
            <@htmlTemplate.lookupField value="${requestParameters.PRIORITIZE_CATEGORY_ID!}" formName="advtokeywordsearchform" name="PRIORITIZE_CATEGORY_ID" id="PRIORITIZE_CATEGORY_ID" fieldFormName="LookupProductCategory"/>
        </@field>
        <@field type="generic" label="${uiLabelMap.ProductGoodIdentificationType}">
            <select name="SEARCH_GOOD_IDENTIFICATION_TYPE">
              <option value="">- ${uiLabelMap.CommonSelectAny} -</option>
              <#list goodIdentificationTypes as goodIdentificationType>
              <option value="${goodIdentificationType.goodIdentificationTypeId}">${goodIdentificationType.get("description")!}</option>
              </#list>
            </select>
        </@field>
        <@field type="generic" label="${uiLabelMap.ProductGoodIdentificationValue}">
            <input type="text" name="SEARCH_GOOD_IDENTIFICATION_VALUE" size="60" maxlength="60" value="${requestParameters.SEARCH_GOOD_IDENTIFICATION_VALUE!}"/>
            ${uiLabelMap.CommonInclude}<input type="radio" name="SEARCH_GOOD_IDENTIFICATION_INCL" value="Y" checked="checked"/>
            ${uiLabelMap.CommonExclude}<input type="radio" name="SEARCH_GOOD_IDENTIFICATION_INCL" value="N"/>
        </@field>
        <#if searchConstraintStrings?has_content>
          <@field type="generic" label="${uiLabelMap.ProductLastSearch}">
                <#list searchConstraintStrings as searchConstraintString>
                    <div>&nbsp;-&nbsp;${searchConstraintString}</div>
                </#list>
                <span>${uiLabelMap.CommonSortedBy}:</span>${searchSortOrderString}
                <div>
                  ${uiLabelMap.ProductNewSearch}<input type="radio" name="clearSearch" value="Y" checked="checked"/>
                  ${uiLabelMap.CommonRefineSearch}<input type="radio" name="clearSearch" value="N"/>
                </div>
          </@field>
        </#if>
        
        <@field type="submitarea">
            <a href="javascript:document.advtokeywordsearchform.submit()" class="${styles.button_default!}">${uiLabelMap.CommonFind}</a>
        </@field>
    </form>
</@section>
