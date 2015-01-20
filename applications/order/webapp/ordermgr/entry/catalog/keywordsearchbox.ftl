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

<@section title="${uiLabelMap.ProductSearchCatalog}" id="keywordsearchbox">
    <form name="keywordsearchform" id="keywordsearchbox_keywordsearchform" method="post" action="<@ofbizUrl>keywordsearch</@ofbizUrl>">
      
        <input type="hidden" name="VIEW_SIZE" value="10" />
        <input type="hidden" name="PAGING" value="Y" />
        <@field type="input" name="SEARCH_STRING" size="14" maxlength="50" value="${requestParameters.SEARCH_STRING!}" />
        
        <#if 0 &lt; otherSearchProdCatalogCategories?size>
          <@field type="select" label="${uiLabelMap.ProductCategoryId}" name="SEARCH_CATEGORY_ID" size="1">
              <option value="${searchCategoryId!}">${uiLabelMap.ProductEntireCatalog}</option>
              <#list otherSearchProdCatalogCategories as otherSearchProdCatalogCategory>
                <#assign searchProductCategory = otherSearchProdCatalogCategory.getRelatedOne("ProductCategory", true)>
                <#if searchProductCategory??>
                  <option value="${searchProductCategory.productCategoryId}">${searchProductCategory.description?default("No Description " + searchProductCategory.productCategoryId)}</option>
                </#if>
              </#list>
          </@field>
        <#else>
          <input type="hidden" name="SEARCH_CATEGORY_ID" value="${searchCategoryId!}" />
        </#if>
          <@field type="radio" name="SEARCH_OPERATOR" id="SEARCH_OPERATOR_OR" value="OR" checked=(searchOperator == "OR") label="${uiLabelMap.CommonAny}"/>
          <@field type="radio" name="SEARCH_OPERATOR" id="SEARCH_OPERATOR_AND" value="AND" checked=(searchOperator == "AND") label="${uiLabelMap.CommonAll}"/>
          <input type="submit" value="${uiLabelMap.CommonFind}" class="button" />
    </form>
    <form name="advancedsearchform" id="keywordsearchbox_advancedsearchform" method="post" action="<@ofbizUrl>advancedsearch</@ofbizUrl>">
      <@fieldset title="${uiLabelMap.CommonAdvancedSearch}" collapsed=true>
        <#if 0 &lt; otherSearchProdCatalogCategories?size>
            <@field type="select" label="${uiLabelMap.ProductAdvancedSearchIn}" name="SEARCH_CATEGORY_ID" id="SEARCH_CATEGORY_ID">
              <option value="${searchCategoryId!}">${uiLabelMap.ProductEntireCatalog}</option>
              <#list otherSearchProdCatalogCategories as otherSearchProdCatalogCategory>
                <#assign searchProductCategory = otherSearchProdCatalogCategory.getRelatedOne("ProductCategory", true)>
                <#if searchProductCategory??>
                  <option value="${searchProductCategory.productCategoryId}">${searchProductCategory.description?default("No Description " + searchProductCategory.productCategoryId)}</option>
                </#if>
              </#list>
            </@field>
        <#else>
          <input type="hidden" name="SEARCH_CATEGORY_ID" value="${searchCategoryId!}" />
        </#if>
          <input type="submit" value="${uiLabelMap.ProductAdvancedSearch}" class="button" />
      </@fieldset>
    </form>
</@section>