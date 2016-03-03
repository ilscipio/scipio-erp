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

<@script>
    function selectChange(formId, elementId) {
        if (elementId.id == 'searchCatalogId') {
            if (document.getElementById('searchCategoryId').selectedIndex) {
               document.getElementById('searchCategoryId')[document.getElementById('searchCategoryId').selectedIndex].value = "";
           } else {
               document.getElementById('searchCategoryId').value = "";
           }
        }
        formId.action="<@ofbizUrl>main</@ofbizUrl>";
        formId.submit();
    }
    function submit (id) {
      var formId = id;
      if(!jQuery('#searchCatalogId').is(":empty")){
          document.getElementById(formId).submit();
      } else {
          if(jQuery('#searchCatalogId').is(":empty")) {
             jQuery('#catalogErrorMessage').fadeIn("fast");
          }
      }
    }
</@script>
<@section title=uiLabelMap.ProductAdvancedSearchInCategory>
    <form id="productSearchform" method="post" action="<@ofbizUrl>productsearch</@ofbizUrl>" name="productSearchform">
    <input type="hidden" name="productStoreId" value="${parameters.productStoreId!}" />
      <@fieldset>
        <#-- Cato: don't hardcode
        <input type="hidden" name="VIEW_SIZE" value="25"/>
        <input type="hidden" name="PAGING" value="Y"/>-->
        <input type="hidden" name="noConditionFind" value="Y"/>
          <@field type="select" label=uiLabelMap.ProductCatalog required=true name="SEARCH_CATALOG_ID" id="searchCatalogId" onChange="javascript:selectChange(document.getElementById('advToKeywordSearchform'), document.getElementById('searchCatalogId'));">
            <#list prodCatalogList as prodCatalog>
              <#assign displayDesc = prodCatalog.catalogName?default("${uiLabelMap.ProductNoDescription}") />
              <#if (18 < displayDesc?length)>
                <#assign displayDesc = displayDesc[0..15] + "...">
              </#if>
              <option value="${prodCatalog.prodCatalogId}" <#if searchCatalogId! == prodCatalog.prodCatalogId> selected="selected"</#if>>${displayDesc} [${prodCatalog.prodCatalogId}]</option>
            </#list>
          </@field>
          <@field type="generic" label=uiLabelMap.ProductCategory>
              <#if categoryIds?has_content>
                <@field type="select" name="SEARCH_CATEGORY_ID" id="searchCategoryId">
                  <option value="">- ${uiLabelMap.ProductAnyCategory} -</option>
                  <#list categoryIds as categoryId>
                    <#assign productCategory = delegator.findOne("ProductCategory", {"productCategoryId" : categoryId}, true) />
                    <#assign displayDesc = productCategory.categoryName?default("${uiLabelMap.ProductNoDescription}") />
                    <#if (18 < displayDesc?length)>
                      <#assign displayDesc = displayDesc[0..15] + "...">
                    </#if>
                    <option value="${productCategory.productCategoryId}">${displayDesc} [${productCategory.productCategoryId}]</option>
                  </#list>
                </@field>
              <#else>
                <@field type="lookup" value="${requestParameters.SEARCH_CATEGORY_ID!}" formName="productSearchform" name="SEARCH_CATEGORY_ID" id="searchCategoryId" fieldFormName="LookupProductCategory"/>
              </#if>
          </@field>
          <@field type="input" label=uiLabelMap.ProductProductName name="SEARCH_PRODUCT_NAME" size="20" value="${requestParameters.SEARCH_PRODUCT_NAME!}" />
          <@field type="input" label=uiLabelMap.ProductInternalName name="SEARCH_INTERNAL_PROD_NAME" size="20" value="${requestParameters.SEARCH_INTERNAL_PROD_NAME!}" />
          <@field type="generic" label=uiLabelMap.ProductKeywords>
              <@field type="input" name="SEARCH_STRING" size="40" value="${requestParameters.SEARCH_STRING!}" />
              <@field type="radio" name="SEARCH_OPERATOR" value="OR" checked=(searchOperator == "OR") label=uiLabelMap.CommonAny />
              <@field type="radio" name="SEARCH_OPERATOR" value="AND" checked=(searchOperator == "AND") label=uiLabelMap.CommonAll/>
          </@field>

          <#--<hr />-->
          
          <@field type="submit" submitType="link" href="javascript:submit('productSearchform');" class="+${styles.link_run_sys!} ${styles.action_find!}" text=uiLabelMap.CommonFind />
      </@fieldset>
    </form>
</@section>