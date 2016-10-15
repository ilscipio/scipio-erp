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

<#assign maxToShow = 8/>
<#assign lastViewedCategories = sessionAttributes.lastViewedCategories!/>
<#if lastViewedCategories?has_content>
  <#if (lastViewedCategories?size > maxToShow)><#assign limit=maxToShow/><#else><#assign limit=(lastViewedCategories?size-1)/></#if>
  <#macro menuContent menuArgs={}>
    <@menu args=menuArgs>
        <@menuitem type="link" href=makeOfbizUrl("clearLastViewed") text="[${uiLabelMap.CommonClear}]" />
    </@menu>      
  </#macro>
  <@section title=uiLabelMap.EcommerceLastCategories menuContent=menuContent id="minilastviewedcategories">
      <ul class="browsecategorylist">
        <#list lastViewedCategories[0..limit] as categoryId>
          <#assign category = delegator.findOne("ProductCategory", {"productCategoryId":categoryId}, true)!>
          <#if category?has_content>
            <li class="browsecategorytext">
              <#-- Scipio: NOTE: category link changed from @ofbizCatalogAltUrl to @ofbizCatalogUrl due to possible loss of browsing information by CatalogUrlFilter and consistency -->
              <#if catContentWrappers?? && catContentWrappers[category.productCategoryId]?? && catContentWrappers[category.productCategoryId].get("CATEGORY_NAME")??>
                <a href="<@ofbizCatalogUrl productCategoryId=categoryId/>" class="browsecategorybutton">${catContentWrappers[category.productCategoryId].get("CATEGORY_NAME")}</a>
              <#elseif catContentWrappers?? && catContentWrappers[category.productCategoryId]?? && catContentWrappers[category.productCategoryId].get("DESCRIPTION")??>
                <a href="<@ofbizCatalogUrl productCategoryId=categoryId/>" class="browsecategorybutton">${catContentWrappers[category.productCategoryId].get("DESCRIPTION")}</a>
              <#else>
                <a href="<@ofbizCatalogUrl productCategoryId=categoryId/>" class="browsecategorybutton">${category.description!}</a>
              </#if>
            </li>
          </#if>
        </#list>
      </ul>
  </@section>
</#if>
