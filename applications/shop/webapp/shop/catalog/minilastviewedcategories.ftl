<#--
This file is subject to the terms and conditions defined in the
files 'LICENSE' and 'NOTICE', which are part of this source
code package.
-->
<#include "component://shop/webapp/shop/catalog/catalogcommon.ftl">

<#assign maxToShow = 8/>
<#assign lastViewedCategories = lastViewedCategories!sessionAttributes.lastViewedCategories!/>
<#if lastViewedCategories?has_content>
  <#if (lastViewedCategories?size > maxToShow)><#assign limit=maxToShow/><#else><#assign limit=(lastViewedCategories?size-1)/></#if>
  <#macro menuContent menuArgs={}>
    <@menu args=menuArgs>
        <@menuitem type="link" href=makeOfbizUrl("clearLastViewed") text="[${rawLabel('CommonClear')}]" />
    </@menu>      
  </#macro>
  <@section title=uiLabelMap.EcommerceLastCategories menuContent=menuContent id="minilastviewedcategories">
      <ul class="browsecategorylist">
        <#list lastViewedCategories[0..limit] as categoryId>
          <#assign category = delegator.findOne("ProductCategory", {"productCategoryId":categoryId}, true)!>
          <#if category?has_content>
            <li class="browsecategorytext">
              <#-- SCIPIO: NOTE: category link changed from @ofbizCatalogAltUrl to @ofbizCatalogUrl due to possible loss of browsing information by CatalogUrlFilter and consistency -->
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
