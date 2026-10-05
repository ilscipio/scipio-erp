<#--
Scipio Commerce
Copyright (C) Ilscipio GmbH

This file is part of Scipio Commerce. Scipio Commerce is free software: you
can redistribute it and modify it under the terms of the GNU Affero General
Public License, version 3, as published by the Free Software Foundation.
Scipio Commerce is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License
for more details. You should have received a copy of the license with this
work (file LICENSE). If not, see <https://www.gnu.org/licenses/agpl-3.0.html>.
A commercial license is available from Ilscipio GmbH.

SPDX-License-Identifier: AGPL-3.0-only
-->
<#include "component://shop/webapp/shop/catalog/catalogcommon.ftl">

<#assign maxToShow = 8/>
<#assign lastViewedCategories = lastViewedCategories!sessionAttributes.lastViewedCategories!/>
<#if lastViewedCategories?has_content>
  <#if (lastViewedCategories?size > maxToShow)><#assign limit=maxToShow/><#else><#assign limit=(lastViewedCategories?size-1)/></#if>
  <#macro menuContent menuArgs={}>
    <@menu args=menuArgs>
        <@menuitem type="link" href=makePageUrl("clearLastViewed") text="[${rawLabel('CommonClear')}]" />
    </@menu>      
  </#macro>
  <@section title=uiLabelMap.EcommerceLastCategories menuContent=menuContent id="minilastviewedcategories">
      <ul class="browsecategorylist">
        <#list lastViewedCategories[0..limit] as categoryId>
          <#assign category = delegator.findOne("ProductCategory", {"productCategoryId":categoryId}, true)!>
          <#if category?has_content>
            <li class="browsecategorytext">
              <#-- SCIPIO: NOTE: category link changed from @catalogAltUrl to @catalogUrl due to possible loss of browsing information by CatalogUrlFilter and consistency -->
              <#if catContentWrappers?? && catContentWrappers[category.productCategoryId]?? && catContentWrappers[category.productCategoryId].get("CATEGORY_NAME")??>
                <a href="<@catalogUrl productCategoryId=categoryId/>" class="browsecategorybutton">${catContentWrappers[category.productCategoryId].get("CATEGORY_NAME")}</a>
              <#elseif catContentWrappers?? && catContentWrappers[category.productCategoryId]?? && catContentWrappers[category.productCategoryId].get("DESCRIPTION")??>
                <a href="<@catalogUrl productCategoryId=categoryId/>" class="browsecategorybutton">${catContentWrappers[category.productCategoryId].get("DESCRIPTION")}</a>
              <#else>
                <a href="<@catalogUrl productCategoryId=categoryId/>" class="browsecategorybutton">${category.description!}</a>
              </#if>
            </li>
          </#if>
        </#list>
      </ul>
  </@section>
</#if>
