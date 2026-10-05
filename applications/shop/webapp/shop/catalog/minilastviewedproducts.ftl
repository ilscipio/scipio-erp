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

<#assign maxToShow = 4/>
<#assign lastViewedProducts = lastViewedProducts!sessionAttributes.lastViewedProducts!/>
<#if lastViewedProducts?has_content>
  <#if (lastViewedProducts?size > maxToShow)><#assign limit=maxToShow/><#else><#assign limit=(lastViewedProducts?size-1)/></#if>
  <#macro menuContent menuArgs={}>
    <@menu args=menuArgs>
        <@menuitem type="link" href=makePageUrl("clearLastViewed") text="[${rawLabel('CommonClear')}]" />
        <#if (lastViewedProducts?size > maxToShow)>
          <@menuitem type="link" href=makePageUrl("lastviewedproducts") text="[${rawLabel('CommonMore')}]" />
        </#if>
    </@menu>
  </#macro>
  <@section title=uiLabelMap.EcommerceLastProducts menuContent=menuContent id="minilastviewedproducts">
      <ul>
        <#list lastViewedProducts[0..limit] as productId>
          <li>
            <@render resource="component://shop/widget/CatalogScreens.xml#miniproductsummary" reqAttribs={"miniProdQuantity":"1", "optProductId":productId, "miniProdFormName":"lastviewed" + productId_index + "form"}/>
          </li>
        </#list>
      </ul>
  </@section>
</#if>
