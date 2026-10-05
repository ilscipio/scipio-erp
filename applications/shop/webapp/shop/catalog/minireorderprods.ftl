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

<#if reorderProducts?has_content>
<@section title="${rawLabel('ProductQuickReorder')}..." id="minireorderprods">
        <#list reorderProducts as miniProduct>
          <div>
              <@render resource="component://shop/widget/CatalogScreens.xml#miniproductsummary" reqAttribs={"miniProdQuantity":reorderQuantities.get(miniProduct.productId), "miniProdFormName":"theminireorderprod" + miniProduct_index + "form", "optProductId":miniProduct.productId}/>
          </div>
          <#if miniProduct_has_next>
              
          </#if>
        </#list>
</@section>
</#if>
