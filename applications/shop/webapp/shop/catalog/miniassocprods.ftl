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

<#assign associatedProducts = Static["org.ofbiz.order.shoppingcart.product.ProductDisplayWorker"].getRandomCartProductAssoc(request, true)!>
<#if associatedProducts?has_content>

  <@section id="miniassocproducts" title="${rawLabel('EcommerceYouMightLike')}...">
        <#-- random complementary products -->
        <ul class="browsecategorylist">
        <#list associatedProducts as miniProduct>
            <li class="browsecategorytext">
                <@render resource="component://shop/widget/CatalogScreens.xml#miniproductsummary" reqAttribs={"miniProdQuantity":1, "miniProdFormName":"theminiassocprod" + miniProduct_index + "form", "optProductId":miniProduct.productId}/>
            </li>
        </#list>
        </ul>
  </@section>
</#if>
