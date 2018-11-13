<#--
This file is subject to the terms and conditions defined in the
files 'LICENSE' and 'NOTICE', which are part of this source
code package.
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
