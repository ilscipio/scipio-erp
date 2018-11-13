<#--
This file is subject to the terms and conditions defined in
 file 'LICENSE', which is part of this source code package.
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
