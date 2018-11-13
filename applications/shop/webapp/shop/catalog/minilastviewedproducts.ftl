<#--
This file is subject to the terms and conditions defined in
 file 'LICENSE', which is part of this source code package.
-->
<#include "component://shop/webapp/shop/catalog/catalogcommon.ftl">

<#assign maxToShow = 4/>
<#assign lastViewedProducts = sessionAttributes.lastViewedProducts!/>
<#if lastViewedProducts?has_content>
  <#if (lastViewedProducts?size > maxToShow)><#assign limit=maxToShow/><#else><#assign limit=(lastViewedProducts?size-1)/></#if>
  <#macro menuContent menuArgs={}>
    <@menu args=menuArgs>
        <@menuitem type="link" href=makeOfbizUrl("clearLastViewed") text="[${rawLabel('CommonClear')}]" />
        <#if (lastViewedProducts?size > maxToShow)>
          <@menuitem type="link" href=makeOfbizUrl("lastviewedproducts") text="[${rawLabel('CommonMore')}]" />
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
