<#--
This file is subject to the terms and conditions defined in the
files 'LICENSE' and 'NOTICE', which are part of this source
code package.
-->
<#include "component://shop/webapp/shop/catalog/catalogcommon.ftl">

<@heading>${uiLabelMap.ProductProductsLastViewed}</@heading>

<#if sessionAttributes.lastViewedProducts?? && sessionAttributes.lastViewedProducts?has_content>
  <@table type="generic">
    <#list sessionAttributes.lastViewedProducts as productId>
      <@tr>
        <@td>
          <@render resource="component://shop/widget/CatalogScreens.xml#productsummary" reqAttribs={"optProductId":productId, "listIndex":productId_index}/>
        </@td>
      </@tr>
    </#list>
  </@table>
<#else>
  <@commonMsg type="result-norecord">${uiLabelMap.ProductNotViewedAnyProducts}.</@commonMsg>
</#if>
