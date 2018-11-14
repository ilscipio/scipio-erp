<#--
This file is subject to the terms and conditions defined in the
files 'LICENSE' and 'NOTICE', which are part of this source
code package.
-->
<#include "component://shop/webapp/shop/order/ordercommon.ftl">

<#if orderHeader?has_content>
  <#-- SCIPIO: NOTE: this condition came from ofbiz patch. TODO: review if we want it/safe
  <#if (maySelectItems!"N") == "Y">-->
  
  <#--</#if>-->
    <@render resource="component://shop/widget/OrderScreens.xml#orderheader" />
    <form name="addCommonToCartForm" action="<@ofbizUrl>addordertocart/orderstatus</@ofbizUrl>" method="post">
        <input type="hidden" name="add_all" value="false" />
        <input type="hidden" name="orderId" value="${orderHeader.orderId}" />
        <@render resource="component://shop/widget/OrderScreens.xml#orderitems" />
        <#--<#if (maySelectItems!"N") == "Y">-->
    </form>
  <#--</#if>-->
<#else>
  <@commonMsg type="error">${uiLabelMap.OrderSpecifiedNotFound}.</@commonMsg>
</#if>
