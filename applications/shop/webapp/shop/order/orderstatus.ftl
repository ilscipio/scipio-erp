<#--
Licensed to the Apache Software Foundation (ASF) under one
or more contributor license agreements.  See the NOTICE file
distributed with this work for additional information
regarding copyright ownership.  The ASF licenses this file
to you under the Apache License, Version 2.0 (the
"License"); you may not use this file except in compliance
with the License.  You may obtain a copy of the License at

http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing,
software distributed under the License is distributed on an
"AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
KIND, either express or implied.  See the License for the
specific language governing permissions and limitations
under the License.
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
