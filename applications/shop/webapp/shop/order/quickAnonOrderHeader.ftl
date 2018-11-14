<#--
This file is subject to the terms and conditions defined in the
files 'LICENSE' and 'NOTICE', which are part of this source
code package.
-->
<#include "component://shop/webapp/shop/order/ordercommon.ftl">

<#-- SCIPIO: DEPRECATED TEMPLATE -->

<#-- NOTE: this template is used for the orderstatus screen in shop AND for order notification emails through the OrderNoticeEmail.ftl file -->
<#-- the "urlPrefix" value will be prepended to URLs by the ofbizUrl transform if/when there is no "request" object in the context -->
<#if baseEcommerceSecureUrl??><#assign urlPrefix = baseEcommerceSecureUrl/></#if>

<@row>
  <@cell columns=6>
    <#macro menuContent menuArgs={}>
      <@menu args=menuArgs>
        <#if (maySelectItems!"N") == "Y" && (returnLink!"N") == "Y" && ((orderHeader.statusId)!) == "ORDER_COMPLETED">
          <@menuitem type="link" href=makeOfbizUrl("makeReturn?orderId=${orderHeader.orderId}") class="+${styles.action_nav!}" text=uiLabelMap.OrderRequestReturn />
        </#if>
      </@menu>
    </#macro>
    <#assign sectionTitle>${getLabel('OrderOrder')} <#if orderHeader?has_content>${getLabel('CommonNbr')}<a href="<@ofbizUrl>orderstatus?orderId=${orderHeader.orderId}</@ofbizUrl>" class="${styles.link_nav_info_id!}">${orderHeader.orderId}</a> </#if>${getLabel('CommonInformation')}</#assign>
    <@section title=wrapAsRaw(sectionTitle, 'htmlmarkup') menuContent=menuContent>
        <@table type="fields"> <#-- orig: width="100%" border="0" cellpadding="1" -->
            <#-- placing customer information -->
            <#if localOrderReadHelper?? && orderHeader?has_content>
              <#assign displayParty = localOrderReadHelper.getPlacingParty()!/>
              <#if displayParty?has_content>
                  <#assign displayPartyNameResult = dispatcher.runSync("getPartyNameForDate", {"partyId":displayParty.partyId, "compareDate":orderHeader.orderDate, "userLogin":userLogin})/>
              </#if>
              <@tr>
                <@td align="right" valign="top" width="15%"><b>${uiLabelMap.PartyName}</b>
                </@td>
                <@td valign="top" width="80%">
                    ${(displayPartyNameResult.fullName)!"[Name Not Found]"}
                </@td>
              </@tr>
              <@tr type="util"><@td colspan="7"><hr /></@td></@tr>
            </#if>
            <#-- order status information -->
            <@tr>
              <@td align="right" valign="top" width="15%"><b>${uiLabelMap.CommonStatus}</b>
              </@td>
              <@td valign="top" width="80%">
                <#if orderHeader?has_content>
                  <div>${localOrderReadHelper.getStatusString(locale)}</div>
                <#else>
                  <div><b>${uiLabelMap.OrderNotYetOrdered}</b></div>
                </#if>
              </@td>
            </@tr>
            <#-- ordered date -->
            <#if orderHeader?has_content>
              <@tr type="util"><@td colspan="7"><hr /></@td></@tr>
              <@tr>
                <@td align="right" valign="top" width="15%"><b>${uiLabelMap.CommonDate}</b>
                </@td>
                <@td valign="top" width="80%">${orderHeader.orderDate.toString()}
                </@td>
              </@tr>
            </#if>
            <#if distributorId??>
              <@tr type="util"><@td colspan="7"><hr /></@td></@tr>
              <@tr>
                <@td align="right" valign="top" width="15%"><b>${uiLabelMap.OrderDistributor}</b>
                </@td>
                <@td valign="top" width="80%">${distributorId}
                </@td>
              </@tr>
            </#if>
        </@table>
    </@section>

    <@render resource="component://shop/widget/OrderScreens.xml#quickAnonPaymentInformation" />

  </@cell>

    
  <@cell columns=6>
    
      <#if orderItemShipGroups?has_content>
        <@section title=uiLabelMap.OrderShippingInformation>

            <#-- shipping address -->
            <#assign groupIdx = 0>
            <#list orderItemShipGroups as shipGroup>
                <#if orderHeader?has_content>
                  <#assign shippingAddress = shipGroup.getRelatedOne("PostalAddress", false)!>
                  <#assign groupNumber = shipGroup.shipGroupSeqId!>
                <#else>
                  <#assign shippingAddress = cart.getShippingAddress(groupIdx)!>
                  <#assign groupNumber = groupIdx + 1>
                </#if>

              <@table type="fields"> <#-- orig: width="100%" border="0" cellpadding="1" -->
                <#if shippingAddress?has_content>
                  <@tr>
                    <@td align="right" valign="top" width="15%">&nbsp;<b>${uiLabelMap.OrderDestination}</b> [${groupNumber}]
                    </@td>
                    <@td valign="top" width="80%">
                        <#if shippingAddress.toName?has_content><b>${uiLabelMap.CommonTo}:</b> ${shippingAddress.toName}<br /></#if>
                        <#if shippingAddress.attnName?has_content><b>${uiLabelMap.PartyAddrAttnName}:</b> ${shippingAddress.attnName}<br /></#if>
                        ${shippingAddress.address1}<br />
                        <#if shippingAddress.address2?has_content>${shippingAddress.address2}<br /></#if>
                        ${shippingAddress.city}<#if shippingAddress.stateProvinceGeoId?has_content>, ${shippingAddress.stateProvinceGeoId} </#if>
                        ${shippingAddress.postalCode!}<br />
                        ${shippingAddress.countryGeoId!}
                    </@td>
                  </@tr>
                  <@tr type="util"><@td colspan="7"><hr /></@td></@tr>
                </#if>
                  <@tr><@td colspan="7">
                     <@render resource="component://shop/widget/OrderScreens.xml#quickAnonOptionSettings" />
                  </@td></@tr>
              </@table>

                <#assign groupIdx = groupIdx + 1>
            </#list><#-- end list of orderItemShipGroups -->

        </@section>
      </#if>

  </@cell>
</@row>
