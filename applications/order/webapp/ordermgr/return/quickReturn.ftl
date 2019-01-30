<#--
This file is subject to the terms and conditions defined in the
files 'LICENSE' and 'NOTICE', which are part of this source
code package.
-->

<@section> <#--  title=uiLabelMap.OrderReturnItems -->
        <#-- DO NOT CHANGE THE NAME OF THIS FORM, it will break the some of the multi-service pattern features -->
        <#assign selectAllFormName = "selectAllForm"/>
        <form name="selectAllForm" method="post" action="<@pageUrl>makeQuickReturn</@pageUrl>">
          <input type="hidden" name="_checkGlobalScope" value="Y"/>
          <input type="hidden" name="_useRowSubmit" value="Y"/>
          <input type="hidden" name="fromPartyId" value="${partyId!}"/>
          <input type="hidden" name="toPartyId" value="${toPartyId!}"/>
          <input type="hidden" name="orderId" value="${orderId}"/>
          <input type="hidden" name="needsInventoryReceive" value="${parameters.needsInventoryReceive!"Y"}"/>
          <input type="hidden" name="destinationFacilityId" value="${destinationFacilityId!}"/>
          <input type="hidden" name="returnHeaderTypeId" value="${returnHeaderTypeId}"/>
        <#if (orderHeader?has_content) && (orderHeader.currencyUom?has_content)>
          <input type="hidden" name="currencyUomId" value="${orderHeader.currencyUom}"/>
        </#if>

    <#include "returnItemInc.ftl"/>

        <hr />
        
    <@section>
        <#if "CUSTOMER_RETURN" == returnHeaderTypeId>
          <@field type="generic" label=uiLabelMap.FormFieldTitle_paymentMethodId>
              <#if creditCardList?? || eftAccountList??>
                <@field type="select" name="paymentMethodId">
                  <option value=""></option>
                  <#if creditCardList?has_content>
                    <#list creditCardList as creditCardPm>
                      <#assign creditCard = creditCardPm.getRelatedOne("CreditCard", false)>
                      <option value="${creditCard.paymentMethodId}">CC:&nbsp;${Static["org.ofbiz.party.contact.ContactHelper"].formatCreditCard(creditCard)}</option>
                    </#list>
                  </#if>
                  <#if eftAccountList?has_content>
                    <#list eftAccountList as eftAccount>
                      <option value="${eftAccount.paymentMethodId}">EFT:&nbsp;${eftAccount.nameOnAccount!}, ${eftAccount.accountNumber!}</option>
                    </#list>
                  </#if>
                </@field>
              <#else>
                <@field type="input" size="20" name="paymentMethodId" />
              </#if>
              <#if (party.partyId)?has_content>
                <a href="<@serverUrl>/partymgr/control/editcreditcard?partyId=${party.partyId}${rawString(externalKeyParam)}</@serverUrl>" target="partymgr" class="${styles.link_nav!} ${styles.action_add!}">${uiLabelMap.AccountingCreateNewCreditCard}</a>
              </#if>
          </@field>
        </#if>
          <#if "CUSTOMER_RETURN" == returnHeaderTypeId>
            <#assign fieldLabel = uiLabelMap.OrderReturnShipFromAddress>
          <#else>
            <#assign fieldLabel = uiLabelMap["checkhelper.select_shipping_destination"]>
          </#if>
          <@field type="generic" label=fieldLabel>
            <@table type="data-list">
              <@tbody>
              <#list shippingContactMechList as shippingContactMech>
                <#assign shippingAddress = shippingContactMech.getRelatedOne("PostalAddress", false)>
                <@tr>
                  <@td align="right" width="1%" valign="top" nowrap="nowrap">
                    <#-- FIXME? labelArea=false is somewhat hardcode -->
                    <@field type="radio" name="originContactMechId" value=shippingAddress.contactMechId checked=(shippingContactMechList?size == 1) labelArea=false />
                  </@td>
                  <@td width="99%" valign="top" nowrap="nowrap">
                      <#if shippingAddress.toName?has_content><span>${uiLabelMap.CommonTo}</span>&nbsp;${shippingAddress.toName}<br /></#if>
                      <#if shippingAddress.attnName?has_content><span>${uiLabelMap.CommonAttn}</span></b>&nbsp;${shippingAddress.attnName}<br /></#if>
                      <#if shippingAddress.address1?has_content>${shippingAddress.address1}<br /></#if>
                      <#if shippingAddress.address2?has_content>${shippingAddress.address2}<br /></#if>
                      <#if shippingAddress.city?has_content>${shippingAddress.city}</#if>
                      <#if shippingAddress.stateProvinceGeoId?has_content><br />${shippingAddress.stateProvinceGeoId}</#if>
                      <#if shippingAddress.postalCode?has_content><br />${shippingAddress.postalCode}</#if>
                      <#if shippingAddress.countryGeoId?has_content><br />${shippingAddress.countryGeoId}</#if>
                      <#--<a href="<@pageUrl>editcontactmech?DONE_PAGE=checkoutoptions&amp;contactMechId=${shippingAddress.contactMechId}</@pageUrl>" class="${styles.link_nav!} ${styles.action_update!}">[${uiLabelMap.CommonUpdate}]</a>-->
                    </@td>
                </@tr>
              </#list>
              </@tbody>
            </@table>
          </@field>
          <@field type="submit" class="+${styles.link_run_sys!} ${styles.action_add!}" text=uiLabelMap.CommonSubmit/>
        </form>
    </@section>
</@section>