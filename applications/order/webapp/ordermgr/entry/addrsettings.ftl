<#--
This file is subject to the terms and conditions defined in
 file 'LICENSE', which is part of this source code package.
-->

<#if security.hasEntityPermission("ORDERMGR", "_CREATE", session) || security.hasEntityPermission("ORDERMGR", "_PURCHASE_CREATE", session)>

  <@section title=uiLabelMap.OrderSelectAShippingAddress> <#-- class="boxoutside" -->
    <form method="post" action="<@ofbizUrl>finalizeOrder</@ofbizUrl>" name="checkoutsetupform">
      <input type="hidden" name="finalizeMode" value="ship"/>
    
      <@table type="data-complex" width="100%" class="+boxbottom"> <#-- orig: class="boxbottom" --> <#-- orig: cellspacing="0" --> <#-- orig: cellpadding="0" --> <#-- orig: border="0" -->
        <@tr type="util"><@td colspan="3"><hr /></@td></@tr>

        <#-- postal addresses for chosen id -->

        <#if partyContactMechPurposes??>
          <#list partyContactMechPurposes as partyContactMechPurpose>
            <#assign shippingAddress = partyContactMechPurpose.getRelatedOne("PostalAddress", false)/>

            <#-- skip non-postal addresses -->

            <#if shippingAddress.toName??>
              <@tr>
                <@td valign="top" nowrap="nowrap">
                  <input type="radio" name="shipping_contact_mech_id" value="${partyContactMechPurpose.contactMechId}" />
                </@td>
                <@td nowrap="nowrap">&nbsp;&nbsp;&nbsp;&nbsp;</@td>
                <@td width="100%">
                    <#if shippingAddress.toName?has_content><b>${uiLabelMap.CommonToName}:</b>&nbsp;${shippingAddress.toName}<br /></#if>
                    <#if shippingAddress.attnName?has_content><b>${uiLabelMap.CommonAttn}:</b>&nbsp;${shippingAddress.attnName}<br /></#if>
                    <#if shippingAddress.address1?has_content>${shippingAddress.address1}<br /></#if>
                    <#if shippingAddress.address2?has_content>${shippingAddress.address2}<br /></#if>
                    <#if shippingAddress.city?has_content>${shippingAddress.city}</#if>
                    <#if shippingAddress.stateProvinceGeoId?has_content><br />${shippingAddress.stateProvinceGeoId}</#if>
                    <#if shippingAddress.postalCode?has_content><br />${shippingAddress.postalCode}</#if>
                    <#if shippingAddress.countryGeoId?has_content><br />${shippingAddress.countryGeoId}</#if>
                  </@td>
              </@tr>
              <@tr type="util"><@td colspan="3"><hr /></@td></@tr>
            </#if>
          </#list>
        </#if>
      </@table>

    </form>
  </@section>

<#else>
  <@commonMsg type="error">${uiLabelMap.OrderViewPermissionError}</@commonMsg>
</#if>
