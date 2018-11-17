<#--
This file is subject to the terms and conditions defined in the
files 'LICENSE' and 'NOTICE', which are part of this source
code package.
-->

<@section title=uiLabelMap.OrderOrderHeaderInfo>
    <@table type="fields">
        <#-- order name -->
        <#if (orderName?has_content)>
            <@tr>
                <@td class="${styles.grid_large!}3">&nbsp;<b>${uiLabelMap.OrderOrderName}</b>
                </@td>
                <@td colspan="2">
                    ${orderName}
                </@td>
            </@tr>
        </#if>
        <#-- order for party -->
        <#if (orderForParty??)>
            <@tr>
                <@td class="${styles.grid_large!}3">&nbsp;<b>${uiLabelMap.OrderOrderFor}</b>
                </@td>
                <@td colspan="2">
                    ${Static["org.ofbiz.party.party.PartyHelper"].getPartyName(orderForParty, false)} [${orderForParty.partyId}]
                </@td>
            </@tr>
        </#if>
        <#if (cart.getPoNumber()?has_content)>
            <@tr>
                <@td class="${styles.grid_large!}3">&nbsp;<b>${uiLabelMap.OrderPONumber}</b>
                </@td>
                <@td colspan="2">
                    ${cart.getPoNumber()}
                </@td>
            </@tr>
        </#if>
        <#if orderTerms?has_content>
            <@tr>
                <@td class="${styles.grid_large!}3">
                    &nbsp;<b>${uiLabelMap.OrderOrderTerms}</b>
                </@td>
                <@td colspan="2">
                    <@table type="data-complex" class="+${styles.table_spacing_tiny_hint!}">
                        <@tr>
                            <@td width="35%"><b>${uiLabelMap.OrderOrderTermType}</b></@td>
                            <@td width="10%"><b>${uiLabelMap.OrderOrderTermValue}</b></@td>
                            <@td width="10%"><b>${uiLabelMap.OrderOrderTermDays}</b></@td>
                            <@td width="10%"><b>${uiLabelMap.OrderOrderTextValue}</b></@td>
                            <@td width="35%"><b>${uiLabelMap.CommonDescription}</b></@td>
                        </@tr>
                        <@tr type="util"><@td colspan="4"><hr /></@td></@tr>
                        <#assign index=0/>
                        <#list orderTerms as orderTerm>
                        <@tr>
                            <@td width="35%">${orderTerm.getRelatedOne("TermType", false).get("description",locale)}</@td>
                            <@td width="10%">${orderTerm.termValue!""}</@td>
                            <@td width="10%">${orderTerm.termDays!""}</@td>
                            <@td width="10%">${orderTerm.textValue!""}</@td>
                            <@td width="35%">${orderTerm.description!""}</@td>
                        </@tr>
                            <#if (orderTerms.size() < index)>
                        <@tr type="util"><@td colspan="5"><hr /></@td></@tr>
                            </#if>
                            <#assign index=index+1/>
                        </#list>
                    </@table>
                </@td>
            </@tr>
        </#if>
        <#-- tracking number -->
        <#if trackingNumber?has_content>
            <@tr>
                <@td class="${styles.grid_large!}3">
                    &nbsp;<b>${uiLabelMap.OrderTrackingNumber}</b>
                </@td>
                <@td colspan="2">
                    <#-- TODO: add links to UPS/FEDEX/etc based on carrier partyId  -->
                    ${trackingNumber}
                </@td>
            </@tr>
        </#if>
        <#-- splitting preference -->
            <@tr>
                <@td class="${styles.grid_large!}3">
                    &nbsp;<b>${uiLabelMap.OrderSplittingPreference}</b>
                </@td>
                <@td colspan="2">
                    
                        <#if maySplit?default("N") == "N">${uiLabelMap.FacilityWaitEntireOrderReady}</#if>
                        <#if maySplit?default("Y") == "Y">${uiLabelMap.FacilityShipAvailable}</#if>
                    
                </@td>
            </@tr>
        <#-- shipping instructions -->
        <#if shippingInstructions?has_content>
            <@tr>
                <@td class="${styles.grid_large!}3">
                    &nbsp;<b>${uiLabelMap.OrderSpecialInstructions}</b>
                </@td>
                <@td colspan="2">
                    ${shippingInstructions}
                </@td>
            </@tr>
        </#if>
        <#if orderType != "PURCHASE_ORDER" && ((productStore.showCheckoutGiftOptions)!) != "N">
        <#-- gift settings -->
            <@tr>
                <@td class="${styles.grid_large!}3">
                    &nbsp;<b>${uiLabelMap.OrderGift}</b>
                </@td>
                <@td colspan="2">
                    
                        <#if isGift?default("N") == "N">${uiLabelMap.OrderThisOrderNotGift}</#if>
                        <#if isGift?default("N") == "Y">${uiLabelMap.OrderThisOrderGift}</#if>
                    
                </@td>
            </@tr>
            <#if giftMessage?has_content>
            <@tr>
                <@td class="${styles.grid_large!}3">
                    &nbsp;<b>${uiLabelMap.OrderGiftMessage}</b>
                </@td>
                <@td colspan="2">
                    ${giftMessage}
                </@td>
            </@tr>
            </#if>
        </#if>
        <#if shipAfterDate?has_content>
            <@tr>
                <@td class="${styles.grid_large!}3">
                    &nbsp;<b>${uiLabelMap.OrderShipAfterDate}</b>
                </@td>
                <@td colspan="2">
                    ${shipAfterDate}
                </@td>
            </@tr>
        </#if>
        <#if shipBeforeDate?has_content>
            <@tr>
                <@td class="${styles.grid_large!}3">
                    &nbsp;<b>${uiLabelMap.OrderShipBeforeDate}</b>
                </@td>
                <@td colspan="2">
                  ${shipBeforeDate}
                </@td>
            </@tr>
        </#if>
    </@table>
</@section>