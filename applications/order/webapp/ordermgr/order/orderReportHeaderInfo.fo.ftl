<#--
This file is subject to the terms and conditions defined in the
files 'LICENSE' and 'NOTICE', which are part of this source
code package.
-->
<#escape x as x?xml>
<fo:block content-width="80mm" font-size="10pt">
    <fo:table table-layout="fixed" width="100%">
        <fo:table-column column-width="30mm"/>
        <fo:table-column column-width="50mm"/>
        <fo:table-body>
            <fo:table-row>
              <fo:table-cell><fo:block>${uiLabelMap.OrderDateOrdered}:</fo:block></fo:table-cell>
              <fo:table-cell><fo:block>${orderHeader.get("orderDate")!?date}</fo:block></fo:table-cell>
            </fo:table-row>
            
            <fo:table-row>
              <fo:table-cell><fo:block>${uiLabelMap.AccountingCustNr}:</fo:block></fo:table-cell>
              <fo:table-cell><fo:block><#if billToParty?has_content>${billToParty.partyId}</#if></fo:block></fo:table-cell>
            </fo:table-row>
            
            <fo:table-row>
              <fo:table-cell><fo:block>${uiLabelMap.OrderOrder} ${uiLabelMap.CommonNbr}:</fo:block></fo:table-cell>
              <fo:table-cell><fo:block><#if orderId?has_content>${orderId}</#if></fo:block></fo:table-cell>
            </fo:table-row>
            
            <!--fo:table-row>
              <fo:table-cell><fo:block>${uiLabelMap.OrderCurrentStatus}:</fo:block></fo:table-cell>
              <fo:table-cell><fo:block font-weight="bold">${currentStatus.get("description",locale)}</fo:block></fo:table-cell>
            </fo:table-row-->
            
            <#if orderItem.cancelBackOrderDate??>
            <fo:table-row>
              <fo:table-cell><fo:block>${uiLabelMap.FormFieldTitle_cancelBackOrderDate}:</fo:block></fo:table-cell>
              <fo:table-cell><fo:block><#if cancelBackOrderDate?has_content><fo:block>${orderItem.get("cancelBackOrderDate")}</fo:block></#if></fo:block></fo:table-cell>
            </fo:table-row>
            </#if>
        
            <#if orderPaymentPreferences?has_content>
                <fo:table-row>
                  <fo:table-cell><fo:block>${uiLabelMap.AccountingPaymentInformation}:</fo:block></fo:table-cell>
                  <fo:table-cell>
                    <#list orderPaymentPreferences as orderPaymentPreference>
                        <fo:block>
                            <#assign paymentMethodType = orderPaymentPreference.getRelatedOne("PaymentMethodType", false)!>
                            <#if (orderPaymentPreference?? && (orderPaymentPreference.getString("paymentMethodTypeId") == "CREDIT_CARD") && (orderPaymentPreference.getString("paymentMethodId")?has_content))>
                                <#assign creditCard = orderPaymentPreference.getRelatedOne("PaymentMethod", false).getRelatedOne("CreditCard", false)>
                                <#list Static["org.ofbiz.party.contact.ContactHelper"].formatCreditCard(creditCard)?split(" ") as sValue>
                                  ${sValue}&#x2028;
                                </#list>
                            <#else>
                                ${paymentMethodType.get("description",locale)!}
                            </#if>
                        </fo:block>
                    </#list>
                  
                  </fo:table-cell>
                </fo:table-row>
            </#if>
            
            <#if orderHeader.getString("orderTypeId") == "SALES_ORDER" && shipGroups?has_content>
                <fo:table-row>
                  <fo:table-cell><fo:block>${uiLabelMap.OrderShipmentInformation}:</fo:block></fo:table-cell>
                  <fo:table-cell  wrap-option="wrap">
                    <#list shipGroups as shipGroup>
                        <fo:block wrap-option="wrap">
                            <#if shipGroups.size() gt 1>${shipGroup.shipGroupSeqId} - </#if>
                            <#if (shipGroup.shipmentMethodTypeId)??>
                                <#assign shipMethodType = shipGroup.getRelatedOne("ShipmentMethodType", false)/>
                                <#if shipGroup.carrierPartyId?has_content>${shipGroup.carrierPartyId!""} -
                                </#if>${(shipMethodType.get("description", locale))?default(shipGroup.shipmentMethodTypeId)}
                            </#if>
                            <#if (shipGroup.shipAfterDate)?? || (shipGroup.shipByDate)??>
                                <#if (shipGroup.shipAfterDate)??>
                                - ${uiLabelMap.OrderShipAfterDate}: ${Static["org.ofbiz.base.util.UtilDateTime"].toDateString(shipGroup.shipAfterDate)}</#if><#if (shipGroup.shipByDate)??> - ${uiLabelMap.OrderShipBeforeDate}: ${Static["org.ofbiz.base.util.UtilDateTime"].toDateString(shipGroup.shipByDate)}</#if>
                            </#if>
                        </fo:block>
                    </#list>
                  </fo:table-cell>
                </fo:table-row>
            </#if>
        </fo:table-body>
    </fo:table>
</fo:block>
</#escape>