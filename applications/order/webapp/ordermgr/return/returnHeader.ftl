<#--
This file is subject to the terms and conditions defined in
 file 'LICENSE', which is part of this source code package.
-->

<@section title=uiLabelMap.PageTitleReturnHeader>
        <#if returnHeader??>
            <form name="returnhead" method="post" action="<@ofbizUrl>updateReturn</@ofbizUrl>">
            <input type="hidden" name="returnId" value="${returnHeader.returnId}" />
            <input type="hidden" name="returnHeaderTypeId" value="CUSTOMER_RETURN"/>
            <input type="hidden" name="currentStatusId" value="${returnHeader.statusId!}" />
        <#else>
            <form name="returnhead" method="post" action="<@ofbizUrl>createReturn</@ofbizUrl>">
            <input type="hidden" name="returnHeaderTypeId" value="CUSTOMER_RETURN"/>
        </#if>

        <#if returnHeader??>
          <@field type="display" label=uiLabelMap.OrderReturnId>${returnHeader.returnId}</@field>
        </#if>
        <#if returnHeader??>
          <@field type="display" label=uiLabelMap.CommonCurrency>${returnHeader.currencyUomId!}</@field>
        <#else>
          <@field type="select" name="currencyUomId" label=uiLabelMap.CommonCurrency>
            <#if (orderHeader?has_content) && (orderHeader.currencyUom?has_content)>
              <option value="${orderHeader.currencyUom}" selected>${orderHeader.getRelatedOne("Uom", false).getString("description",locale)}</option>
              <option value="${orderHeader.currencyUom}">---</option>
            <#elseif defaultCurrency?has_content>
              <option value="${defaultCurrency.uomId}" selected>${defaultCurrency.getString("description")}</option>
              <option value="${defaultCurrency.uomId}">---</option>
            </#if>
            <#if currencies?has_content>
              <#list currencies as currency>
                <option value="${currency.uomId}">${currency.get("description",locale)}</option>
              </#list>
            </#if>
          </@field>
        </#if>
          <#if returnInfo.entryDate??>
            <#assign entryDate = returnInfo.get("entryDate").toString()>
          </#if>
          <@field type="datetime" label=uiLabelMap.OrderEntryDate name="entryDate" value=(entryDate!) size="25" maxlength="30" id="entryDate1"/>
          <@field type="lookup" label=uiLabelMap.OrderReturnFromParty value='${returnInfo.fromPartyId!}' formName="returnhead" name="fromPartyId" id="fromPartyId" fieldFormName="LookupPartyName"/>
          <@field type="select" name="destinationFacilityId" label=uiLabelMap.OrderReturnToFacility>
            <#if currentFacility??>
              <option value="${currentFacility.facilityId}">${currentFacility.facilityName!currentFacility.facilityId}</option>
              <option value="${currentFacility.facilityId}">---</option>
            </#if>
            <option value="">${uiLabelMap.FacilityNoFacility}</option>
            <#list facilityList as facility>
              <option value="${facility.facilityId}" <#if (facilityList?size == 1)>selected="selected"</#if>>${facility.facilityName!facility.facilityId}</option>
            </#list>
          </@field>
        <#if billingAccountList?has_content>
          <@field type="select" name="billingAccountId" label=uiLabelMap.AccountingBillingAccount>
              <#if currentAccount??>
                <option value="${currentAccount.billingAccountId}">${currentAccount.billingAccountId}: ${currentAccount.description!}</option>
                <option value="${currentAccount.billingAccountId}">---</option>
              </#if>
              <option value="">${uiLabelMap.AccountingNewBillingAccount}</option>
              <#list billingAccountList as ba>
                <option value="${ba.billingAccountId}">${ba.billingAccountId}: ${ba.description!}</option>
              </#list>
          </@field>
        <#else>
          <@field type="input" label=uiLabelMap.AccountingBillingAccount size="20" name="billingAccountId" />
        </#if>
          <@field type="generic" label=uiLabelMap.FormFieldTitle_paymentMethodId>
              <#if creditCardList?? || eftAccountList??>
                <@field type="select" name="paymentMethodId">
                  <#if currentCreditCard??>
                    <option value="${currentCreditCard.paymentMethodId}">CC:&nbsp;${Static["org.ofbiz.party.contact.ContactHelper"].formatCreditCard(currentCreditCard)}</option>
                  </#if>
                  <#if currentEftAccount??>
                    <option value="${currentEftAccount.paymentMethodId}">EFT:&nbsp;${currentEftAccount.nameOnAccount!}, ${currentEftAccount.accountNumber!}</option>
                  </#if>
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
                <@field type="input" size="20" name="paymentMethodId" value=((returnHeader.paymentMethodId)!)/>
              </#if>
              <#if (returnHeader.fromPartyId)?has_content>
                <a href="<@ofbizInterWebappUrl>/partymgr/control/editcreditcard?partyId=${returnHeader.fromPartyId}${rawString(externalKeyParam)}</@ofbizInterWebappUrl>" target="partymgr" class="${styles.link_nav!} ${styles.action_add!}">${uiLabelMap.AccountingCreateNewCreditCard}</a>
              </#if>
          </@field>
          <@field type="select" label=uiLabelMap.OrderReturnNeedsAutoReceive name="needsInventoryReceive">
            <#if needsInventoryReceive??>
              <#if "Y" == needsInventoryReceive>
                <option selected="selected" value="${needsInventoryReceive}">${uiLabelMap.CommonYes}</option>
              <#elseif "N" == needsInventoryReceive>
                <option selected="selected" value="${needsInventoryReceive}">${uiLabelMap.CommonNo}</option>
              </#if>
              <option value="${needsInventoryReceive}">---</option>
            </#if>
              <option value="Y">${uiLabelMap.CommonYes}</option>
              <option value="N">${uiLabelMap.CommonNo}</option>
          </@field>
        <#if returnHeader?has_content>
          <@field type="select" label=uiLabelMap.CommonReturnStatus name="statusId">
            <#if currentStatus??>
              <option value="${currentStatus.statusId}">${currentStatus.get("description",locale)}</option>
              <option value="${currentStatus.statusId}">---</option>
            </#if>
            <#list returnStatus as status>
              <option value="${status.statusIdTo}">${status.get("transitionName",locale)}</option>
            </#list>
          </@field>
          <@field type="display" label=uiLabelMap.FormFieldTitle_createdBy>${returnHeader.createdBy!"Unknown"}</@field>
          <@field type="generic" label=uiLabelMap.OrderReturnFromAddress>
              <#if (addressEditable)>
                <#list addresses as address >
                  <@displayAddress postalAddress = address.postalAddress editable = true/>
                </#list>
                <@field type="radio" name="originContactMechId" value="" checked=(!postalAddressFrom?has_content) label=uiLabelMap.CommonNoAddress />
              <#else>
                 <#if (postalAddressFrom?has_content)>
                   <@displayAddress postalAddress = postalAddressFrom editable = false />
                 <#else>
                   ${uiLabelMap.CommonNoAddress}
                 </#if>
              </#if>
          </@field>
          <@field type="generic" label=uiLabelMap.OrderReturnToAddress>
            <#if (postalAddressTo?has_content)>
              <@displayAddress postalAddress = postalAddressTo editable=false />
            </#if>
          </@field>
          <@field type="submit" text=uiLabelMap.CommonUpdate class="+${styles.link_run_sys!} ${styles.action_update!}"/>
        <#else>
          <input type="hidden" name="statusId" value="RETURN_REQUESTED" />
          <@field type="submit" text=uiLabelMap.CommonNew class="+${styles.link_run_sys!} ${styles.action_add!}"/>
        </#if>

        <#macro displayAddress postalAddress editable>
            <#if postalAddress?has_content>
                <div>
                  <#if (editable)>
                    <@field type="radio" name="originContactMechId" value=(postalAddress.contactMechId!) checked=(postalAddressFrom?has_content && postalAddressFrom.contactMechId?default("") == postalAddress.contactMechId) />
                  </#if>
                  <#if postalAddress.toName?has_content><span>${uiLabelMap.CommonTo}</span>&nbsp;${postalAddress.toName}<br /></#if>
                  <#if postalAddress.attnName?has_content><span>${uiLabelMap.CommonAttn}</span>&nbsp;${postalAddress.attnName}<br /></#if>
                  <#if postalAddress.address1?has_content>&nbsp;&nbsp;&nbsp;&nbsp;${postalAddress.address1}<br /></#if>
                  <#if postalAddress.address2?has_content>&nbsp;&nbsp;&nbsp;&nbsp;${postalAddress.address2}<br /></#if>
                  <#if postalAddress.city?has_content>&nbsp;&nbsp;&nbsp;&nbsp;${postalAddress.city}</#if>
                  <#if postalAddress.stateProvinceGeoId?has_content>&nbsp;${postalAddress.stateProvinceGeoId}</#if>
                  <#if postalAddress.postalCode?has_content>&nbsp;${postalAddress.postalCode}</#if>
                  <#if postalAddress.countryGeoId?has_content><br />&nbsp;&nbsp;&nbsp;&nbsp;${postalAddress.countryGeoId}</#if>
                </div>
            </#if>
        </#macro>
</@section>