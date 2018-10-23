<#-- SCIPIO -->
<@section title=uiLabelMap.CommonOverview>
  <#assign invoiceType = invoice.getRelatedOne("InvoiceType", false)/>
  <#assign currentStatus = invoice.getRelatedOne("StatusItem", false)!{}/>
  <@table type="fields">
        <@tr>
          <@td scope="row" class="${styles.grid_large!}3">${uiLabelMap.CommonStatus}</@td>
          <@td colspan="3">
          <#if currentStatus?has_content>
            <@modal id="${invoiceId}_info" label=currentStatus.get('description',locale)>
            <#-- WARN: potential var name conflict for "invoiceStatus", further checks added below in case -->
            <#if invoiceStatus?has_content>
              <ul class="no-bullet">
              <#if invoiceStatus?is_sequence>
                <#list invoiceStatus as iStatus>
                  <#assign loopStatusItem = iStatus.getRelatedOne("StatusItem", false)>
                  <li>${loopStatusItem.get("description",locale)} <#if iStatus.statusDate?has_content>- <@formattedDateTime date=iStatus.statusDate defaultVal="0000-00-00 00:00:00"/></#if></li>
                </#list>
              <#else>
                <#if (invoiceStatus.entityName!) == "InvoiceStatus">
                  <#assign iStatus = invoiceStatus>
                  <#assign loopStatusItem = iStatus.getRelatedOne("StatusItem", false)>
                  <li>${loopStatusItem.get("description",locale)} <#if iStatus.statusDate?has_content>- <@formattedDateTime date=iStatus.statusDate defaultVal="0000-00-00 00:00:00"/></#if></li>
                <#elseif (invoiceStatus.entityName!) == "StatusItem">
                  <li>${invoiceStatus.get("description",locale)}</li>
                </#if>
              </#if>
              </ul>
            <#else>
              <#-- SCIPIO: NOTE: this case shouldn't appear in real data made via createInvoice. we add a note below. -->
              <ul class="no-bullet">
                <li>${currentStatus.get("description",locale)}</li>
              </ul>
              <@commonMsg type="info"><em>${uiLabelMap.CommonNote}: ${getLabel('AccountingNoteInvoiceMissingStatusRecords', '', {"invoiceId":rawString(invoice.invoiceId)})}</em></@commonMsg>
            </#if>
            </@modal>
          </#if>
          </@td>
        </@tr>

      <#if invoice.invoiceDate?has_content>
        <@tr>
          <@td class="${styles.grid_large!}2">${uiLabelMap.CommonDate}
          </@td>
          <@td colspan="3"><@formattedDateTime date=invoice.invoiceDate /></@td>
        </@tr>
      </#if>

      <#if invoice.dueDate?has_content>
        <@tr>
          <@td class="${styles.grid_large!}2">${uiLabelMap.AccountingDueDate}
          </@td>
          <@td colspan="3"><@formattedDateTime date=invoice.dueDate /></@td>
        </@tr>
      </#if>

      <#if total?has_content>
        <@tr>
          <@td class="${styles.grid_large!}2">${uiLabelMap.CommonTotal}</@td>
          <@td colspan="3"><@ofbizCurrency isoCode=invoice.currencyUomId amount=total/></@td>
        </@tr>
      </#if>

      <#if invoiceType?has_content>
        <@tr>
          <@td class="${styles.grid_large!}2">${uiLabelMap.CommonType}
          </@td>
          <@td colspan="3">${invoiceType.get('description',locale)!}</@td>
        </@tr>
      </#if>
      
      <#if invoice.partyIdFrom?has_content>
        <@tr>
            <@td class="${styles.grid_large!}2">${uiLabelMap.CommonFrom}</@td>
            <@td colspan="3"><a href="/partymgr/control/viewprofile?partyId=${invoice.partyIdFrom!}">${partyNameResultFrom.fullName}</a></@td>
        </@tr>
      </#if>

      <#if invoice.partyId?has_content>
        <@tr>
            <@td class="${styles.grid_large!}2">${uiLabelMap.CommonTo}</@td>
            <@td colspan="3"><a href="/partymgr/control/viewprofile?partyId=${invoice.partyId!}">${partyNameResultTo.fullName}</a></@td>
        </@tr>
      </#if>

      <#if invoice.roleTypeId?has_content>
        <#assign roleType = invoice.getRelatedOne("RoleType", false)/> 
        <@tr>
            <@td class="${styles.grid_large!}2">${uiLabelMap.CommonRole}</@td>
            <@td colspan="3">${roleType.get('description',locale)}</@td>
        </@tr>
      </#if>

      <#if invoice.billingAccountId?has_content>
        <#assign billingAcct = invoice.getRelatedOne("BillingAccountAndRole", false)/>
        <@tr>
            <@td class="${styles.grid_large!}2">${uiLabelMap.CommonTo}</@td>
            <@td colspan="3"><a href="<@ofbizUrl>EditBillingAccount?billingAccountId=${invoice.billingAccountId!}</@ofbizUrl>">${billingAcct.get('description',locale)}</a></@td>
        </@tr>
      </#if>

      <#if invoice.description?has_content>
        <@tr>
            <@td class="${styles.grid_large!}2">${uiLabelMap.CommonDescription}</@td>
            <@td colspan="3">${invoice.get('description',locale)!}</@td>
        </@tr>
      </#if>
      <#if invoice.invoiceMessage?has_content>
        <@tr>
            <@td class="${styles.grid_large!}2">${uiLabelMap.CommonMessage}</@td>
            <@td colspan="3">${invoice.get('invoiceMessage',locale)!}</@td>
        </@tr>
      </#if>

      <#if invoice.referenceNumber?has_content>
        <@tr>
            <@td class="${styles.grid_large!}2">${uiLabelMap.FormFieldTitle_referenceNum}</@td>
            <@td colspan="3">${invoice.referenceNumber}</@td>
        </@tr>
      </#if>

  </@table>
</@section>