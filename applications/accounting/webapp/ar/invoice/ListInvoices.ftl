<#--
This file is subject to the terms and conditions defined in the
files 'LICENSE' and 'NOTICE', which are part of this source
code package.
-->
<@script>

    function toggleInvoiceId(master) {
        var invoices = jQuery("#listInvoices :checkbox[name='invoiceIds']");

        jQuery.each(invoices, function() {
            this.checked = master.checked;
        });
        getInvoiceRunningTotal();
    }

    function getInvoiceRunningTotal() {
        var invoices = jQuery("#listInvoices :checkbox[name='invoiceIds']");

        //test if all checkboxes are checked
        var allChecked = true;
        jQuery.each(invoices, function() {
            if (!jQuery(this).is(':checked')) {
                allChecked = false;
                return false;
            }
        });

        if(allChecked) {
            jQuery('#checkAllInvoices').prop('checked', true);
        } else {
            jQuery('#checkAllInvoices').prop('checked', false);
        }

        // check if any checkbox is checked
        var anyChecked = false;
        jQuery.each(invoices, function() {
            if (jQuery(this).is(':checked')) {
                anyChecked = true;
                return false;
            }
        });
        if(anyChecked) {
            jQuery.ajax({
                url: 'getInvoiceRunningTotal',
                type: 'POST',
                async: true,
                data: jQuery('#listInvoices').serialize(),
                success: function(data) { jQuery('#showInvoiceRunningTotal').html(data.invoiceRunningTotal) }
            });

            if(jQuery('#serviceName').val() != "") {
                jQuery('#submitButton').prop('disabled', false);
            }

        } else {
            jQuery('#submitButton').prop('disabled', true);
            jQuery('#showInvoiceRunningTotal').html("");
        }
    }

    function setServiceName(selection) {
        if ( selection.value == 'massInvoicesToApprove' || selection.value == 'massInvoicesToSent' || selection.value == 'massInvoicesToReady' || selection.value == 'massInvoicesToPaid' || selection.value == 'massInvoicesToWriteoff' || selection.value == 'massInvoicesToCancel') {
            jQuery('#listInvoices').attr('action', jQuery('#invoiceStatusChange').val());
        } else {
            jQuery('#listInvoices').attr('action', selection.value);
        }
        if (selection.value == 'massInvoicesToApprove') {
            jQuery('#statusId').val("INVOICE_APPROVED");
        } else if (selection.value == 'massInvoicesToSent') {
            jQuery('#statusId').val("INVOICE_SENT");
        } else if (selection.value == 'massInvoicesToReady') {
            jQuery('#statusId').val("INVOICE_READY");
        } else if (selection.value == 'massInvoicesToPaid') {
            jQuery('#statusId').val("INVOICE_PAID");
        } else if (selection.value == 'massInvoicesToWriteoff') {
            jQuery('#statusId').val("INVOICE_WRITEOFF");
        } else if (selection.value == 'massInvoicesToCancel') {
            jQuery('#statusId').val("INVOICE_CANCELLED");
        }

        var invoices = jQuery("#listInvoices :checkbox[name='invoiceIds']");
        // check if any checkbox is checked
        var anyChecked = false;
        jQuery.each(invoices, function() {
            if (jQuery(this).is(':checked')) {
                anyChecked = true;
                return false;
            }
        });

        if(anyChecked && (jQuery('#serviceName').val() != "")) {
            jQuery('#submitButton').prop('disabled', false);
        } else {
            jQuery('#submitButton').prop('disabled', true);
        }
    }
</@script>
<#if invoices?has_content>
  <#assign invoiceList  =  invoices.getCompleteList() />
  <#assign eliClose = invoices.close() />
</#if>
<#if invoiceList?has_content && (parameters.noConditionFind!) == 'Y'>
  <div>
    <span>${uiLabelMap.AccountingRunningTotalOutstanding} :</span>
    <span id="showInvoiceRunningTotal"></span>
  </div>
  <form name="listInvoices" id="listInvoices"  method="post" action="">
    <div align="right">
      <select name="serviceName" id="serviceName" onchange="javascript:setServiceName(this);">
        <option value="">${uiLabelMap.AccountingSelectAction}</option>
        <option value="<@pageUrl>PrintInvoices</@pageUrl>">${uiLabelMap.AccountingPrintInvoices}</option>
        <option value="massInvoicesToApprove">${uiLabelMap.AccountingInvoiceStatusToApproved}</option>
        <option value="massInvoicesToSent">${uiLabelMap.AccountingInvoiceStatusToSent}</option>
        <option value="massInvoicesToReady">${uiLabelMap.AccountingInvoiceStatusToReady}</option>
        <option value="massInvoicesToPaid">${uiLabelMap.AccountingInvoiceStatusToPaid}</option>
        <option value="massInvoicesToWriteoff">${uiLabelMap.AccountingInvoiceStatusToWriteoff}</option>
        <option value="massInvoicesToCancel">${uiLabelMap.AccountingInvoiceStatusToCancelled}</option>
      </select>
      <input id="submitButton" type="button"  onclick="javascript:jQuery('#listInvoices').submit();" value="${uiLabelMap.CommonRun}" disabled="disabled" />
      <input type="hidden" name="organizationPartyId" value="${defaultOrganizationPartyId}"/>
      <input type="hidden" name="partyIdFrom" value="${parameters.partyIdFrom!}"/>
      <input type="hidden" name="statusId" id="statusId" value="${parameters.statusId!}"/>
      <input type="hidden" name="fromInvoiceDate" value="${parameters.fromInvoiceDate!}"/>
      <input type="hidden" name="thruInvoiceDate" value="${parameters.thruInvoiceDate!}"/>
      <input type="hidden" name="fromDueDate" value="${parameters.fromDueDate!}"/>
      <input type="hidden" name="thruDueDate" value="${parameters.thruDueDate!}"/>
      <input type="hidden" name="invoiceStatusChange" id="invoiceStatusChange" value="<@pageUrl>massChangeInvoiceStatus</@pageUrl>"/>
    </div>

    <@table type="data-list" autoAltRows=true>
      <@thead>
        <@tr class="header-row-2">
          <@td>${uiLabelMap.AccountingInvoice}</@td>
          <@td>${uiLabelMap.CommonDate}</@td>
          <@td>${uiLabelMap.AccountingDueDate}</@td>
          <@td>${uiLabelMap.CommonType}</@td>
          <@td>${uiLabelMap.CommonStatus}</@td>
          <@td>${uiLabelMap.CommonDescription}</@td>
          <@td>${uiLabelMap.AccountingVendorParty}</@td>
          <@td>${uiLabelMap.AccountingToParty}</@td>
          <@td class="align-right">${uiLabelMap.AccountingAmount}</@td>
          <@td class="align-right">${uiLabelMap.FormFieldTitle_paidAmount}</@td>
          <@td class="align-right">${uiLabelMap.FormFieldTitle_outstandingAmount}</@td> 
          <@td align="right">${uiLabelMap.CommonSelectAll} <input type="checkbox" id="checkAllInvoices" name="checkAllInvoices" onchange="javascript:toggleInvoiceId(this);"/></@td>
        </@tr>
      </@thead>
      <@tbody>
        <#list invoiceList as invoice>
          <#assign invoicePaymentInfoList = dispatcher.runSync("getInvoicePaymentInfoList", {"invoiceId":invoice.invoiceId, "userLogin":userLogin})/>
          <#assign invoicePaymentInfo = invoicePaymentInfoList.get("invoicePaymentInfoList").get(0)!>
            <@tr valign="middle">
              <@td><a class="${styles.link_nav_info_id!}" href="<@pageUrl>invoiceOverview?invoiceId=${invoice.invoiceId}</@pageUrl>">${invoice.get("invoiceId")}</a></@td>
              <@td>${(invoice.invoiceDate?date?string.short)!}</@td>
              <@td><#if invoice.get("dueDate")?has_content>${invoice.get("dueDate")?date?string.short}</#if></@td>
            
              <@td>
                <#assign invoiceType = delegator.findOne("InvoiceType", {"invoiceTypeId" : invoice.invoiceTypeId}, true) />
                ${invoiceType.description!invoice.invoiceTypeId}
              </@td>
              <@td>
                <#assign statusItem = delegator.findOne("StatusItem", {"statusId" : invoice.statusId}, true) />
                ${statusItem.description!invoice.statusId}
              </@td>
              <@td>${(invoice.description)!}</@td>
              <@td><a class="${styles.link_nav_info_idname!}" href="<@serverUrl>/partymgr/control/viewprofile?partyId=${invoice.partyIdFrom}</@serverUrl>">${Static["org.ofbiz.party.party.PartyHelper"].getPartyName(delegator, invoice.partyIdFrom, false)!} [${(invoice.partyIdFrom)!}]</a></@td>
              <@td><a class="${styles.link_nav_info_idname!}" href="<@serverUrl>/partymgr/control/viewprofile?partyId=${invoice.partyId}</@serverUrl>">${Static["org.ofbiz.party.party.PartyHelper"].getPartyName(delegator, invoice.partyId, false)!} [${(invoice.partyId)!}]</a></@td>
              <@td class="amount"><@ofbizCurrency amount=invoicePaymentInfo.amount isoCode=defaultOrganizationPartyCurrencyUomId/></@td>
              <@td class="amount"><@ofbizCurrency amount=invoicePaymentInfo.paidAmount isoCode=defaultOrganizationPartyCurrencyUomId/></@td>
              <@td class="amount"><@ofbizCurrency amount=invoicePaymentInfo.outstandingAmount isoCode=defaultOrganizationPartyCurrencyUomId/></@td>
              <@td align="right"><input type="checkbox" id="invoiceId_${invoice_index}" name="invoiceIds" value="${invoice.invoiceId}" onclick="javascript:getInvoiceRunningTotal();"/></@td>
            </@tr>
        </#list>
      </@tbody>
    </@table>
  </form>
<#else>
  <@commonMsg type="result-norecord">${uiLabelMap.AccountingNoInvoicesFound}.</@commonMsg>
</#if>
