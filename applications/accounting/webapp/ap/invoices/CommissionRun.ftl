<#--
This file is subject to the terms and conditions defined in the
files 'LICENSE' and 'NOTICE', which are part of this source
code package.
-->

<@script>
function toggleInvoiceId(master) {
    var form = document.listSalesInvoices;
    var invoices = form.elements.length;
    for (var i = 0; i < invoices; i++) {
        var element = form.elements[i];
        if (element.name == "invoiceIds") {
            element.checked = master.checked;
        }
    }
    enableSubmitButton();
}
function setServiceName(selection) {
    document.listSalesInvoices.action = '<@ofbizUrl>'+selection.value+'</@ofbizUrl>';
    enableSubmitButton();
}
function runAction() {
    var form = document.listSalesInvoices;
    var invoices = form.elements.length;
    for (var i = 0; i < invoices; i++) {
        var element = form.elements[i];
        if (element.name == "invoiceIds") {
            element.disabled = false;
        }
    }
    form.submit();
}
function enableSubmitButton() {
    var form = document.listSalesInvoices;
    var invoices = form.elements.length;
    var isSingle = true;
    var isAllSelected = true;
    for (var i = 0; i < invoices; i++) {
        var element = form.elements[i];
        if (element.name == "invoiceIds") {
            if (element.checked) {
                isSingle = false;
            } else {
                isAllSelected = false;
            }
        }
    }
    if (isAllSelected) {
        jQuery('#checkAllInvoices').attr('checked', true);
    } else {
        jQuery('#checkAllInvoices').attr('checked', false);
    }
    if (!isSingle && jQuery('#serviceName').val() != "") {
        jQuery('#submitButton').removeAttr("disabled"); 
    } else {
        jQuery('#submitButton').attr('disabled', true);
    }
}
</@script>

<#if invoices?has_content >
  <form name="listSalesInvoices" id="listSalesInvoices" method="post">
    <#if salesRepPartyList?has_content>
      <#assign dummy = setRequestAttribute("partyIds", salesRepPartyList)>
    </#if>
    <div align="right">
      <select name="serviceName" id="serviceName" onchange="javascript:setServiceName(this);">
        <option value="">${uiLabelMap.AccountingSelectAction}</options>
        <option value="processCommissionRun">${uiLabelMap.AccountingCommissionRun}</option>
      </select>
      <input id="submitButton" type="button" onclick="javascript:runAction();" value="${uiLabelMap.CommonRun}" disabled="disabled" />
    </div>
    <@table type="data-list" autoAltRows=true> <#-- orig: class="basic-table hover-bar" --> <#-- orig: cellspacing="0" -->
      <@thead>
      <@tr class="header-row-2">
        <@td width="9%"><input type="checkbox" id="checkAllInvoices" name="checkAllInvoices" onchange="javascript:toggleInvoiceId(this);"/> ${uiLabelMap.CommonSelectAll}</@td>
        <@td width="6%">${uiLabelMap.FormFieldTitle_invoiceId}</@td>
        <@td width="10%">${uiLabelMap.AccountingFromParty}</@td>
        <@td width="14%">${uiLabelMap.AccountingToParty}</@td>
        <@td width="4%">${uiLabelMap.CommonStatus}</@td>
        <@td width="9%">${uiLabelMap.AccountingReferenceNumber}</@td>
        <@td width="12%">${uiLabelMap.CommonDescription}</@td>
        <@td width="9%">${uiLabelMap.AccountingInvoiceDate}</@td>
        <@td width="8%">${uiLabelMap.AccountingDueDate}</@td>
        <@td width="8%">${uiLabelMap.AccountingAmount}</@td>
        <@td width="8%">${uiLabelMap.FormFieldTitle_paidAmount}</@td>
        <@td width="8%">${uiLabelMap.FormFieldTitle_outstandingAmount}</@td>
      </@tr>
      </@thead>
      <#list invoices as invoice>
        <#assign invoicePaymentInfoList = dispatcher.runSync("getInvoicePaymentInfoList", {"invoiceId":invoice.invoiceId, "userLogin":userLogin})/>
        <#assign invoicePaymentInfo = invoicePaymentInfoList.get("invoicePaymentInfoList").get(0)!>
        <#assign statusItem = delegator.findOne("StatusItem", {"statusId" : invoice.statusId}, false)!/>
        <@tr valign="middle">
          <@td><input type="checkbox" id="invoiceId_${invoice_index}" name="invoiceIds" value="${invoice.invoiceId}" onclick="javascript:enableSubmitButton();"/></@td>
          <@td><a class="${styles.link_nav_info_id!}" href="<@ofbizUrl>invoiceOverview?invoiceId=${invoice.invoiceId}</@ofbizUrl>">${invoice.get("invoiceId")}</a></@td>
          <@td><a href="<@ofbizInterWebappUrl>/partymgr/control/viewprofile?partyId=${invoice.partyIdFrom}</@ofbizInterWebappUrl>">${Static["org.ofbiz.party.party.PartyHelper"].getPartyName(delegator, invoice.partyIdFrom, false)!}</a></@td>
          <@td><a href="<@ofbizInterWebappUrl>/partymgr/control/viewprofile?partyId=${invoice.invoiceRolePartyId}</@ofbizInterWebappUrl>">${Static["org.ofbiz.party.party.PartyHelper"].getPartyName(delegator, invoice.invoiceRolePartyId, false)!}</a></@td>
          <@td>${statusItem.get("description")!}</@td>
          <@td>${invoice.get("referenceNumber")!}</@td>
          <@td>${invoice.get("description")!}</@td>
          <@td>${invoice.get("invoiceDate")!}</@td>
          <@td>${invoice.get("dueDate")!}</@td>
          <@td><@ofbizCurrency amount=invoicePaymentInfo.amount isoCode=defaultOrganizationPartyCurrencyUomId/></@td>
          <@td><@ofbizCurrency amount=invoicePaymentInfo.paidAmount isoCode=defaultOrganizationPartyCurrencyUomId/></@td>
          <@td><@ofbizCurrency amount=invoicePaymentInfo.outstandingAmount isoCode=defaultOrganizationPartyCurrencyUomId/></@td>
        </@tr>
      </#list>
    </@table>
  </form>
<#else>
  <@commonMsg type="result-norecord">${uiLabelMap.AccountingNoInvoicesFound}.</@commonMsg>
</#if>
