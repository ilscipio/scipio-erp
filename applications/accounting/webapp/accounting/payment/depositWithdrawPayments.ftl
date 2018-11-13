<#--
This file is subject to the terms and conditions defined in
 file 'LICENSE', which is part of this source code package.
-->

<@script>
function togglePaymentId(master) {
    var form = document.depositWithdrawPaymentsForm;
    var payments = form.elements.length;
    for (var i = 0; i < payments; i++) {
        var element = form.elements[i];
        if (element.name == "paymentIds") {
            element.checked = master.checked;
        }
    }
    getPaymentRunningTotal();
}
function getPaymentRunningTotal() {
    var form = document.depositWithdrawPaymentsForm;
    var payments = form.elements.length;
    var isSingle = true;
    var isAllSelected = true;
    for (var i = 0; i < payments; i++) {
        var element = form.elements[i];
        if (element.name == "paymentIds") {
            if (element.checked) {
                isSingle = false;
            } else {
                isAllSelected = false;
            }
        }
    }

    if (isAllSelected) {
        jQuery('#checkAllPayments').attr('checked', true);
    } else {
        jQuery('#checkAllPayments').attr('checked', false);
    }
    if (!isSingle) {
        jQuery('#submitButton').removeAttr('disabled');
        jQuery.ajax({
            url: 'getPaymentRunningTotal',
            async: false,
            type: 'POST',
            data: jQuery('#depositWithdrawPaymentsForm').serialize(),
            success: function(data) {
                jQuery('#showPaymentRunningTotal').html(data.paymentRunningTotal);
            }
        });
    } else {
        jQuery('#showPaymentRunningTotal').html("");
        jQuery('#submitButton').attr('disabled', true);
    }
}


</@script>
<@section>
    <form id="depositWithdrawPaymentsForm" name="depositWithdrawPaymentsForm" method="post" action="<@ofbizUrl>depositWithdrawPayments</@ofbizUrl>">
        <#if paymentList?has_content>
            <input type="hidden" name="organizationPartyId" value="${organizationPartyId!}" />
            <input type="hidden" name="finAccountId" value="${finAccountId!}" />
            <input type="hidden" name="paymentMethodTypeId" value="${paymentMethodTypeId!}" />
            <input type="hidden" name="cardType" value="${cardType!}" />
            <input type="hidden" name="partyIdFrom" value="${partyIdFrom!}" />
            <input type="hidden" name="fromDate" value="${fromDate!}" />
            <input type="hidden" name="thruDate" value="${thruDate!}" />
            <input type="hidden" name="paymentGroupTypeId" value="BATCH_PAYMENT" />
            <div>
                <span>${uiLabelMap.AccountingRunningTotal} :</span>
                <span id="showPaymentRunningTotal"></span>
            </div>
            
                <div align="right">
                    <span>${uiLabelMap.AccountingPayment} ${uiLabelMap.PartyPartyGroupName}</span> <input type="text" size="25" id="paymentGroupName" name="paymentGroupName" />
                    <span>${uiLabelMap.AccountingGroupInOneTransaction}</span>
                    <input type="checkbox" name="groupInOneTransaction" value="Y" checked="checked" />
                    <input id="submitButton" type="button"  onclick="javascript:document.depositWithdrawPaymentsForm.submit();" value="${uiLabelMap.AccountingDepositWithdraw}" disabled="disabled"/>
                </div>
            <@table type="data-list" autoAltRows=true> <#-- orig: class="basic-table" -->
              <@thead>
                <@tr class="header-row-2">
                    <@th>${uiLabelMap.AccountingPayment}</@th>
                    <@th>${uiLabelMap.CommonType}</@th>
                    <@th>${uiLabelMap.CommonFrom}</@th>
                    <@th>${uiLabelMap.CommonTo}</@th>
                    <@th class="align-right">${uiLabelMap.CommonAmount}</@th>
                    <@th>${uiLabelMap.CommonDate}</@th>
                    <@th align="right">${uiLabelMap.CommonSelectAll}<input type="checkbox" id="checkAllPayments" name="checkAllPayments" onchange="javascript:togglePaymentId(this);"/></@th>
                </@tr>
                </@thead>
                <#list paymentList as payment>
                    <@tr>
                        <@td><a href="<@ofbizUrl>paymentOverview?paymentId=${payment.paymentId}</@ofbizUrl>" class="${styles.link_nav_info_id!}">${payment.paymentId}</a></@td>
                        <@td>${payment.paymentTypeDesc!}</@td>
                        <@td>${(payment.partyFromFirstName)!} ${(payment.partyFromLastName)!} ${(payment.partyFromGroupName)!}</@td>
                        <@td>${(payment.partyToFirstName)!} ${(payment.partyToLastName)!} ${(payment.partyToGroupName)!}</@td>
                        <@td class="amount"><@ofbizCurrency amount=payment.amount isoCode=payment.currencyUomId/></@td>
                        <@td>${payment.effectiveDate?date?string.short!}</@td>
                        <@td class="align-right" >${uiLabelMap.AccountingDeposit} <input type="checkbox" id="paymentId_${payment_index}" name="paymentIds" value="${payment.paymentId}" onclick="javascript:getPaymentRunningTotal();"/></@td>
                    </@tr>
                </#list>
            </@table>
        <#else>
            <@commonMsg type="result-norecord"/>
        </#if>
    </form>
</@section>
