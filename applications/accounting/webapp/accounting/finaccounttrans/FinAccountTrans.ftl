<#--
Licensed to the Apache Software Foundation (ASF) under one
or more contributor license agreements.  See the NOTICE file
distributed with this work for additional information
regarding copyright ownership.  The ASF licenses this file
to you under the Apache License, Version 2.0 (the
"License"); you may not use this file except in compliance
with the License.  You may obtain a copy of the License at

http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing,
software distributed under the License is distributed on an
"AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
KIND, either express or implied.  See the License for the
specific language governing permissions and limitations
under the License.
-->

<#-- FIXME: this is not being called in any way right now, @field type 'checkbox'  doesn't accept JS events. On the other hand, we must
    determine if whatever this functions do, makes sense to do it in that way -->  
<@script>
function togglefinAccountTransId(master) {
    var form = document.selectAllForm;
    var finAccountTransList = form.elements.length;
    for (var i = 0; i < finAccountTransList; i++) {
        var element = form.elements[i];
        if (element.type == "checkbox") {
            element.checked = master.checked;
        }
    }
    getFinAccountTransRunningTotalAndBalances();
}


function getFinAccountTransRunningTotalAndBalances() {
    console.log("getFinAccountTransRunningTotalAndBalances init");
    var form = document.selectAllForm;
    var finAccountTransList = form.elements.length;
    var isSingle = true;
    var isAllSelected = true;
    for (var i = 0; i < finAccountTransList; i++) {
        var element = form.elements[i];
        if (jQuery(element[name^="_rowSubmit_o_"])) {
            if (element.checked) {
                isSingle = false;
            } else {
                isAllSelected = false;
            }
        }
    }
    if (isAllSelected) {
        jQuery('#checkAllTransactions').attr('checked', true);
    } else {
        jQuery('#checkAllTransactions').attr('checked', false);
    }
    if (!isSingle) {
        jQuery('#submitButton').removeAttr('disabled');
        if (jQuery('#showFinAccountTransRunningTotal').length) {
            jQuery.ajax({
                url: 'getFinAccountTransRunningTotalAndBalances',
                async: false,
                type: 'POST',
                data: jQuery('#listFinAccTra').serialize(),
                success: function(data) {
                    jQuery('#showFinAccountTransRunningTotal').html(data.finAccountTransRunningTotal);
                    jQuery('#finAccountTransRunningTotal').html(data.finAccountTransRunningTotal);
                    jQuery('#numberOfFinAccountTransaction').html(data.numberOfTransactions);
                    jQuery('#endingBalance').html(data.endingBalance);
                }
            });
        }
    } else {
        if (jQuery('#showFinAccountTransRunningTotal').length) {
            jQuery('#showFinAccountTransRunningTotal').html("");
            jQuery('#finAccountTransRunningTotal').html("");
            jQuery('#numberOfFinAccountTransaction').html("");
            jQuery('#endingBalance').html(jQuery('#endingBalanceInput').val());

        }
        jQuery('#submitButton').attr('disabled', true);
    }
}
</@script>

<@section>
    <#if finAccountTransList?has_content && parameters.noConditionFind?? && parameters.noConditionFind == 'Y'>
        <#if !grandTotal??>
            <h2 id="showFinAccountTransRunningTotal">${uiLabelMap.AccountingRunningTotal} :</h2>             
        </#if>
        <form id="listFinAccTra" name="selectAllForm" method="post" action="<@ofbizUrl><#if !grandTotal??>reconcileFinAccountTrans?clearAll=Y<#else>assignGlRecToFinAccTrans?clearAll=Y</#if></@ofbizUrl>">
            <input name="_useRowSubmit" type="hidden" value="Y"/>
            <input name="finAccountId" type="hidden" value="${parameters.finAccountId}"/>
            <input name="statusId" type="hidden" value="${parameters.statusId!}"/>
            <#if !grandTotal??>
                <input name="reconciledBalance" type="hidden" value="${(glReconciliation.reconciledBalance)!}"/>
                <input name="reconciledBalanceWithUom" type="hidden" id="reconciledBalanceWithUom" value="<@ofbizCurrency amount=(glReconciliation.reconciledBalance)?default('0')/>"/>
            </#if>
        
            <#-- FIXME: Again, we must determine if this makes sense to diplay it in here, imo it breaks the UX. Note that 'Not Assigned' should be selected for the glReconciliationId in the search filter -->
            <#assign glReconciliations = delegator.findByAnd("GlReconciliation", {"glAccountId" : finAccount.postToGlAccountId!, "statusId" : "GLREC_CREATED"}, Static["org.ofbiz.base.util.UtilMisc"].toList("reconciledDate DESC"), false)>
            <#if (glReconciliationId?has_content && (glReconciliationId == "_NA_" && finAccountTransList?has_content)) || !grandTotal??>       
                <@row>
                    <@cell class="+${styles.text_right!}">         
                        <#if grandTotal??>
                            <#if glReconciliations?has_content>
                                <@field type="select" name="glReconciliationId">
                                    <option value="">--${uiLabelMap.CommonSelect}--</option>
                                    <#list glReconciliations as glReconciliation>
                                        <option value="${glReconciliation.glReconciliationId}">${glReconciliation.glReconciliationName!}[[${glReconciliation.glReconciliationId}] [${glReconciliation.reconciledDate!}] [${glReconciliation.reconciledBalance!}]]</option>
                                    </#list>
                                </@field>
                                <@field type="submit" id="submitButton" text=uiLabelMap.AccountingAssignToReconciliation disabled=true onClick="javascript:document.selectAllForm.submit();" class="+${styles.link_run_sys!} ${styles.action_updatestatus!}" />                                                                
                            <#else>
                                ${uiLabelMap.AccountingNoGlReconciliatio??} <a href="<@ofbizUrl>EditFinAccountReconciliations?finAccountId=${parameters.finAccountId!}</@ofbizUrl>" class="${styles.link_nav!} ${styles.action_update!}">${uiLabelMap.CommonClickHere}</a>
                            </#if>
                        <#else>
                            <@field type="submit" id="submitButton" onClick="javascript:document.selectAllForm.submit();" text=uiLabelMap.AccountingReconcile disabled=true class="+${styles.link_run_sys!} ${styles.action_update!}" />
                        </#if>
                    </@cell>
                </@row>
            </#if>
    
            <@table type="data-list" autoAltRows=true responsive=true scrollable=true> <#-- orig: class="basic-table hover-bar" --> <#-- orig: cellspacing="0" -->
                <#-- Header Begins -->
                <@thead>
                    <@tr class="header-row-2">
                        <@th>${uiLabelMap.CommonId}</@th>
                        <@th>${uiLabelMap.CommonType}</@th>
                        <@th>${uiLabelMap.CommonParty}</@th>
                        <@th>${uiLabelMap.CommonName}</@th>
                        <@th>${uiLabelMap.FormFieldTitle_transactionDate}</@th>
                        <@th>${uiLabelMap.FormFieldTitle_entryDate}</@th>
                        <@th class="align-right">${uiLabelMap.CommonAmount}</@th>
                        <@th>${uiLabelMap.CommonPayment}</@th>
                        <@th>${uiLabelMap.OrderPaymentType}</@th>
                        <@th>${uiLabelMap.CommonMethod}</@th>
                        <@th>${uiLabelMap.CommonStatus}</@th>
                        <@th>${uiLabelMap.CommonComments}</@th>
                        <#if grandTotal??>
                            <@th>${uiLabelMap.AccountingCancelTransactionStatus}</@th>
                        </#if>
                        <#if !grandTotal??>
                            <#if (parameters.glReconciliationId?has_content && parameters.glReconciliationId != "_NA_")>
                                <@th>${uiLabelMap.AccountingRemoveFromGlReconciliation}</@th>
                            </#if>
                        </#if>
                        <#-- FIXME: Another topic for debate, should we allow toggle buttons in the grid headers, if so, how should we render them? -->
                        <#if ((glReconciliationId?has_content && glReconciliationId == "_NA_") && (glReconciliations?has_content && finAccountTransList?has_content)) || !grandTotal??>
                            <@th>                                
                                <@field type="checkbox" name="selectAll" value="N" id="checkAllTransactions" onClick="javascript:togglefinAccountTransId(this);" label=uiLabelMap.CommonSelectAll/>
                            </@th>
                        </#if>
                    </@tr>
                </@thead>
                <#-- Header Ends-->
                <@tbody>
                    <#list finAccountTransList as finAccountTrans>
                        <#assign payment = "">
                        <#assign payments = "">
                        <#assign status = "">
                        <#assign paymentType = "">
                        <#assign paymentMethodType = "">
                        <#assign glReconciliation = "">
                        <#assign partyName = "">
                        <#if finAccountTrans.paymentId?has_content>                            
                            <#assign payment = delegator.findOne("Payment", {"paymentId" : finAccountTrans.paymentId}, true)>
                        <#else>                            
                            <#assign payments = delegator.findByAnd("Payment", {"finAccountTransId" : finAccountTrans.finAccountTransId}, null, false)>
                        </#if>
                        <#assign finAccountTransType = delegator.findOne("FinAccountTransType", {"finAccountTransTypeId" : finAccountTrans.finAccountTransTypeId}, true)>
                        <#if finAccountTrans.statusId?has_content>
                            <#assign status = delegator.findOne("StatusItem", {"statusId" : finAccountTrans.statusId}, true)>
                        </#if>
                        <#if payment?has_content && payment.paymentTypeId?has_content>
                            <#assign paymentType = delegator.findOne("PaymentType", {"paymentTypeId" : payment.paymentTypeId}, true)>
                        </#if>
                        <#if payment?has_content && payment.paymentMethodTypeId?has_content>
                            <#assign paymentMethodType = delegator.findOne("PaymentMethodType", {"paymentMethodTypeId" : payment.paymentMethodTypeId}, true)>
                        </#if>
                        <#if finAccountTrans.glReconciliationId?has_content>
                            <#assign glReconciliation = delegator.findOne("GlReconciliation", {"glReconciliationId" : finAccountTrans.glReconciliationId}, true)>
                            <input name="openingBalance_o_${finAccountTrans_index}" type="hidden" value="${glReconciliation.openingBalance!}"/>
                        </#if>
                        <#if finAccountTrans.partyId?has_content>
                            <#assign partyName = (delegator.findOne("PartyNameView", {"partyId" : finAccountTrans.partyId}, true))!>
                        </#if>
                        <@tr valign="middle">
                            <@td>
                                <#-- FIXME: This is ugly but it might make the UX coherent if we determine how buttons must be shown inside grid cells and the most important thing, 
                                    should we allow modal windows to be opened that are located in any place of the grid?
                                -->
                                <#if payments?has_content>
                                    <@modal id="displayPayments_${finAccountTrans.finAccountTransId}" label=finAccountTrans.finAccountTransId>
                                        <@table type="data-list" autoAltRows=true inheritAltRows=true> <#-- orig: class="basic-table hover-bar" -->
                                            <@thead>
                                                <@tr class="header-row-2">
                                                    <@th>${uiLabelMap.AccountingDepositSlipId}</@th>
                                                    <@th>${uiLabelMap.FormFieldTitle_paymentId}</@th>
                                                    <@th>${uiLabelMap.OrderPaymentType}</@th>
                                                    <@th>${uiLabelMap.CommonMethod}</@th>
                                                    <@th class="align-right">${uiLabelMap.CommonAmount}</@th>
                                                    <@th>${uiLabelMap.PartyPartyFrom}</@th>
                                                    <@th>${uiLabelMap.PartyPartyTo}</@th>
                                                </@tr>
                                            </@thead>
                                            <#list payments as payment>
                                                <#if payment?? && payment.paymentTypeId?has_content>
                                                    <#assign paymentType = delegator.findOne("PaymentType", {"paymentTypeId" : payment.paymentTypeId}, true)>
                                                </#if>
                                                <#if payment?has_content && payment.paymentMethodTypeId?has_content>
                                                    <#assign paymentMethodType = delegator.findOne("PaymentMethodType", {"paymentMethodTypeId" : payment.paymentMethodTypeId}, true)>
                                                </#if>
                                                <#if payment?has_content>
                                                    <#assign paymentGroupMembers = Static["org.ofbiz.entity.util.EntityUtil"].filterByDate(payment.getRelated("PaymentGroupMember", null, null, false)!) />
                                                    <#assign fromParty = payment.getRelatedOne("FromParty", false)! />
                                                    <#assign fromPartyName = delegator.findOne("PartyNameView", {"partyId" : fromParty.partyId}, true) />
                                                    <#assign toParty = payment.getRelatedOne("ToParty", false)! />
                                                    <#assign toPartyName = delegator.findOne("PartyNameView", {"partyId" : toParty.partyId}, true) />
                                                    <#if paymentGroupMembers?has_content>
                                                        <#assign paymentGroupMember = Static["org.ofbiz.entity.util.EntityUtil"].getFirst(paymentGroupMembers) />
                                                    </#if>
                                                </#if>
                                                <@tr valign="middle">
                                                    <@td><#if paymentGroupMember?has_content><a href="<@ofbizUrl>EditDepositSlipAndMembers?paymentGroupId=${paymentGroupMember.paymentGroupId!}&amp;finAccountId=${parameters.finAccountId!}</@ofbizUrl>" class="${styles.link_nav_info_id!}">${paymentGroupMember.paymentGroupId!}</a></#if></@td>
                                                    <@td><#if payment?has_content><a href="<@ofbizUrl>paymentOverview?paymentId=${payment.paymentId!}</@ofbizUrl>" class="${styles.link_nav_info_id!}">${payment.paymentId!}</a></#if></@td>
                                                    <@td><#if paymentType?has_content>${paymentType.description!}</#if></@td>
                                                    <@td><#if paymentMethodType?has_content>${paymentMethodType.description!}</#if></@td>
                                                    <@td><@ofbizCurrency amount=(payment.amount!)/></@td>
                                                    <@td><#if fromPartyName?has_content>${fromPartyName.groupName!}${fromPartyName.firstName!} ${fromPartyName.lastName!} <a href="<@ofbizInterWebappUrl>/partymgr/control/viewprofile?partyId=${fromPartyName.partyId!}</@ofbizInterWebappUrl>" class="${styles.link_nav_info_id!}">${fromPartyName.partyId!}</a></#if></@td>
                                                    <@td><#if toPartyName?has_content>${toPartyName.groupName!}${toPartyName.firstName!} ${toPartyName.lastName!} <a href="<@ofbizInterWebappUrl>/partymgr/control/viewprofile?partyId=${toPartyName.partyId!}</@ofbizInterWebappUrl>" class="${styles.link_nav_info_id!}">${toPartyName.partyId!}</a></#if></@td>
                                                </@tr>
                                            </#list>
                                        </@table>
                                    </@modal>
                                    
                                    <#-- FIXME: I dunno where to put this, maybe in a new column?  -->
                                <a href="<@ofbizUrl>DepositSlip.pdf?finAccountTransId=${finAccountTrans.finAccountTransId}</@ofbizUrl>" target="_BLANK" class="${styles.link_run_sys!} ${styles.action_export!}">${uiLabelMap.AccountingDepositSlip}</a>
                                <#else>
                                    ${finAccountTrans.finAccountTransId}<#t>
                                </#if>
                            </@td>
                            <@td>${finAccountTransType.description!}</@td>
                            <@td><#if partyName?has_content>${(partyName.firstName)!} ${(partyName.lastName)!} ${(partyName.groupName)!} <a href="<@ofbizInterWebappUrl>/partymgr/control/viewprofile?partyId=${partyName.partyId}</@ofbizInterWebappUrl>" class="${styles.link_nav_info_id!}">${(partyName.partyId)!}</a></#if></@td>
                            <@td><#if glReconciliation?has_content>${glReconciliation.glReconciliationName!} <a href="ViewGlReconciliationWithTransaction?glReconciliationId=${glReconciliation.glReconciliationId!}&amp;finAccountId=${parameters.finAccountId!}" class="${styles.link_nav_info_id!}">${glReconciliation.glReconciliationId!}</a></#if></@td>
                            <@td>${finAccountTrans.transactionDate!}</@td>
                            <@td>${finAccountTrans.entryDate!}</@td>
                            <@td class="amount">${finAccountTrans.amount!}</@td>
                            <@td>
                                <#if finAccountTrans.paymentId?has_content>
                                    <a href="<@ofbizUrl>paymentOverview?paymentId=${finAccountTrans.paymentId}</@ofbizUrl>" class="${styles.link_nav_info_id!}">${finAccountTrans.paymentId}</a>
                                </#if>
                            </@td>
                            <@td><#if paymentType?has_content>${paymentType.description!}</#if></@td>
                            <@td><#if paymentMethodType?has_content>${paymentMethodType.description!}</#if></@td>
                            <@td><#if status?has_content>${status.description!}</#if></@td>
                            <@td>${finAccountTrans.comments!}</@td>
                            <#if grandTotal??>
                                <@td>
                                    <#if finAccountTrans.statusId?has_content && finAccountTrans.statusId == 'FINACT_TRNS_CREATED'>
                                        <a href="javascript:document.cancelFinAccountTrans_${finAccountTrans.finAccountTransId}.submit();" class="${styles.link_run_sys!} ${styles.action_terminate!}">${uiLabelMap.CommonCancel}</a>
                                    </#if>
                                </@td>
                            </#if>
                            <input name="finAccountTransId_o_${finAccountTrans_index}" type="hidden" value="${finAccountTrans.finAccountTransId}"/>
                            <input name="organizationPartyId_o_${finAccountTrans_index}" type="hidden" value="${defaultOrganizationPartyId}"/>
                            <#if glReconciliationId?has_content && glReconciliationId != "_NA_">
                                <input name="glReconciliationId_o_${finAccountTrans_index}" type="hidden" value="${glReconciliationId}"/>
                            </#if>
                            <#if !(grandTotal??)>
                                <#if (parameters.glReconciliationId?has_content && parameters.glReconciliationId != "_NA_")>
                                    <#if finAccountTrans.statusId == "FINACT_TRNS_CREATED">
                                        <@td><a href="javascript:document.removeFinAccountTransFromReconciliation_${finAccountTrans.finAccountTransId}.submit();" class="${styles.link_run_sys!} ${styles.action_remove!}">${uiLabelMap.CommonRemove}</a></@td>
                                    </#if>
                                </#if>
                            </#if>
                            <#if ((glReconciliationId?has_content && glReconciliationId == "_NA_") && (glReconciliations?has_content && finAccountTransList?has_content)) || !grandTotal??>
                                <#if finAccountTrans.statusId == "FINACT_TRNS_CREATED">
                                    <@td>
                                        <@field id="finAccountTransId_${finAccountTrans_index}" name="_rowSubmit_o_${finAccountTrans_index}" type="checkbox" value="Y" onClick="javascript:getFinAccountTransRunningTotalAndBalances();"/>
                                    </@td>
                                </#if>
                            </#if>
                        </@tr>
                    </#list>
                </@tbody>
            </@table>
        </form>
        
        <#list finAccountTransList as finAccountTrans>
            <form name="removeFinAccountTransFromReconciliation_${finAccountTrans.finAccountTransId}" method="post" action="<@ofbizUrl>removeFinAccountTransFromReconciliation</@ofbizUrl>">
                <input name="finAccountTransId" type="hidden" value="${finAccountTrans.finAccountTransId}"/>
                <input name="finAccountId" type="hidden" value="${finAccountTrans.finAccountId}"/>
            </form>
        </#list>
        <#if grandTotal??>
            <#list finAccountTransList as finAccountTrans>
                <#if finAccountTrans.statusId?has_content && finAccountTrans.statusId == 'FINACT_TRNS_CREATED'>
                    <form name="cancelFinAccountTrans_${finAccountTrans.finAccountTransId}" method="post" action="<@ofbizUrl>setFinAccountTransStatus</@ofbizUrl>">
                        <input name="noConditionFind" type="hidden" value="Y"/>
                        <input name="finAccountTransId" type="hidden" value="${finAccountTrans.finAccountTransId}"/>
                        <input name="finAccountId" type="hidden" value="${finAccountTrans.finAccountId}"/>
                        <input name="statusId" type="hidden" value="FINACT_TRNS_CANCELED"/>
                    </form>
                </#if>
            </#list>
            <@table type="summary"> <#-- orig: class="basic-table" -->
                <@thead>
                    <@tr>
                        <@th class="align-right">${uiLabelMap.FormFieldTitle_grandTotal} / ${uiLabelMap.AccountingNumberOfTransaction}</@th>
                        <@th class="align-right">${uiLabelMap.AccountingCreatedGrandTotal} / ${uiLabelMap.AccountingNumberOfTransaction}</@th>
                        <@th class="align-right">${uiLabelMap.AccountingApprovedGrandTotal} / ${uiLabelMap.AccountingNumberOfTransaction}</@th>
                        <@th class="align-right">${uiLabelMap.AccountingCreatedApprovedGrandTotal} / ${uiLabelMap.AccountingNumberOfTransaction}</@th>
                    </@tr>
                </@thead>
                <@tbody>
                    <@tr>
                        <@td class="align-right">${grandTotal} / ${searchedNumberOfRecords}</@td>
                        <@td class="align-right">${createdGrandTotal} / ${totalCreatedTransactions}</@td>
                        <@td class="align-right">${approvedGrandTotal} / ${totalApprovedTransactions}</@td>
                        <@td class="align-right">${createdApprovedGrandTotal} / ${totalCreatedApprovedTransactions}</@td>
                    </@tr>
                </@tbody>
            </@table>
        <#else>
            <@table type="summary"> <#-- orig: class="basic-table" -->
                <@thead>
                    <@tr>
                        <@th class="align-right">${uiLabelMap.AccountingRunningTotal} / ${uiLabelMap.AccountingNumberOfTransaction}</@th>
                        <@th class="align-right">${uiLabelMap.AccountingOpeningBalance}</@th>
                        <@th class="align-right">${uiLabelMap.FormFieldTitle_reconciledBalance}</@th>
                        <@th class="align-right">${uiLabelMap.FormFieldTitle_closingBalance}</@th>
                    </@tr>
                </@thead>
                <@tbody>
                    <@tr>
                        <@td>
                            <span id="finAccountTransRunningTotal"></span> /
                            <span id="numberOfFinAccountTransaction"></span>
                        </@td>
                        <@td><@ofbizCurrency amount=glReconciliation.openingBalance?default('0')/></@td>
                        <@td><@ofbizCurrency amount=glReconciliation.reconciledBalance?default('0')/></@td>
                        <@td id="endingBalance"><@ofbizCurrency amount=(glReconciliationApprovedGrandTotal!)/></@td>
                        <input type="hidden" id="endingBalanceInput" value="<@ofbizCurrency amount=(glReconciliationApprovedGrandTotal!)/>"/>
                    </@tr>
                </@tbody>
            </@table>
        </#if>
    <#else>
        <@commonMsg type="result-norecord"/>
    </#if>
  
</@section>
