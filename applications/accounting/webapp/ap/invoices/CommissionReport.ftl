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

<#if commissionReportList?has_content>
  <#macro menuContent menuArgs={}>
    <@menu args=menuArgs>
      <@menuitem type="link" href=makeOfbizUrl("CommissionReport.pdf?isSearch=Y&productId=${parameters.productId!}&partyId=${parameters.partyId!}&fromDate=${parameters.fromDate!}&thruDate=${parameters.thruDate!}") text=uiLabelMap.AccountingInvoicePDF target="_BLANK" class="+${styles.action_run_sys!} ${styles.action_export!}" />
    </@menu>
  </#macro>
  <@section menuContent=menuContent>
  <@table type="data-list" autoAltRows=true> <#-- orig: class="basic-table hover-bar" --> <#-- orig: cellspacing="0" -->
    <@thead>
    <@tr class="header-row-2">
      <@th>${uiLabelMap.AccountingLicensedProduct}</@th>
      <@th>${uiLabelMap.AccountingQuantity}</@th>
      <@th>${uiLabelMap.AccountingNumberOfOrders} / ${uiLabelMap.AccountingSalesInvoices}</@th>
      <@th>${uiLabelMap.AccountingCommissionAmount}</@th>
      <@th>${uiLabelMap.AccountingNetSale}</@th>
      <@th>${uiLabelMap.AccountingSalesAgents} / ${uiLabelMap.AccountingTermAmount}</@th>
    </@tr>
    </@thead>
    <#list commissionReportList as commissionReport>
      <@tr valign="middle">
        <@td><a href="<@ofbizInterWebappUrl>/catalog/control/ViewProduct?productId=${commissionReport.productId!}</@ofbizInterWebappUrl>">${commissionReport.productName!}</a></@td>
        <@td>${commissionReport.quantity!}</@td>
        <@td>
          ${commissionReport.numberOfOrders!} /
          <#if commissionReport.salesInvoiceIds?has_content>
            <#list commissionReport.salesInvoiceIds as salesInvoiceId>
              [<a href="<@ofbizInterWebappUrl>/ap/control/invoiceOverview?invoiceId=${salesInvoiceId!}</@ofbizInterWebappUrl>">${salesInvoiceId!}</a>]
            </#list>
          </#if>
        </@td>
        <@td><@ofbizCurrency amount=(commissionReport.commissionAmount!)/></@td>
        <@td><@ofbizCurrency amount=(commissionReport.netSale!)/></@td>
        <@td>
          <#if commissionReport.salesAgentAndTermAmtMap?has_content>
            <#list commissionReport.salesAgentAndTermAmtMap.values() as partyIdAndTermAmountMap>
              <#assign partyName = (delegator.findOne("PartyNameView", {"partyId" : partyIdAndTermAmountMap.partyId}, true))!>
              <p>[${(partyName.firstName)!} ${(partyName.lastName)!} ${(partyName.groupName)!}(<a href="<@ofbizInterWebappUrl>/partymgr/control/viewprofile?partyId=${partyIdAndTermAmountMap.partyId!}</@ofbizInterWebappUrl>">${partyIdAndTermAmountMap.partyId!}</a>)]
                / <@ofbizCurrency amount = ((partyIdAndTermAmountMap.termAmount)!)/>
              </p>
            </#list>
          </#if>
        </@td>
      </@tr>
    </#list>
  </@table>
  
    <ul>
      <li></li>
      <li><@heading>${uiLabelMap.CommonSummary} :</@heading></li>
      <li></li>
      <li>${uiLabelMap.ManufacturingTotalQuantity} : ${totalQuantity!}</li>
      <li>${uiLabelMap.AccountingTotalCommissionAmount} : <@ofbizCurrency amount=(totalCommissionAmount!)/></li>
      <li>${uiLabelMap.AccountingTotalNetSales} : <@ofbizCurrency amount=(totalNetSales!)/></li>
      <li>${uiLabelMap.AccountingTotalNumberOfOrders} : ${totalNumberOfOrders!}</li>
    </ul>
  </@section>
<#else>
  <@section>
    <@commonMsg type="result-norecord"/>
  </@section>
</#if>
