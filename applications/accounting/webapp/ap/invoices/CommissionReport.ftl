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
  <#assign menuHtml>
    <@menu type="section" inlineItems=true>
      <@menuitem type="link" href=makeOfbizUrl("CommissionReport.pdf?isSearch=Y&amp;productId=${parameters.productId!}&amp;partyId=${parameters.partyId!}&amp;fromDate=${parameters.fromDate!}&amp;thruDate=${parameters.thruDate!}") text="${uiLabelMap.AccountingInvoicePDF}" target="_BLANK" />
    </@menu>
  </#assign>
  <@section menuHtml=menuHtml>
  <@table type="data-list" autoAltRows=true class="basic-table hover-bar" cellspacing="0">
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
        <@td><a href="/catalog/control/EditProduct?productId=${commissionReport.productId!}">${commissionReport.productName!}</a></@td>
        <@td>${commissionReport.quantity!}</@td>
        <@td>
          ${commissionReport.numberOfOrders!} /
          <#if commissionReport.salesInvoiceIds?has_content>
            <#list commissionReport.salesInvoiceIds as salesInvoiceId>
              [<a href="/ap/control/invoiceOverview?invoiceId=${salesInvoiceId!}">${salesInvoiceId!}</a>]
            </#list>
          </#if>
        </@td>
        <@td><@ofbizCurrency amount = commissionReport.commissionAmount!/></@td>
        <@td><@ofbizCurrency amount = commissionReport.netSale!/></@td>
        <@td>
          <#if commissionReport.salesAgentAndTermAmtMap?has_content>
            <#list commissionReport.salesAgentAndTermAmtMap.values() as partyIdAndTermAmountMap>
              <#assign partyName = (delegator.findOne("PartyNameView", {"partyId" : partyIdAndTermAmountMap.partyId}, true))!>
              <p>[${(partyName.firstName)!} ${(partyName.lastName)!} ${(partyName.groupName)!}(<a href="/partymgr/control/viewprofile?partyId=${partyIdAndTermAmountMap.partyId!}">${partyIdAndTermAmountMap.partyId!}</a>)]
                / <@ofbizCurrency amount = (partyIdAndTermAmountMap.termAmount)!/>
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
      <li>${uiLabelMap.AccountingTotalCommissionAmount} : <@ofbizCurrency amount = totalCommissionAmount!/></li>
      <li>${uiLabelMap.AccountingTotalNetSales} : <@ofbizCurrency amount = totalNetSales!/></li>
      <li>${uiLabelMap.AccountingTotalNumberOfOrders} : ${totalNumberOfOrders!}</li>
    </ul>
  </@section>
<#else>
  <@section>
    <@resultMsg>${uiLabelMap.CommonNoRecordFound}.</@resultMsg>
  </@section>
</#if>
