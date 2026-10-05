<#--
Scipio Commerce
Copyright (C) Ilscipio GmbH

This file is part of Scipio Commerce. Scipio Commerce is free software: you
can redistribute it and modify it under the terms of the GNU Affero General
Public License, version 3, as published by the Free Software Foundation.
Scipio Commerce is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License
for more details. You should have received a copy of the license with this
work (file LICENSE). If not, see <https://www.gnu.org/licenses/agpl-3.0.html>.
A commercial license is available from Ilscipio GmbH.

SPDX-License-Identifier: AGPL-3.0-only
-->
<#-- SCIPIO: 4.0.0: "Withdraw from contract here" (Directive 2011/83/EU Art. 11a). Two steps and a receipt; no account needed. -->
<#assign step = requestAttributes.scpWithdrawStep!"form">
<#assign form = requestAttributes.scpWithdrawForm!{}>
<#assign err = requestAttributes.scpWithdrawError!"">
<#assign storeId = Static["org.ofbiz.product.store.ProductStoreWorker"].getProductStoreId(request)!>
<#assign profile = Static["com.ilscipio.scipio.compliance.LegalDocumentWorker"].getProfile(delegator, storeId)!>
<#assign days = ((profile.withdrawalDays)!14)?string>
<#assign stepNo = (step == "form")?then(1, (step == "confirm")?then(2, 3))>
<div class="scp-withdraw">
  <p class="scp-eyebrow">${uiLabelMap.ComplianceRightOfWithdrawal}</p>
  <h1>${uiLabelMap.ComplianceWithdrawHere}</h1>
  <ol class="scp-steps" aria-label="${uiLabelMap.ComplianceSteps}">
    <li class="<#if stepNo == 1>is-active<#else>is-done</#if>"<#if stepNo == 1> aria-current="step"</#if>>1 ${uiLabelMap.ComplianceStepOrder}</li>
    <li class="<#if stepNo == 2>is-active<#elseif stepNo gt 2>is-done</#if>"<#if stepNo == 2> aria-current="step"</#if>>2 ${uiLabelMap.ComplianceStepConfirm}</li>
    <li class="<#if stepNo == 3>is-active</#if>"<#if stepNo == 3> aria-current="step"</#if>>3 ${uiLabelMap.ComplianceStepReceipt}</li>
  </ol>
  <#if err?has_content><@alert type="error">${uiLabelMap["ComplianceWithdrawError_" + err]}</@alert></#if>

  <#if step == "form">
    <p class="scp-lead">${rawString(uiLabelMap.ComplianceWithdrawIntro)?replace("{0}", days)}</p>
    <form method="post" action="<@ofbizUrl>withdrawCheck</@ofbizUrl>" class="scp-card scp-form">
      <label>${uiLabelMap.ComplianceName}<input type="text" name="customerName" autocomplete="name" value="${form.customerName!}"/></label>
      <label>${uiLabelMap.ComplianceOrderEmail}<input type="email" name="emailAddress" autocomplete="email" required="required" value="${form.emailAddress!}"/></label>
      <label>${uiLabelMap.ComplianceOrderNumber}<input type="text" name="orderId" required="required" value="${form.orderId!(parameters.orderId!)}"/></label>
      <fieldset>
        <legend>${uiLabelMap.ComplianceWithdrawScope}</legend>
        <label class="scp-choice"><input type="radio" name="scope" value="all"<#if (form.scope!"all") == "all"> checked="checked"</#if>/> ${uiLabelMap.ComplianceWholeOrder}</label>
        <label class="scp-choice"><input type="radio" name="scope" value="some"<#if (form.scope!"") == "some"> checked="checked"</#if>/> ${uiLabelMap.ComplianceSomeItems}</label>
      </fieldset>
      <button type="submit" class="scp-btn scp-btn--primary">${uiLabelMap.CommonContinue}</button>
    </form>

  <#elseif step == "confirm">
    <#assign order = requestAttributes.scpWithdrawOrder!>
    <form method="post" action="<@ofbizUrl>withdrawSubmit</@ofbizUrl>" class="scp-card scp-form">
      <input type="hidden" name="orderId" value="${form.orderId!}"/>
      <input type="hidden" name="emailAddress" value="${form.emailAddress!}"/>
      <input type="hidden" name="customerName" value="${form.customerName!}"/>
      <input type="hidden" name="scope" value="${form.scope!"all"}"/>
      <dl class="scp-summary">
        <dt>${uiLabelMap.ComplianceOrder}</dt><dd class="scp-mono">${form.orderId!}<#if (order.orderDate)??> &middot; ${order.orderDate?date?string.medium}</#if></dd>
        <#if form.customerName?has_content><dt>${uiLabelMap.ComplianceName}</dt><dd>${form.customerName}</dd></#if>
        <dt>${uiLabelMap.ComplianceReceiptTo}</dt><dd>${form.emailAddress!}</dd>
      </dl>
      <#if (form.scope!"all") == "some">
        <fieldset>
          <legend>${uiLabelMap.ComplianceChooseItems}</legend>
          <#list requestAttributes.scpWithdrawItems![] as it>
            <label class="scp-choice"><input type="checkbox" name="orderItemSeqId" value="${it.orderItemSeqId}"<#if (form.chosen![])?seq_contains(it.orderItemSeqId)> checked="checked"</#if>/> ${it.description!} &times; ${it.quantity}</label>
          </#list>
        </fieldset>
      <#else>
        <ul class="scp-items">
          <#list requestAttributes.scpWithdrawItems![] as it><li>${it.description!} &times; ${it.quantity}</li></#list>
        </ul>
      </#if>
      <p class="scp-statement">${uiLabelMap.ComplianceWithdrawStatement}</p>
      <button type="submit" class="scp-btn scp-btn--primary">${uiLabelMap.ComplianceConfirmWithdrawal}</button>
      <a class="scp-back" href="<@ofbizUrl>withdraw</@ofbizUrl>">${uiLabelMap.CommonBack}</a>
    </form>

  <#else>
    <#assign res = requestAttributes.scpWithdrawResult!{}>
    <div class="scp-card scp-receipt" role="status">
      <h2>${uiLabelMap.ComplianceWithdrawReceivedTitle}</h2>
      <p class="scp-mono">${res.receivedDate?string("yyyy-MM-dd HH:mm:ss z")} &middot; ${uiLabelMap.ComplianceReference} ${res.returnId!}</p>
      <#if res.emailSent!false>
        <p>${rawString(uiLabelMap.ComplianceWithdrawEmailSent)?replace("{0}", form.emailAddress!"")}</p>
      <#else>
        <p>${uiLabelMap.ComplianceWithdrawEmailNotSent}</p>
      </#if>
      <ul class="scp-items">
        <#list (res.withdrawnItems![]) + (res.pendingItems![]) as it><li>${it.description!} &times; ${it.quantity}</li></#list>
      </ul>
      <p>${uiLabelMap.ComplianceWithdrawNextSteps}</p>
      <button type="button" class="scp-btn scp-btn--secondary" data-scp-print="true">${uiLabelMap.CommonPrint}</button>
    </div>
  </#if>
</div>
