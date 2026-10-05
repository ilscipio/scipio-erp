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
<#-- SCIPIO: 4.0.0: privacy request without an account (GDPR, CCPA); confirmed by an e-mail link. -->
<#assign done = scpPrivacyDoneOverride!(requestAttributes.scpPrivacyDone!"")>
<div class="scp-withdraw">
  <p class="scp-eyebrow">${uiLabelMap.CompliancePrivacy}</p>
  <h1>${uiLabelMap.CompliancePrivacyRequest}</h1>
  <#if done == "sent">
    <div class="scp-card" role="status"><p>${uiLabelMap.CompliancePrivacyCheckEmail}</p></div>
  <#elseif done == "received">
    <div class="scp-card" role="status"><p>${uiLabelMap.CompliancePrivacyReceived}</p></div>
  <#elseif done == "verified">
    <div class="scp-card" role="status"><p>${uiLabelMap.CompliancePrivacyVerified}</p></div>
  <#elseif done == "deleted">
    <div class="scp-card" role="status"><p>${uiLabelMap.ComplianceAccountDeleted}</p></div>
  <#elseif done == "invalidToken">
    <@alert type="error">${uiLabelMap.CompliancePrivacyInvalidToken}</@alert>
  </#if>
  <#if !done?has_content || done == "invalid" || done == "invalidToken">
    <#if done == "invalid"><@alert type="error">${uiLabelMap.CompliancePrivacyInvalid}</@alert></#if>
    <p class="scp-lead">${uiLabelMap.CompliancePrivacyRequestIntro}</p>
    <form method="post" action="<@ofbizUrl>privacyRequestSubmit</@ofbizUrl>" class="scp-card scp-form">
      <fieldset>
        <legend>${uiLabelMap.ComplianceWhatDoYouNeed}</legend>
        <#list ["PRVREQ_ACCESS", "PRVREQ_DELETE", "PRVREQ_CORRECT", "PRVREQ_OPT_OUT", "PRVREQ_LIMIT_SPI"] as t>
          <#assign e = delegator.findOne("Enumeration", {"enumId": t}, true)!>
          <label class="scp-choice"><input type="radio" name="requestTypeId" value="${t}"<#if t?index == 0> checked="checked"</#if>/> ${(e.get("description", locale))!t}</label>
        </#list>
      </fieldset>
      <label>${uiLabelMap.ComplianceEmailAddress}<input type="email" name="emailAddress" autocomplete="email" required="required"/></label>
      <label>${uiLabelMap.ComplianceDetailsOptional}<input type="text" name="note" maxlength="250"/></label>
      <button type="submit" class="scp-btn scp-btn--primary">${uiLabelMap.ComplianceSendRequest}</button>
    </form>
  </#if>
</div>
