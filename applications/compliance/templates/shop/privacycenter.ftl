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
<#-- SCIPIO: 4.0.0: account privacy center (GDPR Art. 15-21, CCPA): download, choices, consent history, delete account. -->
<#import "component://compliance/templates/shop/complianceLib.ftl" as compliance>
<#assign partyId = (userLogin.partyId)!"">
<#assign consents = delegator.findByAnd("ConsentEvent", {"partyId": partyId}, ["-eventDate"], false)!>
<#assign requests = delegator.findByAnd("PrivacyRequest", {"partyId": partyId}, ["-receivedDate"], false)!>
<div class="scp-privacy-center">
  <p class="scp-eyebrow">${uiLabelMap.ComplianceAccount}</p>
  <h1>${uiLabelMap.CompliancePrivacyCenter}</h1>
  <p class="scp-lead">${uiLabelMap.CompliancePrivacyCenterIntro}</p>

  <div class="scp-pc-grid">
    <section class="scp-card">
      <h2>${uiLabelMap.ComplianceDownloadData}</h2>
      <p>${uiLabelMap.ComplianceDownloadDataText}</p>
      <a class="scp-btn scp-btn--primary" href="<@ofbizUrl>privacyExport</@ofbizUrl>" download="download">${uiLabelMap.ComplianceDownloadJson}</a>
    </section>
    <section class="scp-card">
      <h2>${uiLabelMap.ComplianceYourChoices}</h2>
      <ul class="scp-pc-links">
        <li><button type="button" class="scp-linklike" data-scp-consent-open="true">${uiLabelMap.ComplianceCookieSettings}</button></li>
        <li><a class="scp-privacy-choices" href="<@ofbizUrl>privacyChoices</@ofbizUrl>"><@compliance.privacyChoicesIcon/> ${uiLabelMap.ComplianceYourPrivacyChoices}</a></li>
        <li><a href="<@ofbizUrl>viewprofile</@ofbizUrl>">${uiLabelMap.ComplianceNewsletterAndContact}</a></li>
      </ul>
    </section>
  </div>

  <#if requests?has_content>
  <section class="scp-card">
    <h2>${uiLabelMap.ComplianceYourRequests}</h2>
    <table class="scp-table">
      <thead><tr><th>${uiLabelMap.ComplianceRequest}</th><th>${uiLabelMap.ComplianceReceived}</th><th>${uiLabelMap.ComplianceAnswerBy}</th><th>${uiLabelMap.ComplianceStatus}</th></tr></thead>
      <tbody>
      <#list requests as r>
        <#assign rt = delegator.findOne("Enumeration", {"enumId": r.requestTypeId}, true)!>
        <#assign rs = delegator.findOne("StatusItem", {"statusId": r.statusId}, true)!>
        <tr><td>${(rt.get("description", locale))!r.requestTypeId} &middot; <span class="scp-mono">${r.privacyRequestId}</span></td>
            <td>${r.receivedDate?date?string.medium}</td><td>${(r.dueDate?date?string.medium)!""}</td><td>${(rs.get("description", locale))!r.statusId}</td></tr>
      </#list>
      </tbody>
    </table>
  </section>
  </#if>

  <section class="scp-card">
    <h2>${uiLabelMap.ComplianceConsentHistory}</h2>
    <#if consents?has_content>
    <table class="scp-table">
      <thead><tr><th>${uiLabelMap.ComplianceWhen}</th><th>${uiLabelMap.ComplianceChoice}</th><th>${uiLabelMap.ComplianceWhere}</th><th>${uiLabelMap.ComplianceTextVersion}</th></tr></thead>
      <tbody>
      <#list consents as ce>
        <#if ce?index gte 50><#break></#if>
        <#assign ct = delegator.findOne("Enumeration", {"enumId": ce.consentTypeId}, true)!>
        <#assign cs = delegator.findOne("Enumeration", {"enumId": ce.sourceId!""}, true)!>
        <tr><td class="scp-mono">${ce.eventDate?string("yyyy-MM-dd HH:mm")}</td>
            <td>${(ct.get("description", locale))!ce.consentTypeId}: <strong><#if ce.granted == "Y">${uiLabelMap.ComplianceOn}<#else>${uiLabelMap.ComplianceOff}</#if></strong></td>
            <td>${(cs.get("description", locale))!""}<#if ce.orderId?has_content> &middot; ${ce.orderId}</#if></td>
            <td><#if ce.documentVersion??>v${ce.documentVersion}<#elseif ce.consentVersion??>${uiLabelMap.ComplianceConsentVersionShort} ${ce.consentVersion}</#if></td></tr>
      </#list>
      </tbody>
    </table>
    <#else>
      <p>${uiLabelMap.ComplianceNoConsents}</p>
    </#if>
  </section>

  <section class="scp-card scp-pc-delete">
    <h2>${uiLabelMap.ComplianceDeleteAccount}</h2>
    <p>${uiLabelMap.ComplianceDeleteAccountText}</p>
    <form method="post" action="<@ofbizUrl>privacyDeleteAccount</@ofbizUrl>" class="scp-form">
      <label class="scp-choice"><input type="checkbox" name="confirmDelete" value="Y" required="required"/> ${uiLabelMap.ComplianceDeleteConfirm}</label>
      <button type="submit" class="scp-btn scp-btn--danger">${uiLabelMap.ComplianceDeleteMyAccount}</button>
    </form>
  </section>
</div>
