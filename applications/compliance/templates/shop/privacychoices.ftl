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
<#-- SCIPIO: 4.0.0: Your Privacy Choices (US opt-out page, CCPA/CPRA; honours GPC). Works without JavaScript. -->
<#import "component://compliance/templates/shop/complianceLib.ftl" as compliance>
<#assign c = Static["com.ilscipio.scipio.compliance.ConsentWorker"].getConsentContext(request, delegator, locale)>
<#assign st = c.state!{}>
<#assign saleAllowed = !c.gpc && (st.marketing!true)>
<div class="scp-choices">
  <h1><@compliance.privacyChoicesIcon width=64 height=30/> ${uiLabelMap.ComplianceYourPrivacyChoices}</h1>
  <#if (parameters.saved!"") == "Y"><@alert type="success">${uiLabelMap.ComplianceChoicesSaved}</@alert></#if>
  <p>${uiLabelMap.CompliancePrivacyChoicesIntro}</p>
  <#if c.gpc>
    <div class="scp-choices-gpc" role="status">
      <svg width="26" height="26" viewBox="0 0 24 24" fill="none" stroke="#fff" stroke-width="1.8" stroke-linejoin="round" aria-hidden="true"><path d="M12 3 5 6v6c0 4 3 7 7 9 4-2 7-5 7-9V6l-7-3Z"/><path d="m9 12 2 2 4-4" stroke-linecap="round"/></svg>
      <div><strong>${uiLabelMap.ComplianceGpcHonoured}</strong><br/>${uiLabelMap.ComplianceGpcDetected}</div>
    </div>
  </#if>
  <form method="post" action="<@ofbizUrl>recordConsent</@ofbizUrl>">
    <input type="hidden" name="source" value="privacy-choices"/>
    <input type="hidden" name="returnTo" value="privacyChoices"/>
    <input type="hidden" name="preferences" value="${(st.preferences!true)?string("Y", "N")}"/>
    <input type="hidden" name="statistics" value="${(st.statistics!true)?string("Y", "N")}"/>
    <div class="scp-choices-list">
      <div class="scp-choices-row">
        <div class="scp-choices-row-text"><strong>${uiLabelMap.ComplianceSaleShare}</strong><span>${uiLabelMap.ComplianceSaleShareDesc}</span></div>
        <label class="scp-switch"><input type="checkbox" name="saleShare" value="Y"<#if saleAllowed> checked="checked"</#if><#if c.gpc> disabled="disabled"</#if>/><span class="scp-switch-track" aria-hidden="true"></span><span class="scp-sr">${uiLabelMap.ComplianceSaleShare}</span></label>
        <input type="hidden" name="saleShare" value="N"/>
      </div>
      <div class="scp-choices-row">
        <div class="scp-choices-row-text"><strong>${uiLabelMap.ComplianceTargetedAds}</strong><span>${uiLabelMap.ComplianceTargetedAdsDesc}</span></div>
        <label class="scp-switch"><input type="checkbox" name="marketing" value="Y"<#if saleAllowed> checked="checked"</#if><#if c.gpc> disabled="disabled"</#if>/><span class="scp-switch-track" aria-hidden="true"></span><span class="scp-sr">${uiLabelMap.ComplianceTargetedAds}</span></label>
        <input type="hidden" name="marketing" value="N"/>
        <input type="hidden" name="targetedAds" value="${saleAllowed?string("Y", "N")}"/>
      </div>
      <div class="scp-choices-row">
        <div class="scp-choices-row-text"><strong>${uiLabelMap.ComplianceSpi}</strong><span>${uiLabelMap.ComplianceSpiDesc}</span></div>
      </div>
    </div>
    <p><button type="submit" class="scp-btn scp-btn--primary">${uiLabelMap.ComplianceSaveMyChoices}</button></p>
  </form>
  <section>
    <h2>${uiLabelMap.ComplianceMakeRequest}</h2>
    <p>${uiLabelMap.ComplianceRequestIntro}</p>
    <#assign profile = Static["com.ilscipio.scipio.compliance.LegalDocumentWorker"].getProfile(delegator, c.productStoreId!)!>
    <p><a class="scp-btn scp-btn--secondary" href="<@ofbizUrl>privacyRequest</@ofbizUrl>">${uiLabelMap.CompliancePrivacyRequest}</a></p>
    <p><a href="<@ofbizUrl>legal?doc=privacy</@ofbizUrl>">${uiLabelMap.ComplianceLegal}: Privacy policy</a><#if (profile.contactEmail)?has_content> &middot; <a href="mailto:${profile.contactEmail}">${profile.contactEmail}</a></#if></p>
  </section>
</div>
