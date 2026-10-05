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
<#--
SCIPIO: 4.0.0: storefront macros of the compliance component. Themes import this file:
  <#if Static["org.ofbiz.base.component.ComponentConfig"].isComponentEnabled("compliance")>
    <#import "component://compliance/templates/shop/complianceLib.ftl" as compliance>
  </#if>
and call, in the footer: <@compliance.footerLinks/> <@compliance.footerActions/> <@compliance.consentUi/>
-->

<#-- Links to the legal texts of the store. listClass/linkClass let a theme style them. -->
<#macro footerLinks listClass="scp-legal-links" linkClass="">
  <#local productStoreId = Static["org.ofbiz.product.store.ProductStoreWorker"].getProductStoreId(request)!>
  <#local nav = Static["com.ilscipio.scipio.compliance.LegalDocumentWorker"].getNavigation(delegator, productStoreId, locale)!>
  <#if nav?has_content>
  <ul class="${listClass}">
    <#list nav as n>
      <li><a href="<@ofbizUrl>legal?doc=${n.slug}</@ofbizUrl>"<#if linkClass?has_content> class="${linkClass}"</#if>>${n.label}</a></li>
    </#list>
  </ul>
  </#if>
</#macro>

<#-- The official California opt-out icon shape (CCPA regulations sec. 7015); 30x14. -->
<#macro privacyChoicesIcon width=30 height=14>
  <svg class="scp-ca-icon" width="${width}" height="${height}" viewBox="0 0 30 14" aria-hidden="true" focusable="false"><rect x="0.5" y="0.5" width="29" height="13" rx="6.5" fill="#fff" stroke="#0066ff"/><path d="M7 0.5h9l-4 13H7a6.5 6.5 0 0 1 0-13Z" fill="#0066ff"/><path d="m4.5 7 2 2 3.5-4" stroke="#fff" stroke-width="1.5" fill="none" stroke-linecap="round" stroke-linejoin="round"/><path d="m19 4.5 5 5m0-5-5 5" stroke="#0066ff" stroke-width="1.5" stroke-linecap="round"/></svg>
</#macro>

<#-- Cookie settings and, for US jurisdictions, Your Privacy Choices. -->
<#macro footerActions listClass="scp-legal-links scp-footer-actions" linkClass="">
  <#local c = Static["com.ilscipio.scipio.compliance.ConsentWorker"].getConsentContext(request, delegator, locale)>
  <ul class="${listClass}">
    <li><button type="button" class="scp-linklike<#if linkClass?has_content> ${linkClass}</#if>" data-scp-consent-open="true">${uiLabelMap.ComplianceCookieSettings}</button></li>
    <#if Static["com.ilscipio.scipio.compliance.GuaranteeWorker"].isNoticeRequired(delegator, c.productStoreId!)>
      <li><a href="<@ofbizUrl>withdraw</@ofbizUrl>"<#if linkClass?has_content> class="${linkClass}"</#if>>${uiLabelMap.ComplianceWithdrawHere}</a></li>
      <li><@guaranteeNoticeLink class=linkClass/></li>
    </#if>
    <#if c.showPrivacyChoices>
      <li><a href="<@ofbizUrl>privacyChoices</@ofbizUrl>" class="scp-privacy-choices<#if linkClass?has_content> ${linkClass}</#if>"><@privacyChoicesIcon/> ${uiLabelMap.ComplianceYourPrivacyChoices}</a></li>
    </#if>
    <#if userLogin?? && (userLogin.userLoginId!"anonymous") != "anonymous">
      <li><a href="<@ofbizUrl>privacyCenter</@ofbizUrl>"<#if linkClass?has_content> class="${linkClass}"</#if>>${uiLabelMap.CompliancePrivacyCenter}</a></li>
    <#else>
      <li><a href="<@ofbizUrl>privacyRequest</@ofbizUrl>"<#if linkClass?has_content> class="${linkClass}"</#if>>${uiLabelMap.CompliancePrivacyRequest}</a></li>
    </#if>
  </ul>
</#macro>

<#-- Consent dialog, gated analytics scripts and the consent script. Once per page, at the end of the body. -->
<#macro consentUi>
  <#local c = Static["com.ilscipio.scipio.compliance.ConsentWorker"].getConsentContext(request, delegator, locale)>
  <#local eu = c.regime == "EU">
  <#list c.scripts as s>
    <script type="text/plain" data-scp-consent="${s.category}" data-scp-type="${s.type}">${rawString(s.code)}</script>
  </#list>
  <script type="application/json" id="scp-consent-config">{"regime":"${rawString(c.regime)}","gpc":${c.gpc?c},"consentVersion":"${rawString(c.consentVersion)}","cookieName":"${rawString(c.cookieName)}","recordUrl":"${rawString(makeOfbizUrl("recordConsent"))}"}</script>
  <div id="scp-consent" class="scp-consent scp-consent--${c.regime?lower_case}" role="dialog" aria-modal="false" aria-labelledby="scp-consent-title" aria-describedby="scp-consent-text" hidden>
    <div class="scp-consent-box">
      <h2 id="scp-consent-title" class="scp-consent-title"><#if eu>${uiLabelMap.ComplianceConsentTitle}<#else>${uiLabelMap.ComplianceUsNoticeTitle}</#if></h2>
      <p id="scp-consent-text" class="scp-consent-text"><#if eu>${uiLabelMap.ComplianceConsentText}<#else>${uiLabelMap.ComplianceUsNoticeText}</#if>
        <a href="<@ofbizUrl>legal?doc=cookies</@ofbizUrl>">${uiLabelMap.ComplianceCookiePolicy}</a></p>
      <p class="scp-consent-gpc">${uiLabelMap.ComplianceGpcDetected}</p>
      <div class="scp-consent-detail">
        <#list c.categories as cat>
          <div class="scp-consent-cat">
            <div class="scp-consent-cat-text">
              <span class="scp-consent-cat-name">${uiLabelMap["ComplianceCat_" + cat.id]}</span>
              <span class="scp-consent-cat-desc">${uiLabelMap["ComplianceCatDesc_" + cat.id]}</span>
              <span class="scp-consent-cat-services"><#if cat.services?has_content>${cat.services?join(", ")}<#else>${uiLabelMap.ComplianceNoneInUse}</#if></span>
            </div>
            <#if cat.required>
              <span class="scp-consent-always">${uiLabelMap.ComplianceAlwaysOn}</span>
            <#else>
              <label class="scp-switch"><input type="checkbox" data-scp-cat="${cat.id}"<#if !cat.services?has_content> disabled="disabled"</#if>/><span class="scp-switch-track" aria-hidden="true"></span><span class="scp-sr">${uiLabelMap["ComplianceCat_" + cat.id]}</span></label>
            </#if>
          </div>
        </#list>
      </div>
      <div class="scp-consent-actions">
        <#if eu>
          <button type="button" class="scp-btn scp-btn--primary" data-scp-action="reject-all">${uiLabelMap.ComplianceRejectAll}</button>
          <button type="button" class="scp-btn scp-btn--secondary scp-only-summary" data-scp-action="settings">${uiLabelMap.ComplianceSettings}</button>
          <button type="button" class="scp-btn scp-btn--secondary scp-only-detail" data-scp-action="save">${uiLabelMap.ComplianceSaveChoices}</button>
          <button type="button" class="scp-btn scp-btn--primary" data-scp-action="accept-all">${uiLabelMap.ComplianceAcceptAll}</button>
        <#else>
          <#if c.showPrivacyChoices><a class="scp-btn scp-btn--secondary" href="<@ofbizUrl>privacyChoices</@ofbizUrl>"><@privacyChoicesIcon/> ${uiLabelMap.ComplianceYourPrivacyChoices}</a></#if>
          <button type="button" class="scp-btn scp-btn--secondary scp-only-detail" data-scp-action="save">${uiLabelMap.ComplianceSaveChoices}</button>
          <button type="button" class="scp-btn scp-btn--primary" data-scp-action="ok">${uiLabelMap.ComplianceOk}</button>
        </#if>
      </div>
    </div>
  </div>
  <#if Static["com.ilscipio.scipio.compliance.GuaranteeWorker"].isNoticeRequired(delegator, c.productStoreId!)>
    <@guaranteeNoticeDialog/>
  </#if>
  <dialog id="scp-garan" class="scp-dialog scp-dialog--garan" aria-labelledby="scp-garan-title">
    <div class="scp-dialog-head"><h2 id="scp-garan-title">${uiLabelMap.ComplianceGaranTitle}</h2><button type="button" class="scp-dialog-close" data-scp-dialog-close="true" aria-label="${uiLabelMap.CommonClose}">&times;</button></div>
    <div class="scp-garan-full" data-scp-garan-body="true"></div>
    <p><a href="${rawString(Static["com.ilscipio.scipio.compliance.GuaranteeWorker"].getYourEuropeUrl(locale))}" target="_blank" rel="noopener">${Static["com.ilscipio.scipio.compliance.GuaranteeWorker"].getYourEuropeLabel(locale)}</a></p>
  </dialog>
  <script src="<@ofbizContentUrl>/compliance-static/js/consent.js</@ofbizContentUrl>" defer="defer"></script>
</#macro>

<#-- The official harmonised notice (EU legal guarantee), opened by any [data-scp-notice-open] link. Once per page. -->
<#macro guaranteeNoticeDialog>
  <#local gw = Static["com.ilscipio.scipio.compliance.GuaranteeWorker"]>
  <dialog id="scp-notice" class="scp-dialog scp-dialog--notice" aria-labelledby="scp-notice-title">
    <div class="scp-dialog-head"><h2 id="scp-notice-title">${uiLabelMap.ComplianceGuaranteeRights}</h2><button type="button" class="scp-dialog-close" data-scp-dialog-close="true" aria-label="${uiLabelMap.CommonClose}">&times;</button></div>
    <img class="scp-notice-img" src="<@ofbizContentUrl>${gw.getNoticeUrl(locale)}</@ofbizContentUrl>" alt="${uiLabelMap.ComplianceNoticeAlt}" width="827" height="1170" loading="lazy"/>
    <p class="scp-notice-link"><a href="${rawString(gw.getYourEuropeUrl(locale))}" target="_blank" rel="noopener">${gw.getYourEuropeLabel(locale)}</a></p>
  </dialog>
</#macro>

<#-- "Your legal guarantee rights" link (EU stores); opens the notice. -->
<#macro guaranteeNoticeLink class="">
  <#local storeId = Static["org.ofbiz.product.store.ProductStoreWorker"].getProductStoreId(request)!>
  <#if Static["com.ilscipio.scipio.compliance.GuaranteeWorker"].isNoticeRequired(delegator, storeId)>
    <button type="button" class="scp-linklike<#if class?has_content> ${class}</#if>" data-scp-notice-open="true" aria-haspopup="dialog">${uiLabelMap.ComplianceGuaranteeRights}</button>
  </#if>
</#macro>

<#-- Legal lines under the price on the product page: guarantee (+ GARAN nested label), withdrawal. -->
<#macro productLegalLines product>
  <#local storeId = Static["org.ofbiz.product.store.ProductStoreWorker"].getProductStoreId(request)!>
  <#local gw = Static["com.ilscipio.scipio.compliance.GuaranteeWorker"]>
  <#local profile = Static["com.ilscipio.scipio.compliance.LegalDocumentWorker"].getProfile(delegator, storeId)!>
  <#local garan = gw.getGaranData(delegator, product)!>
  <#if gw.isNoticeRequired(delegator, storeId)>
  <ul class="scp-product-legal">
    <li>${rawString(uiLabelMap.ComplianceLegalGuaranteeLine)?replace("{0}", ((profile.legalGuaranteeYears)!2)?string)} <@guaranteeNoticeLink/></li>
    <#if garan?has_content>
      <li class="scp-garan-line">
        <button type="button" class="scp-garan-nested" data-scp-garan-open="<@ofbizUrl>garanLabel?productId=${product.productId}</@ofbizUrl>"
          aria-haspopup="dialog" aria-label="${rawString(uiLabelMap.ComplianceGaranNestedAlt)?replace("{0}", garan.years)}">${rawString(gw.renderGaranSvg(true, garan, "scpgn-" + product.productId))}</button>
        <span>${rawString(uiLabelMap.ComplianceGaranLine)?replace("{0}", garan.years)}</span>
      </li>
    </#if>
    <li>${rawString(uiLabelMap.ComplianceWithdrawalLine)?replace("{0}", ((profile.withdrawalDays)!14)?string)} <a href="<@ofbizUrl>withdraw</@ofbizUrl>">${uiLabelMap.ComplianceWithdrawHere}</a></li>
  </ul>
  </#if>
</#macro>

<#-- Product safety (GPSR), packaging and seller block for the product page. -->
<#macro productSafety product>
  <#local storeId = Static["org.ofbiz.product.store.ProductStoreWorker"].getProductStoreId(request)!>
  <#local info = Static["com.ilscipio.scipio.compliance.ProductSafetyWorker"].getSafetyInfo(delegator, product, storeId)>
  <#local manu = info.manufacturer!{}>
  <#local resp = info.euResponsible!{}>
  <#local seller = info.seller!{}>
  <section class="scp-safety" aria-labelledby="scp-safety-title-${product.productId}">
    <h2 id="scp-safety-title-${product.productId}" class="scp-safety-title">${uiLabelMap.ComplianceProductSafety}</h2>
    <dl class="scp-safety-list">
      <#if seller?has_content>
        <dt>${uiLabelMap.ComplianceSoldBy}</dt>
        <dd><strong><a href="<@ofbizUrl>seller?sellerId=${seller.partyId}</@ofbizUrl>">${seller.displayName!seller.name!}</a></strong><#if (seller.sellerType!"") == "SELLER_BUSINESS"> &middot; ${uiLabelMap.ComplianceBusinessSeller}<#elseif (seller.sellerType!"") == "SELLER_PRIVATE"> &middot; ${uiLabelMap.CompliancePrivateSeller}</#if>
          <#if seller.address??><br/>${seller.address}</#if><#if seller.email??><br/><a href="mailto:${seller.email}">${seller.email}</a></#if>
          <#if seller.tradeRegister?has_content><br/>${uiLabelMap.ComplianceTradeRegister}: ${seller.tradeRegister}</#if><#if seller.vatId?has_content> &middot; ${uiLabelMap.ComplianceVatId}: ${seller.vatId}</#if></dd>
      </#if>
      <dt>${uiLabelMap.ComplianceManufacturer}</dt>
      <dd><#if manu.name??>${manu.name}<#if manu.address??><br/>${manu.address}</#if><#if manu.email??><br/><a href="mailto:${manu.email}">${manu.email}</a></#if><#if manu.web??><br/><a href="${manu.web}" rel="noopener">${manu.web}</a></#if><#else>${uiLabelMap.ComplianceNotGiven}</#if></dd>
      <#if resp.name??>
        <dt>${uiLabelMap.ComplianceEuResponsible}</dt>
        <dd>${resp.name}<#if resp.address??><br/>${resp.address}</#if><#if resp.email??><br/><a href="mailto:${resp.email}">${resp.email}</a></#if></dd>
      </#if>
      <#if info.identifiers?has_content>
        <dt>${uiLabelMap.ComplianceProductIdentifier}</dt>
        <dd class="scp-mono"><#list info.identifiers as id>${id.type} ${id.value}<#sep> &middot; </#sep></#list></dd>
      </#if>
      <#if info.packaging?has_content>
        <dt>${uiLabelMap.CompliancePackaging}</dt>
        <dd><#list info.packaging as pk>${pk.material}<#if pk.weight??> &middot; ${pk.weight} ${pk.weightUomId!}</#if><#if pk.description?has_content> &middot; ${pk.description}</#if><#sep><br/></#sep></#list></dd>
      </#if>
    </dl>
  </section>
</#macro>

<#-- Price reference: the old price the shop may show (EU: lowest price of the last 30 days). -->
<#function priceReference product currentPrice currencyUomId listPrice=0 record=true>
  <#local storeId = Static["org.ofbiz.product.store.ProductStoreWorker"].getProductStoreId(request)!>
  <#return Static["com.ilscipio.scipio.compliance.PriceHistoryWorker"].getReferencePrice(delegator, product.productId, storeId, currencyUomId, currentPrice, listPrice, record)>
</#function>

<#-- Checkout review: pre-contract information and the terms checkbox (inside the order form). -->
<#macro checkoutBlock>
  <#local storeId = Static["org.ofbiz.product.store.ProductStoreWorker"].getProductStoreId(request)!>
  <#local ldw = Static["com.ilscipio.scipio.compliance.LegalDocumentWorker"]>
  <#local profile = ldw.getProfile(delegator, storeId)!>
  <#local eu = !profile?has_content || ldw.hasJurisdiction(profile, "EU")>
  <#local terms = ldw.getPublished(delegator, storeId, "LEGDOC_TERMS", locale)!>
  <#local privacy = ldw.getPublished(delegator, storeId, "LEGDOC_PRIVACY", locale)!>
  <div class="scp-checkout-legal">
    <#if eu>
      <div class="scp-precontract">
        <strong>${uiLabelMap.ComplianceBeforeYouOrder}</strong>
        <span>${rawString(uiLabelMap.ComplianceWithdrawalLine)?replace("{0}", ((profile.withdrawalDays)!14)?string)} <a href="<@ofbizUrl>legal?doc=withdrawal</@ofbizUrl>" target="_blank">${uiLabelMap.ComplianceWithdrawalPolicy}</a></span>
        <span>${rawString(uiLabelMap.ComplianceLegalGuaranteeLine)?replace("{0}", ((profile.legalGuaranteeYears)!2)?string)} <@guaranteeNoticeLink/></span>
      </div>
    </#if>
    <#if profile?has_content>
      <label class="scp-terms">
        <input type="checkbox" name="scpTermsAccepted" value="Y" required="required"/>
        <span>${uiLabelMap.ComplianceAcceptTermsPrefix} <a href="<@ofbizUrl>legal?doc=terms</@ofbizUrl>" target="_blank">${uiLabelMap.ComplianceTermsLink}</a><#if (terms.versionNum)??> (${uiLabelMap.ComplianceVersion} ${terms.versionNum})</#if>. ${uiLabelMap.ComplianceReadPrivacyPrefix} <a href="<@ofbizUrl>legal?doc=privacy</@ofbizUrl>" target="_blank">${uiLabelMap.CompliancePrivacyLink}</a><#if (privacy.versionNum)??> (${uiLabelMap.ComplianceVersion} ${privacy.versionNum})</#if>.</span>
      </label>
      <#if !eu><p class="scp-notice-collection"><a href="<@ofbizUrl>legal?doc=notice-at-collection</@ofbizUrl>" target="_blank">${uiLabelMap.ComplianceNoticeAtCollection}</a></p></#if>
    </#if>
  </div>
</#macro>

<#-- The order button text: "Order with obligation to pay" for EU stores with the profile flag. -->
<#function orderButtonText>
  <#local storeId = Static["org.ofbiz.product.store.ProductStoreWorker"].getProductStoreId(request)!>
  <#local ldw = Static["com.ilscipio.scipio.compliance.LegalDocumentWorker"]>
  <#local profile = ldw.getProfile(delegator, storeId)!>
  <#if profile?has_content && (profile.euOrderButton!"N") == "Y" && ldw.hasJurisdiction(profile, "EU")>
    <#return uiLabelMap.ComplianceOrderButton>
  </#if>
  <#return uiLabelMap.OrderSubmitOrder>
</#function>

<#-- Home page: newest verified marketplace sellers (marketplace stores only). -->
<#macro newSellers limit=4 title="">
  <#local storeId = Static["org.ofbiz.product.store.ProductStoreWorker"].getProductStoreId(request)!>
  <#local mw = Static["com.ilscipio.scipio.compliance.MarketplaceWorker"]>
  <#if mw.isMarketplace(delegator, storeId)>
    <#local sellers = mw.getNewSellers(delegator, storeId, limit)>
    <#if sellers?has_content>
    <section class="scp-new-sellers" aria-labelledby="scp-new-sellers-title">
      <h2 id="scp-new-sellers-title">${title?has_content?then(title, uiLabelMap.ComplianceNewSellers)}</h2>
      <ul>
        <#list sellers as s>
          <li><a href="<@ofbizUrl>seller?sellerId=${s.partyId}</@ofbizUrl>">
            <span class="scp-seller-mark" aria-hidden="true">${s.letter}</span>
            <span class="scp-new-seller-text"><strong>${s.displayName}</strong><span>${s.category!}<#if s.joinedDate??> &middot; ${uiLabelMap.ComplianceJoined} ${s.joinedDate?date?string.medium}</#if></span></span>
          </a></li>
        </#list>
      </ul>
      <p class="scp-new-sellers-note">${uiLabelMap.ComplianceSellerInfoNote} <a href="<@ofbizUrl>legal?doc=sellers</@ofbizUrl>">${uiLabelMap.ComplianceHowWeCheckSellers}</a></p>
    </section>
    </#if>
  </#if>
</#macro>
