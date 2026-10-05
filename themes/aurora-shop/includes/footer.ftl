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
SCIPIO: 4.0.0: Aurora Shop footer - closes the content area; newsletter, links, the store's legal texts and
privacy actions (compliance component), the consent dialog and the footer scripts.
-->
<#assign asStore = Static["org.ofbiz.product.store.ProductStoreWorker"].getProductStore(request)!>
<#assign asStoreName = (asStore.storeName)!(layoutSettings.companyName!"")>
<#assign asCompliance = Static["org.ofbiz.base.component.ComponentConfig"].isComponentEnabled("compliance")>
<#if asCompliance><#import "component://compliance/templates/shop/complianceLib.ftl" as compliance></#if>
<#-- the imprint also sits in the bottom line, next to the copyright, where shoppers look for it -->
<#assign asImprint = {}>
<#if asCompliance>
  <#list Static["com.ilscipio.scipio.compliance.LegalDocumentWorker"].getNavigation(delegator, (asStore.productStoreId)!"", locale)![] as n>
    <#if n.slug == "imprint"><#assign asImprint = n><#break></#if>
  </#list>
</#if>
<#include "component://aurora-shop-theme/includes/labels.ftl">
<#-- newsletter: the first current contact list of the web site (WebSiteContactList), as ContactList.groovy finds them -->
<#assign asNewsListId = "">
<#assign asWebSiteId = Static["org.ofbiz.webapp.website.WebSiteWorker"].getWebSiteId(request)!"">
<#if asWebSiteId?has_content>
  <#assign asNewsLists = Static["org.ofbiz.entity.util.EntityUtil"].filterByDate(delegator.findByAnd("WebSiteContactList", {"webSiteId": asWebSiteId}, ["fromDate"], true))![]>
  <#-- an e-mail list; a NEWSLETTER list first -->
  <#list asNewsLists as wscl>
    <#assign asCl = delegator.findOne("ContactList", {"contactListId": wscl.contactListId}, true)!>
    <#if asCl?has_content && (asCl.contactMechTypeId!"EMAIL_ADDRESS") == "EMAIL_ADDRESS">
      <#if (asCl.contactListTypeId!"") == "NEWSLETTER"><#assign asNewsListId = asCl.contactListId><#break></#if>
      <#if !asNewsListId?has_content><#assign asNewsListId = asCl.contactListId></#if>
    </#if>
  </#list>
</#if>
</main>
<footer class="as-footer">
  <div class="as-footer-inner">
    <div class="as-footer-top">
      <div class="as-footer-news">
        <p class="as-footer-claim">${asL("AuroraShopNewsletter")}</p>
        <#if asNewsListId?has_content>
        <form class="as-news-form" method="post" action="<@ofbizUrl>signUpForContactList</@ofbizUrl>">
          <input type="hidden" name="contactListId" value="${asNewsListId}"/>
          <input type="hidden" name="baseLocation" value="${request.getContextPath()}"/>
          <label class="as-sr" for="as-news-email">${uiLabelMap.CommonEmail!"E-mail"}</label>
          <input type="email" id="as-news-email" name="email" required="required" autocomplete="email" placeholder="${uiLabelMap.CommonEmail!"E-mail"}"/>
          <button type="submit" class="as-btn as-btn--light">${asL("AuroraShopSubscribe")}</button>
        </form>
        </#if>
        <#if asCompliance><p class="as-footer-note"><a href="<@ofbizUrl>legal?doc=privacy</@ofbizUrl>">${uiLabelMap.CompliancePrivacyLink!"Privacy policy"}</a></p></#if>
      </div>
      <div class="as-footer-cols">
        <div class="as-footer-col">
          <p class="as-footer-head">${uiLabelMap.CommonHelp!"Help"}</p>
          <ul>
            <li><a href="<@ofbizUrl><#if ((userLogin.userLoginId)!"anonymous") != "anonymous">orderhistory<#else>checkLogin</#if></@ofbizUrl>">${uiLabelMap.CommonOrders!"Orders"}</a></li>
            <li><a href="<@ofbizUrl>contactus</@ofbizUrl>">${uiLabelMap.CommonContactUs!"Contact"}</a></li>
          </ul>
        </div>
        <#if asCompliance>
        <div class="as-footer-col as-footer-col--wide">
          <p class="as-footer-head">${uiLabelMap.ComplianceLegal}</p>
          <@compliance.footerLinks listClass="as-footer-list"/>
        </div>
        <div class="as-footer-col">
          <p class="as-footer-head">${uiLabelMap.ComplianceYourChoices!"Your data"}</p>
          <@compliance.footerActions listClass="as-footer-list"/>
        </div>
        </#if>
      </div>
    </div>
    <div class="as-footer-bottom">
      <img src="<@ofbizContentUrl>/aurora/images/scipio-logo-small.svg</@ofbizContentUrl>" alt="" width="17" height="20"/>
      <span>&copy; ${nowTimestamp?string("yyyy")} ${asStoreName}</span>
      <#if asImprint?has_content><a href="<@ofbizUrl>legal?doc=imprint</@ofbizUrl>">${asImprint.label}</a></#if>
      <span>${uiLabelMap.OrderSalesTaxIncluded!"Prices incl. VAT"}</span>
      <span class="as-spacer"></span>
      <span>${uiLabelMap.CommonPoweredBy!"Powered by"} <a href="https://www.scipioerp.com" rel="noopener">SCIPIO ERP</a></span>
    </div>
  </div>
</footer>
<#if asCompliance><@compliance.consentUi/></#if>
<@scripts output=true>
    <#list ["VT_FTPR_JAVASCRIPT", "javaScriptsFooter", "VT_FTR_JAVASCRIPT"] as jsKey>
        <#assign jsList = (jsKey == "javaScriptsFooter")?then(layoutSettings.javaScriptsFooter![], layoutSettings[jsKey]![])>
        <#if jsList?has_content>
            <#assign javaScriptsSet = toSet(jsList)/>
            <#list jsList as javaScript>
                <#if javaScriptsSet.contains(javaScript)><#assign nothing = javaScriptsSet.remove(javaScript)/><@script src=makeOfbizContentUrl(javaScript) /></#if>
            </#list>
        </#if>
    </#list>
    <#assign scpScriptBuffer = getRequestVar("scipioScriptBuffer")!"">
    <#if scpScriptBuffer?has_content><@script merge=false>${scpScriptBuffer}</@script></#if>
    <#if layoutSettings.VT_BTM_JAVASCRIPT?has_content>
        <#list layoutSettings.VT_BTM_JAVASCRIPT as javaScript><@script src=makeOfbizContentUrl(javaScript) /></#list>
    </#if>
</@scripts>
</body>
</html>
