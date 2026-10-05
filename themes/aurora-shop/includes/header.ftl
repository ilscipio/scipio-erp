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
SCIPIO: 4.0.0: Aurora Shop header - the head assets, the body tag and the store header
(logo, categories, search, account, bag). appbarClose.ftl opens the content area, footer.ftl closes it.
-->
<#assign userHasAccount = (userLogin?? && (userLogin.userLoginId!"anonymous") != "anonymous")>
<#assign asStore = Static["org.ofbiz.product.store.ProductStoreWorker"].getProductStore(request)!>
<#assign asStoreName = (asStore.storeName)!(layoutSettings.companyName!"")>
<#assign asCatalogId = Static["org.ofbiz.product.catalog.CatalogWorker"].getCurrentCatalogId(request)!>
<#assign asTopCatId = (asCatalogId?has_content)?then(Static["org.ofbiz.product.catalog.CatalogWorker"].getCatalogTopCategoryId(request, asCatalogId)!"", "")>
<#assign asNavCats = []>
<#if asTopCatId?has_content>
  <#assign asNavCats = Static["org.ofbiz.entity.util.EntityUtil"].filterByDate(delegator.findByAnd("ProductCategoryRollup", {"parentProductCategoryId": asTopCatId}, ["sequenceNum"], true))!>
</#if>
<#assign asCart = sessionAttributes.shoppingCart!>
<#assign asCartCount = 0>
<#if asCart?has_content><#assign asCartCount = asCart.getTotalQuantity()!0></#if>

<@scripts output=true>
    <title>${asStoreName}<#if title?has_content>: ${title}<#elseif titleProperty?has_content>: ${uiLabelMap[titleProperty]}</#if></title>
    <meta name="viewport" content="width=device-width, initial-scale=1"/>
    <#if layoutSettings.VT_SHORTCUT_ICON?has_content>
        <link rel="shortcut icon" href="<@ofbizContentUrl>${raw(layoutSettings.VT_SHORTCUT_ICON.get(0))}</@ofbizContentUrl>" />
    </#if>
    <link rel="preload" href="<@ofbizContentUrl>/aurora/fonts/geist-latin-wght-normal.woff2</@ofbizContentUrl>" as="font" type="font/woff2" crossorigin/>
    <link rel="preload" href="<@ofbizContentUrl>/aurora/fonts/bricolage-grotesque-latin-wght-normal.woff2</@ofbizContentUrl>" as="font" type="font/woff2" crossorigin/>
    <#if layoutSettings.VT_STYLESHEET?has_content>
        <#list layoutSettings.VT_STYLESHEET as styleSheet>
            <link rel="stylesheet" href="<@ofbizContentUrl>${raw(styleSheet)}</@ofbizContentUrl>" type="text/css"/>
        </#list>
    </#if>
    <#-- the store stylesheet also loads on a database that does not have its theme resource row yet -->
    <#assign asStoreCssListed = false>
    <#list (layoutSettings.VT_STYLESHEET![]) as styleSheet><#if raw(styleSheet) == "/aurora-shop/css/aurora-shop-store.css"><#assign asStoreCssListed = true></#if></#list>
    <#if !asStoreCssListed>
        <link rel="stylesheet" href="<@ofbizContentUrl>/aurora-shop/css/aurora-shop-store.css</@ofbizContentUrl>" type="text/css"/>
    </#if>
    <#if layoutSettings.styleSheets?has_content>
        <#list layoutSettings.styleSheets as styleSheet>
            <link rel="stylesheet" href="<@ofbizContentUrl>${raw(styleSheet)}</@ofbizContentUrl>" type="text/css"/>
        </#list>
    </#if>
    <#list ["VT_TOP_JAVASCRIPT", "VT_PRIO_JAVASCRIPT"] as jsKey>
        <#if layoutSettings[jsKey]?has_content>
            <#assign javaScriptsSet = toSet(layoutSettings[jsKey])/>
            <#list layoutSettings[jsKey] as javaScript>
                <#if javaScriptsSet.contains(javaScript)><#assign nothing = javaScriptsSet.remove(javaScript)/><@script src=makeOfbizContentUrl(javaScript) /></#if>
            </#list>
        </#if>
    </#list>
    <#if layoutSettings.javaScripts?has_content>
        <#assign javaScriptsSet = toSet(layoutSettings.javaScripts)/>
        <#list layoutSettings.javaScripts as javaScript>
            <#if javaScriptsSet.contains(javaScript)><#assign nothing = javaScriptsSet.remove(javaScript)/><@script src=makeOfbizContentUrl(javaScript) /></#if>
        </#list>
    </#if>
    <#if layoutSettings.VT_HDR_JAVASCRIPT?has_content>
        <#list layoutSettings.VT_HDR_JAVASCRIPT as javaScript><@script src=makeOfbizContentUrl(javaScript) /></#list>
    </#if>
    <#if layoutSettings.VT_EXTRA_HEAD?has_content>
        <#list layoutSettings.VT_EXTRA_HEAD as extraHead>${extraHead}</#list>
    </#if>
</@scripts>
</head>
<body class="as-body<#if parameters._CURRENT_VIEW_?has_content> page-${parameters._CURRENT_VIEW_!}</#if> <#if userHasAccount>page-auth<#else>page-noauth</#if>">
<a class="as-skip" href="#as-content">${uiLabelMap.CommonSkipToContent!"Skip to content"}</a>
<#-- an announcement above the header, for example "Demo store": entity properties of the resource "shop" (SystemProperty
     rows): shop.announcement.text, .badge, .linkText (each also as .<language>, for example .de) and .link. No text: no bar. -->
<#function asAnnounce key>
  <#local eup = Static["org.ofbiz.entity.util.EntityUtilProperties"]>
  <#local v = rawString(eup.getPropertyValue("shop", "shop.announcement." + key + "." + ((locale.getLanguage())!"en"), delegator)!"")>
  <#if !v?has_content><#local v = rawString(eup.getPropertyValue("shop", "shop.announcement." + key, delegator)!"")></#if>
  <#return v?trim>
</#function>
<#assign asAnnText = asAnnounce("text")>
<#if asAnnText?has_content>
  <#assign asAnnLink = asAnnounce("link")>
  <div class="as-announce" role="note">
    <p class="as-announce-inner">
      <#if asAnnounce("badge")?has_content><span class="as-announce-badge">${escapeVal(asAnnounce("badge"), "html")}</span></#if>
      <span class="as-announce-text">${escapeVal(asAnnText, "html")}</span>
      <#if (asAnnLink?starts_with("http") || asAnnLink?starts_with("/")) && asAnnounce("linkText")?has_content>
        <a class="as-announce-link" href="${escapeVal(asAnnLink, "html")}">${escapeVal(asAnnounce("linkText"), "html")}</a>
      </#if>
    </p>
  </div>
</#if>
<header class="as-header">
  <div class="as-header-inner">
    <button type="button" class="as-icon-btn as-menu-btn" data-as-toggle="as-nav" aria-controls="as-nav" aria-expanded="false" aria-label="${uiLabelMap.CommonMenu!"Menu"}">
      <svg width="22" height="22" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="1.8" stroke-linecap="round" aria-hidden="true"><path d="M4 8h16M4 16h16"/></svg>
    </button>
    <a class="as-logo" href="<@ofbizUrl>main</@ofbizUrl>">
      <img src="<@ofbizContentUrl>/aurora/images/scipio-logo-small.svg</@ofbizContentUrl>" alt="" width="24" height="28"/>
      <span class="as-wordmark">${asStoreName}</span>
    </a>
    <nav class="as-nav" id="as-nav" aria-label="${uiLabelMap.ProductCategories!"Categories"}">
      <ul>
        <#list asNavCats as rollup>
          <#if rollup?index gte 7><#break></#if>
          <#assign navCat = delegator.findOne("ProductCategory", {"productCategoryId": rollup.productCategoryId}, true)!>
          <#if navCat?has_content>
            <#assign navName = (Static["org.ofbiz.product.category.CategoryContentWrapper"].getProductCategoryContentAsText(navCat, "CATEGORY_NAME", locale, dispatcher, "raw"))!"">
            <li><a href="<@ofbizUrl>category?category_id=${navCat.productCategoryId}</@ofbizUrl>">${navName?has_content?then(navName, navCat.categoryName!navCat.productCategoryId)}</a></li>
          </#if>
        </#list>
      </ul>
    </nav>
    <form class="as-search" method="get" action="<@ofbizUrl>keywordsearch</@ofbizUrl>" role="search">
      <svg width="18" height="18" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="1.8" stroke-linecap="round" aria-hidden="true"><circle cx="11" cy="11" r="7"/><path d="m20 20-3.5-3.5"/></svg>
      <input type="search" name="SEARCH_STRING" value="${parameters.SEARCH_STRING!}" placeholder="${uiLabelMap.CommonSearch!"Search"}" aria-label="${uiLabelMap.CommonSearch!"Search"}"/>
      <input type="hidden" name="SEARCH_CATALOG_ID" value="${asCatalogId!}"/>
    </form>
    <div class="as-actions">
      <#assign asUserIcon><svg width="22" height="22" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="1.8" stroke-linecap="round" aria-hidden="true"><circle cx="12" cy="8" r="4"/><path d="M4 21c1.5-4 4.5-6 8-6s6.5 2 8 6"/></svg></#assign>
      <#if userHasAccount>
        <#-- the account menu: every page of the customer account and sign out (details/summary, closes on an outside click) -->
        <#assign asPerson = (userLogin.partyId??)?then(delegator.findOne("Person", {"partyId": userLogin.partyId}, true)!, "")>
        <details class="as-acct">
          <summary class="as-icon-btn" aria-label="${uiLabelMap.CommonProfile!"Account"}">${asUserIcon}</summary>
          <div class="as-acct-menu">
            <p class="as-acct-hello"><#if asPerson?has_content && asPerson.firstName?has_content>${asPerson.firstName} ${asPerson.lastName!}<#else>${userLogin.userLoginId}</#if></p>
            <a href="<@ofbizUrl>viewprofile</@ofbizUrl>">${uiLabelMap.CommonProfile!"Profile"}</a>
            <a href="<@ofbizUrl>orderhistory</@ofbizUrl>">${uiLabelMap.EcommerceOrderHistory!"Orders"}</a>
            <a href="<@ofbizUrl>editShoppingList</@ofbizUrl>">${uiLabelMap.EcommerceShoppingLists!"Shopping lists"}</a>
            <a href="<@ofbizUrl>messagelist</@ofbizUrl>">${uiLabelMap.CommonMessages!"Messages"}</a>
            <a class="as-acct-out" href="<@ofbizUrl>logout</@ofbizUrl>">${uiLabelMap.CommonLogout!"Sign out"}</a>
          </div>
        </details>
      <#else>
        <a class="as-icon-btn" href="<@ofbizUrl>checkLogin</@ofbizUrl>" aria-label="${uiLabelMap.CommonLogin!"Login"}">${asUserIcon}</a>
      </#if>
      <a class="as-icon-btn as-bag" href="<@ofbizUrl>showcart</@ofbizUrl>" aria-label="${uiLabelMap.OrderShoppingCart!"Bag"} (${asCartCount})">
        <svg width="22" height="22" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="1.8" stroke-linejoin="round" aria-hidden="true"><path d="M5 8h14l-1 12H6L5 8Z"/><path d="M9 8V6a3 3 0 0 1 6 0v2"/></svg>
        <#if (asCartCount > 0)><span class="as-bag-count">${asCartCount?int}</span></#if>
      </a>
    </div>
  </div>
</header>
