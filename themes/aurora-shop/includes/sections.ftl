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
SCIPIO: 4.0.0: Aurora Shop - home section library. One macro per section; all data is live store data (products,
prices, promotions, sellers, reviews, compliance profile). The theme home (home.ftl) calls the macros with demo
defaults; the CMS home template calls them with the values a merchant set in the CMS (texts, images, ids).
Parameters are strings, so CMS attributes pass straight through. Merchant values are output escaped.
-->
<#include "component://aurora-shop-theme/includes/labels.ftl">
<#include "component://aurora-shop-theme/includes/homeLabels.ftl">
<#assign asCompliance = Static["org.ofbiz.base.component.ComponentConfig"].isComponentEnabled("compliance")>
<#if asCompliance><#import "component://compliance/templates/shop/complianceLib.ftl" as compliance></#if>
<#assign asStore = Static["org.ofbiz.product.store.ProductStoreWorker"].getProductStore(request)!>
<#assign asStoreId = (asStore.productStoreId)!"">
<#assign asCatalogId = Static["org.ofbiz.product.catalog.CatalogWorker"].getCurrentCatalogId(request)!"">
<#assign asTopCatId = (asCatalogId?has_content)?then(Static["org.ofbiz.product.catalog.CatalogWorker"].getCatalogTopCategoryId(request, asCatalogId)!"", "")>
<#assign asPromoCatId = (asCatalogId?has_content)?then(Static["org.ofbiz.product.catalog.CatalogWorker"].getCatalogPromotionsCategoryId(request, asCatalogId)!"", "")>
<#assign asProfile = asCompliance?then(Static["com.ilscipio.scipio.compliance.LegalDocumentWorker"].getProfile(delegator, asStoreId)!, "")>
<#assign asNow = Static["org.ofbiz.base.util.UtilDateTime"].nowTimestamp()>
<#assign asIconPrev><svg width="18" height="18" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" aria-hidden="true"><path d="M19 12H5M11 6l-6 6 6 6"/></svg></#assign>
<#assign asIconNext><svg width="18" height="18" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" aria-hidden="true"><path d="M5 12h14M13 6l6 6-6 6"/></svg></#assign>

<#-- ======== helpers ======== -->
<#-- products already shown on this page (request attribute, so CMS assets share it) -->
<#-- rawString: in CMS renders a string read back from the request is HTML-encoded; storing it again would encode it
     again on every call (exponential growth, "Java heap space") -->
<#function asShownIds><#return rawString(request.getAttribute("asShownIds")!"")?split(",")></#function>
<#function asMarkShown pid>
  <#local ignored = request.setAttribute("asShownIds", rawString(request.getAttribute("asShownIds")!"") + "," + rawString(pid))!>
  <#return "">
</#function>
<#function asIdList s><#return (s!"")?split(",")?map(x -> x?trim)?filter(x -> x?has_content)></#function>
<#function asProduct pid><#return (delegator.findOne("Product", {"productId": pid}, true))!></#function>
<#function asWrapper p><#return Static["org.ofbiz.product.product.ProductContentWrapper"].makeProductContentWrapper(p, request)></#function>
<#function asProductName p><#return (asWrapper(p).get("PRODUCT_NAME", "raw"))!p.productName!p.productId></#function>
<#function asProductImage p size="MEDIUM_IMAGE_URL">
  <#local w = asWrapper(p)>
  <#local img = (w.get(size, "url"))!"">
  <#if !img?has_content><#local img = (w.get("MEDIUM_IMAGE_URL", "url"))!""></#if>
  <#if !img?has_content><#local img = (w.get("SMALL_IMAGE_URL", "url"))!""></#if>
  <#return img>
</#function>
<#function asPrice p>
  <#local ctx = {"product": p, "productStoreId": asStoreId, "currencyUomId": (sessionAttributes.currencyUom)!((asStore.defaultCurrencyUomId)!"USD")}>
  <#if webSiteId?has_content><#local ctx = ctx + {"webSiteId": webSiteId}></#if>
  <#if userLogin??><#local ctx = ctx + {"userLogin": userLogin}></#if>
  <#return (dispatcher.runSync("calculateProductPrice", ctx))!{}>
</#function>
<#-- products of categories: with an image and a distinct name first; skips virtual variants and shown products -->
<#function asPickProducts catIds max=10 imagesOnly=false>
  <#local ids = []><#local names = []><#local noImg = []><#local shown = asShownIds()>
  <#list catIds as catId>
    <#if !catId?has_content><#continue></#if>
    <#list Static["org.ofbiz.entity.util.EntityUtil"].filterByDate(delegator.findByAnd("ProductCategoryMember", {"productCategoryId": catId}, ["sequenceNum"], true)) as m>
      <#if (ids?size >= max)><#break></#if>
      <#if ids?seq_contains(m.productId) || shown?seq_contains(m.productId)><#continue></#if>
      <#local p = asProduct(m.productId)>
      <#if !p?has_content || (p.isVariant!"N") == "Y"><#continue></#if>
      <#local n = asProductName(p)>
      <#if names?seq_contains(n)><#continue></#if>
      <#local names = names + [n]>
      <#if asProductImage(p, "SMALL_IMAGE_URL")?has_content><#local ids = ids + [m.productId]><#elseif !imagesOnly><#local noImg = noImg + [m.productId]></#if>
    </#list>
  </#list>
  <#list noImg as n><#if (ids?size >= max)><#break></#if><#local ids = ids + [n]></#list>
  <#return ids>
</#function>
<#function asTopCategories max=8>
  <#local cats = []>
  <#if asTopCatId?has_content>
    <#list Static["org.ofbiz.entity.util.EntityUtil"].filterByDate(delegator.findByAnd("ProductCategoryRollup", {"parentProductCategoryId": asTopCatId}, ["sequenceNum"], true)) as r>
      <#if (cats?size >= max)><#break></#if>
      <#local c = asCategory(r.productCategoryId)>
      <#if c?has_content><#local cats = cats + [c]></#if>
    </#list>
  </#if>
  <#return cats>
</#function>
<#function asCategory catId>
  <#local c = (delegator.findOne("ProductCategory", {"productCategoryId": catId}, true))!>
  <#if !c?has_content><#return {}></#if>
  <#local n = (Static["org.ofbiz.product.category.CategoryContentWrapper"].getProductCategoryContentAsText(c, "CATEGORY_NAME", locale, dispatcher, "raw"))!"">
  <#return {"id": c.productCategoryId, "name": n?has_content?then(n, c.categoryName!c.productCategoryId)}>
</#function>
<#-- CMS values: BOOLEAN and INTEGER attributes may arrive as booleans, numbers or strings -->
<#function asTrue v dflt=true>
  <#if !v??><#return dflt></#if>
  <#if v?is_boolean><#return v></#if>
  <#local s = v?string?trim?lower_case>
  <#if !s?has_content><#return dflt></#if>
  <#return s == "true" || s == "y" || s == "yes" || s == "1">
</#function>
<#function asNum v dflt=0>
  <#if !v??><#return dflt></#if>
  <#if v?is_number><#return v></#if>
  <#-- an empty field is the default, not an error (#attempt logs each failure) -->
  <#local s = v?string?trim>
  <#if !s?has_content><#return dflt></#if>
  <#attempt><#return s?number><#recover><#return dflt></#attempt>
</#function>
<#-- journal stories: CMS pages of the story template (AS_STORY), newest first; each with its mapped URL -->
<#function asJournalStories max=3 excludePageId="">
  <#local out = []>
  <#list delegator.findByAnd("CmsPage", {"pageTemplateId": "AS_STORY"}, null, true)![] as pg>
    <#if pg.pageId == excludePageId><#continue></#if>
    <#local st = (Static["org.ofbiz.entity.util.EntityUtil"].getFirst(delegator.findByAnd("CmsPageVersionState", {"pageId": pg.pageId, "versionStateId": "CMS_VER_ACTIVE"}, null, true)))!>
    <#if !st?has_content><#continue></#if>
    <#local ver = (delegator.findOne("CmsPageVersion", {"versionId": st.versionId}, true))!>
    <#local cnt = (ver?has_content)?then((delegator.findOne("Content", {"contentId": ver.contentId!""}, true))!, "")>
    <#local txt = (cnt?has_content)?then((delegator.findOne("ElectronicText", {"dataResourceId": cnt.dataResourceId!""}, true))!, "")>
    <#if !txt?has_content><#continue></#if>
    <#local data = {}>
    <#attempt><#local data = rawString(txt.textData!"{}")?eval_json><#recover></#attempt>
    <#local map = (Static["org.ofbiz.entity.util.EntityUtil"].getFirst(delegator.findByAnd("CmsProcessMapping", {"primaryForPageId": pg.pageId, "active": "Y"}, null, true)))!>
    <#local link = (map?has_content)?then(request.getContextPath() + (map.sourcePath!""), "")>
    <#local out = out + [{"pageId": pg.pageId, "title": data.title!pg.pageName!"", "excerpt": data.excerpt!"", "image": data.coverImage!"",
        "eyebrow": data.eyebrow!"", "minutes": data.minutes!"", "date": data.publishedDate!"", "link": link}]>
  </#list>
  <#local out = out?sort_by("date")?reverse>
  <#return (out?size > max)?then(out[0..(max - 1)], out)>
</#function>
<#-- merchant HTML (CMS story body): OWASP policies for text formatting, blocks, links, images and tables -->
<#function asSafeHtml html>
  <#local S = Static["org.owasp.html.Sanitizers"]>
  <#return S.FORMATTING.and(S.BLOCKS).and(S.LINKS).and(S.IMAGES).and(S.TABLES).sanitize(rawString(html!""))>
</#function>
<#-- a link target: a URL or path is used as is, anything else is a shop request -->
<#macro asHref target><#if target?starts_with("http") || target?starts_with("/") || target?starts_with("#")>${target}<#else><@ofbizUrl>${target}</@ofbizUrl></#if></#macro>
<#macro asImgSrc src><#if src?starts_with("http")>${src}<#else><@contentUrl ctxPrefix=true>${src}</@contentUrl></#if></#macro>
<#-- a title: escaped; a word in *stars* is set in the italic accent font -->
<#function asRich s><#return rawString(s!"")?html?replace("\\*([^*<>]+)\\*", "<em>$1</em>", "r")></#function>
<#-- a colour hue for a name (seller marks and gradients) -->
<#function asHue s>
  <#local l = rawString(s!"")?upper_case>
  <#local i = l?has_content?then("ABCDEFGHIJKLMNOPQRSTUVWXYZ"?index_of(l[0]), 7)>
  <#return ((i < 0)?then(7, i) * 47) % 360>
</#function>
<#macro asStars rating><span class="as-stars" aria-label="${rating?string("0.#")} / 5"><#list 1..5 as i><span class="<#if (rating >= i - 0.25)>is-on</#if>" aria-hidden="true">&#9733;</span></#list></span></#macro>
<#macro asSectionHead id eyebrow="" title="" text="" dot=false>
  <div class="as-section-head">
    <div>
      <#if eyebrow?has_content><p class="as-eyebrow"><#if dot><span class="as-dot" aria-hidden="true"></span></#if>${eyebrow}</p></#if>
      <h2 id="${id}">${asRich(title)}</h2>
      <#if text?has_content><p class="as-section-text">${text}</p></#if>
    </div>
    <#nested>
  </div>
</#macro>
<#macro asAddUrl>data-as-add-url="<@ofbizUrl>additem</@ofbizUrl>" data-as-cart-url="<@ofbizUrl>getCartData</@ofbizUrl>" data-as-bag-url="<@ofbizUrl>showcart</@ofbizUrl>" data-as-msg-ok="${asH("Added")}" data-as-msg-fail="${asH("AddFailed")}" data-as-msg-bag="${asH("ViewBag")}"</#macro>

<#-- ======== product card ======== -->
<#macro asProductCard pid size="" note="">
  <#local p = asProduct(pid)>
  <#if p?has_content>
    <#local ignored = asMarkShown(pid)>
    <#local img = asProductImage(p, (size == "large" || size == "hero")?then("LARGE_IMAGE_URL", "MEDIUM_IMAGE_URL"))>
    <#local priceRes = asPrice(p)>
    <#local price = priceRes.price!>
    <#local cur = priceRes.currencyUsed!"USD">
    <#local ref = {}>
    <#if asCompliance && price?has_content><#local ref = compliance.priceReference(p, price, cur, priceRes.listPrice!0, false)></#if>
    <a class="as-card<#if size?has_content> as-card--${size}</#if>" href="<@ofbizUrl>product?product_id=${pid}</@ofbizUrl>">
      <span class="as-card-img">
        <#if img?has_content><img src="<@contentUrl ctxPrefix=true>${img}</@contentUrl>" alt="" loading="lazy"/></#if>
        <#if (ref.percent!0) gt 0><span class="as-chip as-chip--dark">-${ref.percent}%</span><#elseif note?has_content><span class="as-chip as-chip--light">${note}</span></#if>
      </span>
      <span class="as-card-text">
        <span class="as-card-name">${asProductName(p)}</span>
        <span class="as-card-price">
          <#if price?has_content><b><#if (p.isVirtual!"N") == "Y">${asH("From")} </#if><@ofbizCurrency amount=price isoCode=cur/></b></#if>
          <#if ref.oldPrice??><s><@ofbizCurrency amount=ref.oldPrice isoCode=cur/></s></#if>
        </span>
        <#if (ref.rule!"") == "prior30"><#local priorFmt><@ofbizCurrency amount=ref.oldPrice isoCode=cur/></#local>
          <span class="as-card-note">${rawString(uiLabelMap.CompliancePriorPrice!"")?replace("{0}", priorFmt)}</span></#if>
      </span>
    </a>
  </#if>
</#macro>

<#-- ======== 1. hero stage (full width): slides = [{eyebrow, title, text, cta, link, productId, imageUrl, tone}].
     A slide is a colour panel with the text and a picture to the right edge: the image URL (a wide photo), else the
     product image on a white plate. The product of a slide shows as a chip with its price. tone: graphite, espresso,
     forest, cobalt, crimson, sand or light; empty: graphite, forest and espresso take turns. ======== -->
<#assign asTones = ["graphite", "espresso", "forest", "cobalt", "crimson", "sand", "light"]>
<#assign asToneCycle = ["graphite", "forest", "espresso"]>
<#function asPlain s><#return rawString(s!"")?replace("*", "")?html></#function>
<#macro asHero slides=[]>
  <#local list = slides?filter(s -> (s.title!"")?has_content || (s.productId!"")?has_content || (s.imageUrl!"")?has_content)>
  <#if !list?has_content>
    <#local picks = asPickProducts([asPromoCatId] + asTopCategories()?map(c -> c.id), 2, true)>
    <#local cats = asTopCategories(1)>
    <#local list = [{"eyebrow": (asStore.storeName)!"", "title": (asStore.title)!((asStore.storeName)!""), "text": (asStore.subtitle)!"",
        "cta": asL("AuroraShopShopNow"), "link": cats?has_content?then("category?category_id=" + cats[0].id, ""), "productId": (picks[0])!""}]>
    <#if (picks?size > 1)>
      <#local list = list + [{"eyebrow": asL("AuroraShopFeatured"), "title": asProductName(asProduct(picks[1])), "text": "", "cta": asL("AuroraShopViewProduct"),
          "link": "product?product_id=" + picks[1], "productId": picks[1]}]>
    </#if>
  </#if>
  <section class="as-stage" aria-roledescription="carousel" aria-label="${asL("AuroraShopFeatured")}" data-as-stage="true" data-as-interval="7000">
    <div class="as-stage-slides">
      <#list list as s>
        <#local p = (s.productId!"")?has_content?then(asProduct(s.productId), "")>
        <#if p?has_content><#local ignored = asMarkShown(s.productId)></#if>
        <#local photo = (s.imageUrl!"")?has_content>
        <#local pimg = (!photo && p?has_content)?then(asProductImage(p, "LARGE_IMAGE_URL"), "")>
        <#local tone = rawString(s.tone!"")?trim?lower_case>
        <#if !asTones?seq_contains(tone)><#local tone = asToneCycle[s?index % 3]></#if>
        <div class="as-stage-slide as-tone--${tone}<#if !photo> as-stage-slide--product</#if><#if s?index == 0> is-active</#if>" id="as-stage-${s?index}"
            role="tabpanel" aria-roledescription="slide" aria-labelledby="as-stage-tab-${s?index}" data-as-tone="${(tone == "light" || tone == "sand")?then("light", "dark")}"<#if s?index gt 0> aria-hidden="true" inert="inert"</#if>>
          <div class="as-stage-text">
            <#if (s.eyebrow!"")?has_content><p class="as-stage-eyebrow"><span class="as-dot" aria-hidden="true"></span>${s.eyebrow}</p></#if>
            <#if s?index == 0><h1 class="as-stage-title">${asRich(s.title!"")}</h1><#else><h2 class="as-stage-title">${asRich(s.title!"")}</h2></#if>
            <#if (s.text!"")?has_content><p class="as-stage-lead">${s.text}</p></#if>
            <#if (s.link!"")?has_content><p class="as-stage-cta"><a class="as-btn as-stage-btn" href="<@asHref (s.link)/>">${(s.cta!"")?has_content?then(s.cta, asL("AuroraShopShopNow"))}${asIconNext}</a></p></#if>
          </div>
          <div class="as-stage-media">
            <#if photo><img src="<@asImgSrc (s.imageUrl)/>" alt=""<#if s?index gt 0> loading="lazy"</#if>/>
            <#elseif pimg?has_content><img src="<@contentUrl ctxPrefix=true>${pimg}</@contentUrl>" alt=""<#if s?index gt 0> loading="lazy"</#if>/></#if>
            <#if p?has_content>
              <#local priceRes = asPrice(p)>
              <#local thumb = asProductImage(p, "SMALL_IMAGE_URL")>
              <a class="as-stage-chip" href="<@ofbizUrl>product?product_id=${s.productId}</@ofbizUrl>">
                <#if thumb?has_content><span class="as-stage-chip-img"><img src="<@contentUrl ctxPrefix=true>${thumb}</@contentUrl>" alt="" loading="lazy"/></span></#if>
                <span class="as-stage-chip-text"><b>${asProductName(p)}</b><#if priceRes.price?has_content><span><#if (p.isVirtual!"N") == "Y" || (p.productTypeId!"")?starts_with("AGGREGATED")>${asH("From")} </#if><@ofbizCurrency amount=priceRes.price isoCode=priceRes.currencyUsed!"USD"/></span></#if></span>
                <span class="as-stage-chip-go" aria-hidden="true">${asIconNext}</span>
              </a>
            </#if>
          </div>
        </div>
      </#list>
    </div>
    <#if (list?size > 1)>
    <div class="as-stage-nav">
      <div class="as-stage-tabs" role="tablist" aria-label="${asL("AuroraShopFeatured")}">
        <#list list as s>
          <button type="button" role="tab" class="as-stage-tab" id="as-stage-tab-${s?index}" aria-controls="as-stage-${s?index}" aria-selected="${(s?index == 0)?c}"<#if s?index gt 0> tabindex="-1"</#if> data-as-go="${s?index}">
            <span class="as-stage-bar" aria-hidden="true"><i></i></span>
            <span class="as-stage-num" aria-hidden="true">${(s?index + 1)?string("00")}</span>
            <span class="as-stage-name">${asPlain((s.title!"")?has_content?then(s.title, s.eyebrow!""))}</span>
          </button>
        </#list>
      </div>
      <div class="as-stage-arrows">
        <button type="button" class="as-stage-arrow" data-as-stage-prev="true" aria-label="${asL("AuroraShopPrevious")}">${asIconPrev}</button>
        <button type="button" class="as-stage-arrow" data-as-stage-next="true" aria-label="${asL("AuroraShopNext")}">${asIconNext}</button>
        <button type="button" class="as-stage-arrow as-stage-arrow--sm" data-as-stage-pause="true" aria-pressed="false" aria-label="${asL("AuroraShopPause")}"><svg width="16" height="16" viewBox="0 0 24 24" fill="currentColor" aria-hidden="true"><path d="M7 5h3v14H7zM14 5h3v14h-3z"/></svg></button>
      </div>
    </div>
    </#if>
  </section>
</#macro>

<#-- ======== 2. trust strip: lines separated by | (default: the store's legal settings) ======== -->
<#macro asTrust lines="">
  <#local items = rawString(lines!"")?split("|")?map(x -> x?trim)?filter(x -> x?has_content)>
  <#if items?has_content || (asCompliance && asProfile?has_content)>
  <ul class="as-trust">
    <#if items?has_content>
      <#list items as t><li>${escapeVal(t, "html")}</li></#list>
    <#else>
      <li>${rawString(uiLabelMap.ComplianceWithdrawalLine!"")?replace("{0}", (asProfile.withdrawalDays!14)?string)}</li>
      <li>${rawString(uiLabelMap.ComplianceLegalGuaranteeLine!"")?replace("{0}", (asProfile.legalGuaranteeYears!2)?string)}</li>
    </#if>
    <#if asCompliance><li><@compliance.guaranteeNoticeLink/></li></#if>
  </ul>
  </#if>
</#macro>

<#-- ======== 3. category marquee (full width) ======== -->
<#macro asMarquee categoryIds="">
  <#local cats = asIdList(categoryIds)?map(id -> asCategory(id))?filter(c -> c?has_content)>
  <#if !cats?has_content><#local cats = asTopCategories()></#if>
  <#if cats?has_content>
  <nav class="as-marquee" aria-label="${asL("AuroraShopRooms")}">
    <div class="as-marquee-track">
      <#list 1..2 as pass>
        <#list cats as c>
          <a class="as-marquee-word<#if c?index % 2 == 1> is-outline</#if>" href="<@ofbizUrl>category?category_id=${c.id}</@ofbizUrl>"<#if pass == 2> tabindex="-1" aria-hidden="true"</#if>>${c.name}</a><span class="as-marquee-pill as-marquee-pill--${c?index % 3}" aria-hidden="true"></span>
        </#list>
      </#list>
    </div>
  </nav>
  </#if>
</#macro>

<#-- ======== 4. product rail (to the right edge) ======== -->
<#macro asRail id="as-rail" eyebrow="" title="" categoryId="" productIds="" count="10" linkCategoryId="">
  <#local max = (count?has_content)?then(count?number, 10)>
  <#local ids = asIdList(productIds)>
  <#if !ids?has_content>
    <#local src = categoryId?has_content?then([categoryId], [asPromoCatId] + asTopCategories()?map(c -> c.id))>
    <#local ids = asPickProducts(src, max)>
  </#if>
  <#-- "see all": the given link category, else the rail's category; a rail of chosen products has none -->
  <#local linkCat = linkCategoryId?has_content?then(linkCategoryId, categoryId?has_content?then(categoryId, productIds?has_content?then("", asPromoCatId)))>
  <#if ids?has_content>
  <section class="as-section as-rail-section" aria-labelledby="${id}-title">
    <@asSectionHead id=id + "-title" eyebrow=eyebrow title=title?has_content?then(title, asL("AuroraShopNewArrivals"))>
      <#if linkCat?has_content><a class="as-link" href="<@ofbizUrl>category?category_id=${linkCat}</@ofbizUrl>">${asL("AuroraShopSeeAll")}</a></#if>
      <button type="button" class="as-round" data-as-rail-prev="${id}" aria-label="${asL("AuroraShopPrevious")}">${asIconPrev}</button>
      <button type="button" class="as-round as-round--dark" data-as-rail-next="${id}" aria-label="${asL("AuroraShopNext")}">${asIconNext}</button>
    </@asSectionHead>
    <div class="as-rail" id="${id}">
      <#list ids as pid><#if (pid?index >= max)><#break></#if><@asProductCard pid=pid/></#list>
    </div>
  </section>
  </#if>
</#macro>

<#-- ======== 5. showcase: tiles = "kind:productId,..." (kinds: configure subscribe rent download gift service bundle
     variants finance made marketplace deals global) ======== -->
<#assign asKindKeys = {"configure": "Configure", "subscribe": "Subscribe", "rent": "Rent", "download": "Download", "gift": "Gift", "service": "Service",
    "bundle": "Bundle", "variants": "Variants", "finance": "Finance", "made": "MadeToOrder", "marketplace": "Marketplace", "deals": "Deals", "global": "Global"}>
<#macro asShowcase eyebrow="" title="" text="" tiles="">
  <#local defs = (tiles?has_content)?then(tiles, "configure:PC-1000,rent:RT-1000,subscribe:NEWS-01-1MO,download:MP3-1000,gift:GC-001,service:SV-1000,bundle:EL-BASKET-PICK,variants:CL-1000,made:PIZZA-01,marketplace:,deals:,global:")>
  <#local sellers = []>
  <#if asCompliance && Static["com.ilscipio.scipio.compliance.MarketplaceWorker"].isMarketplace(delegator, asStoreId)>
    <#local sellers = Static["com.ilscipio.scipio.compliance.MarketplaceWorker"].getNewSellers(delegator, asStoreId, 4)>
  </#if>
  <section class="as-section" id="as-showcase" aria-labelledby="as-showcase-title">
    <@asSectionHead id="as-showcase-title" eyebrow=eyebrow?has_content?then(eyebrow, asH("ShowcaseEyebrow")) title=title?has_content?then(title, asH("ShowcaseTitle")) text=text?has_content?then(text, asH("ShowcaseText")) dot=true/>
    <div class="as-showcase">
      <#list asIdList(defs) as def>
        <#local kind = def?keep_before(":")?trim>
        <#local pid = def?contains(":")?then(def?keep_after(":")?trim, "")>
        <#local key = asKindKeys[kind]!"">
        <#if !key?has_content><#continue></#if>
        <#local label = asH("Kind" + key)>
        <#local help = asH("Kind" + key + "Text")>
        <#if kind == "marketplace">
          <#if sellers?has_content>
          <a class="as-tile as-tile--marketplace" href="#as-sellers">
            <span class="as-tile-kind">${label}</span>
            <span class="as-tile-marks" aria-hidden="true"><#list sellers as s><span class="as-mark" style="--as-h: ${asHue(s.letter)};">${s.letter}</span></#list></span>
            <span class="as-tile-body"><b class="as-tile-name">${sellers?size} ${asH("SellersCount")}</b><span class="as-tile-text">${help}</span></span>
          </a>
          </#if>
        <#elseif kind == "deals">
          <#local dealCount = asStorePromos()?size>
          <#if (dealCount > 0)>
          <a class="as-tile as-tile--deals" href="#as-deals">
            <span class="as-tile-kind">${label}</span>
            <span class="as-tile-big" aria-hidden="true">${dealCount}</span>
            <span class="as-tile-body"><b class="as-tile-name">${dealCount} ${asH("DealsEyebrow")?lower_case}</b><span class="as-tile-text">${help}</span></span>
          </a>
          </#if>
        <#elseif kind == "global">
          <#local lang = (locale.getLanguage())!"en">
          <#-- languages only: the demo data has EUR prices for a few products only, a currency switch would show 0.00 -->
          <div class="as-tile as-tile--global">
            <span class="as-tile-kind">${asH("KindGlobalLang")}</span>
            <span class="as-tile-switch">
              <#list ["en", "de"] as l><a class="as-pill<#if l == lang> is-on</#if>" href="<@ofbizUrl>setSessionLocale?newLocale=${l}</@ofbizUrl>" lang="${l}"<#if l == lang> aria-current="true"</#if>>${(l == "de")?then("Deutsch", "English")}</a></#list>
            </span>
            <span class="as-tile-body"><span class="as-tile-text">${asH("KindGlobalLangText")}</span></span>
          </div>
        <#else>
          <#local p = asProduct(pid)>
          <#if !p?has_content><#continue></#if>
          <#local ignored = asMarkShown(pid)>
          <#local img = asProductImage(p, def?is_first?then("LARGE_IMAGE_URL", "MEDIUM_IMAGE_URL"))>
          <#local priceRes = asPrice(p)>
          <a class="as-tile as-tile--${kind}<#if def?is_first> as-tile--lead</#if>" href="<@ofbizUrl>product?product_id=${pid}</@ofbizUrl>">
            <span class="as-tile-kind">${label}</span>
            <#if img?has_content><span class="as-tile-media"><img src="<@contentUrl ctxPrefix=true>${img}</@contentUrl>" alt="" loading="lazy"/></span></#if>
            <span class="as-tile-body">
              <b class="as-tile-name">${asProductName(p)}</b>
              <#if priceRes.price?has_content><span class="as-tile-price"><#if (p.isVirtual!"N") == "Y" || (p.productTypeId!"")?starts_with("AGGREGATED")>${asH("From")} </#if><@ofbizCurrency amount=priceRes.price isoCode=priceRes.currencyUsed!"USD"/></span></#if>
              <span class="as-tile-text">${help}</span>
            </span>
            <span class="as-tile-go" aria-hidden="true">${asIconNext}</span>
          </a>
        </#if>
      </#list>
    </div>
  </section>
</#macro>

<#-- ======== 6. new sellers (marketplace) ======== -->
<#macro asSellers eyebrow="" title="" count="4">
  <#local sellers = []>
  <#if asCompliance && Static["com.ilscipio.scipio.compliance.MarketplaceWorker"].isMarketplace(delegator, asStoreId)>
    <#local sellers = Static["com.ilscipio.scipio.compliance.MarketplaceWorker"].getNewSellers(delegator, asStoreId, (count?has_content)?then(count?number, 4))>
  </#if>
  <#if sellers?has_content>
  <section class="as-section" id="as-sellers" aria-labelledby="as-sellers-title">
    <@asSectionHead id="as-sellers-title" eyebrow=eyebrow?has_content?then(eyebrow, asL("AuroraShopMarketplace")) title=title?has_content?then(title, uiLabelMap.ComplianceNewSellers!"New sellers") dot=true/>
    <div class="as-sellers">
      <#local f = sellers[0]>
      <article class="as-seller-feature" style="--as-h: ${asHue(f.letter)};">
        <div class="as-seller-feature-top">
          <span class="as-mark as-mark--lg">${f.letter}</span>
          <#if f.joinedDate??><span class="as-chip as-chip--ghost">${uiLabelMap.ComplianceJoined!"Joined"} ${f.joinedDate?date?string.medium}</span></#if>
          <#if (f.sellerType!"") == "SELLER_BUSINESS"><span class="as-chip as-chip--light">${uiLabelMap.ComplianceBusinessSeller!"business seller"}</span></#if>
        </div>
        <h3 class="as-display as-display--md">${f.displayName}</h3>
        <#if f.category?has_content><p>${f.category}</p></#if>
        <p><a class="as-btn as-btn--light" href="<@ofbizUrl>seller?sellerId=${f.partyId}</@ofbizUrl>">${asL("AuroraShopVisitShop")}</a></p>
      </article>
      <div class="as-seller-list">
        <#list sellers as s><#if s?index == 0><#continue></#if>
          <a class="as-seller-row" href="<@ofbizUrl>seller?sellerId=${s.partyId}</@ofbizUrl>">
            <span class="as-mark" style="--as-h: ${asHue(s.letter)};">${s.letter}</span>
            <span class="as-seller-row-text"><b>${s.displayName}</b><span>${s.category!}<#if s.joinedDate??> &middot; ${uiLabelMap.ComplianceJoined!"Joined"} ${s.joinedDate?date?string.medium}</#if></span></span>
          </a>
        </#list>
        <p class="as-note">${uiLabelMap.ComplianceSellerInfoNote!""} <a href="<@ofbizUrl>legal?doc=sellers</@ofbizUrl>">${uiLabelMap.ComplianceHowWeCheckSellers!""}</a></p>
      </div>
    </div>
  </section>
  </#if>
</#macro>

<#-- ======== 7. story: feature product, real customer review, film, materials ======== -->
<#macro asStory eyebrow="" title="" productId="" reviewProductId="" filmImage="" filmTitle="" filmLink="" materialsTitle="" materialsText="">
  <#local productId = productId?has_content?then(productId, "CAM-2644")>
  <#local p = asProduct(productId)>
  <#if p?has_content>
  <#local revPid = reviewProductId?has_content?then(reviewProductId, productId)>
  <#local reviews = delegator.findByAnd("ProductReview", {"productId": revPid, "statusId": "PRR_APPROVED"}, ["-postedDateTime"], true)![]>
  <#local avg = 0>
  <#if reviews?has_content><#list reviews as r><#local avg = avg + (r.productRating!0)?number></#list><#local avg = avg / reviews?size></#if>
  <#local filmPid = "SV-1000">
  <#local filmP = asProduct(filmPid)>
  <#local filmImg = filmImage?has_content?then(filmImage, (filmP?has_content)?then(asProductImage(filmP, "LARGE_IMAGE_URL"), ""))>
  <section class="as-section" aria-labelledby="as-story-title">
    <@asSectionHead id="as-story-title" eyebrow=eyebrow?has_content?then(eyebrow, asH("StoryEyebrow")) title=title?has_content?then(title, asH("StoryTitle"))/>
    <div class="as-story">
      <div class="as-story-feature"><@asProductCard pid=productId size="large"/></div>
      <#if reviews?has_content>
      <figure class="as-story-review">
        <@asStars rating=avg/>
        <blockquote>&ldquo;${reviews[0].productReview!""}&rdquo;</blockquote>
        <figcaption>${asH("StoryReview")} &middot; ${reviews?size} ${(reviews?size == 1)?then(asH("StoryReviewOne"), asH("StoryReviews"))}</figcaption>
      </figure>
      </#if>
      <#if filmImg?has_content>
      <a class="as-story-film" href="<#if filmLink?has_content><@asHref filmLink/><#else><@ofbizUrl>product?product_id=${filmPid}</@ofbizUrl></#if>">
        <img src="<@asImgSrc filmImg/>" alt="" loading="lazy"/>
        <span class="as-play" aria-hidden="true"><svg width="22" height="22" viewBox="0 0 24 24" fill="currentColor"><path d="M8 5v14l11-7z"/></svg></span>
        <span class="as-story-film-cap"><span class="as-eyebrow">${asH("StoryFilm")}</span><b>${filmTitle?has_content?then(filmTitle, asH("StoryFilmTitle"))}</b></span>
      </a>
      </#if>
      <div class="as-story-text">
        <p class="as-eyebrow">${materialsTitle?has_content?then(materialsTitle, asH("StoryMaterials"))}</p>
        <p>${materialsText?has_content?then(materialsText, asH("StoryMaterialsText"))}</p>
      </div>
    </div>
  </section>
  </#if>
</#macro>

<#-- ======== 8. shop the room: hotspots = "x,y,productId;..." in percent of the photo ======== -->
<#macro asRoom eyebrow="" title="" text="" imageUrl="" hotspots="">
  <#local imageUrl = imageUrl?has_content?then(imageUrl, "/images/products/shop/gallery/02/tech/workstation-405768/full.jpg")>
  <#local hotspots = hotspots?has_content?then(hotspots, "64,76,KB-5569;27,24,PC-1000;96,57,PR-1000;4,79,PH-1001")>
  <#local spots = []>
  <#list (hotspots!"")?split(";") as h>
    <#local parts = h?split(",")?map(x -> x?trim)>
    <#if (parts?size >= 3)>
      <#local p = asProduct(parts[2])>
      <#if p?has_content><#local spots = spots + [{"x": parts[0], "y": parts[1], "pid": parts[2], "p": p}]></#if>
    </#if>
  </#list>
  <#-- configurable and virtual products need a choice first: they link to the product and stay out of "add all" -->
  <#local addIds = spots?filter(s -> (s.p.isVirtual!"N") != "Y" && !(s.p.productTypeId!"")?starts_with("AGGREGATED"))?map(s -> s.pid)>
  <#if spots?has_content && imageUrl?has_content>
  <section class="as-section" id="as-room" aria-labelledby="as-room-title">
    <@asSectionHead id="as-room-title" eyebrow=eyebrow?has_content?then(eyebrow, asH("RoomEyebrow")) title=title?has_content?then(title, asH("RoomTitle")) text=text?has_content?then(text, asH("RoomText"))/>
    <div class="as-room" data-as-room="true">
      <div class="as-room-media">
        <img src="<@asImgSrc imageUrl/>" alt="" loading="lazy"/>
        <#list spots as s>
          <button type="button" class="as-spot" style="left: ${s.x}%; top: ${s.y}%;" data-as-spot="${s?index}" aria-pressed="${s?is_first?c}" aria-label="${asProductName(s.p)}"><span aria-hidden="true"></span></button>
        </#list>
      </div>
      <div class="as-room-panel">
        <p class="as-eyebrow">${eyebrow?has_content?then(eyebrow, asH("RoomEyebrow"))} &middot; ${spots?size} ${asH("RoomProducts")}</p>
        <ul class="as-room-list">
          <#list spots as s>
            <#local priceRes = asPrice(s.p)>
            <#local img = asProductImage(s.p, "SMALL_IMAGE_URL")>
            <li class="as-room-item<#if s?is_first> is-active</#if>" data-as-spot-item="${s?index}">
              <a href="<@ofbizUrl>product?product_id=${s.pid}</@ofbizUrl>">
                <span class="as-room-thumb"><#if img?has_content><img src="<@contentUrl ctxPrefix=true>${img}</@contentUrl>" alt="" loading="lazy"/></#if></span>
                <span class="as-room-name"><b>${asProductName(s.p)}</b><#if priceRes.price?has_content><span><@ofbizCurrency amount=priceRes.price isoCode=priceRes.currencyUsed!"USD"/></span></#if></span>
                <#if !addIds?seq_contains(s.pid)><span class="as-chip as-chip--line">${asH("KindConfigure")}</span></#if>
              </a>
            </li>
          </#list>
        </ul>
        <#if addIds?has_content><button type="button" class="as-btn as-btn--primary as-btn--block" data-as-add="${addIds?join(",")}" <@asAddUrl/>>${asH("AddAll")}</button></#if>
      </div>
    </div>
  </section>
  </#if>
</#macro>

<#-- ======== 9. build your set: pick N products, a store promotion rewards the set ======== -->
<#macro asSet eyebrow="" title="" text="" productIds="" pick="" promoId="">
  <#local productIds = productIds?has_content?then(productIds, "PH-1001,PH-1004,PH-1005,KB-5569,MPL-9290,CAM-2644,CDR-1111,PR-1000")>
  <#local pick = pick?has_content?then(pick, "5")>
  <#local promoId = promoId?has_content?then(promoId, "9013")>
  <#local ids = asIdList(productIds)?filter(id -> asProduct(id)?has_content)>
  <#local n = (pick?has_content)?then(pick?number, 3)>
  <#local promo = (delegator.findOne("ProductPromo", {"productPromoId": promoId}, true))!>
  <#if (ids?size >= n)>
  <section class="as-section" id="as-set" aria-labelledby="as-set-title">
    <div class="as-set" data-as-set="${n}">
      <div class="as-set-intro">
        <p class="as-eyebrow"><span class="as-dot" aria-hidden="true"></span>${eyebrow?has_content?then(eyebrow, asH("SetEyebrow"))}</p>
        <h2 class="as-display as-display--md" id="as-set-title">${asRich(title?has_content?then(title, asH("SetTitle")))}</h2>
        <p class="as-lead">${text?has_content?then(text, asH("SetText"))}</p>
        <#if promo?has_content><p class="as-set-promo"><span class="as-sticker">${promo.promoName!""}</span></p></#if>
        <div class="as-set-bar">
          <span class="as-set-count" aria-live="polite"><b data-as-set-count>0</b> ${asH("SetOf")} ${n}</span>
          <button type="button" class="as-btn as-btn--primary" data-as-set-add="true" disabled="disabled" <@asAddUrl/>>${asH("SetAdd")}</button>
        </div>
      </div>
      <ul class="as-set-grid">
        <#list ids as pid>
          <#local p = asProduct(pid)>
          <#local priceRes = asPrice(p)>
          <#local img = asProductImage(p, "MEDIUM_IMAGE_URL")>
          <li>
            <label class="as-set-item">
              <input type="checkbox" value="${pid}" data-as-set-item="true"/>
              <span class="as-set-img"><#if img?has_content><img src="<@contentUrl ctxPrefix=true>${img}</@contentUrl>" alt="" loading="lazy"/></#if></span>
              <span class="as-set-name">${asProductName(p)}</span>
              <#if priceRes.price?has_content><span class="as-set-price"><@ofbizCurrency amount=priceRes.price isoCode=priceRes.currencyUsed!"USD"/></span></#if>
              <span class="as-set-check" aria-hidden="true"></span>
            </label>
          </li>
        </#list>
      </ul>
    </div>
  </section>
  </#if>
</#macro>

<#-- ======== 10. deals: the store's running promotions and their codes ======== -->
<#function asStorePromos>
  <#if !asStoreId?has_content><#return []></#if>
  <#local out = []>
  <#list Static["org.ofbiz.entity.util.EntityUtil"].filterByDate(delegator.findByAnd("ProductStorePromoAppl", {"productStoreId": asStoreId}, ["sequenceNum"], true)) as a>
    <#local promo = (delegator.findOne("ProductPromo", {"productPromoId": a.productPromoId}, true))!>
    <#if promo?has_content && (promo.promoName!"")?has_content>
      <#local codes = Static["org.ofbiz.entity.util.EntityUtil"].filterByDate(delegator.findByAnd("ProductPromoCode", {"productPromoId": a.productPromoId}, null, true), asNow, "fromDate", "thruDate", true)![]>
      <#local out = out + [{"promo": promo, "codes": codes}]>
    </#if>
  </#list>
  <#return out>
</#function>
<#macro asDeals eyebrow="" title="" count="6">
  <#local deals = asStorePromos()>
  <#local max = (count?has_content)?then(count?number, 6)>
  <#if deals?has_content>
  <section class="as-section" id="as-deals" aria-labelledby="as-deals-title">
    <@asSectionHead id="as-deals-title" eyebrow=eyebrow?has_content?then(eyebrow, asH("DealsEyebrow")) title=title?has_content?then(title, asH("DealsTitle")) dot=true/>
    <ul class="as-deals">
      <#list deals as d>
        <#if (d?index >= max)><#break></#if>
        <li class="as-deal as-deal--t${d?index % 5}<#if d?is_first> as-deal--lead</#if>">
          <span class="as-deal-stub" aria-hidden="true">${(d?index + 1)?string("00")}</span>
          <div class="as-deal-main">
          <b class="as-deal-name">${d.promo.promoName}</b>
          <#-- promo texts may hold HTML (links): shown as plain text -->
          <#local dText = rawString(d.promo.promoText!"")?replace("<[^>]*>", "", "r")?replace("&amp;", "&")?trim>
          <#if dText?has_content && dText != rawString(d.promo.promoName)><p>${dText}</p></#if>
          <div class="as-deal-codes">
            <#if d.codes?has_content>
              <#list d.codes as c><#if (c?index >= 2)><#break></#if>
                <span class="as-code"><span class="as-code-label">${asH("Code")}</span><code>${c.productPromoCodeId}</code><button type="button" class="as-code-copy" data-as-copy="${c.productPromoCodeId}" data-as-copied="${asH("Copied")}">${asH("Copy")}</button></span>
              </#list>
            <#else>
              <span class="as-chip as-chip--ghost">${asH("NoCode")}</span>
            </#if>
          </div>
          </div>
        </li>
      </#list>
    </ul>
  </section>
  </#if>
</#macro>

<#-- ======== 11. next drop: countdown and "notify me" (full width band) ======== -->
<#-- limit and contactListId: an empty value means none (no limit line, no notify-me form) -->
<#macro asDrop eyebrow="" title="" text="" productId="" dropDate="" limit="3" contactListId="9000" imageUrl="">
  <#local productId = productId?has_content?then(productId, "VH-9944")>
  <#local p = asProduct(productId)>
  <#if p?has_content>
  <#local ignored = asMarkShown(productId)>
  <#local when = "">
  <#if dropDate?has_content><#attempt><#local when = Static["java.sql.Timestamp"].valueOf(dropDate?trim + (dropDate?trim?length == 16)?then(":00", ""))><#recover></#attempt></#if>
  <#if !when?has_content><#local when = Static["org.ofbiz.base.util.UtilDateTime"].getDayStart(asNow, 9)><#local when = Static["java.sql.Timestamp"].valueOf(when?string["yyyy-MM-dd"] + " 10:00:00")></#if>
  <#local live = (when?long <= asNow?long)>
  <#local img = imageUrl?has_content?then(imageUrl, asProductImage(p, "LARGE_IMAGE_URL"))>
  <section class="as-drop<#if live> is-live</#if>" aria-labelledby="as-drop-title">
    <div class="as-drop-media"><#if img?has_content><img src="<@asImgSrc img/>" alt="" loading="lazy"/></#if>
      <#if limit?has_content><span class="as-sticker as-sticker--light">${rawString(asH("DropLimited"))?replace("{0}", limit?html)}</span></#if>
    </div>
    <div class="as-drop-body">
      <p class="as-eyebrow"><span class="as-dot" aria-hidden="true"></span>${eyebrow?has_content?then(eyebrow, asH("DropEyebrow"))}</p>
      <h2 class="as-display" id="as-drop-title">${asRich(title?has_content?then(title, asProductName(p)))}</h2>
      <p class="as-drop-when">${when?string["EEE d MMM yyyy, HH:mm"]}</p>
      <#if text?has_content><p class="as-lead">${text}</p></#if>
      <#if live>
        <p><a class="as-btn as-btn--primary" href="<@ofbizUrl>product?product_id=${productId}</@ofbizUrl>">${asH("OutNow")}</a></p>
      <#else>
        <div class="as-countdown" data-as-countdown="${when?string["yyyy-MM-dd'T'HH:mm:ssXXX"]}" role="timer">
          <span><b data-as-cd="d">--</b>${asH("Days")}</span><span><b data-as-cd="h">--</b>${asH("Hours")}</span><span><b data-as-cd="m">--</b>${asH("Minutes")}</span><span><b data-as-cd="s">--</b>${asH("Seconds")}</span>
        </div>
        <#if contactListId?has_content>
        <form class="as-drop-form" method="post" action="<@ofbizUrl>signUpForContactList</@ofbizUrl>">
          <input type="hidden" name="contactListId" value="${contactListId}"/>
          <input type="hidden" name="baseLocation" value="${request.getContextPath()}"/>
          <label class="as-sr" for="as-drop-email">${uiLabelMap.CommonEmail!"E-mail"}</label>
          <input type="email" id="as-drop-email" name="email" required="required" autocomplete="email" placeholder="${uiLabelMap.CommonEmail!"E-mail"}"/>
          <button type="submit" class="as-btn as-btn--primary">${asH("NotifyMe")}</button>
        </form>
        <p class="as-note">${asH("NotifyText")}<#if asCompliance> <a href="<@ofbizUrl>legal?doc=privacy</@ofbizUrl>">${uiLabelMap.CompliancePrivacyLink!"Privacy policy"}</a></#if></p>
        </#if>
      </#if>
    </div>
  </section>
  </#if>
</#macro>

<#-- ======== 12. journal: stories are CMS pages (see the CMS home template); stories = [{title, excerpt, image, link, eyebrow, minutes}] ======== -->
<#macro asJournal eyebrow="" title="" stories=[] allLink="">
  <#if stories?has_content>
  <section class="as-section" id="as-journal" aria-labelledby="as-journal-title">
    <@asSectionHead id="as-journal-title" eyebrow=eyebrow?has_content?then(eyebrow, asH("JournalEyebrow")) title=title?has_content?then(title, asH("JournalTitle"))>
      <#if allLink?has_content><a class="as-link" href="<@asHref allLink/>">${asH("JournalAll")}</a></#if>
    </@asSectionHead>
    <div class="as-journal">
      <#list stories as s>
        <a class="as-story-card<#if s?is_first> as-story-card--lead</#if>" href="<@asHref (s.link!"#")/>">
          <span class="as-story-card-img"><#if (s.image!"")?has_content><img src="<@asImgSrc s.image/>" alt="" loading="lazy"/></#if></span>
          <span class="as-story-card-body">
            <#if (s.eyebrow!"")?has_content><span class="as-eyebrow">${s.eyebrow}<#if (s.minutes!"")?has_content> &middot; ${s.minutes} ${asH("MinRead")}</#if></span></#if>
            <b>${s.title!""}</b>
            <#if (s.excerpt!"")?has_content><span>${s.excerpt}</span></#if>
          </span>
        </a>
      </#list>
    </div>
  </section>
  </#if>
</#macro>

<#-- ======== 13. know what you buy (compliance promises, full width dark band) ======== -->
<#macro asBand eyebrow="" title="" text="">
  <#if asCompliance>
  <section class="as-dark-band">
    <div class="as-dark-band-text">
      <p class="as-eyebrow">${eyebrow?has_content?then(eyebrow, asL("AuroraShopNoSmallPrint"))}</p>
      <h2 class="as-display as-display--md">${asRich(title?has_content?then(title, asL("AuroraShopKnowWhatYouBuy")))}</h2>
      <p>${text?has_content?then(text, asL("AuroraShopKnowText"))}</p>
    </div>
    <ul class="as-promises">
      <li><b>${asL("AuroraShopWhoMadeIt")}</b><span>${asL("AuroraShopWhoMadeItText")}</span></li>
      <li><b>${asL("AuroraShopHonestPrices")}</b><span>${asL("AuroraShopHonestPricesText")}</span></li>
      <li><b>${asL("AuroraShopWithdraw")}</b><span>${asL("AuroraShopWithdrawText")}</span></li>
      <li><b>${asL("AuroraShopYourData")}</b><span>${asL("AuroraShopYourDataText")}</span></li>
    </ul>
  </section>
  </#if>
</#macro>

<#-- ======== 13b. rules by region: what the store does for the law of each market (compliance component). The regions
     come from the store's compliance profile (EU, US, US-CA; Germany for a store seated there); the selected tab is the
     rule set of this visit (ConsentWorker regime). Each rule links to the feature that runs in this store. ======== -->
<#assign asRuleSets = {
  "eu": [{"id": "Consent", "act": "consent"}, {"id": "Rights", "act": "request"}, {"id": "Withdraw", "act": "withdraw"},
         {"id": "Notice", "act": "notice"}, {"id": "Garan", "act": "product"}, {"id": "Price", "act": ""},
         {"id": "Safety", "act": "product"}, {"id": "Sellers", "act": "sellers"}, {"id": "Packaging", "act": "product"}],
  "de": [{"id": "Imprint", "act": "imprint"}, {"id": "OrderButton", "act": ""}, {"id": "Texts", "act": "terms"}, {"id": "Epr", "act": ""}],
  "us": [{"id": "Gpc", "act": ""}, {"id": "OptOut", "act": "consent"}, {"id": "Inform", "act": "sellers"}, {"id": "Pci", "act": ""}],
  "ca": [{"id": "Choices", "act": "choices"}, {"id": "Nac", "act": "nac"}, {"id": "Requests", "act": "request"}]
}>
<#assign asRuleCodes = {"eu": "EU", "de": "DE", "us": "US", "ca": "CA"}>
<#assign asRuleTints = ["sky", "butter", "mint", "lilac", "peach", "blush"]>
<#macro asRuleAction act productId="" notice=false>
  <#local loggedIn = ((userLogin.userLoginId)!"anonymous") != "anonymous">
  <#local links = {"request": loggedIn?then("privacyCenter", "privacyRequest"), "withdraw": "withdraw", "sellers": "legal?doc=sellers",
    "imprint": "legal?doc=imprint", "terms": "legal?doc=terms", "choices": "privacyChoices", "nac": "legal?doc=notice-at-collection"}>
  <#local labels = {"consent": "RulesActConsent", "notice": "RulesActNotice", "product": "RulesActProduct", "request": "RulesActRequest",
    "withdraw": "RulesActWithdraw", "sellers": "RulesActSellers", "imprint": "RulesActImprint", "terms": "RulesActTerms", "choices": "RulesActChoices", "nac": "RulesActNac"}>
  <#if act == "consent">
    <button type="button" class="as-rule-act" data-scp-consent-open="true">${asH(labels[act])} ${asIconNext}</button>
  <#elseif act == "notice">
    <#if notice><button type="button" class="as-rule-act" data-scp-notice-open="true" aria-haspopup="dialog">${asH(labels[act])} ${asIconNext}</button></#if>
  <#elseif act == "product">
    <#if productId?has_content><a class="as-rule-act" href="<@ofbizUrl>product?product_id=${productId}</@ofbizUrl>">${asH(labels[act])} ${asIconNext}</a></#if>
  <#elseif links[act]??>
    <a class="as-rule-act" href="<@ofbizUrl>${links[act]}</@ofbizUrl>">${asH(labels[act])} ${asIconNext}</a>
  </#if>
</#macro>
<#macro asRules eyebrow="" title="" text="">
  <#if asCompliance && asProfile?has_content>
  <#local ldw = Static["com.ilscipio.scipio.compliance.LegalDocumentWorker"]>
  <#local consent = Static["com.ilscipio.scipio.compliance.ConsentWorker"].getConsentContext(request, delegator, locale)>
  <#local regions = []>
  <#if ldw.hasJurisdiction(asProfile, "EU")><#local regions = regions + ["eu"]></#if>
  <#if (asProfile.countryGeoId!"") == "DEU"><#local regions = regions + ["de"]></#if>
  <#if ldw.hasJurisdiction(asProfile, "US")><#local regions = regions + ["us"]></#if>
  <#if ldw.getJurisdictions(asProfile)?seq_contains("US-CA")><#local regions = regions + ["ca"]></#if>
  <#if regions?has_content>
  <#local regime = (consent.regime!"EU")?lower_case>
  <#local active = regions?seq_contains(regime)?then(regime, regions[0])>
  <#local notice = Static["com.ilscipio.scipio.compliance.GuaranteeWorker"].isNoticeRequired(delegator, asStoreId)>
  <#-- the product the product rules link to: one with a GARAN durability guarantee (it has safety and packaging data too) -->
  <#local garan = (delegator.findByAnd("ProductAttribute", {"attrName": "DURABILITY_GUARANTEE_YEARS"}, null, true))![]>
  <#local productId = garan?has_content?then(garan[0].productId, "")>
  <section class="as-rules" aria-labelledby="as-rules-title" data-as-rules="true">
    <div class="as-rules-head">
      <div class="as-rules-intro">
        <p class="as-eyebrow"><span class="as-dot" aria-hidden="true"></span>${eyebrow?has_content?then(eyebrow, asH("RulesEyebrow"))}</p>
        <h2 class="as-display as-display--md" id="as-rules-title">${asRich(title?has_content?then(title, asH("RulesTitle")))}</h2>
        <p class="as-lead">${text?has_content?then(text, asH("RulesText"))}</p>
      </div>
      <p class="as-rules-visit as-rules-visit--${regime}">
        <span class="as-eyebrow"><span class="as-dot" aria-hidden="true"></span>${asH("RulesVisit")}</span>
        <b>${asH("RulesRegime" + regime?upper_case)}</b>
        <span>${asH(consent.gpc?then("RulesGpcOn", "RulesGpcOff"))}</span>
      </p>
    </div>
    <div class="as-rules-tabs" role="tablist" aria-label="${asH("SlotRules")}">
      <#list regions as r>
      <button type="button" role="tab" class="as-rules-tab as-rules-tab--${r}" id="as-rules-tab-${r}" aria-controls="as-rules-${r}"
        aria-selected="${(r == active)?string("true", "false")}" tabindex="${(r == active)?then("0", "-1")}">
        <span class="as-rules-code" aria-hidden="true">${asRuleCodes[r]}</span>
        <span class="as-rules-name"><b>${asH("RulesRegion" + r?cap_first)}</b><small>${rawString(asH("RulesCount"))?replace("{0}", asRuleSets[r]?size?c)}</small></span>
      </button>
      </#list>
    </div>
    <#list regions as r>
    <div class="as-rules-panel" role="tabpanel" id="as-rules-${r}" aria-labelledby="as-rules-tab-${r}"<#if r != active> hidden="hidden"</#if>>
      <ul class="as-rules-grid as-rules-grid--n${asRuleSets[r]?size}">
        <#list asRuleSets[r] as rule>
        <li class="as-rule as-rule--${asRuleTints[rule?index % asRuleTints?size]}" style="--i: ${rule?index}" data-n="${(rule?index + 1)?string("00")}">
          <p class="as-rule-meta"><span class="as-rule-law">${asH("Rule" + rule.id + "Law")}</span><#if asH("Rule" + rule.id + "Date") != "Rule" + rule.id + "Date"><span class="as-rule-date">${asH("Rule" + rule.id + "Date")}</span></#if></p>
          <b class="as-rule-title">${asH("Rule" + rule.id + "Title")}</b>
          <span class="as-rule-text">${asH("Rule" + rule.id + "Text")}</span>
          <@asRuleAction act=rule.act productId=productId notice=notice/>
        </li>
        </#list>
      </ul>
    </div>
    </#list>
    <p class="as-note as-rules-note">${asH("RulesNote")}</p>
  </section>
  </#if>
  </#if>
</#macro>

<#-- ======== 14. make it yours: a call to action. The section switch (aurora-shop-home.js) lists the sections of the page
     (the CMS slot wrappers) and lets a visitor switch them off and on in the browser, as the CMS does for every visitor ======== -->
<#macro asYours eyebrow="" title="" text="" primaryLabel="" primaryLink="" secondaryLabel="" secondaryLink="" switchboard=true>
  <#local secondaryLink = secondaryLink?has_content?then(secondaryLink, primaryLink?has_content?then("", "https://www.scipioerp.com"))>
  <section class="as-yours" aria-labelledby="as-yours-title"<#if switchboard> data-as-switch="true"</#if>>
    <div class="as-yours-text">
      <p class="as-eyebrow"><span class="as-dot" aria-hidden="true"></span>${eyebrow?has_content?then(eyebrow, asH("YoursEyebrow"))}</p>
      <h2 class="as-display" id="as-yours-title">${asRich(title?has_content?then(title, asH("YoursTitle")))}</h2>
      <p class="as-lead">${text?has_content?then(text, asH("YoursText"))}</p>
      <#if primaryLink?has_content || secondaryLink?has_content>
      <p class="as-yours-actions">
        <#if primaryLink?has_content><a class="as-btn as-btn--light" href="<@asHref primaryLink/>">${primaryLabel?has_content?then(primaryLabel, asH("YoursLink"))}</a></#if>
        <#if secondaryLink?has_content><a class="as-btn as-btn--outline" href="<@asHref secondaryLink/>">${secondaryLabel?has_content?then(secondaryLabel, asH("YoursLink"))}</a></#if>
      </p>
      </#if>
    </div>
    <#-- a page drawn as blocks; with the switch, each block follows its section -->
    <div class="as-yours-side">
      <div class="as-yours-map" aria-hidden="true"><#list 1..8 as i><i></i></#list></div>
      <#if switchboard>
      <div class="as-switch" hidden="hidden" data-as-shown="${asH("SwitchShown")}">
        <p class="as-switch-head"><b>${asH("SwitchTitle")}</b><span data-as-switch-count="true"></span></p>
        <ul class="as-switch-list"></ul>
        <p class="as-switch-foot"><button type="button" class="as-switch-reset" data-as-switch-reset="true">${asH("SwitchReset")}</button><span>${asH("SwitchNote")}</span></p>
      </div>
      </#if>
    </div>
  </section>
</#macro>
