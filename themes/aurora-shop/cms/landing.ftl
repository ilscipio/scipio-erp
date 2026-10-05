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
SCIPIO: 4.0.0: Aurora Shop CMS page template "Aurora Shop landing" (AS_LANDING): the shop home and other landing pages.
The shell is the one of advancedTemplateShop.ftl (theme header and footer, ShopActions through the script AS_LANDING uses).
Each slot is a section asset; the merchant sets "show" and "position" per section, this template sorts the shown
slots by position. Products that sections name on purpose are reserved, so the automatic rails do not repeat them.
-->
<@asset def="global" mode="import" name="CommonTemplateParts" ns="commonTmpl"/><#t/>
<@commonTmpl.headerHeadOpen />
<@commonTmpl.headerIncludes />
<#include "component://aurora-shop-theme/includes/sections.ftl">
<link rel="stylesheet" href="<@ofbizContentUrl>/aurora-shop/css/aurora-shop-home.css</@ofbizContentUrl>" type="text/css"/>
<#assign asSlotNames = ["hero", "trust", "marquee", "rail1", "showcase", "room", "sellers", "set", "drop", "season", "deals", "rail2", "journal", "band", "rules", "yours"]>
<#assign asOrder = []>
<#list asSlotNames as slot>
  <#assign c = (cmsPageContent.getAssetContent(slot))!{}>
  <#if !asTrue(c.show!"true")><#continue></#if>
  <#assign asOrder = asOrder + [{"slot": slot, "pos": asNum(c.position!"", (slot?index + 1) * 10)}]>
  <#-- values from the content map are HTML-encoded when read: raw for IDs and lists -->
  <#list ["productId", "s1ProductId", "s2ProductId", "s3ProductId"] as k><#if (c[k]!"")?has_content><#assign ignored = asMarkShown(rawString(c[k]))></#if></#list>
  <#list asIdList(rawString(c.tiles!"")) as t><#if t?contains(":") && t?keep_after(":")?has_content><#assign ignored = asMarkShown(t?keep_after(":")?trim)></#if></#list>
  <#list rawString(c.hotspots!"")?split(";") as h><#assign hp = h?split(",")><#if (hp?size >= 3)><#assign ignored = asMarkShown(hp[2]?trim)></#if></#list>
</#list>
<div id="content-main-section">
  <div id="main-content">
    <@render resource=messagesTemplateLocation!/>
    <#-- a landing page with a title (not the home page) shows it as its heading; the theme hides its own title here -->
    <#if (title!"")?has_content><h1 class="as-page-title">${title}</h1></#if>
    <div class="as-home">
      <#-- the wrapper has no box (display: contents); it names the section for the section switch (asYours) -->
      <#list asOrder?sort_by("pos") as o>
        <div class="as-slot" data-as-slot="${o.slot}" data-as-slot-label="${asH("Slot" + o.slot?cap_first)}"><@asset name=o.slot ctxVars={"asSlot": o.slot}/></div>
      </#list>
    </div>
  </div>
</div>
<script src="<@ofbizContentUrl>/aurora-shop/js/aurora-shop-home.js</@ofbizContentUrl>" defer="defer"></script>
<@commonTmpl.footerIncludes />
