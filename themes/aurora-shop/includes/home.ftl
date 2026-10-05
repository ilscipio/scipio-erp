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
SCIPIO: 4.0.0: Aurora Shop home page - theme fallback. A CMS page mapped to the shop's "main" view (CMS home
template, cms/homeTemplate.ftl) replaces it and lets the merchant edit, reorder and switch off every section.
All sections come from sections.ftl and show live store data (products, prices, promotions, sellers, reviews).
-->
<#include "component://aurora-shop-theme/includes/sections.ftl">
<link rel="stylesheet" href="<@ofbizContentUrl>/aurora-shop/css/aurora-shop-home.css</@ofbizContentUrl>" type="text/css"/>
<#-- products that sections below name on purpose: the automatic rail does not repeat them -->
<#list ["CAM-2644", "SV-1000", "KB-5569", "PC-1000", "PR-1000", "PH-1001", "VH-9944", "RT-1000", "NEWS-01-1MO", "MP3-1000", "GC-001", "EL-BASKET-PICK", "CL-1000", "PIZZA-01"] as reservedId>
  <#assign ignored = asMarkShown(reservedId)>
</#list>
<div class="as-home">
  <@asHero/>
  <@asTrust/>
  <@asMarquee/>
  <@asRail id="as-rail"/>
  <@asShowcase/>
  <@asRoom/>
  <@asSellers/>
  <@asSet/>
  <@asDrop/>
  <@asStory/>
  <@asDeals/>
  <@asBand/>
  <@asRules/>
  <@asYours switchboard=false/>
</div>
<script src="<@ofbizContentUrl>/aurora-shop/js/aurora-shop-home.js</@ofbizContentUrl>" defer="defer"></script>
