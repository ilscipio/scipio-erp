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
SCIPIO: 4.0.0: Aurora Shop CMS page template "Aurora Shop story" (AS_STORY): a journal story. The body is merchant HTML,
cleaned by the OWASP sanitizer (formatting, blocks, links, images, tables); no FreeMarker runs in merchant fields.
The journal section of the landing template lists the pages of this template.
-->
<@asset def="global" mode="import" name="CommonTemplateParts" ns="commonTmpl"/><#t/>
<@commonTmpl.headerHeadOpen />
<@commonTmpl.headerIncludes />
<#include "component://aurora-shop-theme/includes/sections.ftl">
<link rel="stylesheet" href="<@ofbizContentUrl>/aurora-shop/css/aurora-shop-home.css</@ofbizContentUrl>" type="text/css"/>
<div id="content-main-section">
  <div id="main-content">
    <@render resource=messagesTemplateLocation!/>
    <div class="as-home as-story-page">
      <article class="as-article">
        <header class="as-article-head">
          <p class="as-eyebrow"><a href="${request.getContextPath()}/journal">${asH("JournalTitle")}</a><#if (eyebrow!"")?has_content> &middot; ${eyebrow}</#if></p>
          <h1 class="as-display">${title!""}</h1>
          <#if (excerpt!"")?has_content><p class="as-lead">${excerpt}</p></#if>
          <p class="as-article-meta"><#if (author!"")?has_content>${author}</#if><#if (publishedDate!"")?has_content> &middot; ${publishedDate}</#if><#if (minutes!"")?has_content> &middot; ${minutes} ${asH("MinRead")}</#if></p>
        </header>
        <#if (coverImage!"")?has_content><figure class="as-article-cover"><img src="<@asImgSrc coverImage/>" alt=""/></figure></#if>
        <div class="as-article-body">${rawString(asSafeHtml(body!""))}</div>
      </article>
      <#if (productIds!"")?has_content><@asRail id="as-story-products" title=asH("ShopTheStory") productIds=productIds/></#if>
      <@asJournal title=asH("MoreStories") stories=asJournalStories(3, cmsPageId!"") allLink=request.getContextPath() + "/journal"/>
    </div>
  </div>
</div>
<script src="<@ofbizContentUrl>/aurora-shop/js/aurora-shop-home.js</@ofbizContentUrl>" defer="defer"></script>
<@commonTmpl.footerIncludes />
