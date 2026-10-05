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
SCIPIO: 4.0.0: Aurora Shop CMS asset "Next drop (full width, countdown)" (asset template AS_DROP). The fields are the attributes of the asset template;
the section itself is the asDrop macro of includes/sections.ftl. Merchant fields are plain text (expandLang NONE).
-->
<#include "component://aurora-shop-theme/includes/sections.ftl">
<#if asTrue(show!true)>
<#-- CMS values arrive HTML-encoded (":" as "&#x3a;"): IDs, links, paths and lists are read raw; texts stay encoded -->
<#assign productId = rawString(productId!"")>
<#assign dropDate = rawString(dropDate!"")>
<#assign limit = rawString(limit!"")>
<#assign contactListId = rawString(contactListId!"")>
<#assign imageUrl = rawString(imageUrl!"")>
<@asDrop eyebrow=eyebrow!"" title=title!"" text=text!"" productId=productId!"" dropDate=dropDate!"" limit=limit!"" contactListId=contactListId!"" imageUrl=imageUrl!""/>
</#if>
