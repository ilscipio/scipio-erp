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
SCIPIO: 4.0.0: Aurora Shop CMS asset "Story tiles: product, review, film, materials" (asset template AS_SEASON). The fields are the attributes of the asset template;
the section itself is the asStory macro of includes/sections.ftl. Merchant fields are plain text (expandLang NONE).
-->
<#include "component://aurora-shop-theme/includes/sections.ftl">
<#if asTrue(show!true)>
<#-- CMS values arrive HTML-encoded (":" as "&#x3a;"): IDs, links, paths and lists are read raw; texts stay encoded -->
<#assign productId = rawString(productId!"")>
<#assign filmImage = rawString(filmImage!"")>
<#assign filmLink = rawString(filmLink!"")>
<@asStory eyebrow=eyebrow!"" title=title!"" productId=productId!"" filmImage=filmImage!"" filmTitle=filmTitle!"" filmLink=filmLink!"" materialsTitle=materialsTitle!"" materialsText=materialsText!""/>
</#if>
