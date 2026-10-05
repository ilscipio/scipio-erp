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
<#escape x as x?xml>
<fo:block-container height="45mm" margin-bottom="5mm" display-align="center">
    <fo:block>
        <#if logoImageUrl?has_content><fo:external-graphic src="<@contentUrl>${logoImageUrl}</@contentUrl>" overflow="hidden" height="45mm" content-height="scale-to-fit" content-width="scale-to-fit" width="100%" scaling="uniform"/></#if>
    </fo:block>
</fo:block-container>
</#escape>
