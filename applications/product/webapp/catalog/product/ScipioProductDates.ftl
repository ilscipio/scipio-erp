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
<@section>
    <@table type="fields">
        <#-- availability -->
        <#if product.introductionDate?has_content>
            <@tr>
              <@td class="${styles.grid_large!}2">${uiLabelMap.CommonIntroductionDate}
              </@td>
              <@td colspan="3"><@formattedDateTime date=product.introductionDate defaultVal="0000-00-00 00:00:00"/></@td>
            </@tr>    
        </#if>

        <#if product.releaseDate?has_content>
            <@tr>
              <@td class="${styles.grid_large!}2">${uiLabelMap.CommonReleaseDate}
              </@td>
              <@td colspan="3"><@formattedDateTime date=product.releaseDate defaultVal="0000-00-00 00:00:00"/></@td>
            </@tr>    
        </#if>

        <#if product.salesDiscontinuationDate?has_content>
            <@tr>
              <@td class="${styles.grid_large!}2">${uiLabelMap.ProductSalesThruDate}
              </@td>
              <@td colspan="3"><@formattedDateTime date=product.salesDiscontinuationDate defaultVal="0000-00-00 00:00:00"/></@td>
            </@tr>    
        </#if>

        <#if product.supportDiscontinuationDate?has_content>
            <@tr>
              <@td class="${styles.grid_large!}2">${uiLabelMap.ProductSupportThruDate}
              </@td>
              <@td colspan="3"><@formattedDateTime date=product.supportDiscontinuationDate defaultVal="0000-00-00 00:00:00"/></@td>
            </@tr>    
        </#if>
    </@table>


</@section>