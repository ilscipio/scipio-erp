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

<@section title=uiLabelMap.EcommerceQuoteHistory>
    <#if quoteList?has_content>
        <@table type="data-list">
          <@thead>
            <@tr>
                <@th width="10%"><span style="white-space: nowrap;">${uiLabelMap.OrderQuote} ${uiLabelMap.CommonNbr}</span></@th>
                <@th width="20%">${uiLabelMap.CommonName}</@th>
                <@th width="40%">${uiLabelMap.CommonDescription}</@th>
                <@th width="10%">${uiLabelMap.CommonStatus}</@th>
                <@th width="20%">
                    <div>${uiLabelMap.OrderOrderQuoteIssueDate}</div>
                    <div>${uiLabelMap.CommonValidFromDate}</div>
                    <div>${uiLabelMap.CommonValidThruDate}</div>
                </@th>
                <@th width="10">&nbsp;</@th>
            </@tr>
          </@thead>
          <@tbody>
            <#list quoteList as quote>
                <#assign status = quote.getRelatedOne("StatusItem", true)>
                <@tr>
                    <@td>${quote.quoteId}</@td>
                    <@td>${quote.quoteName!}</@td>
                    <@td>${quote.description!}</@td>
                    <@td>${status.get("description",locale)}</@td>
                    <@td>
                        <div><span style="white-space: nowrap;">${quote.issueDate!}</span></div>
                        <div><span style="white-space: nowrap;">${quote.validFromDate!}</span></div>
                        <div><span style="white-space: nowrap;">${quote.validThruDate!}</span></div>
                    </@td>
                    <@td align="right">
                        <a href="<@pageUrl>ViewQuote?quoteId=${quote.quoteId}</@pageUrl>" class="${styles.link_nav!} ${styles.action_view!}">${uiLabelMap.CommonView}</a>
                    </@td>
                </@tr>
            </#list>
          </@tbody>
        </@table>
    <#else>
        <@commonMsg type="result-norecord">${uiLabelMap.OrderNoQuoteFound}</@commonMsg>
    </#if>
</@section>

