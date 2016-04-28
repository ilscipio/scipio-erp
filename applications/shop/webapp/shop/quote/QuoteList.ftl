<#--
Licensed to the Apache Software Foundation (ASF) under one
or more contributor license agreements.  See the NOTICE file
distributed with this work for additional information
regarding copyright ownership.  The ASF licenses this file
to you under the Apache License, Version 2.0 (the
"License"); you may not use this file except in compliance
with the License.  You may obtain a copy of the License at

http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing,
software distributed under the License is distributed on an
"AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
KIND, either express or implied.  See the License for the
specific language governing permissions and limitations
under the License.
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
                        <a href="<@ofbizUrl>ViewQuote?quoteId=${quote.quoteId}</@ofbizUrl>" class="${styles.link_nav!} ${styles.action_view!}">${uiLabelMap.CommonView}</a>
                    </@td>
                </@tr>
            </#list>
          </@tbody>
        </@table>
    <#else>
        <@commonMsg type="result-norecord">${uiLabelMap.OrderNoQuoteFound}</@commonMsg>
    </#if>
</@section>

