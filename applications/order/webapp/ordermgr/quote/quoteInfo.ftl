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
<@section title="${rawLabel('OrderOrderQuoteId')} ${rawString(quote.quoteId)} ${rawLabel('CommonInformation')}">
        <@table type="fields"> <#-- orig: class="basic-table" --> <#-- orig: cellspacing="0" -->
            <#-- quote header information -->
            <@tr>
                <@td align="right" valign="top" width="20%">
                    &nbsp;${uiLabelMap.CommonType}
                </@td>
                <@td valign="top" width="80%">
                    ${(quoteType.get("description",locale))?default(quote.quoteTypeId!)}
                </@td>
            </@tr>
            <@tr type="util"><@td colspan="3"><hr /></@td></@tr>

            <#-- quote Channel information -->
            <@tr>
                <@td align="right" valign="top" width="20%">
                    &nbsp;${uiLabelMap.OrderSalesChannel}
                </@td>
                <@td valign="top" width="80%">
                    ${(salesChannel.get("description",locale))?default(quote.salesChannelEnumId!)}
                </@td>
            </@tr>
            <@tr type="util"><@td colspan="3"><hr /></@td></@tr>

            <#-- quote status information -->
            <@tr>
                <@td align="right" valign="top" width="20%">
                    &nbsp;${uiLabelMap.CommonStatus}
                </@td>
                <@td valign="top" width="80%">
                    ${(statusItem.get("description", locale))?default(quote.statusId!)}
                </@td>
            </@tr>
            <#-- party -->
            <@tr type="util"><@td colspan="3"><hr /></@td></@tr>
            <@tr>
                <@td align="right" valign="top" width="20%">
                    &nbsp;${uiLabelMap.PartyPartyId}
                </@td>
                <@td valign="top" width="80%">
                    ${quote.partyId!}
                </@td>
            </@tr>
            <#-- quote name -->
            <@tr type="util"><@td colspan="3"><hr /></@td></@tr>
            <@tr>
                <@td align="right" valign="top" width="20%">
                    &nbsp;${uiLabelMap.OrderOrderQuoteName}
                </@td>
                <@td valign="top" width="80%">
                    ${quote.quoteName!}
                </@td>
            </@tr>
            <#-- quote description -->
            <@tr type="util"><@td colspan="3"><hr /></@td></@tr>
            <@tr>
                <@td align="right" valign="top" width="20%">
                    &nbsp;${uiLabelMap.CommonDescription}
                </@td>
                <@td valign="top" width="80%">
                    ${quote.description!}
                </@td>
            </@tr>
            <#-- quote currency -->
            <@tr type="util"><@td colspan="3"><hr /></@td></@tr>
            <@tr>
                <@td align="right" valign="top" width="20%">
                    &nbsp;${uiLabelMap.CommonCurrency}
                </@td>
                <@td valign="top" width="80%">
                    <#if currency??>${currency.get("description",locale)!(quote.currencyUomId!)}</#if>
                </@td>
            </@tr>
            <#-- quote currency -->
            <@tr type="util"><@td colspan="3"><hr /></@td></@tr>
            <@tr>
                <@td align="right" valign="top" width="20%">
                    &nbsp;${uiLabelMap.ProductProductStore}
                </@td>
                <@td valign="top" width="80%">
                    <#if store??>${store.storeName!(quote.productStoreId!)}</#if>
                </@td>
            </@tr>
        </@table>
</@section>
