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
<form method="post" action="createInvoiceItemPayrol">
<input type="hidden" name="invoiceId" value="${invoice.invoiceId}" />
<@table type="data-complex" class="basic-table hover-bar" cellspacing="0">

<#if PayrolGroup?has_content>
<#list PayrolGroup as payrolGroup>
<@thead>
<@tr class="header-row">
    <@th>
    [${payrolGroup.description}]
    </@th>
    <@th>&nbsp;</@th>
    <@th>&nbsp;</@th>
    <@th>&nbsp;</@th>
    <@th>&nbsp;</@th>
</@tr>
<@tr class="header-row">
    <@th width="50%" align="center">Description</@th>
    <@th width="10px" align="center">Quantity</@th>
    <@th width="10px" align="center">Amount</@th>
    <@th>&nbsp;</@th>
    <@th>&nbsp;</@th>
</@tr>
</@thead>
    <#if PayrolList?has_content>
        <#list PayrolList as payrolList>
            <#if payrolList.parentTypeId! == payrolGroup.invoiceItemTypeId!>
<@tr>
    <@td class="align-right">
            ${payrolList.description} :
    </@td>
    <@td><input type="text" size=10 name="${payrolList.invoiceItemTypeId}_Quantity"/></@td>
    <@td><input type="text" size=10 name="${payrolList.invoiceItemTypeId}_Amount"/></@td>
    <@td>&nbsp;</@td>
    <@td>&nbsp;</@td>
</@tr>
            </#if>
        </#list>
    </#if>
</#list>
</#if>
<@tfoot>
<@tr class="header-row">
<@td>&nbsp;</@td>
<@td>&nbsp;</@td>
<@td>&nbsp;</@td>
<@td>&nbsp;</@td>
<@td>&nbsp;</@td>
</@tr>
<@tr>
    <@td class="align-right"><b>Add all values : </b></@td>
    <@td align="center"><input type="submit" value="Add" /></@td>
    <@td>&nbsp;</@td>
    <@td>&nbsp;</@td>
    <@td>&nbsp;</@td>
</@tr>
</@tfoot>
</@table>
</form>
