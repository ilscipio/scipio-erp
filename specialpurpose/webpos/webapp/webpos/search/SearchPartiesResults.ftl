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
<div id="searchPartiesResults" style="display:none">
  <form method="post" action="javascript:void(0);" id="SearchPartiesResultsForm" name="SearchPartiesResultsForm">
    <@table type="fields" class="" cellspacing="">
      <@tr>
        <@td width="12.5%">
          <label for="searchByPartyLastName"><b>&nbsp;${uiLabelMap.PartyLastName}</b></label>
        </@td>
        <@td width="12.5%">
          <input type="text" id="searchByPartyLastName" name="searchByPartyLastName"/>
        </@td>
        <@td width="12.5%">
          <label for="searchByPartyFirstName"><b>&nbsp;${uiLabelMap.PartyFirstName}</b></label>
        </@td>
        <@td width="12.5%">
          <input type="text" id="searchByPartyFirstName" name="searchByPartyFirstName"/>
        </@td>
        <@td width="20%">
          <label for="billingLocation"><b>&nbsp;${uiLabelMap.WebPosBillingAddress}</b></label>
        </@td>
        <@td width="5%">
          <input type="checkbox" id="billingLoc" name="billingLoc"/>
        </@td>
        <@td width="20%">
          <label for="shippingLocation"><b>&nbsp;${uiLabelMap.WebPosShippingAddress}</b></label>
        </@td>
        <@td width="5%">
          <input type="checkbox" id="shippingLoc" name="shippingLoc"/>
        </@td>
      </@tr>
      <@tr>
        <@td width="12.5%">
          <label for="searchByPartyIdValue"><b>&nbsp;${uiLabelMap.PartyPartyIdentification}</b></label>
        </@td>
        <@td width="12.5%">
          <input type="text" id="searchByPartyIdValue" name="searchByPartyIdValue"/>
        </@td>
        <@td width="50%" colspan="4" style="text-align:center">
          <input type="submit" value="${uiLabelMap.CommonSearch}" id="searchPartiesResultsSearch"/>
          &nbsp;
          <input type="submit" value="${uiLabelMap.CommonCancel}" id="searchPartiesResultsCancel"/>
        </@td>
        <@td width="25%" colspan="2"></@td>
      </@tr>
    </@table>
    <@table type="data-list" cellspacing="0" cellpadding="2" class="${styles.table_default!}">
      <@thead class="searchPartiesResultsHead">
        <@tr class="header-row">
          <@th>&nbsp;</@th>
          <@th>${uiLabelMap.PartyPartyId}</@th>
          <@th>${uiLabelMap.PartyLastName}</@th>
          <@th>${uiLabelMap.PartyFirstName}</@th>
          <@th>${uiLabelMap.PartyAddressLine1}</@th>
          <@th>${uiLabelMap.PartyCity}</@th>
          <@th>${uiLabelMap.PartyPostalCode}</@th>
          <@th>${uiLabelMap.PartyState}</@th>
          <@th>${uiLabelMap.CommonCountry}</@th>
          <@th>B/S</@th>
        </@tr>
      </@thead>
      <@tbody id="searchPartiesResultsList" class="searchPartiesResultsCartBody">
        <@tr>
          <@td colspan="9">
          </@td>
        </@tr>
      </@tbody>
    </@table>
  </form>
</div>