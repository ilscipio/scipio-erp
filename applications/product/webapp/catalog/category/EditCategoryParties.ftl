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

<#if productCategoryId?? && productCategory??>
    <@section title="${uiLabelMap.PageTitleEditCategoryParties}">
        <@table type="data-list" autoAltRows=true cellspacing="0" class="basic-table">
            <@thead>
            <@tr class="header-row">
                <@th>${uiLabelMap.PartyPartyId}</@th>
                <@th>${uiLabelMap.PartyRole}</@th>
                <@th>${uiLabelMap.CommonFromDateTime}</@th>
                <@th align="center">${uiLabelMap.CommonThruDateTime}</@th>
                <@th>&nbsp;</@th>
            </@tr>
            </@thead>
            <#assign line = 0>
            <#list productCategoryRoles as productCategoryRole>
            <#assign line = line + 1>
            <#assign curRoleType = productCategoryRole.getRelatedOne("RoleType", true)>
            <@tr valign="middle">
            <@td><a href="/partymgr/control/viewprofile?party_id=${(productCategoryRole.partyId)!}" target="_blank" class="${styles.button_default!}">${(productCategoryRole.partyId)!}</a></@td>
            <@td>${(curRoleType.get("description",locale))!}</@td>
            <#assign hasntStarted = false>
            <#if (productCategoryRole.getTimestamp("fromDate"))?? && Static["org.ofbiz.base.util.UtilDateTime"].nowTimestamp().before(productCategoryRole.getTimestamp("fromDate"))> <#assign hasntStarted = true></#if>
            <#assign colorStyle><#if hasntStarted> style="color: red;"</#if></#assign>
            <@td style=colorStyle>${(productCategoryRole.fromDate)!}</@td>
            <@td align="center">
                <form method="post" action="<@ofbizUrl>updatePartyToCategory</@ofbizUrl>" name="lineForm_update${line}">
                    <#assign hasExpired = false>
                    <#if (productCategoryRole.getTimestamp("thruDate"))?? && (Static["org.ofbiz.base.util.UtilDateTime"].nowTimestamp().after(productCategoryRole.getTimestamp("thruDate")))> <#assign hasExpired = true></#if>
                    <input type="hidden" name="productCategoryId" value="${(productCategoryRole.productCategoryId)!}" />
                    <input type="hidden" name="partyId" value="${(productCategoryRole.partyId)!}" />
                    <input type="hidden" name="roleTypeId" value="${(productCategoryRole.roleTypeId)!}" />
                    <input type="hidden" name="fromDate" value="${(productCategoryRole.getTimestamp("fromDate"))!}" />
                    <#if hasExpired><#assign class="alert"></#if>
                    <@htmlTemplate.renderDateTimeField name="thruDate" event="" action="" className="${class!''}"  title="Format: yyyy-MM-dd HH:mm:ss.SSS" value="${(productCategoryRole. getTimestamp('thruDate'))!}" size="25" maxlength="30" id="thruDate_1" dateType="date" shortDateInput=false timeDropdownParamName="" defaultDateTimeString="" localizedIconTitle="" timeDropdown="" timeHourName="" classString="" hour1="" hour2="" timeMinutesName="" minutes="" isTwelveHour="" ampmName="" amSelected="" pmSelected="" compositeType="" formName=""/>
                    <input type="submit" value="${uiLabelMap.CommonUpdate}" style="font-size: x-small;" />
                </form>
            </@td>
            <@td align="center">
                <form method="post" action="<@ofbizUrl>removePartyFromCategory</@ofbizUrl>" name="lineForm_delete${line}">
                    <#assign hasExpired = false>
                    <input type="hidden" name="productCategoryId" value="${(productCategoryRole.productCategoryId)!}" />
                    <input type="hidden" name="partyId" value="${(productCategoryRole.partyId)!}" />
                    <input type="hidden" name="roleTypeId" value="${(productCategoryRole.roleTypeId)!}" />
                    <input type="hidden" name="fromDate" value="${(productCategoryRole.getTimestamp("fromDate"))!}" />
                    <a href="javascript:document.lineForm_delete${line}.submit()" class="${styles.button_default!}">${uiLabelMap.CommonDelete}</a>
                </form>
            </@td>
            </@tr>
            </#list>
        </@table>
    </@section>
    <@section title="${uiLabelMap.ProductAssociatePartyToCategory}">
        <form method="post" action="<@ofbizUrl>addPartyToCategory</@ofbizUrl>" name="addNewForm">
          <@fields labelArea=false>
            <input type="hidden" name="productCategoryId" value="${productCategoryId}" />
            <@field type="generic">
                <input type="text" size="20" maxlength="20" name="partyId" value="" />
            </@field>
            <@field type="generic">
                <select name="roleTypeId" size="1">
                <#list roleTypes as roleType>
                    <option value="${(roleType.roleTypeId)!}" <#if roleType.roleTypeId.equals("_NA_")> selected="selected"</#if>>${(roleType.get("description",locale))!}</option>
                </#list>
                </select>
            </@field>
            <@field type="generic">
                <@htmlTemplate.renderDateTimeField name="fromDate" event="" action="" className=""  title="Format: yyyy-MM-dd HH:mm:ss.SSS" value="" size="25" maxlength="30" id="fromDate_1" dateType="date" shortDateInput=false timeDropdownParamName="" defaultDateTimeString="" localizedIconTitle="" timeDropdown="" timeHourName="" classString="" hour1="" hour2="" timeMinutesName="" minutes="" isTwelveHour="" ampmName="" amSelected="" pmSelected="" compositeType="" formName=""/>
            </@field>
            <@field type="submitarea">
                <input type="submit" value="${uiLabelMap.CommonAdd}" />
            </@field>
          </@fields>
        </form>
    </@section>
</#if>
