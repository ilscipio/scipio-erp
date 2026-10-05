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
<#macro menuContent menuArgs={}>
  <@menu args=menuArgs>
        <@menuitem type="link" href=makePageUrl("TransferInventoryItem?facilityId=${facilityId}") text=uiLabelMap.ProductNewInventoryTransfer class="+${styles.action_run_sys!} ${styles.action_update!}" />
  </@menu>
</#macro>

<@section menuContent=menuContent>
    <@paginate mode="content" url=makePageUrl("FindFacilityTransfers") paramStr=paramStr viewSize=viewSize!1 viewIndex=viewIndex!0 listSize=arraySize!0>
        <form action="<@pageUrl>FindFacilityTransfers</@pageUrl>" method="post" name="findFacilityTransfers" class="basic-form">
            <input type="hidden" name="facilityId" value="${facilityId}" />
            <@row>
                <@cell columns=12>
                    <@field type="input" label=uiLabelMap.ProductInventoryItemId name="inventoryItemId" value="" size="20" maxlength="50" />
                </@cell>
            </@row>
           <@field type="select" label=uiLabelMap.ProductTransferStatus name="statusId">                
                <#if statusItems?has_content>
                    <option value="">--</option>
                    <#list statusItems as statusItem>
                        <option value="${(statusItem.statusId)!}">${(statusItem.get("description",locale))!}</option>
                    </#list>
                </#if>
            </@field>
            <@field type="submit" name="look_up" text=uiLabelMap.CommonFind class="+${styles.link_run_sys!} ${styles.action_find!}" />
        </form>
    </@paginate>
</@section>