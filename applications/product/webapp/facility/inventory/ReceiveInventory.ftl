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
<@section menuContent=menuContent>
    <#if invalidProductId??>
        <@commonMsg type="error">${invalidProductId}</@commonMsg>
    </#if>    
    <@section title=uiLabelMap.ProductReceiveItem>
        <form name="receiveInventoryItem" method="post" action="<@pageUrl>ReceiveInventory</@pageUrl>">
            <input type="hidden" name="facilityId" value="${requestParameters.facilityId!}"/>
            <#-- <input type="hidden" name="initialSelected" value="Y"/> -->
            <@field type="lookup" label=uiLabelMap.ProductPurchaseOrderNumber tooltip=uiLabelMap.ProductLeaveSingleProductReceiving value=(requestParameters.purchaseOrderId!) formName="receiveInventoryItem" name="purchaseOrderId" id="purchaseOrderId" fieldFormName="LookupPurchaseOrderHeaderAndShipInfo"/>
            <@field type="lookup" label=uiLabelMap.ProductProductId tooltip=uiLabelMap.ProductLeaveEntirePoReceiving value=(requestParameters.productId!) formName="receiveInventoryItem" name="productId" id="productId" fieldFormName="LookupProduct"/>
            <@field type="submit" submitType="link" href="javascript:document.receiveInventoryItem.submit();" class="+${styles.link_run_sys!} ${styles.action_receive!}" text=uiLabelMap.ProductReceiveProduct />
        </form>
    </@section>
</@section>