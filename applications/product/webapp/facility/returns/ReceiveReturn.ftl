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
   
<@section title=uiLabelMap.ProductReceiveReturn>
    <form name="receiveInventoryReturn" method="post" action="<@pageUrl>ReceiveInventory</@pageUrl>">
        <input type="hidden" name="facilityId" value="${requestParameters.facilityId!}" />
        <input type="hidden" name="initialSelected" value="Y" />
        <@field type="input" label=uiLabelMap.ProductReturnNumber name="returnId" size="20" maxlength="20" value=(requestParameters.returnId!) />
        <@field type="submit" submitType="link" href="javascript:document.receiveInventoryReturn.submit();" class="+${styles.link_run_sys!} ${styles.action_receive!}" text=uiLabelMap.ProductReceiveProduct />
    </form>
</@section>