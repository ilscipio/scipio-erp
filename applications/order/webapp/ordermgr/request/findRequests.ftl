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
<form method="post" name="lookuporder" id="lookuporder" action="<@pageUrl>FindRequest</@pageUrl>" >
<input type="hidden" name="viewSize" value="${viewSize}"/>
<input type="hidden" name="viewIndex" value="${viewIndex}"/>

<@section title=uiLabelMap.OrderFindOrder>
  <@row>
    <@cell columns=9>
      <@field type="input" label=uiLabelMap.OrderOrderId name="orderId"/>

      <@field type="generic" label=uiLabelMap.CommonDateFilter>
          <@field type="datetime" dateType="datetime" label=uiLabelMap.CommonFrom name="minDate" value=(requestParameters.minDate!) size="25" maxlength="30" id="minDate1" collapse=true/>
          <@field type="datetime" dateType="datetime" label=uiLabelMap.CommonThru name="maxDate" value=(requestParameters.maxDate!) size="25" maxlength="30" id="maxDate" collapse=true/>
      </@field>
      
        <@fieldset title=uiLabelMap.CommonAdvancedSearch collapsed=true>
          
        </@fieldset>
        <input type="hidden" name="showAll" value="Y"/>
        <@field type="submit" text=uiLabelMap.CommonFind class="+${styles.link_run_sys!} ${styles.action_find!}"/>
    </@cell>
  </@row>    
</@section>
<input type="image" src="<@contentUrl>/images/spacer.gif</@contentUrl>" onclick="javascript:lookupOrders(true);"/>
</form>
