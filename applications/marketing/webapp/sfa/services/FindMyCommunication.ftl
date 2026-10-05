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
<@section>
    <form name="MyCommunicationEvents" action="<@pageUrl>MyCommunicationEvents</@pageUrl>" method="POST">
    
        <@field type="lookup" name="partyIdTo" formName="MyCommunicationEvents" id="partyIdTo" fieldFormName="LookupPartyName" label=uiLabelMap.PartyPartyTo />
    
        <@field type="select" name="comEventStatus" label=uiLabelMap.MarketingCommunicationStatusId>
            <option value=""></value>
            <#list comEventStatusList as comEventStatus>
                <option value="${comEventStatus.statusId}">${comEventStatus.description}</option>
            </#list>
       </@field>

        <@field type="select" name="comEventRoleStatus" label=uiLabelMap.MarketingCommunicationRoleStatusId>
            <option value=""></value>   
            <#list comEventRoleStatusList as comEventRoleStatus>
                <option value="${comEventRoleStatus.statusId}">${comEventRoleStatus.description}</option>
            </#list>
       </@field>

       <@field type="select" name="communicationEventTypeId" label=uiLabelMap.MarketingContactListCommEventTypeId>
            <option value=""></value>           
            <#list communicationEventTypeList as communicationEventType>
                <option value="${communicationEventType.communicationEventTypeId}">${communicationEventType.description}</option>
            </#list>
       </@field>

       <@field type="submit" name="find" text=uiLabelMap.CommonFind class="+${styles.link_run_sys!} ${styles.action_find!}" />
    </form>
</@section>