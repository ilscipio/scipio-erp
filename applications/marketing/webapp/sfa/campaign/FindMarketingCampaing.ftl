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
    <form name="FindMarketingCampaign" action="<@pageUrl>FindMarketingCampaign</@pageUrl>" method="POST">
       <@field type="input" name="marketingCampaignId" label=uiLabelMap.MarketingCampaignId />
       <@field type="input" name="campaignName" label=uiLabelMap.MarketingCampaignName />
       <@field type="input" name="parentCampaignId" label=uiLabelMap.MarketingParentCampaignId />
       <@field type="select" name="statusId" label=uiLabelMap.CommonStatus>
            <#assign statusList=delegator.findByAnd("StatusItem", {"statusTypeId" , "MKTG_CAMP_STATUS"}, null, true)/>
            <option value=""></option>
            <#list statusList as status>
                <option value="${status.statusId}">${status.description}</option>
            </#list>
       </@field>
       <@field type="submit" name="find" text=uiLabelMap.CommonFind class="+${styles.link_run_sys!} ${styles.action_find!}" />
    </form>
</@section>