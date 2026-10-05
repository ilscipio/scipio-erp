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
<#include "component://setup/webapp/setup/common/common.ftl">
  

<hr/>

<#assign htmlSectionTitle>${uiLabelMap.CommonOverview}<#rt/>
  <#lt/> (<@setupExtAppLink uri="/partymgr/control/viewprofile?partyId=${raw(userPartyId!)}" text=uiLabelMap.CommonManage/>)</#assign>
<#assign sectionTitle = wrapAsRaw({"htmlmarkup":htmlSectionTitle, "raw":rawLabel('PartyContactInformation')})>
      
<@section title=sectionTitle relHeadingLevel=+1>

  <#-- not sure this adds anything
  <@render name="Party" resource="component://party/widget/partymgr/ProfileScreens.xml"/>-->

  <#macro menuContent menuArgs={}>
    <@menu args=menuArgs>
        <@menuitem type="link" href=makePageUrl("editcontactmech?partyId=${raw(userPartyId)}") text=uiLabelMap.CommonNew class="+${styles.action_nav!} ${styles.action_add!}"/>
    </@menu>
  </#macro>
  <@section title=uiLabelMap.PartyContactInformation menuContent=menuContent>
    <@render name="Contact" resource="component://party/widget/partymgr/ProfileScreens.xml"/>
  </@section>

</@section>