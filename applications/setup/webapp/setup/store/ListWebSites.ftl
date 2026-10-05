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

    <@heading>${uiLabelMap.ContentWebSites}</@heading>

    <@table type="data-list">
      <@thead>
        <@tr>
          <@th>${uiLabelMap.CommonId}</@th>
          <@th>${uiLabelMap.CommonName}</@th>
          <@th>${uiLabelMap.FormFieldTitle_isStoreDefault}</@th>
          <@th></@th>
        </@tr>
      </@thead>
      <@tbody>
        <#list webSiteList as currWebSite>
          <@tr>
            <@td><@setupExtAppLink uri="/catalog/control/EditWebSite?webSiteId=${raw(currWebSite.webSiteId)}" text=currWebSite.webSiteId/></@td>
            <@td>${currWebSite.siteName!}</@td>
            <@td>${currWebSite.isStoreDefault!}</@td>
            <@td>
              <form method="get" action="<@pageUrl uri=makeSetupStepUri("store") escapeAs="html"/>">
                <@setupStepFields name="store" exclude=["webSiteId"]/>
                <input type="hidden" name="webSiteId" value="${currWebSite.webSiteId}"/>
                <@field type="submit" text=uiLabelMap.CommonSelect class="+${styles.link_nav!} ${styles.action_update!}"/>
              </form>
              
              <a href="javascript:document.setProductStoreDefaultWebSite_${currWebSite_index}.submit();" class="${styles.link_run_sys!} ${styles.action_update!}">${uiLabelMap.CommonSetDefault}</a>
              <form name="setProductStoreDefaultWebSite_${currWebSite_index}" method="post" action="<@pageUrl>setProductStoreDefaultWebSite</@pageUrl>">
                <@setupStepFields name="store" exclude=["webSiteId"]/>
                <input type="hidden" name="webSiteId" value="${currWebSite.webSiteId}"/>
              </form>
            </@td>
          </@tr>
        </#list>
      </@tbody>
    </@table>
