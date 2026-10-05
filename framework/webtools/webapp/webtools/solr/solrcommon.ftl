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
<#include "component://webtools/webapp/webtools/service/servicecommon.ftl">

<#macro solrServiceForm>
  <form name="${SERVICE_NAME}SchedForm" method="post" action="<@pageUrl>${runServiceTarget!"runSolrService"}</@pageUrl>">
    <#nested>
  </form>
</#macro>

<#macro solrServiceFields params=true initParams={} exclude=[] defaultSyncMode="sync">
  <#if params?is_boolean>
    <#if params>
      <#local params = {"POOL_NAME":POOL_NAME} + initParams>
      <#if SERVICE_NAME == (parameters.SERVICE_NAME!)>
        <#local params = parameters>
      </#if>
    <#else>
      <#local params = {}>
    </#if>
  </#if>

      <input type="hidden" name="_SOLR_SRV_RUN_" value="Y"/><#-- for event message handling, etc. -->

    <#list scheduleOptions as scheduleOption>
      <input type="hidden" name="${scheduleOption.name}" value="${scheduleOption.value}"/>
    </#list>
    
  <@fields fieldArgs={"labelColumns":4}>
    <@serviceInitFields serviceName=SERVICE_NAME srvInput=false params=params defaultSyncMode=defaultSyncMode/>
  </@fields>
  
    <hr/>
    
  <#-- SCIPIO: leave room for the label area because service parameter names can be long -->
  <@fields fieldArgs={"labelColumns":4}>
    <@serviceFields serviceParameters=(serviceParameters!) params=params exclude=exclude/>
  </@fields>

    <@field type="submit" text=uiLabelMap.PageTitleRunService class="${styles.link_run_sys!} ${styles.action_begin!}" />

</#macro>
