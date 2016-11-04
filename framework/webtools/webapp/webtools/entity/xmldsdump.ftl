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
<#if tobrowser?? && tobrowser>
<@heading>${uiLabelMap.PageTitleEntityExport}</@heading>
<p>This page can be used to export data from the database. The exported documents will have a root tag of "&lt;entity-engine-xml&gt;".</p>
<hr />
<#if security.hasPermission("ENTITY_MAINT", session)>
  <@menu type="button">
    <@menuitem type="link" href=makeOfbizUrl("xmldsrawdump") target="_blank" text="Click Here to Get Data (or save to file)" class="+${styles.action_run_sys!} ${styles.action_export!}" />
  </@menu>
<#else>
  <@commonMsg type="error">You do not have permission to use this page (ENTITY_MAINT needed)</@commonMsg>
</#if>
<#else>
<#macro displayButtonBar>
  <@menu type="button">
    <@menuitem type="submit" text=uiLabelMap.WebtoolsExport class="+${styles.action_run_sys!} ${styles.action_export!}" />
    <@menuitem type="link" href=makeOfbizUrl("xmldsdump?checkAll=true") text=uiLabelMap.WebtoolsCheckAll class="+${styles.action_run_local!} ${styles.action_select!}" />
    <@menuitem type="link" href=makeOfbizUrl("xmldsdump") text=uiLabelMap.WebtoolsUnCheckAll class="+${styles.action_run_local!} ${styles.action_select!}" />
  </@menu>
</#macro>

<p>${uiLabelMap.WebtoolsXMLExportInfo}</p>
<hr />

<#if security.hasPermission("ENTITY_MAINT", session)>
  <@heading>${uiLabelMap.WebtoolsResults}:</@heading>
  <#if parameters.filename?has_content && (numberOfEntities?number > 0)>
    <p>${uiLabelMap.WebtoolsWroteXMLForAllDataIn}</p>
    <p>${uiLabelMap.WebtoolsWroteNRecordsToXMLFile}</p>
  <#elseif parameters.outpath?has_content && (numberOfEntities?number > 0)>
    <#list results as result>
      <p>${result}</p>
    </#list>
  <#else>
    <p>${uiLabelMap.WebtoolsNoFilenameSpecified}</p>
  </#if>

  <hr />

  <@heading>${uiLabelMap.WebtoolsExport}:</@heading>
  <form method="post" action="<@ofbizUrl>xmldsdump</@ofbizUrl>" name="entityExport">
  <@row>
    <@cell columns=6>
    <@field type="input" label=uiLabelMap.WebtoolsOutputDirectory size="60" name="outpath" value=(parameters.outpath!)/>
    <@field type="input" label=uiLabelMap.WebtoolsMaxRecordsPerFile size="10" name="maxrecords"/>
    <@field type="input" label=uiLabelMap.WebtoolsSingleFilename size="60" name="filename" value=(parameters.filename!)/>
    <@field type="datetime" label=uiLabelMap.WebtoolsRecordsUpdatedSince name="entityFrom"  value="" size="25" maxlength="30" id="entityFrom1" />
    <@field type="datetime" label=uiLabelMap.WebtoolsRecordsUpdatedBefore name="entityThru" value="" size="25" maxlength="30" id="entityThru1" />
    <@field type="checkbox" name="tobrowser" value="N" label=uiLabelMap.WebtoolsOutToBrowser/>
    </@cell>
  </@row>
    <hr>
     <@row>
        <@cell columns=6>
          <@displayButtonBar/>
          <@field type="input" name="entitySyncId" size="30" value=(entitySyncId!) label=uiLabelMap.WebtoolsEntitySyncDump/>
          <@field type="select" label=uiLabelMap.WebtoolsPreConfiguredSet name="preConfiguredSetName">
            <option value="">${uiLabelMap.CommonNone}</option>
            <option value="CatalogExport">${uiLabelMap.WebtoolsPreConfiguredSet1}</option>
            <option value="Product1">${uiLabelMap.WebtoolsPreConfiguredSet2}</option>
            <option value="Product2">${uiLabelMap.WebtoolsPreConfiguredSet3}</option>
            <option value="Product3">${uiLabelMap.WebtoolsPreConfiguredSet4}</option>
            <option value="Product4">${uiLabelMap.WebtoolsPreConfiguredSet5}</option>
          </@field>
        </@cell>
    </@row>

    <@heading relLevel=1>${uiLabelMap.WebtoolsEntityNames}:</@heading>
    <@grid>
          <#assign entCount = 0>
          <#assign checkAll = parameters.checkAll!"false">
          <#list modelEntities as modelEntity>
            <#assign entCount = entCount + 1>
            <#assign check = checkAll/>
            <#if checkAll == "true" && modelEntity.getClass().getName() == "org.ofbiz.entity.model.ModelViewEntity">
                <#assign check = "false"/>
            </#if>
            <#assign curEntityName = modelEntity.getEntityName()/>
            <li><@field type="checkbox" name="entityName" checked=check id=curEntityName value=curEntityName label=(curEntityName) norows=true nocells=true/></li>
          </#list>
    </@grid>

      <@displayButtonBar/>
    </form>

<#else>
    <@commonMsg type="error">${uiLabelMap.WebtoolsPermissionMaint}</@commonMsg>
</#if>

</#if>