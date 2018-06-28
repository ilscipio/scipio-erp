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
<p>${uiLabelMap.WebtoolsXMLExportSingleInfo!""}</p>
<hr />
<#if security.hasPermission("ENTITY_MAINT", session)>
  <@menu type="button">
    <@menuitem type="link" href=makeOfbizUrl("xmldsrawdump") target="_blank" text="Click Here to Get Data (or save to file)" class="+${styles.action_run_sys!} ${styles.action_export!}" />
  </@menu>
<#else>
  <@commonMsg type="error">${uiLabelMap.WebtoolsPermissionMaint}</@commonMsg>
</#if>
<#else>
<#macro displayButtonBar>
  <@menu type="button">
    <@menuitem type="submit" text=uiLabelMap.WebtoolsExport class="+${styles.action_run_sys!} ${styles.action_export!}" />
    <@menuitem type="link" href=makeOfbizUrl("xmldsdump?checkAll=true") text=uiLabelMap.WebtoolsCheckAll class="+${styles.action_run_local!} ${styles.action_select!}" />
    <@menuitem type="link" href=makeOfbizUrl("xmldsdump") text=uiLabelMap.WebtoolsUnCheckAll class="+${styles.action_run_local!} ${styles.action_select!}" />
  </@menu>
</#macro>


<@alert type="info">${uiLabelMap.WebtoolsXMLExportInfo}</@alert>


<#if security.hasPermission("ENTITY_MAINT", session)>
  <#if exportList?has_content>
      <#--
         # Format Number of Bytes in SI Units
         # -->
      <#function si num>
        <#assign byteStr = Static["org.apache.commons.io.FileUtils"].byteCountToDisplaySize(num?long)/>
        <#return byteStr />
      </#function>
      <@section title=uiLabelMap.WebtoolsResults> 
          <@table type="data-list" autoAltRows=true scrollable=true fixedColumnsLeft=1 fixedColumnsRight=1> <#-- orig: class="basic-table" --> <#-- orig: cellspacing="0" -->
              <@thead>
                <@tr>
                  <@th>${uiLabelMap.File}</@th>
                  <@th>${uiLabelMap.CommonDate}</@th>
                  <@th>${uiLabelMap.CommonSize}</@th>
                  <@th>${uiLabelMap.CommonDescription}</@th>
                  <@th>&nbsp;</@th>
              </@tr>
              </@thead>
                <#if exportList?has_content>
                    <#list exportList as record>
                        <@tr>
                            <@td width="100">
                                <#assign exportUrl=makeOfbizWebappUrl("/export?exportId=" + record.exportId!"") />
                                <a href="${exportUrl!""}" target="_blank">${uiLabelMap.FormFieldTitle_downloadAction}</a>
                            </@td>
                            
                            <@td>
                                ${record.createdStamp?string.short!}
                            </@td>
                            
                            <@td>
                                ${si(record.fileSize!0)}
                            </@td>
                            <@td>
                                ${record.description!""}
                            </@td>
                            <@td>
                               <#-- <a href="">${uiLabelMap.CommonDelete}</a>-->
                            </@td>
                        </@tr>
                    </#list>
                </#if>
            </@table>
          
          <#-- 
              <#if parameters.filename?has_content && (numberOfEntities?has_content && numberOfEntities?number > 0)>
                <p>${uiLabelMap.WebtoolsWroteXMLForAllDataIn}</p>
                <p>${uiLabelMap.WebtoolsWroteNRecordsToXMLFile}</p>
              <#elseif parameters.outpath?has_content && (numberOfEntities?has_content && numberOfEntities?number > 0)>
                <#list results as result>
                  <p>${result}</p>
                </#list>
              <#else>
                <p>${uiLabelMap.WebtoolsNoFilenameSpecified}</p>
              </#if>
          -->
      </@section>
  </#if>
  
  <form method="post" action="<@ofbizUrl>xmldsdump</@ofbizUrl>" name="entityExport">

  <@section title=uiLabelMap.WebtoolsExport>
      
      <@row>
        <@cell columns=6>
            <@displayButtonBar/>
            <#-- 
                <@field type="input" label=uiLabelMap.WebtoolsOutputDirectory size="60" name="outpath" value=(parameters.outpath!)/>
                <@field type="input" label=uiLabelMap.WebtoolsMaxRecordsPerFile size="10" name="maxrecords"/>
                <@field type="input" label=uiLabelMap.WebtoolsSingleFilename size="60" name="filename" value=(parameters.filename!)/>
            -->
            <@field type="input" label=uiLabelMap.CommonDescription size="60" name="description" value=""/>
            <@field type="datetime" label=uiLabelMap.WebtoolsRecordsUpdatedSince name="entityFrom"  value="" size="25" maxlength="30" id="entityFrom1" />
            <@field type="datetime" label=uiLabelMap.WebtoolsRecordsUpdatedBefore name="entityThru" value="" size="25" maxlength="30" id="entityThru1" />
            <#-- <@field type="checkbox" name="tobrowser" value="N" label=uiLabelMap.WebtoolsOutToBrowser/>-->
            
            <hr/>
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
       

    </@section>

    <@section title=uiLabelMap.WebtoolsEntityNames>
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
    </@section>
    </form>

<#else>
    <@commonMsg type="error">${uiLabelMap.WebtoolsPermissionMaint}</@commonMsg>
</#if>

</#if>