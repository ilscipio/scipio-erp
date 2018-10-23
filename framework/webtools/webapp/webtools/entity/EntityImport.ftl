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
<#include "component://webtools/webapp/webtools/entity/entitycommon.ftl">

<#-- SCIPIO: 2017-06-15: extra template configuration -->
<#assign eiActionUri = eiActionUri!"entityImport">
<#assign eiAllowServerLocs = eiAllowServerLocs!true><#-- if false, no file locations allowed -->
<#assign eiUnsafeFieldOpt = eiUnsafeFieldOpt!true>
<#assign eiInfoMsg = eiInfoMsg!(uiLabelMap.WebtoolsXMLImportInfo)>
<#assign eiShowMsgs = eiShowMsgs!true>

  <#if eiInfoMsg?has_content>
    <p>${eiInfoMsg}</p>
  </#if>

  <#-- SCIPIO: 2017-06-15: Improved source selection -->
  <@script>
    jQuery(document).ready(function() {
        var changeImportSrcType = function() {
            var importSrcType = jQuery('#entityimport-sel-form input[name=importSrcType]:checked').val();
            jQuery('.entityimport-form').hide();
            jQuery('.entityimport-' + importSrcType + '-form').show();
        };
        changeImportSrcType();
        jQuery('#entityimport-sel-form input[name=importSrcType]').change(changeImportSrcType);
    });
  </@script>
  <form method="post" action="#" id="entityimport-sel-form" class="entityimport-sel-form">
    <#if !importSrcTypeItems??>
      <#assign importSrcTypeItems = [
        {"value":"upload", "description":uiLabelMap.WebtoolsUploadXmlFile}
        {"value":"inline", "description":uiLabelMap.WebtoolsCompleteXMLDocument}
      ]>
      <#if eiAllowServerLocs>
        <#assign importSrcTypeItems = importSrcTypeItems + [
          {"value":"location", "description":uiLabelMap.WebtoolsAbsoluteFileNameOrUrl}
        ]>
      </#if>
    </#if>
    
    <#assign defImportSrcType = defImportSrcType!"upload">
    <#assign importSrcType = limitStrValToItems(importSrcType!parameters.importSrcType!, importSrcTypeItems)!defImportSrcType>
    <@field type="radio" name="importSrcType" label=uiLabelMap.DataSource items=importSrcTypeItems currentValue=importSrcType inlineItems=false/>
  </form>

  <#macro entityImportForm srcType enctype="" submitText="">
    <#if limitStrValToItems(srcType, importSrcTypeItems)??><#-- only show forms for available items -->
      <form method="post" action="<@ofbizUrl uri=eiActionUri />"<#if enctype?has_content> enctype="${enctype}"</#if><#rt/>
         <#lt/> id="entityimport-${srcType}-form" class="entityimport-form entityimport-${srcType}-form"<#if srcType != importSrcType> style="display:none;"</#if>>
        <@field type="hidden" name="importSrcType" value=srcType/>
        <#nested>
      <#if eiAllowServerLocs>
        <#-- SCIPIO: NOTE: 2017-06-15: fmfilename can be combined with any data source -->
        <@field type="input" size="40" name="fmfilename" value=(fmfilename!) label=uiLabelMap.WebtoolsAbsoluteFTLFilename/>
      </#if>
      <#if eiUnsafeFieldOpt>
        <@eiUnsafeEntityField values=parameters/>
      </#if>
        <#-- SCIPIO: NOTE: 2017-06-15: the common fields are now available to all source types (they were not in stock ofbiz) -->
        <@field type="checkbox" name="mostlyInserts" value="true" checked=mostlyInserts?? label=uiLabelMap.WebtoolsMostlyInserts/>
        <@field type="checkbox" name="maintainTimeStamps" value="true" checked=keepStamps?? label=uiLabelMap.WebtoolsMaintainTimestamps/>
        <@field type="checkbox" name="createDummyFks" value="true" checked=createDummyFks?? label=uiLabelMap.WebtoolsCreateDummyFks/>
        <@field type="checkbox" name="checkDataOnly" value="true" checked=checkDataOnly?? label=uiLabelMap.WebtoolsCheckDataOnly/>
        <@field type="input" name="txTimeout" value=(txTimeoutStr!"7200") label=uiLabelMap.WebtoolsTimeoutSeconds size="6"/>
        <@field type="submit" text=submitText class="+${styles.link_run_sys!} ${styles.action_import!}"/>
      </form>
    </#if>
  </#macro>

  <#-- SCIPIO: 2017-06-15: now supports file upload -->
  <@entityImportForm srcType="upload" enctype="multipart/form-data" submitText=uiLabelMap.WebtoolsImportFile>
    <@field type="file" name="uploadedFile" label=uiLabelMap.WebtoolsUploadXmlFile />
  </@entityImportForm>
  <@entityImportForm srcType="inline" submitText=uiLabelMap.WebtoolsImportText>
    <@field type="textarea" rows="20" cols="85" name="fulltext" label=uiLabelMap.WebtoolsCompleteXMLDocument>${fulltext!"<entity-engine-xml>\n</entity-engine-xml>"}</@field>
  </@entityImportForm>
  <@entityImportForm srcType="location" submitText=uiLabelMap.WebtoolsImportFile>
    <@field type="input" size="60" name="filename" value=(filename!) label=uiLabelMap.WebtoolsAbsoluteFileNameOrUrl/>
    <@field type="checkbox" name="isUrl" checked=isUrl?? label=uiLabelMap.WebtoolsIsURL/>
  </@entityImportForm>

  <#if eiShowMsgs && messages??>
    <@section title="${rawLabel('WebtoolsResults')}:">
      <#list messages as message>
          <p>${message}</p>
      </#list>
    </@section>
  </#if>
