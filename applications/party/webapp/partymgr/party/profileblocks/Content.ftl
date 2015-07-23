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
  <@section id="partyContent" title="${uiLabelMap.PartyContent}">
    <@row>
      <@cell>
    
      ${screens.render("component://party/widget/partymgr/ProfileScreens.xml#ContentList")}
      
    <@section title="${uiLabelMap.PartyAttachContent}">
      <@row>
        <@cell>
      <form id="uploadPartyContent" method="post" enctype="multipart/form-data" action="<@ofbizUrl>uploadPartyContent</@ofbizUrl>">
        <input type="hidden" name="dataCategoryId" value="PERSONAL"/>
        <input type="hidden" name="contentTypeId" value="DOCUMENT"/>
        <input type="hidden" name="statusId" value="CTNT_PUBLISHED"/>
        <input type="hidden" name="partyId" value="${partyId}" id="contentPartyId"/>

        <@field type="file" label="${uiLabelMap.PartyAttachFile}" name="uploadedFile" required=true addClass="error" size=25 />
        
        <@field type="select" label="${uiLabelMap.PartyContentType}" name="partyContentTypeId" required=true addClass="error">
          <option value="">${uiLabelMap.PartySelectPurpose}</option>
          <#list partyContentTypes as partyContentType>
            <option value="${partyContentType.partyContentTypeId}">${partyContentType.get("description", locale)!(partyContentType.partyContentTypeId)}</option>
          </#list> 
        </@field>

        <@field type="select" label="${uiLabelMap.PartyIsPublic}" name="isPublic">
          <option value="N">${uiLabelMap.CommonNo}</option>
          <option value="Y">${uiLabelMap.CommonYes}</option>
        </@field>
       
        <@field type="select" label="${uiLabelMap.PartySelectRole}" name="roleTypeId">
          <#list roles as role>
            <option value="${role.roleTypeId}" <#if role.roleTypeId == "_NA_">selected="selected"</#if>>${role.get("description", locale)!(role.roleTypeId)}</option>
          </#list>
        </@field>
   
        <@row>
          <@cell class="${style_grid_small!}3 ${style_grid_large!}2">
            <input type="submit" value="${uiLabelMap.CommonUpload}" class="smallSubmit" />
          </@cell>
          <@cell class="${style_grid_small!}6 ${style_grid_large!}6">
            <@progress id="upc_progress_bar" type="info" addWrapClass="${style_hidden!}"/>
          </@cell>
          <@cell class="${style_grid_small!}3 ${style_grid_large!}4" id="upcProgressMsgBox">
          </@cell>
        </@row>
      </form>
 
  <script type="text/javascript">
  
    <@requireScriptOfbizUrl uri="getFileUploadProgressStatus" />
    
    <#-- functions defined in PartyProfileContent.js -->
    var ppcUploadProgress = null;

    jQuery(document).ready(function() {
        ppcUploadProgress = new CatoUploadProgress({
            formId : "uploadPartyContent",
            progBarId : "upc_progress_bar",
            progTextBoxId : "upcProgressMsgBox",
            iframeParentId : "partyContent",
            msgContainerId : "upc_content_messages",
            msgContainerSiblingId : "partyContentList",
            contentContainerId : "partyContentList",
            targetContentContainerId : "partyContentList"
        });
        ppcUploadProgress.reset();
    });
    
    jQuery("#uploadPartyContent").validate({
        submitHandler: function(form) {
            ppcUploadProgress.initUpload();
            form.submit();
        }
    });
  </script>
        </@cell>
      </@row>
    </@section>
    
      </@cell>
    </@row>
  </@section>
