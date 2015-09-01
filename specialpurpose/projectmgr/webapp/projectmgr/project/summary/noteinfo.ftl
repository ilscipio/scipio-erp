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
  <#assign menuHtml>
      <#--if project?has_content>
        <li><a href="<@ofbizUrl>newNotesForProject?workEffortId=${project.workEffortId!}&amp;showForm=Y</@ofbizUrl>" class="${styles.button_default!}">${uiLabelMap.ProjectMgrNotesCreateNew}</a></li>
      <#else>
        <li><a href="<@ofbizUrl>newNotesForTask?workEffortId=${task.workEffortId!}&amp;showForm=Y</@ofbizUrl>" class="${styles.button_default!}">${uiLabelMap.ProjectMgrNotesCreateNew}</a></li>
      </#if-->
  </#assign>
  <@section title="${uiLabelMap.WorkEffortNotes}" menuHtml=menuHtml>
      <@table type="generic" width='100%' border='0' cellspacing='0' cellpadding='0' class='boxbottom'>
        <@tr>
          <@td>
            <#if workEffortNoteandDetails?has_content>
            <@table type="data-complex" class="" width="100%" border="0" cellpadding="1">
              <#list workEffortNoteandDetails as note>
                <@tr>
                  <@td valign="top" width="35%">
                    <div>&nbsp;<b>${uiLabelMap.CommonBy}: </b>${Static["org.ofbiz.party.party.PartyHelper"].getPartyName(delegator, note.noteParty, true)}</div>
                    <div>&nbsp;<b>${uiLabelMap.CommonAt}: </b>${Static["org.ofbiz.base.util.UtilDateTime"].timeStampToString(note.noteDateTime!,"dd-MM-yyyy HH:mm",Static["java.util.TimeZone"].getDefault(),context.get("locale"))}</div>
                  </@td>
                  <@td valign="top" width="50%">${note.noteInfo!}
                  </@td>
                  <@td align="right" valign="top" width="15%">
                    <#if note.internalNote! == "N">
                        <div>${uiLabelMap.ProjectMgrPrintableNote}</div>
                          <#if project?has_content>
                            <a href="<@ofbizUrl>updateProjectNote?workEffortId=${project.workEffortId!}&amp;noteId=${note.noteId}&amp;internalNote=Y</@ofbizUrl>" class="${styles.button_default!}">${uiLabelMap.OrderNotesPrivate}</a>
                          <#else>
                            <a href="<@ofbizUrl>updateTaskNoteSummary?workEffortId=${task.workEffortId!}&amp;noteId=${note.noteId}&amp;internalNote=Y</@ofbizUrl>" class="${styles.button_default!}">${uiLabelMap.OrderNotesPrivate}</a>
                          </#if>
                    </#if>
                    <#if note.internalNote! == "Y">
                        <div>${uiLabelMap.OrderNotPrintableNote}</div>
                           <#if project?has_content>
                             <a href="<@ofbizUrl>updateProjectNote?workEffortId=${project.workEffortId!}&amp;noteId=${note.noteId}&amp;internalNote=N</@ofbizUrl>" class="${styles.button_default!}">${uiLabelMap.OrderNotesPublic}</a>
                          <#else>
                            <a href="<@ofbizUrl>updateTaskNoteSummary?workEffortId=${task.workEffortId!}&amp;noteId=${note.noteId}&amp;internalNote=N</@ofbizUrl>" class="${styles.button_default!}">${uiLabelMap.OrderNotesPublic}</a>
                          </#if>
                    </#if>
                  </@td>
                </@tr>
                <#if note_has_next>
                  <@tr><@td colspan="3"><hr/></@td></@tr>
                </#if>
              </#list>
            </@table>
            <#else>
              <#if project?has_content>
                <@resultMsg>${uiLabelMap.ProjectMgrProjectNoNotes}.</@resultMsg>
              <#else>
                <@resultMsg>${uiLabelMap.ProjectMgrTaskNoNotes}.</@resultMsg>
              </#if>

            </#if>
          </@td>
        </@tr>
      </@table>
  </@section>
  
      <#if parameters.showForm??>
        <@section title="${uiLabelMap.OrderAddNote}">
      
          <form name="createnoteform" method="post"
            <#if project?has_content> action="<@ofbizUrl>createNewNotesForProject</@ofbizUrl>"
            <#else> action="<@ofbizUrl>createNewNotesForTask</@ofbizUrl>"
            </#if>>
            <@table type="fields" class="" width="90%" border="0" cellpadding="2" cellspacing="0">
              <@tr>
                <#if project?has_content>
                  <@td><input type="hidden" name="workEffortId" value="${project.workEffortId}" /></@td>
                <#else>
                  <@td><input type="hidden" name="workEffortId" value="${task.workEffortId}" /></@td>
                </#if>
              </@tr>
              <@tr>
                <@td width="26%" align="right">${uiLabelMap.OrderNote}</@td>
                <@td width="54%">
                  <textarea name="noteInfo" rows="5" cols="70"></textarea>
                </@td>
              </@tr>
              <@tr>
                <td/><@td>${uiLabelMap.OrderInternalNote} :
                  <select name="internalNote" size="1"><option value=""></option><option value="Y" selected>${uiLabelMap.CommonYes}</option><option value="N">${uiLabelMap.CommonNo}</option></select></@td>
              </@tr>
              <@tr>
                <td/><@td><i>${uiLabelMap.OrderInternalNoteMessage}</i></@td>
              </@tr>
            </@table>
            <#if project?has_content>
              &nbsp;<a href="javascript:document.createnoteform.submit()" class="${styles.button_default!}">${uiLabelMap.CommonSave}</a>
            <#else>
              &nbsp;<a href="javascript:document.createnoteform.submit()" class="${styles.button_default!}">${uiLabelMap.CommonSave}</a>
            </#if>
          </form>
        </@section>
      </#if>


