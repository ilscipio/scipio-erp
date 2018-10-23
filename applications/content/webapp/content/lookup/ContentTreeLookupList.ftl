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
  
<#-- SCIPIO: TODO?: convert to @paginate, but this has special AJAX (NOTE: maybe leave as-is...) -->
<@menu type="button">
  <#if (arraySize > 0)>
    <#assign url='/views/'+activeSubMenuItem+'?'+curFindString+'&amp;VIEW_SIZE='+viewSize+'&amp;VIEW_INDEX='+viewIndexFirst>
    <@menuitem type="link" href="javascript:nextPrevDocumentList('${url}');" text=uiLabelMap.CommonFirst disabled=(!(viewIndex > 0)) />
      <#assign url='/views/'+activeSubMenuItem+'?'+curFindString+'&amp;VIEW_SIZE='+viewSize+'&amp;VIEW_INDEX='+viewIndexPrevious>
    <@menuitem type="link" href="javascript:nextPrevDocumentList('${url}');" text=uiLabelMap.CommonPrevious disabled=(!(viewIndex > 0)) />
    <@menuitem type="text" text="${lowIndex} - ${highIndex} ${rawLabel('CommonOf')} ${arraySize}" />
    <#assign url='/views/'+activeSubMenuItem+'?'+curFindString+'&amp;VIEW_SIZE='+viewSize+'&amp;VIEW_INDEX='+viewIndexNext>|
    <@menuitem type="link" href="javascript:nextPrevDocumentList('${url}');" text=uiLabelMap.CommonNext disabled=(!(arraySize > highIndex)) />
    <#assign url='/views/'+activeSubMenuItem+'?'+curFindString+'&amp;VIEW_SIZE='+viewSize+'&amp;VIEW_INDEX='+viewIndexLast>|
    <@menuitem type="link" href="javascript:nextPrevDocumentList('${url}');" text=uiLabelMap.CommonLast disabled=(!(arraySize > highIndex)) />
  </#if>
</@menu>

<#--<hr />-->

<#if contentAssoc?has_content>  
<@table type="data-list" autoAltRows=true> <#-- orig: class="basic-table hover-bar" --> <#-- orig: cellspacing="0" -->
    <#if activeSubMenuItem=="ListContentTree">
    <#--Form ListContentTree-->
    <@thead>
      <@tr class="header-row">
        <@th>${uiLabelMap.FormFieldTitle_contentId}</@th>
        <@th>${uiLabelMap.FormFieldTitle_coContentName}</@th>
        <@th>${uiLabelMap.FormFieldTitle_mimeTypeId}</@th>
      </@tr>
    </@thead>
    <#elseif activeSubMenuItem=="ListDocument">
    <#--Form ListDocument-->
    <@thead>
     <@tr class="header-row">
        <@th>${uiLabelMap.FormFieldTitle_contentId}</@th>
        <@th>${uiLabelMap.CommonView}</@th>
        <@th>${uiLabelMap.FormFieldTitle_contentTypeId}</@th>
        <@th>${uiLabelMap.FormFieldTitle_mimeTypeId}</@th>
        <@th>${uiLabelMap.FormFieldTitle_contentStatusId}</@th>
        <@th>${uiLabelMap.FormFieldTitle_caCratedDate}</@th>
        <@th>${uiLabelMap.CommonDelete}</@th>
      </@tr>
    </@thead>
    </#if>
       <#assign alt_row = false/>
       <#assign listcount=0>
      <#list contentAssoc as contentData>
      <#if activeSubMenuItem=="ListContentTree">
        <#--Form ListContentTree-->
          <@tr> 
              <@td><a class="plain" href="javascript:set_value('${contentData.contentId!}')" class="${styles.link_run_local!} ${styles.action_select!}">${contentData.contentId!}</a></@td>
              <@td>${contentData.contentName!}</@td>
              <@td>${contentData.mimeTypeId!}</@td>
          </@tr>
      <#elseif activeSubMenuItem=="ListDocument">
          <#--Form ListDocument-->
          <@tr>
              <@td><a class="plain" href="<@ofbizInterWebappUrl>/content/control/editContent?contentId=${contentData.contentId!}</@ofbizInterWebappUrl>">${contentData.contentName!}[${contentData.contentId!}]</a></@td>
              <@td><a class="plain" href="<@ofbizInterWebappUrl>/content/control/showContent?contentId=${contentData.contentId!}</@ofbizInterWebappUrl>" target="_blank">${uiLabelMap.CommonView}</a></@td>
              <@td>${contentData.contentTypeId!}</@td>
              <@td>${contentData.mimeTypeId!}</@td>
              <@td>${contentData.statusId!}</@td>
              <#if contentData.caFromDate?has_content>
             <#assign caFromDate = Static["org.ofbiz.base.util.UtilDateTime"].toDateString(contentData.caFromDate, "dd/MM/yyyy")/>
            </#if> 
              <@td>${caFromDate!}</@td>
              <@td><a href="javascript:document.listDocumentForm_${listcount}.submit()" class="${styles.link_run_sys!} ${styles.action_remove!}">${uiLabelMap.CommonDelete}</a></@td>
          </@tr>
          <form action="<@ofbizUrl>removeDocumentFromTree</@ofbizUrl>" name="listDocumentForm_${listcount}" method="post">
            <input type="hidden" name="contentId" value="${contentData.contentIdStart!}"/>
            <input type="hidden" name="contentIdTo" value="${contentData.contentId!}"/>
            <input type="hidden" name="contentAssocTypeId" value="${contentData.caContentAssocTypeId!}"/>
            <input type="hidden" name="fromDate" value="${contentData.fromDate!}"/>
          </form>
     </#if>
         <#assign listcount=listcount+1>
      </#list>
</@table>
<#else>
  <@commonMsg type="result-norecord" />
</#if>
