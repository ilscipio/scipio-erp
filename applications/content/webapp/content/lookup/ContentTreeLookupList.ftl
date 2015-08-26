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
  
<ul class="${styles.button_group!}">
  <#if (arraySize > 0)>
        <#assign url='/views/'+tabButtonItem+'?'+curFindString+'&amp;VIEW_SIZE='+viewSize+'&amp;VIEW_INDEX='+viewIndexFirst>
        <li><a href="javascript:nextPrevDocumentList('${url}');" class="${styles.button_default!}<#if !(viewIndex > 0)> disabled</#if>">${uiLabelMap.CommonFirst}</a></li>
          <#assign url='/views/'+tabButtonItem+'?'+curFindString+'&amp;VIEW_SIZE='+viewSize+'&amp;VIEW_INDEX='+viewIndexPrevious>
        <li><a href="javascript:nextPrevDocumentList('${url}');" class="${styles.button_default!}<#if !(viewIndex > 0)> disabled</#if>">${uiLabelMap.CommonPrevious}</a></li>
        <li><span class="text-entry">${lowIndex} - ${highIndex} ${uiLabelMap.CommonOf} ${arraySize}</span></li>
        <#assign url='/views/'+tabButtonItem+'?'+curFindString+'&amp;VIEW_SIZE='+viewSize+'&amp;VIEW_INDEX='+viewIndexNext>|
        <li><a href="javascript:nextPrevDocumentList('${url}');" class="${styles.button_default!}<#if !(arraySize > highIndex)> disabled</#if>">${uiLabelMap.CommonNext}</a></li>
        <#assign url='/views/'+tabButtonItem+'?'+curFindString+'&amp;VIEW_SIZE='+viewSize+'&amp;VIEW_INDEX='+viewIndexLast>|
        <li><a href="javascript:nextPrevDocumentList('${url}');" class="${styles.button_default!}<#if !(arraySize > highIndex)> disabled</#if>">${uiLabelMap.CommonLast}</a></li>
  </#if>
</ul>

<hr />

<@table type="data-list" autoAltRows=true class="basic-table hover-bar" cellspacing="0">
<#if tabButtonItem=="ListContentTree">
<#--Form ListContentTree-->
<@thead>
  <@tr class="header-row">
    <@th>${uiLabelMap.FormFieldTitle_contentId}</@th>
    <@th>${uiLabelMap.FormFieldTitle_coContentName}</@th>
    <@th>${uiLabelMap.FormFieldTitle_mimeTypeId}</@th>
  </@tr>
</@thead>
<#elseif tabButtonItem=="ListDocument">
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
<#if contentAssoc?has_content>  
       <#assign alt_row = false/>
       <#assign listcount=0>
      <#list contentAssoc as contentData>
      <#if tabButtonItem=="ListContentTree">
        <#--Form ListContentTree-->
          <@tr> 
              <@td><a class="plain" href="javascript:set_value('${contentData.contentId!}')">${contentData.contentId!}</a></@td>
              <@td>${contentData.contentName!}</@td>
              <@td>${contentData.mimeTypeId!}</@td>
          </@tr>
      <#elseif tabButtonItem=="ListDocument">
          <#--Form ListDocument-->
          <@tr>
              <@td><a class="plain" href="/content/control/editContent?contentId=${contentData.contentId!}">${contentData.contentName!}[${contentData.contentId!}]</a></@td>
              <@td><a class="plain" href="/content/control/showContent?contentId=${contentData.contentId!}" target="_blank">${uiLabelMap.CommonView}</a></@td>
              <@td>${contentData.contentTypeId!}</@td>
              <@td>${contentData.mimeTypeId!}</@td>
              <@td>${contentData.statusId!}</@td>
              <#if contentData.caFromDate?has_content>
             <#assign caFromDate = Static["org.ofbiz.base.util.UtilDateTime"].toDateString(contentData.caFromDate, "dd/MM/yyyy")/>
            </#if> 
              <@td>${caFromDate!}</@td>
              <@td><a href="javascript:document.listDocumentForm_${listcount}.submit()">${uiLabelMap.CommonDelete}</a></@td>
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
</#if>
</@table>
