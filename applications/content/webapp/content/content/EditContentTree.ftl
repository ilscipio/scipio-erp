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
<form name="editContentTree" action="<#if parameters.rename?has_content><@pageUrl>updateDocumentTree</@pageUrl><#else><@pageUrl>addDocumentToTree</@pageUrl></#if>" method="post">
    <#assign content  = delegator.findOne("Content",{"contentId":parameters.contentId}, false)/>
    <#if parameters.rename?has_content>
        <@heading relLevel=+1>${uiLabelMap.ContentRenameFolder}</@heading>
        ${uiLabelMap.ContentRenameFolder} : ${content.contentName}<br />
        <input type="text" name="contentName" value="${content.contentName}" />
        <br /><a class="${styles.link_run_sys!} ${styles.action_update!}" href="javascript:document.editContentTree.submit();">${uiLabelMap.ContentRenameFolder}</a><a class="${styles.link_nav_cancel!}" href="<@pageUrl>navigateContent</@pageUrl>">${uiLabelMap.CommonCancel}</a>
    <#else>
        <@heading relLevel=+1>${uiLabelMap.ContentNewFolder}</@heading>
        ${uiLabelMap.ContentRoot} : ${content.contentName}
        <input type="text" name="contentName" />
        <br /><a class="${styles.link_run_sys!} ${styles.action_add!}" href="javascript:document.editContentTree.submit();">${uiLabelMap.CommonCreate}</a><a class="${styles.link_nav_cancel!}" href="<@pageUrl>navigateContent</@pageUrl>">${uiLabelMap.CommonCancel}</a>
    </#if>
    <input type="hidden" name="contentId" value="${parameters.contentId}"/>
    <input type="hidden" name="contentAssocTypeId" value="TREE_CHILD"/>
</form>
<hr />
