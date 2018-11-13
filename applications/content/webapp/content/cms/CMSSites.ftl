<#--
This file is subject to the terms and conditions defined in the
files 'LICENSE' and 'NOTICE', which are part of this source
code package.
-->
<@script>
function call_fieldlookup4(rootForumId, parentForumId ) {
    var obj_lookupwindow = window.open("addSubSite?rootForumId=" + rootForumId + "&amp;parentForumId=" + parentForumId, 'FieldLookup', 'width=500,height=250,scrollbars=yes,status=no,top='+my+',left='+mx+',dependent=yes,alwaysRaised=yes');
    obj_lookupwindow.opener = window;
    obj_lookupwindow.focus();
}
</@script>


<#--
<#include "publishlib.ftl" />
-->
<#if !rootForumId?has_content>
    <#assign rootForumId=requestParameters.rootForumId!/>
</#if>
<#if !rootForumId?has_content>
    <#assign rootForumId=defaultSiteId!/>
</#if>
<@checkPermission entityOperation="_ADMIN" targetOperation="CONTENT_ADMIN" >

<@table type="generic" border="0" width="100%" cellspacing="0" cellpadding="0" class="+boxoutside"> <#-- orig: class="boxoutside" -->
  <@tr>
    <@td width='100%'>
      <form name="userform" method="post" action="<@ofbizUrl>CMSSites</@ofbizUrl>" >
      <@table type="fields" width="100%" class="+appTitle"> <#-- orig: class="appTitle" --> <#-- orig: cellspacing="0" --> <#-- orig: cellpadding="0" --> <#-- orig: border="0" -->
        <@tr>
          <@td colspan="1" valign="middle" align="right">
            <div class="boxhead">&nbsp; Root Site ID&nbsp;&nbsp; </div>
          </@td>
          <@td valign="middle">
            <div class="boxhead">
             <input type="text" name="rootForumId" size="20" value="${rootForumId!}"/>
            </div>
          </@td>
          <@td valign="middle" align="right">
            <a href="javascript:document.userform.submit()" class="${styles.link_run_sys!} ${styles.action_reload!}">Refresh</a>
          </@td>
        </@tr>
      </@table>
      </form>
    </@td>
  </@tr>
  <@tr>
    <@td width='100%'>
      <@table type="fields" width="100%" class="+boxbottom"> <#-- orig: class="boxbottom" --> <#-- orig: cellspacing="0" --> <#-- orig: cellpadding="0" --> <#-- orig: border="0" -->
        <@tr>
          <@td>
            <form method="post" name="publishsite" action="<@ofbizUrl>linkContentToPubPt</@ofbizUrl>">
              <@table type="generic" width="100%" border="0" cellpadding="1">
                <#assign rowCount = 0 />
                <@showSites forumId=rootForumId />
              </@table>
            </form>
          </@td>
        </@tr>
        <@tr>
         <@td>
            <a class="${styles.link_nav!} ${styles.action_add!}" href="<@ofbizUrl>addSubSite?rootForumId=${rootForumId}&amp;parentForumId=${rootForumId}</@ofbizUrl>">Add Top Level Forum</a>
         </@td >
        </@tr>

      </@table>
    </@td>
  </@tr>
<#if requestParameters.moderatedSiteId?has_content>
  <@tr>
    <@td width='100%'>
      <@table type="fields" width="100%" class="+boxoutside"> <#-- orig: class="boxoutside" --> <#-- orig: cellspacing="0" --> <#-- orig: cellpadding="0" --> <#-- orig: border="0" -->
         <@tr type="util"><@td><hr /></@td></@tr>
         <@tr><@td align="center"><@heading>Unapproved entries for forum Id: ${requestParameters.moderatedSiteId}</@heading></@td></@tr>
         <@tr type="util"><@td><hr /></@td></@tr>
         <@moderateSite rootForumId=rootForumId forumId=requestParameters.moderatedSiteId />
      </@table>
    </@td>
  </@tr>
</#if>
<#if requestParameters.permRoleSiteId?has_content>
  <@tr>
    <@td width='100%'>
      <@table type="fields" width="100%" class="+boxoutside"> <#-- orig: class="boxoutside" --> <#-- orig: cellspacing="0" --> <#-- orig: cellpadding="0" --> <#-- orig: border="0" -->
         <@tr type="util"><@td><hr /></@td></@tr>
         <@tr><@td align="center"><@heading>Associated roles for forum Id: ${requestParameters.permRoleSiteId}</@heading></@td></@tr>
         <@tr type="util"><@td><hr /></@td></@tr>
         <@grantSiteRoles rootForumId=rootForumId forumId=requestParameters.permRoleSiteId/>
      </@table>
    </@td>
  </@tr>
</#if>
</@table>
</@checkPermission>

<#macro showSites forumId formAction="/enableSites"  indentIndex=0 catTrail=[]>

<#local thisContentId=catTrail[indentIndex]!/>

<#local indent = "">
<#if 0 < indentIndex >
  <#list 0..(indentIndex - 1) as idx>
      <#local indent = indent + "&nbsp;&nbsp;&nbsp;&nbsp;">
  </#list>
</#if>


<@loopSubContent contentId=forumId viewIndex=0 viewSize=9999 contentAssocTypeId="SUBSITE" returnAfterPickWhen="1==1";>
       <@tr>
         <@td>
            ${indent}
            <#local plusMinus="-"/>
            ${plusMinus} ${content.contentName!}
         </@td >
         <@td>
            <a class="${styles.link_nav!} ${styles.action_update!}" href="<@ofbizUrl>CMSSites?rootForumId=${rootForumId}&amp;moderatedSiteId=${content.contentId}</@ofbizUrl>">Moderate</a>
         </@td >
         <@td>&nbsp;&nbsp;&nbsp;|&nbsp;&nbsp;&nbsp; </@td >
         <@td>
            <a class="${styles.link_nav!} ${styles.action_view!}" href="<@ofbizUrl>CMSSites?rootForumId=${rootForumId}&amp;permRoleSiteId=${content.contentId}</@ofbizUrl>">User Roles</a>
         </@td >
         <@td>&nbsp;&nbsp;&nbsp;|&nbsp;&nbsp;&nbsp; </@td >
         <@td>
            <a class="${styles.link_nav!} ${styles.action_add!}" href="<@ofbizUrl>addSubSite?rootForumId=${rootForumId}&amp;parentForumId=${content.contentId}</@ofbizUrl>">Add Child Forum</a>
         </@td >
         <@td>&nbsp;&nbsp;&nbsp;|&nbsp;&nbsp;&nbsp; </@td >
         <@td>
            <a class="${styles.link_run_sys!} ${styles.action_remove!}" href="<@ofbizUrl>removeSite?rootForumId=${rootForumId}&amp;contentId=${content.contentId}&amp;contentIdTo=${forumId}&amp;contentAssocTypeId=SUBSITE</@ofbizUrl>">Remove Site</a>
         </@td>
       </@tr>
       <#assign rowCount = rowCount + 1 />
       <@showSites forumId=subContentId indentIndex=(indentIndex + 1)/>
</@loopSubContent>

</#macro>


<#macro moderateSite forumId rootForumId >
<@table type="fields" class="+${styles.table_spacing_tiny_hint!}" width="100%"> <#-- orig: class="" --> <#-- orig: cellspacing="" --> <#-- orig: border="0" -->
 <form name="mostrecent" method="post" action="<@ofbizUrl>publishResponse</@ofbizUrl>"/>
  <#assign row=0/>
  <#list mostRecentList as content>
    <@checkPermission entityOperation="_ADMIN" targetOperation="CONTENT_PUBLISH" subContentId=forumId >
        <#-- FIXME: restructure to avoid open/close -->
        <@tr open=true close=false />
          <@td> <b>id:</b>${content.contentId} </@td>
          <@td> <b>name:</b>${content.contentName} </@td>
      <@injectNodeTrailCsv subContentId=content.contentId redo="true" contentAssocTypeId="PUBLISH_LINK">
          <@td>
          <a class="tabButton" href="<@ofbizUrl>CMSContentEdit?contentId=${content.contentId}&amp;nodeTrailCsv=${nodeTrailCsv!}</@ofbizUrl>" >View</a>
          </@td>
          <@td>
          <b>submitted:</b>
          <input type="radio" name="statusId_o_${row}" value="CTNT_FINAL_DRAFT" checked="checked"/>
          </@td>
          <@td>
          <b>publish:</b>
          <input type="radio" name="statusId_o_${row}" value="CTNT_PUBLISHED"/>
          </@td>
          <@td>
          <b>reject:</b>
          <input type="radio" name="statusId_o_${row}" value="CTNT_DEACTIVATED"/>
          </@td>
        <@tr close=true open=false />
          <input type="hidden" name="contentId_o_${row}" value="${content.contentId}"/>
        <@tr>
          <@td colspan="6">
          <b>content:</b><br />
            <@renderSubContentCache subContentId=content.contentId/>
          </@td>
        </@tr>
        <@tr type="util"> <@td colspan="5"> <hr /> </@td> </@tr>
        <#assign row = row + 1/>
      </@injectNodeTrailCsv >
    </@checkPermission >
  </#list>
    <#if (0 < mostRecentList?size)>
        <@tr>
          <@td colspan="5">
            <input type="submit" name="submitBtn" value="${uiLabelMap.CommonUpdate}" class="${styles.link_run_sys!} ${styles.action_updatestatus!}"/>
          </@td>
        </@tr>
    </#if>
          <input type="hidden" name="moderatedSiteId" value="${forumId}"/>
          <input type="hidden" name="rootForumId" value="${rootForumId}"/>
          <input type="hidden" name="_rowCount" value="${mostRecentList?size}"/>
 </form>
</@table>


</#macro>


<#macro grantSiteRoles forumId rootForumId >
<@table type="fields" class="+${styles.table_spacing_tiny_hint!}" width="100%"> <#-- orig: class="" --> <#-- orig: cellspacing="" --> <#-- orig: border="0" -->
  <@tr>
    <@td width='100%'>
      <form name="siteRoleForm" method="post" action="<@ofbizUrl>updateSiteRoles</@ofbizUrl>">
        <@fields type="default-manual">
      <input type="hidden" name="permRoleSiteId" value="${forumId}"/>
      <input type="hidden" name="forumId" value="${forumId}"/>
      <input type="hidden" name="rootForumId" value="${rootForumId}"/>
      <@table type="fields" width="100%" class="+${styles.table_spacing_small_hint!} boxoutside"> <#-- orig: class="boxoutside" --> <#-- orig: class="" --> <#-- orig: cellspacing="0" --> <#-- orig: cellpadding="4" --> <#-- orig: border="0" -->
        <@tr>
            <@td>User</@td>
            <#list blogRoleIdList as roleTypeId>
              <@td>${roleTypeId}</@td>
            </#list>
        </@tr>

      <#assign rowCount=0/>
        <#list siteList as siteRoleMap>
          <@tr>
            <@td>${siteRoleMap.partyId}</@td>
            <#list blogRoleIdList as roleTypeId>
              <#assign cappedSiteRole= Static["org.ofbiz.entity.model.ModelUtil"].dbNameToVarName(roleTypeId) />
              <@td align="center">
              <input type="checkbox" name="${cappedSiteRole}_o_${rowCount}" value="Y" <#if (siteRoleMap[cappedSiteRole]!) == "Y">checked="checked"</#if>/>
              </@td>
          <input type="hidden" name="${cappedSiteRole}FromDate_o_${rowCount}" value="${siteRoleMap[cappedSiteRole + "FromDate"]!}"/>
            </#list>
          </@tr>
          <input type="hidden" name="contentId_o_${rowCount}" value="${forumId}"/>
          <input type="hidden" name="partyId_o_${rowCount}" value="${siteRoleMap.partyId}"/>
          <#assign rowCount=rowCount + 1/>
        </#list>
        <@tr>
          <@td valign="middle">
            <@field type="lookup" formName="siteRoleForm" name="partyId_o_${rowCount}" id="partyId_o_${rowCount}" fieldFormName="LookupPerson"/><#-- FIXME check if should be changed -->
          </@td>
            <#list blogRoleIdList as roleTypeId>
              <#assign cappedSiteRole= Static["org.ofbiz.entity.model.ModelUtil"].dbNameToVarName(roleTypeId) />
              <@td align="center">
              <input type="checkbox" name="${cappedSiteRole}_o_${rowCount}" value="Y" />
              </@td>
            </#list>
            <input type="hidden" name="contentId_o_${rowCount}" value="${forumId}"/>
            <#assign rowCount=rowCount + 1/>
        </@tr>
          <@tr>
            <@td>
            <input type="submit" name="submitBtn" value="${uiLabelMap.CommonUpdate}" class="${styles.link_run_sys!} ${styles.action_update!}"/>
            </@td>
          </@tr>
      </@table>
          <input type="hidden" name="_rowCount" value="${blogRoleIdList}"/>
        </@fields>
      </form>
    </@td>
  </@tr>
</@table>

<@script>
function call_fieldlookup3(view_name) {
        window.target = document.siteRoleForm.partyId_o_${rowCount - 1};
    var obj_lookupwindow = window.open(view_name,'FieldLookup', 'width=700,height=550,scrollbars=yes,status=no,top='+my+',left='+mx+',dependent=yes,alwaysRaised=yes');
    obj_lookupwindow.opener = window;
    obj_lookupwindow.focus();
}
</@script>

</#macro>
