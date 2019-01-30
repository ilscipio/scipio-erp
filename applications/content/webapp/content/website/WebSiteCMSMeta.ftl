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

<#macro cmsNewMetaRec>
    <input type="hidden" name="contentTypeId" value="DOCUMENT"/>
    <input type="hidden" name="dataResourceTypeId" value="SHORT_TEXT"/>
    <input type="hidden" name="contentAssocTypeId" value="SUB_CONTENT"/>
    <input type="hidden" name="statusId" value="CTNT_PUBLISHED"/>
    <input type="hidden" name="ownerContentId" value="${(content.contentId)!}"/>
    <input type="hidden" name="contentIdFrom" value="${(content.contentId)!}"/>
</#macro>

<#-- cms menu bar -->
<div id="cmsmenu" style="margin-bottom: 8px;">
    <#if (content?has_content)>
        <a href="javascript:void(0);" onclick="javascript:callDocument(true, '${content.contentId}', '', 'ELECTRONIC_TEXT');" class="${styles.link_nav!}">Quick Sub-Content</a>
        <a href="javascript:void(0);" onclick="javascript:callPathAlias('${content.contentId}');" class="${styles.link_nav!}">Path Alias</a>
        <a href="javascript:void(0);" onclick="javascript:callMetaInfo('${content.contentId}');" class="${styles.link_nav!} ${styles.selected!}">Meta Tags</a>
    </#if>
</div>

<#if (content?has_content)>
    <div style="margin-bottom: 8px;">
        Set <b>Meta-Data</b> for Content: <b>${content.contentId}</b></b>
    </div>
</#if>

<#if (title?has_content)>
    <#assign titleAction = "/updateWebSiteMetaInfoJson"/>
<#else>
    <#assign titleAction = "/createWebSiteMetaInfoJson"/>
</#if>
<#if (titleProperty?has_content)>
    <#assign titlePropertyAction = "/updateWebSiteMetaInfoJson"/>
<#else>
    <#assign titlePropertyAction = "/createWebSiteMetaInfoJson"/>
</#if>
<#if (metaDescription?has_content)>
    <#assign metaDescriptionAction = "/updateWebSiteMetaInfoJson"/>
<#else>
    <#assign metaDescriptionAction = "/createWebSiteMetaInfoJson"/>
</#if>
<#if (metaKeywords?has_content)>
    <#assign metaKeywordsAction = "/updateWebSiteMetaInfoJson"/>
<#else>
    <#assign metaKeywordsAction = "/createWebSiteMetaInfoJson"/>
</#if>

<form name="cmsmeta_title" action="<@pageUrl>${titleAction}</@pageUrl>">
    <#if (title?has_content)>
        <input type="hidden" name="dataResourceId" value="${title.dataResourceId}"/>
    <#else>
        <input type="hidden" name="contentName" value="Meta-Title: ${contentId}"/>
        <input type="hidden" name="mapKey" value="title"/>
        <@cmsNewMetaRec/>
    </#if>
    <input type="hidden" name="objectInfo" value=""/>
</form>
<form name="cmsmeta_titleProperty" action="<@pageUrl>${titlePropertyAction}</@pageUrl>">
    <#if (titleProperty?has_content)>
        <input type="hidden" name="dataResourceId" value="${titleProperty.dataResourceId}"/>
    <#else>
        <input type="hidden" name="contentName" value="Meta-TitleProperty: ${contentId}"/>
        <input type="hidden" name="mapKey" value="titleProperty"/>
        <@cmsNewMetaRec/>
    </#if>
    <input type="hidden" name="objectInfo" value=""/>
</form>
<form name="cmsmeta_metaDescription" action="<@pageUrl>${metaDescriptionAction}</@pageUrl>">
    <#if (metaDescription?has_content)>
        <input type="hidden" name="dataResourceId" value="${metaDescription.dataResourceId}"/>
    <#else>
        <input type="hidden" name="contentName" value="Meta-Description: ${contentId}"/>
        <input type="hidden" name="mapKey" value="metaDescription"/>
        <@cmsNewMetaRec/>
    </#if>
    <input type="hidden" name="objectInfo" value=""/>
</form>
<form name="cmsmeta_metaKeywords" action="<@pageUrl>${metaKeywordsAction}</@pageUrl>">
    <#if (metaKeywords?has_content)>
        <input type="hidden" name="dataResourceId" value="${metaKeywords.dataResourceId}"/>
    <#else>
        <input type="hidden" name="contentName" value="Meta-Keywords: ${contentId}"/>
        <input type="hidden" name="mapKey" value="metaKeywords"/>
        <@cmsNewMetaRec/>
    </#if>
    <input type="hidden" name="objectInfo" value=""/>
</form>

<form name="cmsmetaform" action="javascript:void(0);">
        <@field type="input" label="Page Title" name="title" value=((title.objectInfo)!) size="40" />
        <@field type="input" label="Title Property" name="titleProperty" value=((titleProperty.objectInfo)!) size="40" />
        <@field type="input" label="Meta-Description" name="metaDescription" value=((metaDescription.objectInfo)!) size="40" />
        <@field type="input" label="Meta-Keywords" name="metaKeywords" value=((metaKeywords.objectInfo)!) size="40" />
        <@field type="submit" submitType="input-button" id="submit" onClick="javascript:saveMetaInfo(cmsmetaform);" class="+${styles.link_run_sys!} ${styles.action_update!}" text=uiLabelMap.CommonSave/>
</form>