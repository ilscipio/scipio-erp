<#--
Scipio Commerce
Copyright (C) Ilscipio GmbH

This file is part of Scipio Commerce. Scipio Commerce is free software: you
can redistribute it and modify it under the terms of the GNU Affero General
Public License, version 3, as published by the Free Software Foundation.
Scipio Commerce is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License
for more details. You should have received a copy of the license with this
work (file LICENSE). If not, see <https://www.gnu.org/licenses/agpl-3.0.html>.
A commercial license is available from Ilscipio GmbH.

SPDX-License-Identifier: AGPL-3.0-only
-->
<#include "component://cms/webapp/cms/common/common.ftl">

<#if "CONTENT" == envAssetType!>
    <#assign sectionTitle = uiLabelMap.CmsContentAssets>
    <#assign editUri = "editContentAsset">
<#else>
    <#assign sectionTitle = uiLabelMap.CmsAssets>
    <#assign editUri = "editAsset">
</#if>
<#macro menuContent menuArgs={}>
    <@menu args=menuArgs>
        <@menuitem type="link" href=makePageUrl(editUri) class="+${styles.action_nav!} ${styles.action_add!}" text=uiLabelMap.CommonNew/>
    </@menu>  
</#macro>
<@section title=sectionTitle menuContent=menuContent>
    <#if assetList?has_content>
        <@paginate mode="content" url=makePageUrl("assets") viewSize=(viewSize!50) viewIndex=(viewIndex!0) listSize=(listSize!0)>
            <@table type="data-list" autoAltRows=true>
                <@thead>
                    <@tr class="header-row">
                        <@th width="100px">${uiLabelMap.CmsAsset}</@th>
                        <@th width="200px">${uiLabelMap.CmsWebSite}</@th>
                        <@th>${uiLabelMap.CmsTemplateName}</@th>
                        <@th>${uiLabelMap.ContentType}</@th>
                        <@th>${uiLabelMap.CommonDescription}</@th>
                        <@th>${uiLabelMap.CommonType}</@th>
                    </@tr>
                </@thead>
                <#list assetList as asset>
                    <#assign assetModel = Static["com.ilscipio.scipio.cms.template.CmsAssetTemplate"].getWorker().makeFromValue(asset)>
                    <@tr>
                        <@td>${assetModel.id}</@td>
                        <@td>${assetModel.webSiteId!}</@td>
                        <@td><a href="<@pageUrl>${editUri}?assetTemplateId=${assetModel.id}</@pageUrl>">${assetModel.name!}</a></@td>
                        <@td><#if asset.contentTypeId?has_content>${(delegator.findOne("ContentType", {"contentTypeId":asset.contentTypeId}, true).get("description", locale))!asset.contentTypeId}</#if></@td>
                        <@td>${makeShortCmsDesc(assetModel.getDescription(locale)!)}</@td>
                        <@td>${uiLabelMap["CmsAssetType_"+raw(asset.assetType!"TEMPLATE")]}</@td>
                    </@tr>
                </#list>
            </@table>
        </@paginate>
    <#else>
        <@commonMsg type="result-norecord"/>
    </#if>
</@section>
