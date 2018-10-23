<#include "../common/common.ftl">

<#macro menuContent menuArgs={}>
    <@menu args=menuArgs>
        <@menuitem type="link" href=makeOfbizUrl("editAsset") class="+${styles.action_run_sys!} ${styles.action_create!}" text=uiLabelMap.CmsCreateAsset/>
    </@menu>  
</#macro>
<@section title=uiLabelMap.CmsAssets menuContent=menuContent>
    <#if assetList?has_content>
        <@paginate mode="content" url=makeOfbizUrl("assets") viewSize=(viewSize!50) viewIndex=(viewIndex!0) listSize=(listSize!0)>
            <@table type="data-list" autoAltRows=true>
                <@thead>
                    <@tr class="header-row">
                        <@th width="100px">${uiLabelMap.CmsAsset}</@th>
                        <@th width="200px">${uiLabelMap.CmsWebSite}</@th>
                        <@th>${uiLabelMap.CmsTemplateName}</@th>
                        <@th>${uiLabelMap.ContentType}</@th>
                        <@th>${uiLabelMap.CommonDescription}</@th>
                    </@tr>
                </@thead>
                <#list assetList as asset>
                    <#assign assetModel = Static["com.ilscipio.scipio.cms.template.CmsAssetTemplate"].getWorker().makeFromValue(asset)>
                    <@tr>
                        <@td>${assetModel.id}</@td>
                        <@td>${assetModel.webSiteId!}</@td>
                        <@td><a href="<@ofbizUrl>editAsset?assetTemplateId=${assetModel.id}</@ofbizUrl>">${assetModel.name!}</a></@td>
                        <@td><#if asset.contentTypeId?has_content>${(delegator.findOne("ContentType", {"contentTypeId":asset.contentTypeId}, true).get("description", locale))!asset.contentTypeId}</#if></@td>
                        <@td>${makeShortCmsDesc(assetModel.getDescription(locale)!)}</@td>
                    </@tr>
                </#list>
            </@table>
        </@paginate>
    <#else>
        <@commonMsg type="result-norecord"/>
    </#if>
</@section>
