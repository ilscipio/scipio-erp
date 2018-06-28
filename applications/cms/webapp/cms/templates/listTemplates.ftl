<#include "../common/common.ftl">

<#macro menuContent menuArgs={}>
    <@menu args=menuArgs>
        <@menuitem type="link" href=makeOfbizUrl("editTemplate") class="+${styles.action_run_sys!} ${styles.action_create!}" text=uiLabelMap.CmsCreateTemplate/>
    </@menu>  
</#macro>
<@section title=uiLabelMap.CmsTemplates menuContent=menuContent>
    <#if templateList?has_content>
        <@paginate mode="content" url=makeOfbizUrl("templates") viewSize=(viewSize!50) viewIndex=(viewIndex!0) listSize=(listSize!0)>
            <@table type="data-list" autoAltRows=true>
                <@thead>
                    <@tr class="header-row">
                        <@th width="100px">${uiLabelMap.CmsTemplate}</@th>
                        <@th width="200px">${uiLabelMap.CmsWebSite}</@th>
                        <@th>${uiLabelMap.CmsTemplateName}</@th>
                        <@th>${uiLabelMap.CommonDescription}</@th>
                    </@tr>
                </@thead>
                <#list templateList as template>
                    <#assign templateModel = Static["com.ilscipio.scipio.cms.template.CmsPageTemplate"].getWorker().makeFromValue(template)>
                    <@tr>
                        <@td>${templateModel.id}</@td>
                        <@td>${templateModel.webSiteId!}</@td>
                        <@td><a href="<@ofbizUrl>editTemplate?pageTemplateId=${templateModel.id}</@ofbizUrl>">${templateModel.name!}</a></@td>
                        <@td>${makeShortCmsDesc(templateModel.getDescription(locale)!)}</@td>
                    </@tr>
                </#list>
            </@table>
        </@paginate>
    <#else>
        <@commonMsg type="result-norecord"/>
    </#if>
</@section>