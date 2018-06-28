<#include "../common/common.ftl">

<#macro menuContent menuArgs={}>
    <@menu args=menuArgs>
        <@menuitem type="link" href=makeOfbizUrl("editScript") class="+${styles.action_run_sys!} ${styles.action_create!}" text=uiLabelMap.CmsCreateScript/>
        <@menuitem type="link" href=makeOfbizUrl(("Y" == showStandaloneOnly)?string("scripts", "scripts?showStandaloneOnly=Y")) class="+${styles.action_run_sys!} ${styles.action_show!}" 
            text=("Y" == showStandaloneOnly)?string(uiLabelMap.CommonShowAll, uiLabelMap.CmsShowStandaloneOnly)/>
    </@menu>  
</#macro>
<@section title=uiLabelMap.CmsScripts menuContent=menuContent>
    <#if scriptList?has_content>
        <@paginate mode="content" url=makeOfbizUrl(("Y" == showStandaloneOnly)?string("scripts?showStandaloneOnly=Y", "scripts")) viewSize=(viewSize!50) viewIndex=(viewIndex!0) listSize=(listSize!0)>
            <@table type="data-list" autoAltRows=true>
                <@thead>
                    <@tr class="header-row">
                        <@th width="100px">${uiLabelMap.CmsScript}</@th>
                        <@th width="100px">${uiLabelMap.CommonLanguageTitle}</@th>
                        <@th width="50px">${uiLabelMap.CmsStandalone}</@th>
                        <@th>${uiLabelMap.CommonName}</@th>
                        <@th>${uiLabelMap.CommonDescription}</@th>
                    </@tr>
                </@thead>
                <#list scriptList as scriptEntity>
                    <#assign scriptTmpl = cmsScriptUtil.getScriptTemplateFromEntity(scriptEntity)>
                    <@tr>
                        <@td>${scriptTmpl.id!}</@td>
                        <@td>${scriptTmpl.resolvedScriptLang!"invalid"}</@td>
                        <@td>${scriptTmpl.standalone?string("Y", "N")}</@td>
                        <@td><a href="<@ofbizUrl>editScript?scriptTemplateId=${scriptTmpl.id!}</@ofbizUrl>">${scriptTmpl.templateName!}</a></@td>
                        <@td>${makeShortCmsDesc(scriptTmpl.getDescription(locale)!)}</@td>
                    </@tr>
                </#list>
            </@table>
        </@paginate>
    <#else>
        <@commonMsg type="result-norecord"/>
    </#if>
</@section>
