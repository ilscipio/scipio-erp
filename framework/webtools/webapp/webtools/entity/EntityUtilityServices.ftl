<#macro menuContent menuArgs={}>
  <@menu args=menuArgs>
    <@menuitem type="link" href=makeOfbizUrl("SolrServices") text=uiLabelMap.SolrSolrServices class="+${styles.action_nav!}" />
  </@menu>
</#macro>
<@section menuContent=menuContent>

  <@section title=getLabel("WorkEffortGeneral", "WorkEffortUiLabels")>     
      <@table type="data-list" autoAltRows=true scrollable=true> 
        <@thead>
        <@tr class="header-row">
          <@th id="Service_all">${uiLabelMap.WebtoolsServiceName}</@th>
          <#--<@th>${uiLabelMap.WebtoolsEngineName}</@th>-->
          <#--<@th>${uiLabelMap.WebtoolsDefaultEntityName}</@th>-->
          <@th>${uiLabelMap.WebtoolsInvoke}</@th>
          <#--<@th>${uiLabelMap.WebtoolsLocation}</@th>-->
          <#--@th>${uiLabelMap.WebtoolsDefinitionLocation}</@th-->
        </@tr>
        </@thead>

          <@tr>
            <@td><a href="<@ofbizUrl>scheduleServiceSync?SERVICE_NAME=reloadVisualThemeResources&POOL_NAME=pool&_RUN_SYNC_=Y&visualThemeId=</@ofbizUrl>">Visual Theme Resources - Reload All, Now</a></@td>
            <#--<@td>${service.engineName}</@td>-->
            <#--<@td>${service.defaultEntityName}</@td>-->
            <@td>reloadVisualThemeResources</@td>
            <#--<@td>${service.location}</@td>-->
            <#--@td><a href="<@ofbizUrl>${url!}?constraint=definitionLocation@${service.definitionLocation}</@ofbizUrl>">${service.definitionLocation}</a></@td-->
          </@tr>

          <@tr>
            <@td><a href="<@ofbizUrl>setSyncServiceParameters?SERVICE_NAME=reloadVisualThemeResources&POOL_NAME=pool&_RUN_SYNC_=Y</@ofbizUrl>">Visual Theme Resources - Reload, Configurable</a></@td>
            <#--<@td>${service.engineName}</@td>-->
            <#--<@td>${service.defaultEntityName}</@td>-->
            <@td>reloadVisualThemeResources</@td>
            <#--<@td>${service.location}</@td>-->
            <#--@td><a href="<@ofbizUrl>${url!}?constraint=definitionLocation@${service.definitionLocation}</@ofbizUrl>">${service.definitionLocation}</a></@td-->
          </@tr>

      </@table>
      
  </@section>
 
</@section>

