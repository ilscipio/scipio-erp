
<#macro menuContent menuArgs={}>
    <@menu args=menuArgs>
        <@menuitem type="link" href=makeOfbizUrl("scheduleJob?SERVICE_NAME=rebuildSolrIndex") text="${rawLabel('WebtoolsScheduleJob')}: rebuildSolrIndex" class="+${styles.action_nav!} ${styles.action_begin!}"/>
        <@menuitem type="link" href=makeOfbizUrl("scheduleJob?SERVICE_NAME=rebuildSolrIndexAuto") text="${rawLabel('WebtoolsScheduleJob')}: rebuildSolrIndexAuto" class="+${styles.action_nav!} ${styles.action_begin!}"/>
        <@menuitem type="link" href=makeOfbizUrl("FindJob?noConditionFind=Y&serviceName_op=like&serviceName=rebuildSolrIndex"+"%"?url) text=uiLabelMap.PageTitleFindJob class="+${styles.action_nav!} ${styles.action_find!}"/>
    </@menu>
</#macro>
<@section title="rebuildSolrIndex" class="scpadmservlist-service" menuContent=menuContent>

  <@render resource=rebuildIndexCoreWidgetLoc/>

</@section>