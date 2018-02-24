<#-- shared between Status & Services pages (special-case service) -->
<#include "component://webtools/webapp/webtools/service/servicecommon.ftl">

  <p class="scpadmservlist-servdesc">${uiLabelMap.SolrRebuildIndexServiceDesc}</p>

<#assign servParamsMap = {"POOL_NAME":POOL_NAME}>
<#if SERVICE_NAME == (parameters.SERVICE_NAME!)>
  <#assign servParamsMap = parameters>
</#if>

<form name="rebuildSolrIndexScheduleForm" method="post" action="<@ofbizUrl>${runServiceTarget!"runSolrService"}</@ofbizUrl>">
      <input type="hidden" name="_SOLR_SRV_RUN_" value="Y"/><#-- for event message handling, etc. -->

    <#list scheduleOptions as scheduleOption>
      <input type="hidden" name="${scheduleOption.name}" value="${scheduleOption.value}"/>
    </#list>
    
  <@fields fieldArgs={"labelColumns":4}>
    <@serviceInitFields serviceName=SERVICE_NAME srvInput=false params=servParamsMap/>
  </@fields>
  
    <hr/>
    
  <#-- SCIPIO: leave room for the label area because service parameter names can be long -->
  <@fields fieldArgs={"labelColumns":4}>
    <@serviceFields serviceParameters=(serviceParameters!) params=servParamsMap/>
  </@fields>

    <@field type="submit" text=uiLabelMap.PageTitleRunService class="${styles.link_run_sys!} ${styles.action_begin!}" />

</form>
  
  <p class="scpadmservlist-servtip">
    ${uiLabelMap.CommonNote}: ${uiLabelMap.SolrRebuildIndexStartupTip}:
      <code>./ant start-reindex-solr</code> || <code>./ant start-debug-reindex-solr</code> (Linux),
      <code>ant start-reindex-solr</code> || <code>ant start-debug-reindex-solr</code> (Windows)
  </p>