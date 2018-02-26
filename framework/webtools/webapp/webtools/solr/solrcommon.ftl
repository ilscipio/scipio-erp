<#include "component://webtools/webapp/webtools/service/servicecommon.ftl">

<#macro solrServiceForm>
  <form name="${SERVICE_NAME}SchedForm" method="post" action="<@ofbizUrl>${runServiceTarget!"runSolrService"}</@ofbizUrl>">
    <#nested>
  </form>
</#macro>

<#macro solrServiceFields params=true initParams={} exclude={}>
  <#if params?is_boolean>
    <#if params>
      <#local params = {"POOL_NAME":POOL_NAME} + initParams>
      <#if SERVICE_NAME == (parameters.SERVICE_NAME!)>
        <#local params = parameters>
      </#if>
    <#else>
      <#local params = {}>
    </#if>
  </#if>

      <input type="hidden" name="_SOLR_SRV_RUN_" value="Y"/><#-- for event message handling, etc. -->

    <#list scheduleOptions as scheduleOption>
      <input type="hidden" name="${scheduleOption.name}" value="${scheduleOption.value}"/>
    </#list>
    
  <@fields fieldArgs={"labelColumns":4}>
    <@serviceInitFields serviceName=SERVICE_NAME srvInput=false params=params/>
  </@fields>
  
    <hr/>
    
  <#-- SCIPIO: leave room for the label area because service parameter names can be long -->
  <@fields fieldArgs={"labelColumns":4}>
    <@serviceFields serviceParameters=(serviceParameters!) params=params exclude=exclude/>
  </@fields>

    <@field type="submit" text=uiLabelMap.PageTitleRunService class="${styles.link_run_sys!} ${styles.action_begin!}" />

</#macro>
