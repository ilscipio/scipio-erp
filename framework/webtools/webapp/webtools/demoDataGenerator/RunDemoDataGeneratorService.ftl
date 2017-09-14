<#include "component://webtools/webapp/webtools/service/servicecommon.ftl">

<form name="demoDataGeneratorForm" method="post" action="<@ofbizUrl>DemoDataGeneratorResult?_RUN_SYNC_=Y</@ofbizUrl>">

      <#-- SCIPIO: leave room for the label area because service parameter names can be long -->
      <@fields fieldArgs={"labelColumns":4}>
        <@serviceFields serviceParameters=(serviceParameters!)/>
      </@fields>
      
      <#assign serviceParameterNames = serviceParameterNames![]><#-- 2017-09-13: this is now set by ScheduleJob.groovy -->
      <#list scheduleOptions as scheduleOption>
         <#if !serviceParameterNames?seq_contains(scheduleOption.name)>
            <input type="hidden" name="${scheduleOption.name}" value="${scheduleOption.value}"/>
         </#if>
      </#list>

      <@field type="submit" text=uiLabelMap.CommonSubmit class="${styles.link_run_sys!} ${styles.action_begin!}" />
</form>