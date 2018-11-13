<#--
This file is subject to the terms and conditions defined in the
files 'LICENSE' and 'NOTICE', which are part of this source
code package.
-->
<#include "component://webtools/webapp/webtools/service/servicecommon.ftl">

<form name="scheduleForm" method="post" action="<@ofbizUrl>scheduleService</@ofbizUrl>">

    <#list scheduleOptions as scheduleOption>
      <input type="hidden" name="${scheduleOption.name}" value="${scheduleOption.value}"/>
    </#list>

  <#-- SCIPIO: leave room for the label area because service parameter names can be long -->
  <@fields fieldArgs={"labelColumns":4}>
    <@serviceFields serviceParameters=(serviceParameters!)/>
  </@fields>

    <@field type="submit" text=uiLabelMap.CommonSubmit class="${styles.link_run_sys!} ${styles.action_begin!}" />

</form>