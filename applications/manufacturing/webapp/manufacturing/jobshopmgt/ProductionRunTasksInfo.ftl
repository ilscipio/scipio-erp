<#--
This file is subject to the terms and conditions defined in the
files 'LICENSE' and 'NOTICE', which are part of this source
code package.
-->

<#if taskInfos?has_content>
<#list taskInfos as taskInfo>
  <#assign task = taskInfo.task>
  <#assign taskForm = taskInfo.taskForm>
  <@section title="${rawString(task.workEffortName!)} [${rawString(task.workEffortId)}]">
    ${taskForm.renderFormString(context)}
  </@section>
</#list>
</#if>