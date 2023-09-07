<#--
This file is subject to the terms and conditions defined in the
files 'LICENSE' and 'NOTICE', which are part of this source
code package.
-->

<#if taskCosts?has_content>
<#list taskCosts as taskCost>
  <#assign task = taskCost.task!>
  <@section title="${rawLabel('ManufacturingActualCosts')} ${raw(task.workEffortName!)} [${raw(task.workEffortId!)}]">
      <#assign costsForm = taskCost.costsForm>
      ${costsForm.renderFormString(context)}
  </@section>
</#list>
</#if>
