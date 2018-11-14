<#--
This file is subject to the terms and conditions defined in the
files 'LICENSE' and 'NOTICE', which are part of this source
code package.
-->
<#assign selected = activeScheduleSubMenuItem!"void">

<#if facilityId?has_content>
  <@menu type="tab">
    <@menuitem type="link" href=makeOfbizUrl("ScheduleShipmentRouteSegment?facilityId=${facilityId}") text=uiLabelMap.ProductSchedule selected=(selected=="ScheduleTabButton") class="+${styles.action_nav!} ${styles.action_configure!}" />
    <@menuitem type="link" href=makeOfbizUrl("Labels?facilityId=${facilityId}") text=uiLabelMap.ProductLabels selected=(selected=="LabelsTabButton") class="+${styles.action_nav!}" />
  </@menu> 
</#if>
