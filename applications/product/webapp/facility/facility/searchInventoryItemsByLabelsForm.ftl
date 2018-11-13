<#--
This file is subject to the terms and conditions defined in the
files 'LICENSE' and 'NOTICE', which are part of this source
code package.
-->

<form method="post" action="<@ofbizUrl>SearchInventoryItemsByLabels</@ofbizUrl>">
  <input type="hidden" name="facilityId" value="${facility.facilityId}"/>
  <@table type="data-list">
  <#assign index = 0>
  <#list labelTypes as labelType>
    <#assign index = index + 1>
    <#assign labels = labelType.getRelated("InventoryItemLabel", null, Static["org.ofbiz.base.util.UtilMisc"].toList("inventoryItemLabelId"), false)>
    <@tr>
      <@td>
          <span>${labelType.description!} [${labelType.inventoryItemLabelTypeId}]</span>
          &nbsp;
          <select name="inventoryItemLabelId_${index}">
            <option></option>
            <#list labels as label>
            <option value="${label.inventoryItemLabelId}" <#if parameters["inventoryItemLabelId_" + index]?has_content && parameters["inventoryItemLabelId_" + index] == label.inventoryItemLabelId>selected="selected"</#if>>${label.description!} [${label.inventoryItemLabelId}]</option>
            </#list>
          </select>
      </@td>
    </@tr>
  </#list>
  <@tfoot>
  <@tr>
    <@td>
      <input type="submit" value="${uiLabelMap.CommonSubmit}" class="${styles.link_run_sys!} ${styles.action_find!}"/>
    </@td>
  </@tr>
  </@tfoot>
  </@table>
  <input type="hidden" name="numberOfFields" value="${index}"/>
</form>
