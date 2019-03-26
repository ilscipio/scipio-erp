<#--
This file is subject to the terms and conditions defined in the
files 'LICENSE' and 'NOTICE', which are part of this source
code package.
-->
<#macro menuContent menuArgs={}>
  <@menu args=menuArgs>
    <#if modelEntity??>
      <@menuitem type="link" href=makePageUrl("FindGeneric?entityName=${entityName}&find=true&VIEW_SIZE=${getPropertyValue('webtools', 'webtools.record.paginate.defaultViewSize')!50}&VIEW_INDEX=0") text=uiLabelMap.WebtoolsBackToFindScreen class="+${styles.action_nav!} ${styles.action_cancel!}" />
    <#else>
      <@menuitem type="link" href=makePageUrl("entitymaint") text=uiLabelMap.WebtoolsBackToEntityList class="+${styles.action_nav!} ${styles.action_cancel!}" />
    </#if>
  </@menu>
</#macro>
<@section menuContent=menuContent><#-- title=uiLabelMap.WebtoolsRelations --><#-- SCIPIO: already in title -->
  <#if modelEntity??>
      <#if hasViewPermission>
        <#--<@heading>${uiLabelMap.WebtoolsForEntity}: ${entityName}</@heading>-->

        <@table type="data-list" autoAltRows=true>
          <@thead>
            <@tr class="header-row">
                <@th>${uiLabelMap.WebtoolsTitle}</@th>
                <@th>${uiLabelMap.WebtoolsRelatedEntity}</@th>
                <@th>${uiLabelMap.WebtoolsRelationType}</@th>
                <@th>${uiLabelMap.WebtoolsFKName}</@th>
                <@th>${uiLabelMap.WebtoolsFieldsList}</@th>
            </@tr>
           </@thead>
            <#list (relations![]) as relation>
                <@tr>
                    <@td>${relation.title}</@td>
                    <@td class="button-col"><a href="<@pageUrl>FindGeneric?entityName=${relation.relEntityName}&amp;find=true&amp;VIEW_SIZE=${getPropertyValue("webtools", "webtools.record.paginate.defaultViewSize")!50}&amp;VIEW_INDEX=0</@pageUrl>">${relation.relEntityName}</a></@td>
                    <@td>${relation.type}</@td>
                    <@td>${relation.fkName}</@td>
                    <@td>
                        <#list relation.relFields as field>
                            ${field.fieldName} -> ${field.relFieldName}<br />
                        </#list>
                    </@td>
                </@tr>
            </#list>
        </@table>
    <#else>
        <@commonMsg type="error">${uiLabelMap.WebtoolsEntityCreatePermissionError} ${entityName} ${plainTableName}.</@commonMsg>
    </#if>
  <#else>
    <#--<#if entityName?has_content>
      <@commonMsg type="error">${getLabel('WebtoolsEntityNotFoundSpecified', '', {"entityName":entityName})}.</@commonMsg>
    </#if>-->
  </#if>
</@section>
