<#if generatedData?has_content>
  
  <@paginate mode="content" url=makeOfbizUrl("FindGeneric") paramStr=paramStr viewSize=viewSize!1 viewIndex=viewIndex!0 listSize=arraySize!0>
    <@table type="data-list" autoAltRows=true scrollable=true fixedColumnsLeft=1 fixedColumnsRight=1> <#-- orig: class="basic-table" --> <#-- orig: cellspacing="0" -->
      <#--@thead>
        <@tr class="header-row-2">
          <@th>&nbsp;</@th>
          <#list fieldList as field>
            <@th>${field.name}</@th>
          </#list>
          <@th>&nbsp;</@th>
        </@tr>
      </@thead-->
       
            <#list generatedData as record>
                <@tr>
                    <@td>
                        <a href="<@ofbizUrl>ViewGeneric?${record.findString}</@ofbizUrl>">${uiLabelMap.CommonView}</a>
                    </@td>
                    <#list fieldList as field>
                        <@td>${record.fields.get(field.name)!?string}</@td>
                    </#list>
                    <@td>
                    <#if hasDeletePermission == 'Y'>
                       <a href="<@ofbizUrl>UpdateGeneric?${record.findString}&amp;UPDATE_MODE=DELETE</@ofbizUrl>">${uiLabelMap.CommonDelete}</a>
                    </#if>
                    </@td>
                </@tr>
            </#list>
        
    </@table>
  </@paginate>
<#else>
   <@panel><@commonMsg type="result-norecord">${uiLabelMap.WebtoolsNoEntityRecordsFound} ${entityName}.</@commonMsg></@panel>
</#if>