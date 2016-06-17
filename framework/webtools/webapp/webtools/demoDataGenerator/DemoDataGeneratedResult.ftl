<#if generatedDataStats?has_content>
  
    <@paginate mode="content" url=makeOfbizUrl("FindGeneric") paramStr=paramStr viewSize=viewSize!1 viewIndex=viewIndex!0 listSize=arraySize!0>
        <@table type="data-list" autoAltRows=true scrollable=true fixedColumnsLeft=1 fixedColumnsRight=1> <#-- orig: class="basic-table" --> <#-- orig: cellspacing="0" -->
            <@thead>
                <@tr class="header-row-2">
                    <@th>${uiLabelMap.CommonEntityName}</@th>          
                    <@th>${uiLabelMap.CommonFieldsStored}</@th>          
                    <@th>${uiLabelMap.CommonFieldsFailed}</@th>
                </@tr>
            </@thead>
            <#list generatedDataStats as stat>
                ${Static["org.ofbiz.base.util.Debug"].log("stat ====> " + stat)}
                <@tr>
                    <@td>
                        <@table type="data-list">
                            <#list stat.keySet() as s>
                                <@tr>
                                    <@td>${stat[s].entityName}</@td>
                                    <@td>${stat[s].stored}</@td>
                                    <@td>${stat[s].failed}</@td>                    
                                </@tr>
                            </#list>
                        </@table>
                    </@td>
                </@tr>
            </#list>
        </@table>
    </@paginate>
<#else>
   <@panel><@commonMsg type="result-norecord">${uiLabelMap.WebtoolsNoEntityRecordsFound}.</@commonMsg></@panel>
</#if>