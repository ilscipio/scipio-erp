<#assign sectionTitle = uiLabelMap.WebtoolsDemoDataGeneratorResults + " [" + parameters.SERVICE_NAME! + "]">
<@section title=sectionTitle>
    <#if generatedDataStats?has_content>
        <@table type="data-list" autoAltRows=true scrollable=true fixedColumnsLeft=1> <#-- orig: class="basic-table" --> <#-- orig: cellspacing="0" -->
            <@thead>
                <@tr class="header-row-2">
                    <@td>${uiLabelMap.CommonIteration}</@td>
                    <#assign maxSize = 0>                    
                    <#list generatedDataStats as data>
                        <#if data.size() &gt; maxSize>
                            <#assign maxSize = data.size()>
                            <#assign stat = data>
                        </#if>
                    </#list>
                    <#if stat?has_content>
                        <#list stat.keySet() as s>
                            <@th>${s!"NoEntityName"}</@th>
                        </#list>
                    </#if>
                </@tr>
            </@thead>
            <#list generatedDataStats as stat>                
                <@tr>
                    <@td>#${stat_index + 1}</@td>
                    <#list 0..maxSize - 1 as i>
                        <#if stat.keySet().size() &gt; i>
                            <#assign statKeys = stat.keySet().toArray()>               
                            <@td>
                                Stored: ${stat[statKeys[i]].stored}<br/>
                                Failed: ${stat[statKeys[i]].failed} 
                            </@td>
                        <#else>
                            <@td>
                                Stored: 0<br/>
                                Failed: 0 
                            </@td>                  
                        </#if>
                    </#list> 
                </@tr>
            </#list>
            <#-- TODO: total records added per entity -->
            <@tfoot>
                <@tr>
                    <@td>${uiLabelMap.CommonTotal}</@td>
                    <#list 0..maxSize - 1 as i>
                        <@td>0</@td>
                    </#list>
                </@tr>
            </@tfoot>
        </@table>
        
    <#else>
       <@panel><@commonMsg type="result-norecord">${uiLabelMap.WebtoolsDemoDataGeneratorNoRecordsGenerated}.</@commonMsg></@panel>
    </#if>
</@section>