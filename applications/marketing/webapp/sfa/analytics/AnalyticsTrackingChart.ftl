<#assign chartType=chartType!"bar"/>    <#-- (line|bar|pie) default: line -->
<#assign library=chartLibrary!"chart"/>
<#assign datasets=(chartDatasets!1)?number />
<#if result??>
  <#assign result=rewrapMap(result, "raw-simple")>
</#if>

<#if result?has_content>
    <#if chartType == "line" || chartType == "bar">        
        <@chart type=chartType title=chartTitle library=library xlabel=(xlabel!"") ylabel=(ylabel!"") label1=(label1!"") label2=(label2!"")>
            <#list mapKeys(result) as key>                
                <#assign currData = result[key] />                
                <#if currData?has_content> 
                    <#if datasets == 1>                         
                        <@chartdata value=(currData.totalVisits!0) title=key/>
                    <#elseif datasets == 2>
                        <@chartdata value=(currData.totalVisits!0) value2=(currData.totalOrders!0)  title=key/>                    
                    </#if>
                </#if>
            </#list>
        </@chart>
    <#elseif chartType == "pie">
        <@commonMsg type="error">${uiLabelMap.CommonUnsupported}</@commonMsg>
    <#else>
        <@commonMsg type="error">${uiLabelMap.CommonUnsupported}</@commonMsg>
    </#if>
<#elseif result??>
    <@commonMsg type="result-norecord"/>            
</#if>