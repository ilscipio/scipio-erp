<#assign chartType=chartType!"pie"/>    <#-- (line|bar|pie) default: line -->
<#assign library=chartLibrary!"chart"/>
<#assign datasets=chartDatasets?number!1 />

<#if memoryInfo?has_content> 
    <#if chartType == "pie" || chartType == "bar">
        <@chart type=chartType library=library xlabel=xlabel!"" ylabel=ylabel!"" label1=label1!"" label2=label2!"" labelUom1="MB">
            <#list memoryInfo.keySet() as key>        
                <#assign currData = memoryInfo[key] />
                <#if currData?has_content>
                    <#if datasets == 1>
                        <@chartdata value="${currData!0}"  title="${key!}"/>
                    </#if>                
                </#if>
            </#list>
        </@chart>
    <#elseif chartType == "line">
        <@commonMsg type="error">${uiLabelMap.CommonUnsupported}</@commonMsg>
    <#else>
        <@commonMsg type="error">${uiLabelMap.CommonUnsupported}</@commonMsg>
    </#if>
<#else>
    <@commonMsg type="result-norecord"/>            
</#if>