<#assign chartType=chartType!"bar"/>    <#-- (line|bar|pie) default: line -->
<#assign library=chartLibrary!"chart"/>
<#assign datasets=chartDatasets?number!1 />

<#if totalMap?has_content>
    <@heading relLevel=1>
        <#if title?has_content>${title!} - </#if>
        ${uiLabelMap.AccountingIncomesExpenses}            
    </@heading>            
    
    <#if chartType == "line" || chartType == "bar">
        <@chart type=chartType library=library xlabel=xlabel!"" ylabel=ylabel!"" label1=label1!"" label2=label2!"">
            <#list totalMap.keySet() as key>        
                <#assign currData = totalMap[key] />
                <#if currData?has_content>
                    <#if datasets == 1>
                        <@chartdata value="${currData['income']!0}"  title="${key!}"/>
                    <#elseif datasets == 2>                    
                        <@chartdata value="${currData['income']!0}" value2="${currData['expense']!0}" title="${key!}"/>
                    </#if>
                </#if>
            </#list>
        </@chart>
    <#elseif chartType == "pie">
        <@commonMsg type="error">${uiLabelMap.CommonUnsupported}</@commonMsg>
    <#else>
        <@commonMsg type="error">${uiLabelMap.CommonUnsupported}</@commonMsg>
    </#if>
<#else>
    <@commonMsg type="result-norecord"/>            
</#if>