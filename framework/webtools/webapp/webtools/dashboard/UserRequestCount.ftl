<#assign chartType=chartType!"bar"/>    <#-- (line|bar|pie) default: line -->
<#assign library=chartLibrary!"chart"/>
<#assign datasets=chartDatasets?number!1 />

<#if userRequestCount?has_content> 
    <#if chartType == "line" || chartType == "bar">
        <@chart type=chartType library=library xlabel=(xlabel!"") ylabel=(ylabel!"") label1=(label1!"") label2=(label2!"")>
            <#list mapKeys(userRequestCount) as key>        
                <#assign currData = userRequestCount[key] />
                <#if currData?has_content>
                    <#if datasets == 1>
                        <@chartdata value="${currData!0}" title=key/>
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