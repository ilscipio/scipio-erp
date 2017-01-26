<#assign chartType=chartType!"bar"/>    <#-- (line|bar|pie) default: line -->
<#assign library=chartLibrary!"chart"/>
<#assign datasets=chartDatasets?number!1 />
<#assign totalMap=rewrapMap(totalMap!{}, "raw-simple")>

<#if totalMap?has_content> 
    <#if chartType == "line" || chartType == "bar">
        <#-- FIXME: I don't know how to pass currency symbol, Freemarker doesn't let me get the symbol as is, either it transforms to unicode (ie: dollar) or to html entity (ie: euro)  -->
        <@chart type=chartType library=library xlabel=(xlabel!"") ylabel=(ylabel!"") label1=(label1!"") label2=(label2!"") labelUom1=(currencyUomId!"") labelUom2=(currencyUomId!"")>
            <#list mapKeys(totalMap) as key>        
                <#assign currData = totalMap[key] />
                <#if currData?has_content>
                    <#if datasets == 1>
                        <@chartdata value=(currData['income']!0) title=key/>
                    <#elseif datasets == 2>                    
                        <@chartdata value=(currData['income']!0) value2=(currData['expense']!0) title=key/>
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
