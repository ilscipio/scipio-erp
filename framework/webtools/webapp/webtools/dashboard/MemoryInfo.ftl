<#assign chartType=chartType!"pie"/>    <#-- (line|bar|pie) default: line -->
<#assign library=chartLibrary!"chart"/>
<#assign datasets=(chartDatasets!1)?number />

<#if memoryInfo?has_content>
    <@row><@cell columns=12>${uiLabelMap.WebtoolsMaxMemory}: ${maxMemoryMB} MB</@cell></@row>
    <@row><@cell columns=12>${uiLabelMap.WebtoolsTotalMemory}: ${totalMemoryMB} MB</@cell></@row>    
    <@row>
        <@cell columns=12>
            <#if chartType == "pie" || chartType == "bar">
                <@chart type=chartType library=library xlabel=(xlabel!"") ylabel=(ylabel!"") label1=(label1!"") label2=(label2!"") labelUom1="MB">
                    <#list mapKeys(memoryInfo) as key>        
                        <#assign currData = memoryInfo[key] />
                        <#if currData?has_content>
                            <#if datasets == 1>
                                <@chartdata value=(currData!0) title=key/>
                            </#if>                
                        </#if>
                    </#list>
                </@chart>
            <#elseif chartType == "line">
                <@commonMsg type="error">${uiLabelMap.CommonUnsupported}</@commonMsg>
            <#else>
                <@commonMsg type="error">${uiLabelMap.CommonUnsupported}</@commonMsg>
            </#if>
        </@cell>
    </@row>
<#else>
    <@commonMsg type="result-norecord"/>            
</#if>