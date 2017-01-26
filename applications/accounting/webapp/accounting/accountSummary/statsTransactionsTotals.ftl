<#assign chartType=chartType!"bar"/>    <#-- (line|bar|pie) default: line -->
<#assign chartValue=chartValue!"total"/> <#-- (total|count) default: total -->
<#assign library=chartLibrary!"foundation"/>
<#assign chartDataMap=rewrapMap({
    "C": rewrapMap(creditStats, "raw-simple"), 
    "D": rewrapMap(debitStats, "raw-simple")
}, "raw-simple")/>

<@row>
    <#list mapKeys(chartDataMap) as key>        
        <#assign currData = chartDataMap[key] />
        <@cell columns=6>
            <@heading relLevel=1>
                <#if title?has_content>${title} - </#if>
                <#if key == "C">${uiLabelMap.AccountingCreditAmount}<#elseif key == "D">${uiLabelMap.AccountingDebitAmount}</#if>
                [${fromDate!?string("MM/dd/yyyy")} - ${thruDate!?string("MM/dd/yyyy")}]
            </@heading>            
            <#if currData?has_content>
              <@chart type=chartType library=library xlabel=(xlabel!"") ylabel=(ylabel!"") label1=(label1!"")>
                <#list mapKeys(currData) as key>
                <#if chartType=="line">
                    <@chartdata value=((currData[key][chartValue])!0) value2=((currData[key].pos)!0) title=key/>
                <#else>
                    <@chartdata value=((currData[key][chartValue])!0) title=key/>
                </#if>
                </#list>  
              </@chart>
            <#else>
              <@commonMsg type="result-norecord"/>
            </#if>
        </@cell>
    </#list>
</@row>
        