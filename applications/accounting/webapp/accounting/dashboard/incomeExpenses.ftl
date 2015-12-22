<#assign chartType=chartType!"bar"/>    <#-- (line|bar|pie) default: line -->
<#assign chartValue=chartValue!"total"/> <#-- (total|count) default: total -->
<#assign library=chartLibrary!"foundation"/>


<#if totalMap?has_content>
	<#list totalMap?keys as key>		
		<#-- <#assign currData = totalMap[key] /> -->
				
		<@heading relLevel=1>
			<#if title?has_content>${title!} - </#if>
			${uiLabelMap.AccountingIncomesExpenses}			
		</@heading>			
		<#-- <#if currData?has_content>
			<@chart type=chartType library=library>
		    	<#list currData.keys as type>
		    		${Static["org.ofbiz.base.util.Debug"].log("key =========> " + key  + "type ==========> " + type + "   currData ==========> " + currData[type])}
		      		<#if chartType=="line">
		        		<@chartdata value="${(currData[type])!0}" value2="${(currData[type].pos)!0}" title="${type}"/>
		      		<#else>
		        		<@chartdata value="${(currData[type])!0}" title="${type}"/>
		      		</#if>
		    	</#list>  
		  	</@chart>
		<#else>
		  	<@resultMsg>${uiLabelMap.CommonNoRecordFound}.</@resultMsg>
		</#if> -->
	</#list>
</#if>