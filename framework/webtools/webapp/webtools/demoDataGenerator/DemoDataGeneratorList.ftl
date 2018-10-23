<@script>
	function runDemoDataService(serviceName) {
		if (serviceName) {
			$("input[name='SERVICE_NAME']").val(serviceName);
		}
		document.forms.runDemoDataService.submit();
	}
</@script>

<@section title=uiLabelMap.WebtoolsDemoDataGeneratorServiceList>
	 <form name="runDemoDataService" id="runDemoDataService" action="<@ofbizUrl>RunDemoDataGeneratorService</@ofbizUrl>" method="POST">
	 	 <input type="hidden" name="SERVICE_NAME" value="" />		
	               
	      <@table type="data-list" autoAltRows=true scrollable=true> 
	        <@thead>
	        <@tr class="header-row">
	          <@th id="Service_all">${uiLabelMap.WebtoolsServiceName}</@th>
	          <@th>${uiLabelMap.WebtoolsEngineName}</@th>          
	          <@th>${uiLabelMap.WebtoolsLocation}</@th>          
	        </@tr>
	        </@thead>
	        
	        <#list servicesList as service>
	          <#if service.serviceName?has_content>
	              <@tr>
	                <@td>	                	
	                	<a href="javascript:runDemoDataService('${service.serviceName}')">${service.serviceName}</a>
	                </@td>
	                <@td>${service.engineName}</@td>
	                <@td>${service.location}</@td>                
	              </@tr>            
	          </#if>
	        </#list>
	      </@table>
	</form>	      
</@section>