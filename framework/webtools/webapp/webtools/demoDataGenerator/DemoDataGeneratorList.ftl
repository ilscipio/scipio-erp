<@section title=uiLabelMap.WebtoolsDemoDataGeneratorServiceList>     
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
                <@td><a href="<@ofbizUrl>RunDemoDataGeneratorService?SERVICE_NAME=${service.serviceName}</@ofbizUrl>">${service.serviceName}</a></@td>
                <@td>${service.engineName}</@td>
                <@td>${service.location}</@td>                
              </@tr>            
          </#if>
        </#list>
      </@table>
</@section>