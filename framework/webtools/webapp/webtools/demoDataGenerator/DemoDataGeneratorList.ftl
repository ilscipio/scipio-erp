<@section title="${uiLabelMap.ListDemoDataGeneratorServices}">     
      <@table type="data-list" autoAltRows=true scrollable=true> 
        <@thead>
        <@tr class="header-row">
          <@th id="Service_all">${uiLabelMap.WebtoolsServiceName}</@th>
          <@th>${uiLabelMap.WebtoolsEngineName}</@th>
          <@th>${uiLabelMap.WebtoolsDefaultEntityName}</@th>
          <@th>${uiLabelMap.WebtoolsInvoke}</@th>
          <@th>${uiLabelMap.WebtoolsLocation}</@th>
          <#--@th>${uiLabelMap.WebtoolsDefinitionLocation}</@th-->
        </@tr>
        </@thead>
        
        <#list servicesList as service>
          <#if service.serviceName?has_content>
             
              <@tr>
                <@td><a href="<@ofbizUrl>RunDemoDataGeneratorService?SERVICE_NAME=${service.serviceName}</@ofbizUrl>">${service.serviceName}</a></@td>
                <@td>${service.engineName}</@td>
                <@td>${service.defaultEntityName}</@td>
                <@td>${service.invoke}</@td>
                <@td>${service.location}</@td>
                <#--@td><a href="<@ofbizUrl>${url!}?constraint=definitionLocation@${service.definitionLocation}</@ofbizUrl>">${service.definitionLocation}</a></@td-->
              </@tr>
            
          </#if>
        </#list>
      </@table>
</@section>