<#--
Scipio Commerce
Copyright (C) Ilscipio GmbH

This file is part of Scipio Commerce. Scipio Commerce is free software: you
can redistribute it and modify it under the terms of the GNU Affero General
Public License, version 3, as published by the Free Software Foundation.
Scipio Commerce is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License
for more details. You should have received a copy of the license with this
work (file LICENSE). If not, see <https://www.gnu.org/licenses/agpl-3.0.html>.
A commercial license is available from Ilscipio GmbH.

SPDX-License-Identifier: AGPL-3.0-only
-->
<@script>
    function runDemoDataService(serviceName) {
        if (serviceName) {
            $("input[name='SERVICE_NAME']").val(serviceName);
        }
        document.forms.runDemoDataService.submit();
    }
</@script>

<@section title=uiLabelMap.WebtoolsDemoDataGeneratorServiceList>
     <form name="runDemoDataService" id="runDemoDataService" action="<@pageUrl>RunDemoDataGeneratorService</@pageUrl>" method="POST">
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