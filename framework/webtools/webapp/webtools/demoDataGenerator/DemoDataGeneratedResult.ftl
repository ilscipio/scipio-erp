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
<#assign sectionTitle = rawLabel('WebtoolsDemoDataGeneratorResults') + " - " + raw(parameters.SERVICE_NAME!)>
<@section title=sectionTitle>
    <#if generatedDataStats?has_content>
        <@table type="data-list" autoAltRows=true scrollable=true fixedColumnsLeft=1>
            <@thead>
                <@tr class="header-row-2">
                    <@td>${uiLabelMap.CommonIteration}</@td>
                    <#assign maxSize = 0>                    
                    <#list generatedDataStats as data>
                        <#if data.size() &gt; maxSize>
                            <#assign maxSize = data.size()>
                            <#assign stat = data>
                        </#if>
                    </#list>
                    <#if stat?has_content>
                        <#list stat.keySet() as s>
                            <@th>${s!uiLabelMap.WebtoolsNoEntityNameFound}</@th>
                        </#list>
                    </#if>
                </@tr>
            </@thead>
            
            <#assign totalStored = {}>
            <#assign totalFailed = {}>
            <#list generatedDataStats as stat>                
                <@tr>
                    <@td>#${stat_index + 1}</@td>
                    <#list 0..maxSize - 1 as i>
                        <#if stat.keySet().size() &gt; i>
                            <#assign statKeys = stat.keySet().toArray()>                            
                            <#if totalStored[statKeys[i]]?has_content>
                                <#assign totalStored = totalStored + {statKeys[i] : stat[statKeys[i]].stored + totalStored[statKeys[i]]}>
                            <#else>
                                <#assign totalStored = totalStored + {statKeys[i] : stat[statKeys[i]].stored}>
                            </#if>    
                                
                             <#if totalFailed[statKeys[i]]?has_content>
                                <#assign totalFailed = totalFailed + {statKeys[i] : stat[statKeys[i]].failed + totalFailed[statKeys[i]]}>
                            <#else>
                                <#assign totalFailed = totalFailed + {statKeys[i] : stat[statKeys[i]].failed}>
                            </#if>    
                            <@td>                                 
                                <@modal id="displayRecords_${statKeys[i]}_${stat_index}" label="${rawLabel('WebtoolsGeneratedDataStored')}: ${raw(stat[statKeys[i]].stored)}">
                                    <@heading level=1>${uiLabelMap.CommonIteration} #${stat_index + 1}</@heading>
                                    <@table type="data-list">
                                        <@thead>
                                            <@tr>
                                                <@td>${statKeys[i]}</@td>
                                            </@tr>
                                        </@thead> 
                                        <@tbody>      
                                            <#list stat[statKeys[i]].generatedValues as generatedValue>
                                                <@tr>
                                                    <@td>
                                                        <#if generatedValue.getPrimaryKey()?has_content>
                                                            <#assign keys = generatedValue.getPrimaryKey().getAllKeys()>
                                                            <#assign primaryKey = "&">
                                                            <#list keys as field>                                                            
                                                                <#assign primaryKey = primaryKey + field + "=" + generatedValue.get(field)>
                                                                <#if field_index + 1 < keys.size()>
                                                                    <#assign primaryKey = primaryKey + "&">
                                                                </#if>
                                                            </#list>
                                                            <a href="<@pageUrl>ViewGeneric?entityName=${statKeys[i]}${primaryKey!}</@pageUrl>">${generatedValue.getPkShortValueString()}</a>    
                                                        </#if>    
                                                    </@td>
                                                </@tr>
                                            </#list>
                                        </@tbody>                                       
                                    </@table>
                                </@modal>
                                <br/>
                                ${uiLabelMap.WebtoolsGeneratedDataFailed}: ${stat[statKeys[i]].failed} 
                            </@td>
                        <#else>
                            <@td>
                                ${uiLabelMap.WebtoolsGeneratedDataStored}: 0<br/>
                                ${uiLabelMap.WebtoolsGeneratedDataFailed}: 0 
                            </@td>                  
                        </#if>
                    </#list> 
                </@tr>
            </#list>
            <@tfoot>
                <@tr>
                    <@td>${uiLabelMap.CommonTotal}</@td>
                    <#if stat?has_content>
                        <#list stat.keySet() as s>
                            <@td>
                                ${uiLabelMap.WebtoolsGeneratedDataStored}: ${totalStored[s]!0}<br/>
                                ${uiLabelMap.WebtoolsGeneratedDataFailed}: ${totalFailed[s]!0}
                            </@td>
                        </#list>
                    </#if>
                </@tr>
            </@tfoot>
        </@table>
    <#else>
       <@panel><@commonMsg type="result-norecord">${uiLabelMap.WebtoolsDemoDataGeneratorNoRecordsGenerated}.</@commonMsg></@panel>
    </#if>
</@section>