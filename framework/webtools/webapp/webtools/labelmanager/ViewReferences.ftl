<#--
Licensed to the Apache Software Foundation (ASF) under one
or more contributor license agreements.  See the NOTICE file
distributed with this work for additional information
regarding copyright ownership.  The ASF licenses this file
to you under the Apache License, Version 2.0 (the
"License"); you may not use this file except in compliance
with the License.  You may obtain a copy of the License at

http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing,
software distributed under the License is distributed on an
"AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
KIND, either express or implied.  See the License for the
specific language governing permissions and limitations
under the License.
-->
<#-- SCIPIO: duplicate:
<@heading>${uiLabelMap.WebtoolsLabelManagerViewReferences}</@heading>-->
        <@table type="data-list" autoAltRows=true> <#-- orig: class="basic-table" --> <#-- orig: cellspacing="0" -->
        <@thead>
            <@tr>
                <@th>${uiLabelMap.WebtoolsLabelManagerKey}</@th>
                <@th colspan="2">${parameters.sourceKey!}</@th>
            </@tr>
            <@tr class="header-row">
                <@th>${uiLabelMap.WebtoolsLabelManagerRow}</@th>
                <@th>${uiLabelMap.WebtoolsLabelManagerFileName}</@th>
                <@th>${uiLabelMap.WebtoolsLabelManagerReferences}</@th>
            </@tr>
         </@thead>
            <#if parameters.sourceKey?? && parameters.sourceKey?has_content>
              <#assign rowNumber = 1>
              <#assign totalRefs = 0/>
              <#assign reference = references.get(parameters.sourceKey)!>
              <#if reference?? &&  reference?has_content>
                <#assign entries = reference.entrySet()>
                <#list entries as entry>
                  <@tr>
                    <@td>${rowNumber}</@td>
                    <@td><a href="<@ofbizUrl>ViewFile?fileName=${entry.getKey()}&amp;sourceKey=${parameters.sourceKey!}</@ofbizUrl>">${entry.getKey()}</a></@td>
                    <@td>${entry.getValue()}</@td>
                  </@tr>
                  <#assign totalRefs = totalRefs + entry.getValue()/>
                <#assign rowNumber = rowNumber + 1>
                </#list>
                <@tfoot>
                  <@tr>
                    <@td>&nbsp;</@td>
                    <@td><b>${uiLabelMap.CommonTotal}</b></@td>
                    <@td>${totalRefs}</@td>
                  </@tr>
                </@tfoot>
              </#if>
            </#if>
        </@table>
