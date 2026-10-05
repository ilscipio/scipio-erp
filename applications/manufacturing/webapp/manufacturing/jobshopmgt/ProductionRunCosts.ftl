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
<#--
Changes to this file: Copyright (C) Ilscipio GmbH. The changes are licensed
under the GNU Affero General Public License, version 3, or a commercial
license from Ilscipio GmbH (file LICENSE). The original code stays under
the Apache License, version 2.0, as stated above.
-->

<#macro costRowsTable rows>
  <#assign curIsoCode = "USD">
  <#if rows?has_content><#assign curIsoCode = (rows[0].currencyUomId)!"USD"></#if>
  <@table type="data-list" autoAltRows=true>
    <@thead>
      <@tr class="header-row">
        <@th>${uiLabelMap.CommonType}</@th>
        <@th align="right">${uiLabelMap.ManufacturingCostTypeStandard}</@th>
        <@th align="right">${uiLabelMap.ManufacturingCostTypeActual}</@th>
        <@th align="right">${uiLabelMap.ManufacturingCostTypeVariance}</@th>
      </@tr>
    </@thead>
    <@tbody>
      <#if rows?has_content>
        <#list rows as row>
          <@tr>
            <@td><#if row.labelKey?has_content>${uiLabelMap[row.labelKey]}<#else>${row.baseType!}</#if></@td>
            <@td align="right"><@ofbizCurrency amount=(row.standard!0) isoCode=(row.currencyUomId!curIsoCode)/></@td>
            <@td align="right"><@ofbizCurrency amount=(row.actual!0) isoCode=(row.currencyUomId!curIsoCode)/></@td>
            <@td align="right" class=(((row.variance!0) gt 0)?then(styles.text_color_warning!"", ""))><@ofbizCurrency amount=(row.variance!0) isoCode=(row.currencyUomId!curIsoCode)/></@td>
          </@tr>
        </#list>
      <#else>
        <@tr><@td colspan=4><@commonMsg type="result-norecord"/></@td></@tr>
      </#if>
    </@tbody>
  </@table>
</#macro>

<@section title=uiLabelMap.ManufacturingActualCosts>
  <p>${uiLabelMap.ManufacturingProductionRunCostsExplanation}</p>

  <@section title=uiLabelMap.ManufacturingProductionRunTotalCosts>
    <@costRowsTable rows=runTotalRows![]/>
  </@section>

  <#if taskCosts?has_content>
    <#list taskCosts as taskCost>
      <#assign task = taskCost.task!>
      <#assign rows = taskCost.rows![]>
      <#if rows?has_content>
        <@section title="${raw(task.workEffortName!)} [${raw(task.workEffortId!)}]">
          <@costRowsTable rows=rows/>
        </@section>
      </#if>
    </#list>
  </#if>
</@section>
