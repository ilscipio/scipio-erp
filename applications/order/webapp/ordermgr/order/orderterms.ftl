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

<#if orderTerms?has_content>
  <@section title=uiLabelMap.OrderOrderTerms>
    <@table type="data-list"> <#-- orig: class="basic-table" --> <#-- orig: cellspacing="0" -->
      <@thead> 
        <@tr class="header-row">
          <@th width="35%">${uiLabelMap.OrderOrderTermType}</@th>
          <@th width="10%" align="center">${uiLabelMap.OrderOrderTermValue}</@th>
          <@th width="10%" align="center">${uiLabelMap.OrderOrderTermDays}</@th>
          <@th width="10%" align="center">${uiLabelMap.OrderOrderTextValue}</@th>        
          <@th width="35%" align="center">${uiLabelMap.CommonDescription}</@th>
        </@tr>
      </@thead>
      <@tbody>
      <#list orderTerms as orderTerm>
        <@tr>
          <@td width="35%">${orderTerm.getRelatedOne("TermType", false).get("description", locale)}</@td>
          <@td width="10%" align="center">${orderTerm.termValue!""}</@td>
          <@td width="10%" align="center">${orderTerm.termDays!""}</@td>
          <@td width="10%" align="center">${orderTerm.textValue!""}</@td>
          <@td width="35%" align="center">${orderTerm.description!""}</@td>
        </@tr>
      </#list>
      </@tbody>
    </@table>
  </@section>
</#if>