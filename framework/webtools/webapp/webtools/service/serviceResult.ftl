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
<@section>${uiLabelMap.PageTitleScheduleJob}</h3>

    <@row>
        <@cell>
        <form method="post" action="<@ofbizUrl>saveServiceResultsToSession</@ofbizUrl>">
        <@table type="data-list"> <#-- orig: class="basic-table" --> <#-- orig: cellspacing="0" -->
              <@thead>
          <@tr class="header-row">
                <@th>${uiLabelMap.WebtoolsParameterName}</@th>
                <@th>${uiLabelMap.WebtoolsParameterValue}</@th>
                <@th>${uiLabelMap.WebtoolsServiceSaveValue} ?</@th>
          </@tr>
              </@thead>
            <#if serviceResultList?has_content>
              <#list serviceResultList as srl>
                <@tr>
                  <#if srl.hasChild=="Y">
                    <@td><a href="<@ofbizUrl>serviceResult?servicePath=</@ofbizUrl><#if parameters.servicePath??>${parameters.servicePath}||</#if>${srl.key!}">${srl.key!}</a></@td>
                  <#else>
                    <@td>${srl.key!}</@td>
                  </#if>
                    <@td>${srl.value!}</@td>
                    <@td><input type="checkbox" name="<#if parameters.servicePath??>${parameters.servicePath}||</#if>${srl.key!}" /></@td>
                  </@tr>
               </#list>
            </#if>
        <@tfoot>
          <@tr>
            <@td>&nbsp;</@td>
                <@td>${uiLabelMap.WebtoolsServiceClearPreviousParams} ? <input type="checkbox" name="_CLEAR_PREVIOUS_PARAMS_" /></@td>
            <@td><input type="submit" value="${uiLabelMap.CommonSubmit}" class="${styles.link_run_sys!} ${styles.action_update!}"/></@td>
          </@tr>
        </@tfoot>
        </@table>
      </form>
        </@cell>
    </@row>
</@section>
