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
<#if parameters.maxElements?has_content><#assign maxElements = parameters.maxElements?number/><#else><#assign maxElements = 10/></#if>

    <p>${uiLabelMap.WebtoolsThisThread}<b> ${currentThread.getName()} (${currentThread.getId()})</b></p>
   
    <@table type="data-list" autoAltRows=true>
     <@thead>
      <@tr class="header-row">
        <@th>${uiLabelMap.WebtoolsGroup}</@th>
        <@th>${uiLabelMap.WebtoolsThreadId}</@th>
        <@th>${uiLabelMap.WebtoolsThread}</@th>
        <@th>${uiLabelMap.CommonStatus}</@th>
        <@th>${uiLabelMap.WebtoolsPriority}</@th>
        <@th>${uiLabelMap.WebtoolsDaemon}</@th>
      </@tr>
      </@thead>
      <#list allThreadInfos as threadInfo><#-- SCIPIO: changed iteration to avoid getStackTrace -->
      <#--<#if threadInfo.thread??>-->
        <#assign javaThread = threadInfo.thread><#-- SCIPIO -->
        <#--<#assign stackTraceArray = javaThread.getStackTrace()!/>-->
        <#assign stackTraceArray = threadInfo.stackTrace!/>
        <@tr valign="middle">
          <@td valign="top">${(javaThread.getThreadGroup().getName())!}</@td>
          <@td valign="top">${javaThread.getId()?string}</@td>
          <@td valign="top">
            <b>${javaThread.getName()!}</b>
            <#list 1..maxElements as stackIdx>
              <#assign stackElement = stackTraceArray[stackIdx]!/>
              <#if (stackElement.toString())?has_content><div>${stackElement.toString()}</div></#if>
            </#list>
          </@td>
          <@td valign="top">${javaThread.getState().name()!}&nbsp;</@td>
          <@td valign="top">${javaThread.getPriority()}</@td>
          <@td valign="top">${javaThread.isDaemon()?string}<#-- /${javaThread.isAlive()?string}/${javaThread.isInterrupted()?string} --></@td>
        </@tr>
      <#--</#if>-->
      </#list>
    </@table>
