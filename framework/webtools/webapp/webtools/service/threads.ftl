<#--
This file is subject to the terms and conditions defined in
 file 'LICENSE', which is part of this source code package.
-->
<#if parameters.maxElements?has_content><#assign maxElements = parameters.maxElements?number/><#else><#assign maxElements = 10/></#if>

    <p>${uiLabelMap.WebtoolsThisThread}<b> ${Static["java.lang.Thread"].currentThread().getName()} (${Static["java.lang.Thread"].currentThread().getId()})</b></p>
   
    <@table type="data-list" autoAltRows=true> <#-- orig: class="basic-table hover-bar" --> <#-- orig: cellspacing="0" -->
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
      <#list allThreadList as javaThread>
      <#if javaThread??>
        <#assign stackTraceArray = javaThread.getStackTrace()/>
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
      </#if>
      </#list>
    </@table>
