<#--
This file is subject to the terms and conditions defined in the
files 'LICENSE' and 'NOTICE', which are part of this source
code package.
-->
<@section>${uiLabelMap.PageTitleScheduleJob}</h3>

    <@row>
        <@cell>
        <form method="post" action="<@pageUrl>saveServiceResultsToSession</@pageUrl>">
        <@table type="data-list">
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
                    <@td><a href="<@pageUrl>serviceResult?servicePath=</@pageUrl><#if parameters.servicePath??>${parameters.servicePath}||</#if>${srl.key!}">${srl.key!}</a></@td>
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
