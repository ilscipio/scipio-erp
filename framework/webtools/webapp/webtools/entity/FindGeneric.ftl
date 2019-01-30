<#--
This file is subject to the terms and conditions defined in the
files 'LICENSE' and 'NOTICE', which are part of this source
code package.
-->
<@section>
        <form method="post" action="<@pageUrl>FindGeneric?entityName=${entityName}</@pageUrl>">
          <input type="hidden" name="find" value="true"/>
          <input type="hidden" name="searchOptions_collapsed" value="true"/>
          <@table type="data-list">
            <@thead>
                <@tr class="header-row-2">
                  <@th>${uiLabelMap.WebtoolsFieldName}</@th>
                  <@th>${uiLabelMap.WebtoolsPk}</@th>
                  <@th>${uiLabelMap.WebtoolsFieldType}</@th>
                  <@th>&nbsp;</@th>
                </@tr>
            </@thead>
            <@tbody>
            <#list fieldList as field>
                <@tr>
                    <@td>${field.name}</@td>
                    <@td><#if field.isPk == 'Y'>*</#if></@td>
                    <@td>${field.javaType},&nbsp;${field.sqlType}</@td>
                    <@td><input type="text" name="${field.name}" value="${field.param}" size="40"/></@td>
                </@tr>
            </#list>
            </@tbody>
            <@tfoot>
                <@tr>
                    <@td colspan="3">${uiLabelMap.WebtoolsToFindAll} ${entityName}, ${uiLabelMap.WebtoolsLeaveAllEntriesBlank}</@td>
                    <@td><input type="submit" value="${uiLabelMap.CommonFind}" class="${styles.link_run_sys!} ${styles.action_find!}"/></@td>
                </@tr>
            </@tfoot>
          </@table>
        </form>
</@section>