<#--
This file is subject to the terms and conditions defined in the
files 'LICENSE' and 'NOTICE', which are part of this source
code package.
-->
    <form method="post" action="EntitySQLProcessor" name="EntitySQLCommand">
        <@field type="select" label=uiLabelMap.CommonGroup name="group">
                <#list groups as group>
                    <option value="${group}"<#if selGroup??><#if group == selGroup> selected="selected"</#if></#if>>${group}</option>
                </#list>
        </@field>
        <@field type="textarea" label=uiLabelMap.WebtoolsSqlCommand name="sqlCommand" cols="100" rows="5">${sqlCommand!}</@field>
        <@field type="input" label=uiLabelMap.WebtoolsLimitRowsTo name="rowLimit" size="5" value=rowLimit!200/>
        <@field type="submit" name="submitButton" text=uiLabelMap.CommonSubmit class="${styles.link_run_sys!} ${styles.action_begin!}"/>
    </form>

<@section title=uiLabelMap.WebtoolsResults>
    <#if resultMessage?has_content>
      <@commonMsg type="result">${resultMessage}</@commonMsg>
    </#if>

    <#if columns?has_content>
        <@table type="data-list" autoAltRows=true>
          <@thead>
            <@tr class="header-row">
            <#list columns as column>
                <@th>${column}</@th>
            </#list>
            </@tr>
           </@thead>
            <#if records?has_content>
            <#list records as record>
                <@tr>
                <#list record as field>
                    <@td><#if field?has_content>${field}</#if></@td>
                </#list>
                </@tr>
            </#list>
            </#if>
        </@table>
    </#if>
</@section>
