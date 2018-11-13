<#--
This file is subject to the terms and conditions defined in the
files 'LICENSE' and 'NOTICE', which are part of this source
code package.
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
