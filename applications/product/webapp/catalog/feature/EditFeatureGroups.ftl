<#--
This file is subject to the terms and conditions defined in the
files 'LICENSE' and 'NOTICE', which are part of this source
code package.
-->
<@section>
    <@table type="data-list" autoAltRows=true>
        <@thead>
          <@tr class="header-row">
            <@th>${uiLabelMap.CommonId}</@th>
            <@th>${uiLabelMap.CommonDescription}</@th>
            <@th>&nbsp;</@th>
            <@th>&nbsp;</@th>
          </@tr>
        </@thead>
        <@tbody>
          <#list productFeatureGroups as productFeatureGroup>
            <@tr valign="middle">
                <form method="post" action="<@pageUrl>UpdateProductFeatureGroup</@pageUrl>">
                    <input type="hidden" name="productFeatureGroupId" value="${productFeatureGroup.productFeatureGroupId}" />
                    <@td><a href="<@pageUrl>EditProductFeatureGroup?productFeatureGroupId=${productFeatureGroup.productFeatureGroupId}</@pageUrl>" class="${styles.link_nav_info_id!}">${productFeatureGroup.productFeatureGroupId}</a></@td>
                    <@td>
                         <input type="text" size="30" name="description" value="${productFeatureGroup.description!}" />
                    </@td>
                    <@td>
                        <input type="submit" value="${uiLabelMap.CommonUpdate}" class="${styles.link_run_sys!} ${styles.action_update!}" />
                    </@td>
                    <@td><a href="<@pageUrl>EditFeatureGroupAppls?productFeatureGroupId=${productFeatureGroup.productFeatureGroupId}</@pageUrl>" class="${styles.link_nav!}">${uiLabelMap.ProductFeatureGroupAppls}</a></@td>
                </form>
            </@tr>
          </#list>
        </@tbody>
    </@table>
</@section>