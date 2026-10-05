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