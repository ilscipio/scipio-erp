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
<@section title="${uiLabelMap.PageTitleEditProductFeatureGroups}">
    <@table type="data-list" autoAltRows=true cellspacing="0"> <#-- orig: class="basic-table" -->
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
                <@td><a href='<@ofbizUrl>EditFeatureGroupAppls?productFeatureGroupId=${productFeatureGroup.productFeatureGroupId}</@ofbizUrl>' class="${styles.link_nav_record_id!}">${productFeatureGroup.productFeatureGroupId}</a></@td>
                <@td>
                    <form method='post' action='<@ofbizUrl>UpdateProductFeatureGroup</@ofbizUrl>'>
                    <input type='hidden' name="productFeatureGroupId" value="${productFeatureGroup.productFeatureGroupId}" />
                    <input type='text' size='30' name="description" value="${productFeatureGroup.description!}" />
                    <input type="submit" value="${uiLabelMap.CommonUpdate}" />
                    </form>
                </@td>
                <@td><a href='<@ofbizUrl>EditFeatureGroupAppls?productFeatureGroupId=${productFeatureGroup.productFeatureGroupId}</@ofbizUrl>' class="${styles.link_nav!}">${uiLabelMap.ProductFeatureGroupAppls}</a></@td>
            </@tr>
          </#list>
        </@tbody>
    </@table>
</@section>

<@section title="${uiLabelMap.ProductCreateProductFeatureGroup}">
        <form method="post" action="<@ofbizUrl>CreateProductFeatureGroup</@ofbizUrl>">
            <@field type="generic" label="${uiLabelMap.CommonDescription}">
                <input type="text" size='30' name='description' value='' />
            </@field>
            <@field type="submitarea">
              <input type="submit" value="${uiLabelMap.CommonCreate}" />
            </@field>
        </form>
</@section>