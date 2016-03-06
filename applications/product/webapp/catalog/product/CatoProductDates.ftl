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
<@section>
    <@table type="fields">
        <#-- availability -->
        <#if product.introductionDate?has_content>
            <@tr>
              <@td class="${styles.grid_large!}2">${uiLabelMap.CommonIntroductionDate}
              </@td>
              <@td colspan="3"><@formattedDateTime date=product.introductionDate defaultVal="0000-00-00 00:00:00"/></@td>
            </@tr>    
        </#if>

        <#if product.releaseDate?has_content>
            <@tr>
              <@td class="${styles.grid_large!}2">${uiLabelMap.CommonReleaseDate}
              </@td>
              <@td colspan="3"><@formattedDateTime date=product.releaseDate defaultVal="0000-00-00 00:00:00"/></@td>
            </@tr>    
        </#if>

        <#if product.salesDiscontinuationDate?has_content>
            <@tr>
              <@td class="${styles.grid_large!}2">${uiLabelMap.ProductSalesThruDate}
              </@td>
              <@td colspan="3"><@formattedDateTime date=product.salesDiscontinuationDate defaultVal="0000-00-00 00:00:00"/></@td>
            </@tr>    
        </#if>

        <#if product.supportDiscontinuationDate?has_content>
            <@tr>
              <@td class="${styles.grid_large!}2">${uiLabelMap.ProductSupportThruDate}
              </@td>
              <@td colspan="3"><@formattedDateTime date=product.supportDiscontinuationDate defaultVal="0000-00-00 00:00:00"/></@td>
            </@tr>    
        </#if>
    </@table>


</@section>