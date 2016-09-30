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

<#if productIds?has_content>
  <@section title=uiLabelMap.OrderProductsForPromotion>
    <#if (listSize > 0)>
      <@menu type="button">
        <@menuitem type="link" href=makeOfbizUrl("showPromotionDetails?productPromoId=${productPromoId!}&amp;VIEW_SIZE=${viewSize}&amp;VIEW_INDEX=${viewIndex-1}") text=uiLabelMap.CommonPrevious disabled=(!(viewIndex > 0)) class="${styles.action_nav!}" />
        <@menuitem type="text" text="${lowIndex+1} - ${highIndex} ${rawString(uiLabelMap.CommonOf)} ${listSize}" />
        <@menuitem type="link" href=makeOfbizUrl("showPromotionDetails?productPromoId=${productPromoId!}&amp;VIEW_SIZE=${viewSize}&amp;VIEW_INDEX=${viewIndex+1}") text=uiLabelMap.CommonNext disabled=(!(listSize > highIndex)) class="${styles.action_nav!}" />
      </@menu>
    </#if>

    <#if (listSize > 0)>
      <@table type="data-list" autoAltRows=false width="100%" class="+boxbottom"> <#-- orig: class="boxbottom" --> <#-- orig: cellspacing="0" --> <#-- orig: cellpadding="0" --> <#-- orig: border="0" -->
        <@thead>
          <@tr>
            <@td>${uiLabelMap.CommonQualifier}</@td>
            <@td>${uiLabelMap.CommonBenefit}</@td>
            <@td>&nbsp;</@td>
          </@tr>
        </@thead>
        <@tbody>
        <#list productIds[lowIndex..highIndex-1] as productId>
          <@tr>
            <@td>[<#if productIdsCond.contains(productId)>x<#else>&nbsp;</#if>]</@td>
            <@td>[<#if productIdsAction.contains(productId)>x<#else>&nbsp;</#if>]</@td>
            <@td>
              <@render resource=productsummaryScreen reqAttribs={"optProductId":productId, "listIndex":productId_index}/>
            </@td>
          </@tr>
        </#list>
        </@tbody>
      </@table>
    <#else>
      <@commonMsg type="result-norecord"/>
    </#if>
      
  </@section>
</#if>
