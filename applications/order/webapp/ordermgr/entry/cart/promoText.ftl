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
<#if showPromoText?? && showPromoText>
    <@section title=uiLabelMap.OrderSpecialOffers>
        <@table type="data-list"> <#-- orig: class="basic-table" -->
          <#-- show promotions text -->
          <#list productPromos as productPromo>
            <@tr>
              <@td>
                <a href="<@ofbizUrl>showPromotionDetails?productPromoId=${productPromo.productPromoId}</@ofbizUrl>" class="+${styles.link_run_sys!} ${styles.action_view!}">${uiLabelMap.CommonDetails}</a> ${productPromo.promoText!}
              </@td>
            </@tr>
          </#list>
        </@table>
        
        <@menu type="button">
          <@menuitem type="link" href=makeOfbizUrl("showAllPromotions") text=uiLabelMap.OrderViewAllPromotions class="+${styles.action_nav!}" />
        </@menu>        
    </@section>
</#if>
