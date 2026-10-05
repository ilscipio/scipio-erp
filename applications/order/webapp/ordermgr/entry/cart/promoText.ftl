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
<#if showPromoText?? && showPromoText>
    <@section title=uiLabelMap.OrderSpecialOffers>
        <@table type="data-list">
          <#-- show promotions text -->
          <#list productPromos as productPromo>
            <@tr>
              <@td>
                <a href="<@pageUrl>showPromotionDetails?productPromoId=${productPromo.productPromoId}</@pageUrl>" class="+${styles.link_run_sys!} ${styles.action_view!}">${uiLabelMap.CommonDetails}</a> ${productPromo.promoText!}
              </@td>
            </@tr>
          </#list>
        </@table>
        
        <@menu type="button">
          <@menuitem type="link" href=makePageUrl("showAllPromotions") text=uiLabelMap.OrderViewAllPromotions class="+${styles.action_nav!}" />
        </@menu>        
    </@section>
</#if>
