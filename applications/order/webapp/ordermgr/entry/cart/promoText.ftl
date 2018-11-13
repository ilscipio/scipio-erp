<#--
This file is subject to the terms and conditions defined in
 file 'LICENSE', which is part of this source code package.
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
