<#--
This file is subject to the terms and conditions defined in the
files 'LICENSE' and 'NOTICE', which are part of this source
code package.
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
