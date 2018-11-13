<#--
This file is subject to the terms and conditions defined in
 file 'LICENSE', which is part of this source code package.
-->

<#if showPromoText>
  <@section title=uiLabelMap.OrderSpecialOffers id="minipromotext">
  <#-- show promotions text -->
    <ul>
      <#list productPromos as productPromo>
        <li>
          <p>
            <a href="<@ofbizUrl>showPromotionDetails?productPromoId=${productPromo.productPromoId}</@ofbizUrl>" class="${styles.link_nav!} ${styles.action_view!}">${uiLabelMap.CommonDetails}</a>
            ${productPromo.promoText!}
          </p>
        </li>
      </#list>
      <li>
        <a href="<@ofbizUrl>showAllPromotions</@ofbizUrl>" class="${styles.link_nav!}">${uiLabelMap.OrderViewAllPromotions}</a>
      </li>
    </ul>
  </@section>
</#if>
