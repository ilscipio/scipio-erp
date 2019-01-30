<#--
This file is subject to the terms and conditions defined in the
files 'LICENSE' and 'NOTICE', which are part of this source
code package.
-->

<#if showPromoText>
  <@section title=uiLabelMap.OrderSpecialOffers id="minipromotext">
  <#-- show promotions text -->
    <ul>
      <#list productPromos as productPromo>
        <li>
          <p>
            <a href="<@pageUrl>showPromotionDetails?productPromoId=${productPromo.productPromoId}</@pageUrl>" class="${styles.link_nav!} ${styles.action_view!}">${uiLabelMap.CommonDetails}</a>
            ${productPromo.promoText!}
          </p>
        </li>
      </#list>
      <li>
        <a href="<@pageUrl>showAllPromotions</@pageUrl>" class="${styles.link_nav!}">${uiLabelMap.OrderViewAllPromotions}</a>
      </li>
    </ul>
  </@section>
</#if>
