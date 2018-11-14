<#--
This file is subject to the terms and conditions defined in the
files 'LICENSE' and 'NOTICE', which are part of this source
code package.
-->

<@section title=uiLabelMap.OrderSpecialOffers>
    <ol>
        <#-- show promotions text -->
        <#list productPromosAllShowable as productPromo>
            <li>
                <@heading level=3>${productPromo.promoName!}</@heading>
                ${escapeVal(productPromo.promoText!, 'htmlmarkup', {"allow":"internal"})}
                <br/><a href="<@ofbizUrl>showPromotionDetails?productPromoId=${productPromo.productPromoId}</@ofbizUrl>" class="${styles.action_view!}">${uiLabelMap.CommonDetails}</a>
            </li>
        </#list>
    </ol>
</@section>
