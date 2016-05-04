
<#-- Cato: common shop-wide helper definitions and macros -->

<#macro formattedAddress address emphasis=false updateLink="">
  <#if address.toName?has_content><#if emphasis><b></#if>${uiLabelMap.CommonTo}:<#if emphasis></b></#if>&nbsp;${address.toName}<br /></#if>
  <#if address.attnName?has_content><#if emphasis><b></#if>${uiLabelMap.PartyAddrAttnName}:<#if emphasis></b></#if>&nbsp;${address.attnName}<br /></#if>
  <#if address.address1?has_content>${address.address1}<br /></#if>
  <#if address.address2?has_content>${address.address2}<br /></#if>
  <#if address.city?has_content>${address.city}</#if><#rt>
  <#lt><#if address.stateProvinceGeoId?has_content><#if address.city?has_content>, <#else><br/></#if>${address.stateProvinceGeoId}</#if>
  <#if address.postalCode?has_content><br />${address.postalCode}</#if>
  <#if address.countryGeoId?has_content><br />${address.countryGeoId}</#if>
  <#if updateLink?has_content>
    <a href="${escapeFullUrl(updateLink, 'html')}" class="${styles.link_nav!} ${styles.action_update!}">${uiLabelMap.CommonUpdate}</a>
  </#if>
</#macro>

<#macro formattedCreditCard creditCard paymentMethod={} verbose=true>
  <#if verbose>
    <#--
    <#if !paymentMethod?has_content>
      <#local paymentMethod = creditCard.getRelatedOne("PaymentMethod")>
    </#if>
    -->
    ${(delegator.findOne("Enumeration", {"enumId":creditCard.cardType!}, true).get("description", locale))!creditCard.cardType!}<#t>
    <#local cardNum = creditCard.cardNumber!?string>
    <#if cardNum?has_content>
      <#if (cardNum?length > 4)>
        <#t> ${cardNum[0..<(cardNum?length-4)]?replace('.','*','r')}${cardNum[(cardNum?length-4)..]}
      <#else>
        <#t> ${cardNum}
      </#if>
    </#if>
    <#if creditCard.expireDate?has_content>
      <#t> ${creditCard.expireDate}
    </#if>
  <#else>
    <#-- stock ofbiz method -->
    ${Static["org.ofbiz.party.contact.ContactHelper"].formatCreditCard(creditCard)}<#t>
  </#if>
</#macro>

<#-- Migrated from checkoutpayment.ftl -->
<#function getGiftCardDisplayNumber giftCard>
    <#if giftCard?has_content && giftCard.cardNumber?has_content>
      <#local giftCardNumber = "" />
      <#local pcardNumber = giftCard.cardNumber />
      <#if pcardNumber?has_content>
        <#local psize = pcardNumber?length - 4 />
        <#if (0 < psize)>
          <#list 0 .. psize-1 as foo>
            <#local giftCardNumber = giftCardNumber + "*" />
          </#list>
          <#local giftCardNumber = giftCardNumber + pcardNumber[psize .. psize + 3] />
        <#else>
          <#local giftCardNumber = pcardNumber />
        </#if>
        <#return giftCardNumber>
      </#if>
    </#if>
</#function>


<#-- Generic javascript for radios that have a "new item" option that should display content when radio selected 
    WARN: JS callbacks are highly coupled with macro -->
<#macro initItemSelectionWithContentFormScript itemFieldClass contentItems=[] 
    updateCallbackPerElemJs="" updateCallbackPreVisibJs="" updateCallbackPostVisibJs="">

if (typeof getCatoFieldCheckElems === 'undefined') {
    <#-- returns radio and/or checkbox with given class -->
    function getCatoFieldCheckElems(fieldClass) {
        <#-- NOTE: alt markup support -->
        return jQuery.merge(jQuery('input.'+fieldClass), jQuery('.'+fieldClass+' input'))
    }
}  
  
jQuery(document).ready(function() {
    var allItems = getCatoFieldCheckElems('${escapePart(itemFieldClass, 'js')}');
    
    var contentItemMap = {
      <#list contentItems as item>
        "${escapePart(item.fieldId, 'js')}" : "${escapePart(item.contentId, 'js')}"<#if item_has_next>, </#if>
      </#list>
    };
    
    var updateItemVisibility = function(event) {
        var elem = jQuery(this);
        var fieldIdShowMap = {}; <#-- for callbacks -->
        var fieldIdHideMap = {};
        var contentIdShowMap = {};
        var contentIdHideMap = {};
        allItems.each(function(i, e) {
            e = jQuery(e);
            var eid = e.attr('id');
            var cid = contentItemMap[eid];
            if (e.is(":checked")) {
                contentIdShowMap[cid] = jQuery('#'+cid);
                fieldIdShowMap[eid] = e;
            } else {
                contentIdHideMap[cid] = jQuery('#'+cid);
                fieldIdHideMap[eid] = e;
            }
            ${rawString(updateCallbackPerElemJs)}
        });
        ${rawString(updateCallbackPreVisibJs)}
        jQuery.each(contentIdHideMap, function(k, v) {
            if (v) {
                v.hide();
            }
        });
        jQuery.each(contentIdShowMap, function(k, v) {
            if (v) {
                v.show();
            }
        });
        ${rawString(updateCallbackPostVisibJs)}
    };

    updateItemVisibility(); <#-- Needed for page refreshes to work and to set initial visibility (if FTL doesn't do it) -->
    allItems.change(updateItemVisibility);
});

</#macro>

<#-- Cato: local macro where cells of label and widget areas are inverted and tweaked 
    NOTE: You can set labelContentFieldsType="default-compact" for a different default look
    NOTE: the labelContent bypasses the regular @field parent-child field relation; set markup with labelContentFieldsType -->
<#macro commonInvField type="generic" labelContentFieldsType="default" postfixColumns="" labelContent="" labelContentArgs={} widgetAreaClass="" widgetPostfixColumns="" postfixContent="" postfix=false inlineArgs...>
<#--
  <#local gridStyles = getDefaultFieldGridStyles({"labelArea":true, "postfix":true, "postfixColumns":postfixColumns, "widgetPostfixCombined":false})>
  <@row>
    <@cell class=addClassArg(gridStyles.labelArea, "${styles.text_right!}")>
      <#nested>
    </@cell>
    <#local id = (getRequestVar("catoLastFieldInfo").id)!"">
    <@cell class=gridStyles.widgetArea>
      ${labelContent}
    </@cell>  
    <@cell class=gridStyles.postfixArea>
      ${postfixContent}
    </@cell>  
  </@row>
-->
  <#if !postfixColumns?has_content>
    <#local postfixColumns = 3>
  </#if>
  <#if !widgetPostfixColumns?has_content>
    <#local widgetPostfixColumns = 10>
  </#if>
  <#if postfixContent?is_directive || postfixContent?has_content>
    <#local postfix = true>
  </#if>
  <#--<#local widgetAreaClass = addClassArg(widgetAreaClass, styles.text_right!)>-->
  <#if labelContent?has_content || labelContent?is_directive>
    <#local labelContentArgs = {"labelContentFieldsType":labelContentFieldsType, "labelContent":labelContent, "labelContentArgs":labelContentArgs}>
    <#local labelContent = commonInvFieldLabelRender>
  </#if>
  <@field type=type inverted=true args=inlineArgs widgetAreaClass=widgetAreaClass postfix=postfix
    labelContent=labelContent labelContentArgs=labelContentArgs
    postfixContent=postfixContent widgetPostfixColumns=widgetPostfixColumns postfixColumns=postfixColumns><#nested></@field>
</#macro>

<#-- this is an ugly kludge needed due to only having one #nested in freemarker -->
<#macro commonInvFieldLabelRender args={}>
  <@fields type=args.labelContentFieldsType ignoreParentField=true>
    <@contentArgRender content=args.labelContent args=args.labelContentArgs />
  </@fields>
</#macro>

<#macro addressList>
  <#-- Use simple inline-blocks for this... -->
  <div>
    <#nested>
  </div>
</#macro>

<#macro addressEntry>
  <#-- Use simple inline-blocks for this... -->
  <div style="display:inline-block; vertical-align:top; width:20em; margin-bottom:1em;"><#-- FIXME: Unhardcode -->
    <#nested>
  </div>
</#macro>


