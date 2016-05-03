
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
    <a href="${escapeFullUrl(updateLink, 'html')}" class="${styles.link_run_sys!} ${styles.action_update!}">${uiLabelMap.CommonUpdate}</a>
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

<#-- Generic javascript for radios that have a "new item" option that should display content when radio selected -->
<#macro initItemSelectionWithNewFormScript itemFieldClass newItems=[] updateCallbackJs="">
  <#assign itemFieldClass = escapePart(itemFieldClass, 'js')>
  
if (typeof getCatoFieldRadioInput === 'undefined') {
    function getCatoFieldRadioInput(fieldId) {
        var radio = jQuery('#' + fieldId + ' input');
        if (!radio.length) {
            radio = jQuery('input#' + fieldId); <#-- alt markup support -->
        }
        return radio;
    }
}
  
jQuery(document).ready(function() {
    
    var updateNewItemContentVisibility = function(elem, fieldId, contentId) {
        var radio = getCatoFieldRadioInput(fieldId);
        if (radio.length > 0) {
            if (radio.is(":checked")) {
                jQuery('#'+contentId).show();
                jQuery('#'+fieldId).focus();
            } else {
                jQuery('#'+contentId).hide();
            }
        }
    };
    
    var updateItemVisibility = function(event) {
        var elem = jQuery(this);
        <#list newItems as newItem>
          updateNewItemContentVisibility(elem, '${escapePart(newItem.fieldId, 'js')}', '${escapePart(newItem.contentId, 'js')}');
        </#list>
        ${updateCallbackJs}
    };

    <#-- Cato: Needed for page refreshes to work -->
    updateItemVisibility();
    jQuery('input.${itemFieldClass}').change(updateItemVisibility);
    jQuery('.${itemFieldClass} input').change(updateItemVisibility); <#-- alt markup support -->
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
  <#-- Use simple floats for this. it works out better... -->
  <div class="${styles.float_clearfix}">
    <#nested>
  </div>
</#macro>

<#macro addressEntry>
  <#-- Use simple floats for this. it works out better... -->
  <div style="float:left; width:20em; margin-bottom:1em;"><#-- FIXME: Unhardcode -->
    <#nested>
  </div>
</#macro>


