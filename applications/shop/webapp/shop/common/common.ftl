
<#-- Scipio: common shop-wide helper definitions and macros -->

<#macro addressUpdateLink address updateLink class="">
  <#local class = addClassArg(class, styles.action_update!)>
  <#local class = addClassArgDefault(class, styles.link_nav_inline!)>
  <a href="${escapeFullUrl(updateLink, 'html')}"<@compiledClassAttribStr class=class />>${uiLabelMap.CommonUpdate}</a><#t>
</#macro>

<#macro formattedAddressBasic address emphasis=false abbrev=false verbose=true>
  <#-- NOTE: This must NEVER end with a <br/>. only use between elements. -->
  <#if address.toName?has_content><#if emphasis><b></#if>${uiLabelMap.CommonTo}<#if emphasis></b></#if>&nbsp;${address.toName}<br /></#if>
  <#if address.attnName?has_content><#if emphasis><b></#if>${uiLabelMap.PartyAddrAttnName}:<#if emphasis></b></#if>&nbsp;${address.attnName}<br /></#if>
  <#if address.address1?has_content>${address.address1}<br /></#if>
  <#if address.address2?has_content>${address.address2}<br /></#if>
  <#if address.city?has_content>${address.city}</#if><#rt>
  <#lt><#if address.stateProvinceGeoId?has_content><#if address.city?has_content>, <#else><br/></#if><#rt>
    <#if verbose>${(delegator.findOne("Geo", {"geoId", address.stateProvinceGeoId}, true).get("geoName", locale))!address.stateProvinceGeoId}<#else>${address.stateProvinceGeoId}</#if></#if><#lt>
  <#if address.postalCode?has_content><br />${address.postalCode}</#if>
  <#if address.countryGeoId?has_content><br /><#rt>
    <#if verbose>${(delegator.findOne("Geo", {"geoId", address.countryGeoId}, true).get("geoName", locale))!address.countryGeoId}<#else>${address.countryGeoId}</#if></#if><#lt>
</#macro>

<#-- high-level address formatting macro -->
<#macro formattedAddress address emphasis=false updateLink="" abbrev=false verbose=true usePanel=false 
    partyContactMechPurposes=[] title="">
  <#if usePanel>   
    <#local bottomContent = false> 
    <#if updateLink?has_content>
      <#-- NOTE: This is currently using absolute positioning; also possible was float:right on button
          with manipulating panel padding and exploiting text wrapping to save space, but the wrapping did not work out well -->
      <#local bottomContent><@addressUpdateLink address=address updateLink=updateLink class="${styles.link_nav!} panel-update-link" /></#local>
    </#if>
    <@panel class="+address-panel ${styles.float_clearfix!}" bottomContent=bottomContent>
      <#-- NOTE: use spans only for now!! -->
      <#if title?has_content><span class="address-panel-content"><b>${title}</b></span><br/></#if>
      <span class="address-panel-content">
        <@formattedAddressBasic address=address emphasis=emphasis abbrev=abbrev verbose=verbose/>
      </span>
    <#list partyContactMechPurposes as partyContactMechPurpose>
      <#assign contactMechPurposeType = partyContactMechPurpose.getRelatedOne("ContactMechPurposeType", true) />
      <br/>
      <span class="address-panel-content">
        <em>
        ${contactMechPurposeType.get("description",locale)!}
        <#if partyContactMechPurpose.thruDate??>(${uiLabelMap.CommonExpire}: ${partyContactMechPurpose.thruDate})</#if>
        </em>
      </span>
    </#list>
    <#--
    <#if updateLink?has_content>
      <@addressUpdateLink address=address updateLink=updateLink class="${styles.link_nav!} panel-update-link" />
    </#if>-->
    </@panel>
  <#else>
    <#if title?has_content><span><b>${title}</b></span><br/></#if>
    <span>
      <@formattedAddressBasic address=address emphasis=emphasis abbrev=abbrev verbose=verbose/>
    </span>
    <#list partyContactMechPurposes as partyContactMechPurpose>
      <#assign contactMechPurposeType = partyContactMechPurpose.getRelatedOne("ContactMechPurposeType", true) />
      <br/>
      <span>
        <em>
        ${contactMechPurposeType.get("description",locale)!}
        <#if partyContactMechPurpose.thruDate??>(${uiLabelMap.CommonExpire}: ${partyContactMechPurpose.thruDate})</#if>
        </em>
      </span>
    </#list>
    <#if updateLink?has_content>
      <br/><br/><@addressUpdateLink address=address updateLink=updateLink />
    </#if>
  </#if>
</#macro>

<#macro formattedCreditCardShort creditCard paymentMethod={} verbose=true>
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

<#macro formattedPayMethGeneralDetail paymentMethod={}>
    <#if paymentMethod.description?has_content>(${paymentMethod.description})</#if>
    <#if paymentMethod.fromDate?has_content>(${uiLabelMap.CommonUpdated}:&nbsp;${paymentMethod.fromDate.toString()})</#if>
    <#if paymentMethod.thruDate??>(${uiLabelMap.CommonDelete}:&nbsp;${paymentMethod.thruDate.toString()})</#if>
</#macro>

<#macro formattedCreditCardDetail creditCard paymentMethod={}>
  <@formattedCreditCardShort creditCard=creditCard verbose=true /><br/>
  <#if creditCard.companyNameOnCard?has_content>${creditCard.companyNameOnCard}</#if> <#t>
  <#if creditCard.titleOnCard?has_content>${creditCard.titleOnCard}</#if> <#t>
  ${creditCard.firstNameOnCard} <#t>
  <#if creditCard.middleNameOnCard?has_content>${creditCard.middleNameOnCard}</#if> <#t>
  ${creditCard.lastNameOnCard} <#t>
  <#if creditCard.suffixOnCard?has_content>${creditCard.suffixOnCard}</#if> <#t>
</#macro>

<#macro formattedGiftCardShort giftCard paymentMethod={} verbose=true>
  ${getGiftCardDisplayNumber(giftCard)!}<#t>
</#macro>

<#macro formattedGiftCardDetail giftCard paymentMethod={} verbose=true>
  <@formattedGiftCardShort giftCard=giftCard paymentMethod=paymentMethod verbose=verbose /><#t>
</#macro>

<#macro formattedEftAccountShort eftAccount paymentMethod={} verbose=true>
  ${eftAccount.bankName!}: ${eftAccount.accountNumber!}<#t>
</#macro>

<#macro formattedEftAccountDetail eftAccount paymentMethod={} verbose=true>
  ${uiLabelMap.AccountingAccount} #: ${eftAccount.accountNumber!}<br/>
  ${eftAccount.nameOnAccount!}<br/>
  <#if eftAccount.companyNameOnAccount?has_content>${eftAccount.companyNameOnAccount}<br/></#if>
  ${uiLabelMap.AccountingBank}: ${eftAccount.bankName!}, ${eftAccount.routingNumber!}
</#macro>

<#macro formattedBillingAccountShort billingAccount paymentMethod={} customerPoNumberSet=[] verbose=true>
  ${billingAccount.billingAccountId!} - ${billingAccount.description!}<#t>
</#macro>

<#macro formattedBillingAccountDetail billingAccount paymentMethod={} customerPoNumberSet=[] verbose=true>
  <#if billingAccount?has_content>
    ${uiLabelMap.AccountingBillingAccount} #: ${billingAccount.billingAccountId!} - ${billingAccount.description!}
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

if (typeof getScipioFieldCheckElems === 'undefined') {
    <#-- returns radio and/or checkbox with given class -->
    function getScipioFieldCheckElemsByClass(fieldClass) {
        <#-- NOTE: alt markup support -->
        return jQuery.merge(jQuery('input.'+fieldClass), jQuery('.'+fieldClass+' input'))
    }
}  
  
jQuery(document).ready(function() {
    var allItems = getScipioFieldCheckElemsByClass('${escapeVal(itemFieldClass, 'js')}');
    
    var contentItemMap = {
      <#list contentItems as item>
        "${escapeVal(item.fieldId, 'js')}" : <@objectAsScript lang="js" object=item />,
      </#list>
    };
    
    var updateItemVisibility = function(event) {
        var elem = jQuery(this);
        var isTargetElem = false;
        if (elem.is(":checked")) {
            isTargetElem = true;
        }
        
        var fieldIdShowMap = {}; <#-- for callbacks -->
        var fieldIdHideMap = {};
        var contentIdShowMap = {};
        var contentIdHideMap = {};
        allItems.each(function(i, e) {
            e = jQuery(e);
            var eid = e.attr('id');
            if (contentItemMap[eid] && contentItemMap[eid].contentId) {
                var cid = contentItemMap[eid].contentId;
                if (e.is(":checked")) {
                    contentIdShowMap[cid] = jQuery('#'+cid);
                    fieldIdShowMap[eid] = e;
                } else if (contentItemMap[eid].noHideContent !== true) {
                    contentIdHideMap[cid] = jQuery('#'+cid);
                    fieldIdHideMap[eid] = e;
                }
            }
            ${rawString(updateCallbackPerElemJs)}
        });
        ${rawString(updateCallbackPreVisibJs)}
        jQuery.each(contentIdHideMap, function(k, v) {
            if (v && v.is(':visible')) {
                v.fadeOut('fast');
            }
        });
        jQuery.each(contentIdShowMap, function(k, v) {
            if (v && !v.is(':visible')) {
                v.fadeIn('fast');
            }
        });
        <#-- focus on the content of the last one clicked -->
        <#-- FIXME: focus does not work
        if (isTargetElem) {
            var eid = elem.attr('id');
            if (contentItemMap[eid] && contentItemMap[eid].contentId) {
                var cid = contentItemMap[eid].contentId;
                var elemContent = contentIdShowMap[cid];
                if (elemContent) {

                    if (elemContent.is(':visible')) {
                        elemContent.focus();
                    } else {
                        elemContent.fadeIn('fast', function() {
                            elemContent.focus();
                        });
                    } 
                }
            }
        }
        -->
        ${rawString(updateCallbackPostVisibJs)}
    };

    updateItemVisibility(); <#-- Needed for page refreshes to work and to set initial visibility (if FTL doesn't do it) -->
    allItems.change(updateItemVisibility);
});

</#macro>

<#-- Scipio: local macro where cells of label and widget areas are inverted and tweaked 
    NOTE: You can set labelContentFieldsType="default-compact" for a different default look
    NOTE: the labelContent bypasses the regular @field parent-child field relation; set markup with labelContentFieldsType -->
<#macro commonInvField args={} inlineArgs...>
    <#local args = mergeArgMaps(args, inlineArgs, {
        "type":"generic", "labelContentFieldsType":"default", "postfixColumns":"", "labelContent":"", "labelContentArgs":{}, "widgetAreaClass":"",
        "labelColumns":"", "postfixContent":"", "postfix":false
    })>
    <#local dummy = localsPutAll(args)>

<#--
  <#local gridStyles = getDefaultFieldGridStyles({"labelArea":true, "postfix":true, "postfixColumns":postfixColumns, "widgetPostfixCombined":false})>
  <@row>
    <@cell class=addClassArg(gridStyles.labelArea, "${styles.text_right!}")>
      <#nested>
    </@cell>
    <#local id = (getRequestVar("scipioLastFieldInfo").id)!"">
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
  <#if !labelColumns?has_content>
    <#local labelColumns = 2>
  </#if>
  <#if postfixContent?is_directive || postfixContent?has_content>
    <#local postfix = true>
  </#if>
  <#--<#local widgetAreaClass = addClassArg(widgetAreaClass, styles.text_right!)>-->
  <#if labelContent?has_content || labelContent?is_directive>
    <#local labelContentArgs = {"labelContentFieldsType":labelContentFieldsType, "labelContent":labelContent, "labelContentArgs":labelContentArgs}>
    <#local labelContent = commonInvFieldLabelRender>
  </#if>
  <@field inverted=true args=args postfix=postfix
    labelContent=labelContent labelContentArgs=labelContentArgs
    labelColumns=labelColumns postfixColumns=postfixColumns><#nested></@field>
</#macro>

<#-- this is an ugly kludge needed due to only having one #nested in freemarker -->
<#macro commonInvFieldLabelRender args={}>
  <@fields type=args.labelContentFieldsType ignoreParentField=true>
    <@contentArgRender content=args.labelContent args=args.labelContentArgs />
  </@fields>
</#macro>

<#-- Almost same as commonInvField currently, except we assume it's a radio or checkbox and set widget area size to 1 column -->
<#macro checkAddressInvField args={} inlineArgs...>
    <#local args = mergeArgMaps(args, inlineArgs, {
        "labelColumns":""
    })>
    <#local dummy = localsPutAll(args)>
  <#if !labelColumns?has_content>
    <#local labelColumns = 1>
  </#if>
    <#-- FORCE this to zero -->
    <#local labelSmallDiffColumns = 0>
    <@commonInvField labelColumns=labelColumns labelSmallDiffColumns=labelSmallDiffColumns args=inlineArgs />
</#macro>

<#macro addressList>
  <#-- NOTE: These are currently shop-theme-specific styles -->
  <#-- Use simple inline-blocks for this... -->
  <div class="${styles.address_list!}">
    <#nested>
  </div>
</#macro>

<#macro addressEntry ownLine=false>
  <#-- Use simple inline-blocks for this... -->
  <div class="${styles.address_entry!}<#if ownLine> ${styles.address_entry_ownline!}</#if>">
    <#nested>
  </div>
</#macro>

<#macro telecomNumberField params=true label="" fieldNamePrefix="" required=false showExt=true tooltip=false inlineArgs...>
  <#if params?is_boolean>
    <#local params = parameters>
  </#if>
  <#local args = inlineArgs>
  <#local countryCodeName = fieldNamePrefix + (args.countryCodeName)!"countryCode">
  <#local areaCodeName = fieldNamePrefix + (args.areaCodeName)!"areaCode">
  <#local contactNumberName = fieldNamePrefix + (args.contactNumberName)!"contactNumber">
  <#local extensionName = fieldNamePrefix + (args.extensionName)!"extension">
  <@field type="generic" label=label tooltip=tooltip required=required args=args>
      <@field type="input" inline=true size="1" maxlength="10" name=countryCodeName value=((params[countryCodeName])!(args.countryCode)!) tooltip=uiLabelMap.CommonCountryCode required=required/>
      -&nbsp;<@field type="input" inline=true size="2" maxlength="10" name=areaCodeName value=((params[areaCodeName])!(args.areaCode)!) tooltip=uiLabelMap.PartyAreaCode required=required/>
      -&nbsp;<@field type="input" inline=true size="8" maxlength="15" name=contactNumberName value=((params[contactNumberName])!(args.contactNumber)!) tooltip=uiLabelMap.PartyContactNumber required=required/>
      <#if showExt>&nbsp;<span style="white-space: nowrap;">${uiLabelMap.PartyContactExt}&nbsp;<@field type="input" inline=true size="4" maxlength="10" name=extensionName 
        value=((params[extensionName])!(args.extension)!) tooltip=uiLabelMap.PartyExtension /></span></#if>
    <#nested>
  </@field>
</#macro>

<#macro allowSolicitationField params=true name="" inlineArgs...>
  <#if params?is_boolean>
    <#local params = parameters>
  </#if>
  <#local args = inlineArgs>
  <@field type="select" label="${rawLabel('PartyAllowSolicitation')}?" name=name args=args>
    <option></option><#-- NOTE: Empty must be allowed? -->
    <option value="Y"<#if (params[name]!(args.allowSolicitation)!) == "Y"> selected="selected"</#if>>${uiLabelMap.CommonYes}</option>
    <option value="N"<#if (params[name]!(args.allowSolicitation)!) == "N"> selected="selected"</#if>>${uiLabelMap.CommonNo}</option>
  </@field>
</#macro>

<#macro personalTitleField params=true name="" label="" inlineArgs...>
  <#if params?is_boolean>
    <#local params = parameters>
  </#if>
    <#local args = inlineArgs>
    <@field type="select" name=name label=label>
        <#-- Scipio: NOTE: Stock Ofbiz seems to write code that causes pers title to be stored in localized form.
            This is probably an error because the values read from DB become hard to re-localize.
            It is better to store these as values coded in english (and localize on print only). -->
        <#assign personalTitle = params[name]!(args.personalTitle)!"">
        <#if personalTitle?has_content>
          <#local localizedPersTitle = getLocalizedPersonalTitle(personalTitle)!false>
          <#if localizedPersTitle?is_boolean>
            <#-- we failed... -->
            <option value="${personalTitle}">${personalTitle}</option>
            <option value=""></option>
            <option value="Mr.">${uiLabelMap.CommonTitleMr}</option>
            <option value="Mrs.">${uiLabelMap.CommonTitleMrs}</option>
            <option value="Ms.">${uiLabelMap.CommonTitleMs}</option>
            <option value="Dr.">${uiLabelMap.CommonTitleDr}</option>
          <#else>
            <option value=""></option><#-- NOTE: empty is allowed -->
            <option value="Mr."<#if personalTitle == "Mr." || personalTitle == uiLabelMap.CommonTitleMr> selected="selected"</#if>>${uiLabelMap.CommonTitleMr}</option>
            <option value="Mrs."<#if personalTitle == "Mrs." || personalTitle == uiLabelMap.CommonTitleMrs> selected="selected"</#if>>${uiLabelMap.CommonTitleMrs}</option>
            <option value="Ms."<#if personalTitle == "Ms." || personalTitle == uiLabelMap.CommonTitleMs> selected="selected"</#if>>${uiLabelMap.CommonTitleMs}</option>
            <option value="Dr."<#if personalTitle == "Dr." || personalTitle == uiLabelMap.CommonTitleDr> selected="selected"</#if>>${uiLabelMap.CommonTitleDr}</option>
          </#if>
        <#else>
          <#--<option value="">${uiLabelMap.CommonSelectOne}</option>-->
          <option value=""></option><#-- NOTE: empty is allowed -->
          <option value="Mr.">${uiLabelMap.CommonTitleMr}</option>
          <option value="Mrs.">${uiLabelMap.CommonTitleMrs}</option>
          <option value="Ms.">${uiLabelMap.CommonTitleMs}</option>
          <option value="Dr.">${uiLabelMap.CommonTitleDr}</option>
        </#if>
    </@field>   
</#macro>


<#function getLocalizedPersonalTitle personalTitle>
    <#-- Scipio: NOTE: Stock Ofbiz seems to write code that causes pers title to be stored in localized form.
        This is probably an error because the values read from DB become hard to re-localize.
        It is better to store these as values coded in english (and localize on print only). 
        But otherwise, this function is a best-effort only. -->
    <#if personalTitle == "Mr." || personalTitle == uiLabelMap.CommonTitleMr><#return uiLabelMap.CommonTitleMr></#if>
    <#if personalTitle == "Mrs." || personalTitle == uiLabelMap.CommonTitleMrs><#return uiLabelMap.CommonTitleMrs></#if>
    <#if personalTitle == "Ms." || personalTitle == uiLabelMap.CommonTitleMs><#return uiLabelMap.CommonTitleMs></#if>
    <#if personalTitle == "Dr." || personalTitle == uiLabelMap.CommonTitleDr><#return uiLabelMap.CommonTitleDr></#if>
    <#-- return nothing if could not detect so caller can default -->
</#function>
