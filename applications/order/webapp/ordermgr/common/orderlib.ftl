<#-- SCIPIO: Common Order utilities and definitions library
    May be imported by other applications' templates, preferably using:
      <#import "component://order/webapp/ordermgr/common/orderlib.ftl" as orderlib>
    NOTE: For this application's own templates, please include common.ftl instead (which includes this). -->

<#-- SCIPIO: local macro where cells of label and widget areas are inverted and tweaked 
    NOTE: the labelContent bypasses the regular @field parent-child field relation; set markup with labelContentFieldsType-->
<#macro checkoutInvField type="generic" labelContentFieldsType="default-compact" postfixColumns="" labelContent="" labelContentArgs={} widgetAreaClass="" labelColumns="" postfixContent="" postfix=false inlineArgs...>
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
  <#if postfixContent?is_directive || postfixContent?has_content>
    <#local postfix = true>
  </#if>
  <#local widgetAreaClass = addClassArg(widgetAreaClass, styles.text_right!)>
  <#if labelContent?has_content || labelContent?is_directive>
    <#local labelContentArgs = {"labelContentFieldsType":labelContentFieldsType, "labelContent":labelContent, "labelContentArgs":labelContentArgs}>
    <#local labelContent = checkoutInvFieldLabelRender>
  </#if>
  <@field type=type inverted=true args=inlineArgs widgetAreaClass=widgetAreaClass postfix=postfix
    labelContent=labelContent labelContentArgs=labelContentArgs
    postfixContent=postfixContent labelColumns=labelColumns postfixColumns=postfixColumns><#nested></@field>
</#macro>

<#-- this is an ugly kludge needed due to only having one #nested in freemarker -->
<#macro checkoutInvFieldLabelRender args={}>
  <@fields type=args.labelContentFieldsType ignoreParentField=true>
    <@contentArgRender content=args.labelContent args=args.labelContentArgs />
  </@fields>
</#macro>

<#-- #### PAYMENT INFO MACROS #### -->

<#macro payMethInfoPanel title updateAction="">
  <#-- SCIPIO: Every non-new may meth option gets a panel, for consistency
      because the "new" pay meths show their fields below the title, currently
      doing the same for panel, though not sure about look
  <@row>
    <@cell columns=2>
      <@heading>${title}</@heading>
    </@cell>
    <@cell columns=10>
  -->
    <@heading>${title}</@heading>
    <@row>
      <@cell small=6>
      <div class="pay-meth-info-panel-container">
        <#assign bottomContent = false>
        <#if updateAction?has_content>
          <#assign bottomContent>
            <a href="javascript:submitForm(document.getElementById('checkoutInfoForm'), '${updateAction}', '${paymentMethod.paymentMethodId}');" class="${styles.link_nav!} ${styles.action_update!} panel-update-link">${uiLabelMap.CommonUpdate}</a>
          </#assign>
        </#if>
        <@panel bottomContent=bottomContent class="+pay-meth-info-panel">
          <#nested>
        </@panel>
      </div>
      </@cell>
    </@row>
  <#--
    </@cell>
  </@row>
  -->
  <#--<br/>-->
</#macro>

<#macro payMethAmountField type="pay-meth" payMethId="" params=true id=true name=true cart=false>
      <#if cart?is_boolean>
        <#local cart = getShoppingCart()>
      </#if>
      <#if params?is_boolean>
        <#local params = parameters>
      </#if>
      <#if name?is_boolean>
        <#local name = "amount_${payMethId}">
      </#if>
      <#if id?is_boolean>
        <#local id = name>
      </#if>
      <#assign realCurPayAmount = "">
      <#if type == "simple" || type == "new-pay-meth">
          <#assign fieldValue = params[name]!>
      <#else>
          <#assign curPayAmountStr = "">
          <#assign fieldValue = "">
          <#if params[name]??>
            <#assign fieldValue = params[name]>
          <#else>
            <#-- SCIPIO: changed to use getPaymentOrigAmount (new method) instead of getPaymentAmount because we want to be consistent
                and only show the amounts if the user originally entered one. Otherwise, leave null, and submit will recalculate as needed: cart.getPaymentAmount(paymentMethod.paymentMethodId) -->
            <#assign curPayAmountStr><#if ((cart.getPaymentOrigAmount(payMethId)!0) > 0)>${cart.getPaymentOrigAmount(payMethId)?string("##0.00")}</#if></#assign>
            <#-- Also get the real current amount, but we'll only show it in tooltip (if use this amount as value, sometimes resubmission issues when going back pages) -->
            <#if ((cart.getPaymentAmount(payMethId)!0) > 0)>
              <#assign realCurPayAmount = cart.getPaymentAmount(payMethId)>
            </#if>
            <#-- SCIPIO: NOTE: We ONLY set the previous pay amount as field value if the payments are adequate to cover the current amount (NOTE: currently this definition is very strict - see ShoppingCart).
                Otherwise, the amount specified here is likely to be invalid if resubmitted as-is (as in stock it does not really function as a "bill up to" amount).
                FIXME?: There is an inconsistency here: user may have left this empty, but re-viewing payment will populate this field. Currently ShoppingCart
                    offers no way to distinguish between user-entered and validation-determined amounts. -->
            <#if cart.isPaymentsAdequate()>
              <#assign fieldValue = curPayAmountStr>
            </#if>
          </#if>
      </#if> 
      <#assign fieldDescription = uiLabelMap.AccountingLeaveEmptyForMaximumAmount + ".">
      <#assign realCurPayAmountFullStr = "">
      <#if realCurPayAmount?has_content>
        <#assign realCurPayAmountFullStr>${rawLabel('CommonCurrent')}: <@ofbizCurrency amount=realCurPayAmount isoCode=cart.getCurrency()/></#assign>
      </#if>

      <#assign postfixContent>
        <@row>
          <@cell small=8><#-- class="+${styles.float_right!}"-->
            <@commonMsg type="info-important" closable=false>${fieldDescription}</@commonMsg>
          </@cell>
        </@row>
      </#assign>
      <#-- SCIPIO: NOTE: Stock ofbiz labels this as "bill up to", but it does NOT function as a "bill up to" but rather as an exact amount.
          Unless this behavior is changed, show "Amount" instead of "BillUpTo": uiLabelMap.OrderBillUpTo -->
      <@field type="input" label=uiLabelMap.AccountingAmount size="5" id=id name=name value=fieldValue 
        tooltip=realCurPayAmountFullStr postfix=true collapsePostfix=false postfixColumns=9 postfixContent=postfixContent />  
</#macro>

<#-- SCIPIO: Payment method content and markup.
        This pattern allows to avoid duplicating the control/loop/ordering logic and keeps the definitions for each pay method together so easier to follow alongside the original.
        The macro is invoked in steps with a different type each time. 
        WARN: Freemarker language limits use of this because can't define nested macros. Only for this page. -->
<#macro paymentMethodContent showPrimary=false showSupplemental=false showSelect=false showDetails=false cart=false>
      <#if cart?is_boolean>
        <#local cart = getShoppingCart()>
      </#if>
      <#local detailHeadingArgs = {}><#-- current is good: {"relLevel":+1} -->

      <#local primSupplSuffix = "">
      <#if showPrimary>
        <#local primSupplSuffix = "_primary">
      </#if>
      <#if showSupplemental>
        <#local primSupplSuffix = "_suppl">
      </#if>

      <@fields type="inherit-all" checkboxType="simple-standard" inlineItems=false>

      <#if showPrimary>
        <#if productStorePaymentMethodTypeIdMap.EXT_OFFLINE??>
          <#assign methodLabel>${rawString(getLabel('PaymentMethodType.description.EXT_OFFLINE', 'AccountingEntityLabels'))} (${rawLabel('OrderMoneyOrder')})</#assign>
          <#if showSelect>
            <#-- ${uiLabelMap.OrderPaymentOfflineCheckMoney} -->
            <#assign dummy = registerFieldContent({"fieldId":"checkOutPaymentId_OFFLINE", "contentId":"content_OFFLINE"})>
            <@field type="radio" id="checkOutPaymentId_OFFLINE" name="checkOutPaymentId" value="EXT_OFFLINE" checked=(selectedCheckOutPaymentIdList?seq_contains("EXT_OFFLINE")) 
              class="+pay-select-radio pay-select-field" label=methodLabel /><#--tooltip=(getPayMethTypeDesc("EXT_OFFLINE")!)-->
          </#if>
          <#if showDetails>
            <@section containerId="content_OFFLINE" containerClass="+pay-meth-content" containerStyle="display:none;"><#-- title=methodLabel -->
              <@payMethInfoPanel title=methodLabel>
                <#-- SCIPIO: TODO?: These descriptions could probably be integrated into the entity values using get("xxx", locale)... -->
                <p>${uiLabelMap.OrderPaymentDescOffline}</p>
              </@payMethInfoPanel>
              <@payMethAmountField payMethId="EXT_OFFLINE"/>
            </@section>
          </#if>
        </#if>
        <#if productStorePaymentMethodTypeIdMap.EXT_COD??>
          <#assign methodLabel>${rawString(getLabel('PaymentMethodType.description.EXT_COD', 'AccountingEntityLabels'))} (${rawLabel('OrderCOD')})</#assign>
          <#if showSelect>
            <#assign dummy = registerFieldContent({"fieldId":"checkOutPaymentId_COD", "contentId":"content_COD"})>
            <@field type="radio" type="radio" id="checkOutPaymentId_COD" name="checkOutPaymentId" value="EXT_COD" checked=(selectedCheckOutPaymentIdList?seq_contains("EXT_COD")) 
              class="+pay-select-radio pay-select-field" label=methodLabel /><#--tooltip=(getPayMethTypeDesc("EXT_COD")!)-->
          </#if>
          <#if showDetails>
            <@section containerId="content_COD" containerClass="+pay-meth-content" containerStyle="display:none;"><#-- title=methodLabel-->
              <@payMethInfoPanel title=methodLabel>
                <p>${uiLabelMap.OrderPaymentDescCOD}</p>
              </@payMethInfoPanel>
              <@payMethAmountField payMethId="EXT_COD"/>
            </@section>
          </#if>
        </#if>
        
        <#if productStorePaymentMethodTypeIdMap.EXT_PAYPAL??>
          <#if showSelect>
            <#assign dummy = registerFieldContent({"fieldId":"checkOutPaymentId_PAYPAL", "contentId":"content_PAYPAL"})>
            <@field type="radio" id="checkOutPaymentId_PAYPAL" name="checkOutPaymentId" value="EXT_PAYPAL" checked=(selectedCheckOutPaymentIdList?seq_contains("EXT_PAYPAL")) 
              class="+pay-select-radio pay-select-field" label=uiLabelMap.AccountingPayWithPayPal /><#--tooltip=(getPayMethTypeDesc("EXT_PAYPAL")!) -->
          </#if>
          <#if showDetails>
            <@section containerId="content_PAYPAL" containerClass="+pay-meth-content" containerStyle="display:none;"><#-- title=uiLabelMap.AccountingPayWithPayPal-->
              <@payMethInfoPanel title=uiLabelMap.AccountingPayWithPayPal>
                <p>${uiLabelMap.OrderPaymentDescPaypal}</p>
              </@payMethInfoPanel>
            </@section>
          </#if>
        </#if>
        <#if productStorePaymentMethodTypeIdMap.EXT_LIGHTNING??>
          <#if showSelect>
            <#assign dummy = registerFieldContent({"fieldId":"checkOutPaymentId_LIGHTNING", "contentId":"content_LIGHTNING"})>
            <@field type="radio" id="checkOutPaymentId_LIGHTNING" name="checkOutPaymentId" value="EXT_LIGHTNING" checked=(selectedCheckOutPaymentIdList?seq_contains("EXT_LIGHTNING")) 
              class="+pay-select-radio pay-select-field" label=uiLabelMap.AccountingPayWithLightning /><#--tooltip=(getPayMethTypeDesc("EXT_LIGHTNING")!) -->
          </#if>
          <#if showDetails>
            <@section containerId="content_LIGHTNING" containerClass="+pay-meth-content" containerStyle="display:none;"><#-- title=uiLabelMap.AccountingPayWithBitcoin-->
              <@payMethInfoPanel title=uiLabelMap.AccountingPayWithLightning>
                <p>${uiLabelMap.OrderPaymentDescLightning}</p>
                <#assign xbtAmount = Static["org.ofbiz.common.uom.UomWorker"].convertDatedUom(cart.getCartCreatedTime(),cart.getDisplayGrandTotal()!0, cart.getCurrency(),"XBT",dispatcher,true)>
                <p>${uiLabelMap.CommonAmount}: <@ofbizCurrency amount=xbtAmount?default(0.00) rounding="8" isoCode="XBT"/></p>
              </@payMethInfoPanel>
            </@section>
          </#if>
        </#if>
        <#if productStorePaymentMethodTypeIdMap.EXT_WORLDPAY??>
          <#if showSelect>
            <#assign dummy = registerFieldContent({"fieldId":"checkOutPaymentId_WORLDPAY", "contentId":"content_WORLDPAY"})>
            <@field type="radio" id="checkOutPaymentId_WORLDPAY" name="checkOutPaymentId" value="EXT_WORLDPAY" checked=(selectedCheckOutPaymentIdList?seq_contains("EXT_WORLDPAY")) 
              class="+pay-select-radio pay-select-field" label=uiLabelMap.AccountingPayWithWorldPay /><#--tooltip=(getPayMethTypeDesc("EXT_WORLDPAY")!)-->
          </#if>
          <#if showDetails>
            <@section containerId="content_WORLDPAY" containerClass="+pay-meth-content" containerStyle="display:none;"><#-- title=uiLabelMap.AccountingPayWithWorldPay-->
              <@payMethInfoPanel title=uiLabelMap.AccountingPayWithWorldPay>
                <p>${uiLabelMap.OrderPaymentDescWorldpay}</p>
              </@payMethInfoPanel>
            </@section>
          </#if>
        </#if>
    
        <#if productStorePaymentMethodTypeIdMap.EXT_IDEAL??>
          <#if showSelect>
            <#assign dummy = registerFieldContent({"fieldId":"checkOutPaymentId_IDEAL", "contentId":"content_IDEAL"})>
            <@field type="radio" id="checkOutPaymentId_IDEAL" name="checkOutPaymentId" value="EXT_IDEAL" checked=(selectedCheckOutPaymentIdList?seq_contains("EXT_IDEAL")) 
              class="+pay-select-radio pay-select-field" label=uiLabelMap.AccountingPayWithiDEAL /><#--tooltip=(getPayMethTypeDesc("EXT_IDEAL")!) -->
          </#if>
          <#if showDetails>
            <@section containerId="content_IDEAL" containerClass="+pay-meth-content" containerStyle="display:none;"><#-- title=uiLabelMap.AccountingPayWithiDEAL-->
              <@payMethInfoPanel title=uiLabelMap.AccountingPayWithiDEAL>
                <p>${uiLabelMap.OrderPaymentDescIdeal}</p>
              </@payMethInfoPanel>
              <#if issuerList?has_content>
                <@field type="select" name="issuer" id="issuer" label=uiLabelMap.AccountingBank>
                  <#list issuerList as issuer>
                    <option value="${issuer.getIssuerID()}"<#if issuer.getIssuerID() == (parameters.issuer!)> selected="selected"</#if>>${issuer.getIssuerName()}</option>
                  </#list>
                </@field>
              </#if>
            </@section>
          </#if>
        </#if>
        
        <#if productStorePaymentMethodTypeIdMap.EXT_STRIPE??>
          <#if showSelect>
            <#assign dummy = registerFieldContent({"fieldId":"checkOutPaymentId_STRIPE", "contentId":"content_STRIPE"})>
            <@field type="radio" id="checkOutPaymentId_STRIPE" name="checkOutPaymentId" value="EXT_STRIPE" checked=(selectedCheckOutPaymentIdList?seq_contains("EXT_STRIPE")) 
              class="+pay-select-radio pay-select-field" label=uiLabelMap.AccountingPayWithStripe /><#--tooltip=(getPayMethTypeDesc("EXT_STRIPE")!) -->
          </#if>
          <#if showDetails>
            <@section containerId="content_STRIPE" containerClass="+pay-meth-content" containerStyle="display:none;"><#-- title=uiLabelMap.AccountingPayWithStripe-->
              <@payMethInfoPanel title="">
                <#assign pk = Static["com.ilscipio.scipio.accounting.payment.stripe.StripeHelper"].getPublishableKey(request)!>
                <#assign stripeIntegrationMode = Static["com.ilscipio.scipio.accounting.payment.stripe.StripeHelper"].getIntegrationMode(request)!>
                <#assign options = {
                  "locale": Static["org.ofbiz.base.util.UtilHttp"].getLocale(request).getLanguage()
                }>
                <#assign style = {
                  "base": {
                    "color": '#32325d',
                    "lineHeight": '25px',
                    "fontFamily": '"Helvetica Neue", Helvetica, sans-serif',
                    "fontSmoothing": 'antialiased',
                    "fontSize": '20px',
                    "::placeholder": {
                      "color": '#aab7c4'
                    }
                  },
                  "invalid": {
                    "color": '#fa755a',
                    "iconColor": '#fa755a'
                  }
                }>
                <#assign hooks = {
                  "stripe.preinit" : wrapRawScript("function() { console.debug('stripe.preinit hook'); return true; }"),
                  "stripe.postinit" : wrapRawScript("function() { console.debug('stripe.postinit hook'); return true; }"),
                  "validation" : wrapRawScript("function() { console.debug('stripe.validation hook'); return true; }"),
                  "payment.preprocess" : wrapRawScript("function() { console.debug('payment.preprocess hook'); return true; }"),
                  "payment.postprocess" : wrapRawScript("function() { console.debug('payment.postprocess hook'); return true; }"),
                  "order.preprocess" : wrapRawScript("function() { console.debug('order.preprocess hook'); return true; }"),
                  "order.postprocess" : wrapRawScript("function() { console.debug('order.postprocess hook'); return true; }")
                }/>

                <#-- NOTE: This is meant for normal checkout, not OnePageCheckout; remove multiStepCheckout={"capture":true} in order to switch to OnePageCheckout -->
                <@renderStripe mode=stripeIntegrationMode pk=pk! options=options! style=style! checkoutFormId="orderSubmitForm" checkoutButtonId="processButton"
                  checkOutPaymentSel=("input[name=checkOutPaymentId]") hooks=hooks debug=true />
              </@payMethInfoPanel>
            </@section>
          </#if>
        </#if>

        <#if productStorePaymentMethodTypeIdMap.EXT_REDSYS??>
          <#if showSelect>
            <#assign dummy = registerFieldContent({"fieldId":"checkOutPaymentId_REDSYS", "contentId":"content_REDSYS"})>
            <@field type="radio" id="checkOutPaymentId_REDSYS" name="checkOutPaymentId" value="EXT_REDSYS" checked=(selectedCheckOutPaymentIdList?seq_contains("EXT_REDSYS")) 
              class="+pay-select-radio pay-select-field" label=uiLabelMap.AccountingPayWithRedsys />
          </#if>
          <#if showDetails>
            <@section containerId="content_REDSYS" containerClass="+pay-meth-content" containerStyle="display:none;">
              <@payMethInfoPanel title=uiLabelMap.AccountingPayWithRedsys>
                <p>${uiLabelMap.OrderPaymentDescRedsys}</p>
              </@payMethInfoPanel>
            </@section>
          </#if>
        </#if>

        <#if productStorePaymentMethodTypeIdMap.CREDIT_CARD??>
          <#-- User's credit cards -->
          <#if (paymentMethodListsByType.CREDIT_CARD)?has_content>
            <#list paymentMethodListsByType.CREDIT_CARD as paymentMethodLocal>
              <#assign paymentMethod = paymentMethodLocal><#-- SCIPIO: workaround for access from macros -->
              <#assign creditCard = paymentMethod.getRelatedOne("CreditCard", false) />
              <#assign methodShortInfo><@formattedCreditCardShort creditCard=creditCard paymentMethod=paymentMethod verbose=true /></#assign>
              <#if showSelect>
                <#assign dummy = registerFieldContent({"fieldId":"checkOutPaymentId_${paymentMethod.paymentMethodId}", "contentId":"content_${paymentMethod.paymentMethodId}"})>
               <#-- SCIPIO: NOTE: I've changed this from checkbox to radio, because I'm not sure why this would be an addon while EFT is not (from user POV)
                    cart.isPaymentSelected(paymentMethod.paymentMethodId) -->
                <@field type="radio" id="checkOutPaymentId_${paymentMethod.paymentMethodId}" name="checkOutPaymentId" value=paymentMethod.paymentMethodId checked=(selectedCheckOutPaymentIdList?seq_contains(paymentMethod.paymentMethodId)) 
                  class="+pay-select-radio pay-select-field" label=wrapAsRaw("${uiLabelMap.AccountingCreditCard}: ${methodShortInfo}", 'htmlmarkup') /><#--tooltip=(getPayMethTypeDesc("CREDIT_CARD")!) -->
              </#if>
              <#if showDetails>
                <@section containerId="content_${paymentMethod.paymentMethodId}" containerClass="+pay-meth-content" containerStyle="display:none;"><#--title=uiLabelMap.AccountingCreditCard-->
                  <@payMethInfoPanel title=uiLabelMap.AccountingCreditCard updateAction='EC'>
                    <@formattedCreditCardDetail creditCard=creditCard paymentMethod=paymentMethod />
                    <#if (paymentMethod.get("description", locale))?has_content><br/>(${paymentMethod.get("description", locale)})</#if>
                  </@payMethInfoPanel>
                  <@payMethAmountField payMethId=paymentMethod.paymentMethodId/>
                </@section>
              </#if>
            </#list>
          </#if>
        
          <#-- SCIPIO: JS-based new credit card option -->
          <#assign methodShortInfo><em>${uiLabelMap.CommonNew}</em></#assign>
          <#if showSelect>
            <#assign dummy = registerFieldContent({"fieldId":"newCreditCard", "contentId":"content__NEW_CREDIT_CARD"})>
            <@field type="radio" id="newCreditCard" name="checkOutPaymentId" value="_NEW_CREDIT_CARD_" 
              checked=(selectedCheckOutPaymentIdList?seq_contains("_NEW_CREDIT_CARD_")) class="+pay-select-radio pay-select-field" 
              label=wrapAsRaw("${uiLabelMap.AccountingCreditCard}: ${methodShortInfo}", 'htmlmarkup') /><#--tooltip=(getPayMethTypeDesc("CREDIT_CARD")!) -->
          </#if>
          <#if showDetails>
            <@section containerId="content__NEW_CREDIT_CARD" containerClass="+new-item-selection-content pay-meth-content" 
                containerStyle="display:none;" title=uiLabelMap.AccountingCreditCard><#-- let JS do the following (there is no point; even if do it, still need JS for page refresh): <#if (!selectedCheckOutPaymentIdList?seq_contains("_NEW_CREDIT_CARD_"))> style="display:none;"</#if> -->
              
              <#-- SCIPIO: FIELDS BASED ON editcreditcard.ftl -->
              <#assign titleOnCard = "">
              <#assign firstNameOnCard = "">
              <#assign middleNameOnCard = "">
              <#assign lastNameOnCard = "">
              <#if person?has_content>
                <#assign titleOnCard = person.personalTitle!>
                <#assign firstNameOnCard = person.firstName!>
                <#assign middleNameOnCard = person.middleName!>
                <#assign lastNameOnCard = person.lastName!>
              </#if>
              <@fields type="inherit-all" fieldArgs={"gridArgs":{"totalLarge":8}}>
                <@render resource="component://shop/widget/CustomerScreens.xml#creditCardFields" 
                  ctxVars={
                    "ccfFieldNamePrefix": "newCreditCard_",
                    "ccfFallbacks":{
                        "titleOnCard":titleOnCard,
                        "firstNameOnCard":firstNameOnCard, 
                        "middleNameOnCard":middleNameOnCard, 
                        "lastNameOnCard":lastNameOnCard
                    },
                    "ccfParams":newCreditCardParams!parameters
                  }/>
              </@fields>
              <@field type="generic" label=uiLabelMap.PartyBillingAddress>
                <@render resource="component://shop/widget/CustomerScreens.xml#billaddresspickfields" 
                    ctxVars={
                        "bapfUseNewAddr":true,
                        "bapfUseUpdate":true,
                        "bapfUpdateLink":"javascript:submitForm(document.checkoutInfoForm, 'EA', '_CONTACT_MECH_ID_');",
                        "bapfNewAddrInline":true, 
                        "bapfFieldNamePrefix":"newCreditCard_",
                        "bapfNewAddrContentId":"newcreditcard_newbilladdrcontent",
                        "bapfPickFieldClass":"new-cc-bill-addr-pick-radio",
                        "bapfNewAddrFieldId":"newcreditcard_newaddrradio",
                        "bapfParams":newCreditCardParams!parameters,
                        "bapfNewAddressFieldsWrapperArgs": {
                            "type":"default",
                            "ignoreParentField":true,
                            "fieldArgs": {
                                "gridArgs": {
                                    "totalLarge":8
                                }
                            }
                        }
                    }/>
              </@field>

              <#if userHasAccount?? && userHasAccount>
                <#-- SCIPIO: This should be the opposite of "single use payment"... so we use form submit hook to set the hidden field -->
                <@field type="checkbox" checkboxType="simple-standard" id="newCreditCard_saveToAccount" name="saveToAccount__NEW_CREDIT_CARD_" value="Y" 
                    checked=((newCreditCardParams.singleUsePayment__NEW_CREDIT_CARD_!"") != "Y") label=uiLabelMap.OrderSaveToAccount/>
                <input type="hidden" id="singleUsePayment__NEW_CREDIT_CARD_" name="singleUsePayment__NEW_CREDIT_CARD_" value="" />
              </#if>
              <@payMethAmountField payMethId="_NEW_CREDIT_CARD_" type="new-pay-meth" params=newCreditCardParams />
            </@section>
          </#if>
        </#if>

        <#if productStorePaymentMethodTypeIdMap.EFT_ACCOUNT??>
          <#-- User's EFT accounts -->
          <#if (paymentMethodListsByType.EFT_ACCOUNT)?has_content>
            <#list paymentMethodListsByType.EFT_ACCOUNT as paymentMethodLocal>
              <#assign paymentMethod = paymentMethodLocal><#-- SCIPIO: workaround for access from macros -->
              <#assign eftAccount = paymentMethod.getRelatedOne("EftAccount", false) />
              <#assign methodShortInfo><@formattedEftAccountShort eftAccount=eftAccount paymentMethod=paymentMethod /></#assign>
              <#if showSelect>
                <#assign dummy = registerFieldContent({"fieldId":"checkOutPaymentId_${paymentMethod.paymentMethodId}", "contentId":"content_${paymentMethod.paymentMethodId}"})>
                <@field type="radio" id="checkOutPaymentId_${paymentMethod.paymentMethodId}" name="checkOutPaymentId" value=(paymentMethod.paymentMethodId) checked=(selectedCheckOutPaymentIdList?seq_contains(paymentMethod.paymentMethodId)) 
                  class="+pay-select-radio pay-select-field" label=wrapAsRaw("${uiLabelMap.AccountingEFTAccount}: ${methodShortInfo}", 'htmlmarkup') /><#--tooltip=(getPayMethTypeDesc("EFT_ACCOUNT")!) -->
              </#if>
              <#if showDetails>
                <@section containerId="content_${paymentMethod.paymentMethodId}" containerClass="+pay-meth-content" containerStyle="display:none;"><#--title=uiLabelMap.AccountingEFTAccount-->
                  <@payMethInfoPanel title=uiLabelMap.AccountingEFTAccount updateAction='EE'>
                    <@formattedEftAccountDetail eftAccount=eftAccount paymentMethod=paymentMethod />
                    <#if paymentMethod.get("description", locale)?has_content><br/>(${paymentMethod.get("description", locale)})</#if>
                  </@payMethInfoPanel>
                  <#-- SCIPIO: NOTE: This field was added by us, was missing for EFT accounts -->
                  <@payMethAmountField payMethId=paymentMethod.paymentMethodId/>
                </@section>
              </#if>
            </#list>
          </#if>

          <#-- SCIPIO: JS-based new eft account option -->
          <#assign methodShortInfo><em>${uiLabelMap.CommonNew}</em></#assign>
          <#if showSelect>
            <#assign dummy = registerFieldContent({"fieldId":"newEftAccount", "contentId":"content__NEW_EFT_ACCOUNT_"})>
            <@field type="radio" id="newEftAccount" name="checkOutPaymentId" value="_NEW_EFT_ACCOUNT_" checked=(selectedCheckOutPaymentIdList?seq_contains("_NEW_EFT_ACCOUNT_"))
              class="+pay-select-radio pay-select-field" label=wrapAsRaw("${uiLabelMap.AccountingEFTAccount}: ${methodShortInfo}", 'htmlmarkup') /><#--tooltip=(getPayMethTypeDesc("EFT_ACCOUNT")!) -->
          </#if>
          <#if showDetails>
            <@section containerId="content__NEW_EFT_ACCOUNT_" containerClass="+new-item-selection-content pay-meth-content" 
                containerStyle="display:none;" title=uiLabelMap.AccountingEFTAccount><#-- let JS do it: <#if (!selectedCheckOutPaymentIdList?seq_contains("_NEW_EFT_ACCOUNT_"))> style="display:none;"</#if> -->
              
              <#-- SCIPIO: FIELDS BASED ON editeftaccount.ftl -->
              <#assign nameOnAccount = "">
              <#if person?has_content>
                <#-- TODO: Unhardcode -->
                <#assign nameOnAccount = "${person.firstName!} ${person.lastName!}">
              </#if>
              <@fields type="inherit-all" fieldArgs={"gridArgs":{"totalLarge":8}}>
                <@render resource="component://shop/widget/CustomerScreens.xml#eftAccountFields" 
                  ctxVars={
                    "eafFieldNamePrefix": "newEftAccount_",
                    "eafFallbacks":{"nameOnAccount":nameOnAccount},
                    "eafParams":newEftAccountParams!parameters
                    } />
              </@fields>
              <@field type="generic" label=uiLabelMap.PartyBillingAddress>
                <@render resource="component://shop/widget/CustomerScreens.xml#billaddresspickfields" 
                    ctxVars={
                        "bapfUseNewAddr":true,
                        "bapfUseUpdate":true, 
                        "bapfUpdateLink":"javascript:submitForm(document.checkoutInfoForm, 'EA', '_CONTACT_MECH_ID_');",
                        "bapfNewAddrInline":true, 
                        "bapfFieldNamePrefix":"newEftAccount_",
                        "bapfNewAddrContentId":"neweftaccount_newbilladdrcontent",
                        "bapfPickFieldClass":"new-eft-bill-addr-pick-radio",
                        "bapfNewAddrFieldId":"neweftaccount_newaddrradio",
                        "bapfParams":newEftAccountParams!parameters,
                        "bapfNewAddressFieldsWrapperArgs": {
                            "type":"default",
                            "ignoreParentField":true,
                            "fieldArgs": {
                                "gridArgs": {
                                    "totalLarge":8
                                }
                            }
                        }
                    }/>
              </@field>

              <#if userHasAccount?? && userHasAccount>
                <#-- SCIPIO: This should be the opposite of "single use payment"... so we use form submit hook to set the hidden field -->
                <@field type="checkbox" checkboxType="simple-standard" id="newEftAccount_saveToAccount" name="saveToAccount__NEW_EFT_ACCOUNT_" 
                    value="Y" checked=((newEftAccountParams.singleUsePayment__NEW_EFT_ACCOUNT_!"") != "Y") label=uiLabelMap.OrderSaveToAccount/>
                <input type="hidden" id="singleUsePayment__NEW_EFT_ACCOUNT_" name="singleUsePayment__NEW_EFT_ACCOUNT_" value="" />
              </#if>
              <@payMethAmountField payMethId="_NEW_EFT_ACCOUNT_" params=newEftAccountParams type="new-pay-meth" />
            </@section>
          </#if>
        </#if>

        <#-- SCIPIO: "Additional Payment Options" radio, which leads further below -->
        <#if showSelect>
          <#if (productStorePaymentMethodTypeIdMap.EXT_BILLACT?? && billingAccountList?has_content) || productStorePaymentMethodTypeIdMap.GIFT_CARD??>
            <#assign methodLabel>${uiLabelMap.AccountingAdditionalPaymentMethods} (<#rt>
                <#if (productStorePaymentMethodTypeIdMap.EXT_BILLACT?? && billingAccountList?has_content)><#t>
                ${uiLabelMap.AccountingBillingAccount}<#t>
                <#if (productStorePaymentMethodTypeIdMap.GIFT_CARD??)>, </#if><#t>
                </#if><#t>
                <#if (productStorePaymentMethodTypeIdMap.GIFT_CARD??)>${uiLabelMap.AccountingGiftCard}</#if><#t>
            )</#assign><#lt>
            <#-- Special case: click never hides content -->
            <#assign dummy = registerFieldContent({"fieldId":"supplPayMeth", "contentId":"paymeth_supplemental", "noHideContent":true})>
            <@field type="radio" id="supplPayMeth" name="checkOutPaymentId" value="" checked=false label=wrapAsRaw(methodLabel, 'htmlmarkup') 
              class="+pay-select-radio pay-select-field" /><#-- checked handled by JS for the moment --><#--tooltip=uiLabelMap.AccountingAdditionalPaymentMethods -->
          </#if>
        </#if>

      </#if><#-- /showPrimary -->

        <#-- SCIPIO: The following fields are supplemental and difficult to manage, so will not try to
            combine into the above (the pre-selection logic becomes too complicated and ambiguous when we have entries
            for them in both primary and supplemental, and the javascript becomes too heavy). 
            Instead, we have a "other/additional" radio button in the primary to fulfill the HTML requirements. -->

        <#-- special billing account functionality to allow use w/ a payment method -->
        <#if productStorePaymentMethodTypeIdMap.EXT_BILLACT??>
          <#if billingAccountList?has_content>
              <#if showSupplemental>
                <#if showSelect>
                  <#-- MUST SUBMIT EMPTY VALUE FOR checkOutPaymentId -->
                  <#assign dummy = registerFieldContent({"fieldId":"checkOutPaymentId_billingAccount${primSupplSuffix}", "contentId":"content_BILLACT${primSupplSuffix}"})>
                  <@field type="checkbox" id="checkOutPaymentId_billingAccount${primSupplSuffix}" name="checkOutPaymentId" value="" checked=(selectedBillingAccountId?has_content) 
                    class="+pay-select-checkbox pay-select-field" label=uiLabelMap.AccountingBillingAccount />
                </#if>
              </#if>
              <#if showDetails && showSupplemental>
                <@section containerId="content_BILLACT${primSupplSuffix}" containerClass="+pay-meth-content" containerStyle="display:none;" title=uiLabelMap.AccountingBillingAccount>
                  <@field type="select" name="billingAccountId" id="billingAccountId${primSupplSuffix}" label=uiLabelMap.FormFieldTitle_billingAccountId>
                    <option value=""></option>
                    <#list billingAccountList as billingAccount>
                      <#assign availableAmount = billingAccount.accountBalance>
                      <#assign accountLimit = billingAccount.accountLimit>
                      <option value="${billingAccount.billingAccountId}"<#if billingAccount.billingAccountId == selectedBillingAccountId> selected="selected"</#if>><#rt>
                        <#lt>${billingAccount.description!""} [${billingAccount.billingAccountId}] ${uiLabelMap.EcommerceAvailable} <#rt>
                        <#lt><@ofbizCurrency amount=availableAmount isoCode=billingAccount.accountCurrencyUomId/> ${uiLabelMap.EcommerceLimit} <#rt>
                        <#lt><@ofbizCurrency amount=accountLimit isoCode=billingAccount.accountCurrencyUomId/></option>
                    </#list>
                  </@field>
                  <@payMethAmountField name="billingAccountAmount" id="billingAccountAmount${primSupplSuffix}" params=parameters simple=true /><#-- label=uiLabelMap.OrderBillUpTo -->
                </@section>
              </#if>
          </#if>
        </#if>

        <#if productStorePaymentMethodTypeIdMap.GIFT_CARD??>
          <#-- User's gift cards -->
          <#if (paymentMethodListsByType.GIFT_CARD)?has_content>
            <#list paymentMethodListsByType.GIFT_CARD as paymentMethodLocal>
              <#assign paymentMethod = paymentMethodLocal><#-- SCIPIO: workaround for access from macros -->
              <#assign giftCard = paymentMethod.getRelatedOne("GiftCard", false) />
              <#assign methodShortInfo><@formattedGiftCardShort giftCard=giftCard paymentMethod=paymentMethod /></#assign>
              <#if showSupplemental>
                <#if showSelect>
                  <#assign dummy = registerFieldContent({"fieldId":"checkOutPaymentId_${paymentMethod.paymentMethodId}${primSupplSuffix}", "contentId":"content_${paymentMethod.paymentMethodId}${primSupplSuffix}"})>
                  <@field type="checkbox" id="checkOutPaymentId_${paymentMethod.paymentMethodId}${primSupplSuffix}" name="checkOutPaymentId" value=(paymentMethod.paymentMethodId) 
                    class="+pay-select-checkbox pay-select-field" checked=(selectedCheckOutPaymentIdList?seq_contains(paymentMethod.paymentMethodId)) 
                    label=wrapAsRaw("${uiLabelMap.AccountingGiftCard}: ${methodShortInfo}", 'htmlmarkup') /><#--tooltip=(getPayMethTypeDesc("GIFT_CARD")!) -->
                </#if>
              </#if>
              <#if showDetails && showSupplemental>
                <@section containerId="content_${paymentMethod.paymentMethodId}${primSupplSuffix}" containerClass="+pay-meth-content" containerStyle="display:none;"><#--title=uiLabelMap.AccountingGiftCard -->
                  <@payMethInfoPanel title=uiLabelMap.AccountingGiftCard updateAction='EG'>
                    <@formattedGiftCardDetail giftCard=giftCard paymentMethod=paymentMethod />
                    <#if paymentMethod.get("description", locale)?has_content><br/>(${paymentMethod.get("description", locale)})</#if>
                  </@payMethInfoPanel>
                  <@payMethAmountField payMethId=paymentMethod.paymentMethodId/>
                </@section>
              </#if>
            </#list>
          </#if>

          <#-- Gift card not on file -->
          <#-- SCIPIO: NOTE: This was initially implemented in stock code and is not 1-for-1 equivalent to our new inline
              forms for CC && EFT above... 
              TODO?: Reimplement using _NEW_GIFT_CARD_ hook --> 
          <#if showSupplemental>
            <#if showSelect>
              <#-- MUST SUBMIT EMPTY VALUE FOR checkOutPaymentId -->
              <#assign dummy = registerFieldContent({"fieldId":"checkOutPaymentId_addGiftCard${primSupplSuffix}", "contentId":"content__NEW_GIFT_CARD_${primSupplSuffix}"})>
              <@field type="checkbox" id="checkOutPaymentId_addGiftCard${primSupplSuffix}" name="addGiftCard" value="Y" checked=((selectedCheckOutPaymentIdList?seq_contains("_NEW_GIFT_CARD_")) || ((newGiftCardParams.addGiftCard!) == "Y")) 
                class="+pay-select-checkbox pay-select-field" label=uiLabelMap.AccountingUseGiftCardNotOnFile /><#--tooltip=(getPayMethTypeDesc("GIFT_CARD")!) -->
            </#if>
          </#if>
          <#if showDetails && showSupplemental>
            <@section containerId="content__NEW_GIFT_CARD_${primSupplSuffix}" containerClass="+pay-meth-content" containerStyle="display:none;" title=uiLabelMap.AccountingGiftCard>
            <@fields type="inherit-all" fieldArgs={"gridArgs":{"totalLarge":8}}>
              <@field type="input" size="15" id="giftCardNumber${primSupplSuffix}" name="giftCardNumber" value=((newGiftCardParams.giftCardNumber)!) label=uiLabelMap.AccountingNumber
                tooltip="DemoCustomer: test: 123412341234 or 432143214321"/><#--onFocus="document.getElementById('addGiftCard').checked=true;"-->
              <#if cart.isPinRequiredForGC(delegator)>
                <@field type="input" size="10" id="giftCardPin${primSupplSuffix}" name="giftCardPin" value=((newGiftCardParams.giftCardPin)!) label=uiLabelMap.AccountingPIN/><#--onFocus="document.getElementById('addGiftCard').checked=true;"-->
              </#if>

              <#-- SCIPIO: Unhardcode the single-use flag so it follows our new inlines above
              <input type="hidden" name="singleUseGiftCard" value="Y" /> -->
              <#if userHasAccount?? && userHasAccount>
                <#-- SCIPIO: This should be the opposite of "single use payment"... so we use form submit hook to set the hidden field -->
                <@field type="checkbox" checkboxType="simple-standard" id="newGiftCard_saveToAccount" name="saveToAccount__NEW_GIFT_CARD_" 
                    value="Y" checked=((singleUseGiftCard!"") != "Y") label=uiLabelMap.OrderSaveToAccount/>
                <input type="hidden" id="singleUseGiftCard" name="singleUseGiftCard" value="N" />
              </#if>
            </@fields>

              <@payMethAmountField type="new-pay-meth" id="giftCardAmount${primSupplSuffix}" name="giftCardAmount" params=newGiftCardParams/><#--onFocus="document.getElementById('addGiftCard').checked=true;"-->
            </@section>
          </#if>
        </#if>
      </@fields>
</#macro>
    
<#-- SCIPIO: Used to build a radio/checkbox to content div mapping for JS -->
<#function registerFieldContent fieldContentArgs>
  <#assign fieldContentIdMapList = (fieldContentIdMapList![]) + ([fieldContentArgs])>
  <#return "">
</#function>
<#function getPayMethTypeDesc paymentMethodTypeId>
  <#local desc = (productStorePaymentMethodSettingByTypeMap[paymentMethodTypeId].getRelatedOne("PaymentMethodType", true).get("description", locale))!false>
  <#if !desc?is_boolean>
    <#return desc>
  </#if>
  <#-- return nothing otherwise -->
</#function>

<#macro formattedCreditCardShort creditCard paymentMethod={} verbose=true>
  <#if verbose>
    <#--
    <#if !paymentMethod?has_content>
      <#local paymentMethod = creditCard.getRelatedOne("PaymentMethod", false)>
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
  <#import "component://accounting/webapp/accounting/common/acctlib.ftl" as acctlib>
  ${acctlib.getGiftCardDisplayNumber(giftCard)!}<#t>
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

<#macro orderItemSurvResList survResList srqaArgs={} useTitleLine=true interactive=true maxInline=-1 class="" listClass="+order-item-attrib-list">
  <#if survResList?has_content>
  <#local class = addClassArgDefault(class, "order-item-survres-list")>
  <div<@compiledClassAttribStr class=class/>>
    <#list survResList as surveyResponse>
      <div class="survres-entry">
        <#local survey = surveyResponse.getRelatedOne("Survey")!>
        <#if useTitleLine>
          <em class="survres-label">${uiLabelMap.ContentSurvey}:</em>
          <#local surveyResponseId = surveyResponse.surveyResponseId!>
          <#if interactive>
            <#-- SCIPIO: 2019-03-11: This page contains other survey responses and is very confusing
            <a href="<@serverUrl>/content/control/ViewSurveyResponses?surveyResponseId=${surveyResponseId}${raw(externalKeyParam)}</@serverUrl>"
              class="${styles.link_nav_info_id!}" style="font-size: xx-small;">${surveyResponseId}</a>-->
            <@modal id="${raw(surveyResponseId)}_surveyresp" label=surveyResponseId linkClass="${styles.link_nav_info_id!} ${styles.action_view!}">
              <#-- DEV NOTE: Must use @section instead of @heading so that the heading levels in the details start at the right level -->
              <@section title=getLabel("DataResourceType.description.SURVEY_RESPONSE", "ContentEntityLabels") relHeadingLevel=+1>
                <@fields type="default" ignoreParentField=true>
                  <#-- SCIPIO: TODO?: Link to real edit forms -->
                  <@field type="display" label=getLabel("FormFieldTitle_surveyResponseId")>${surveyResponseId}</@field>
                  <@field type="display" label=getLabel("ContentSurveySurveyId")>
                      <a target="_blank" href="<@serverUrl>/content/control/ViewSurveyResponses?surveyResponseId=${surveyResponseId}${raw(externalKeyParam)}</@serverUrl>"<#t>
                          class="${styles.link_nav_info_id!} ${styles.action_view!}">${surveyResponse.surveyId!}</a><#t>
                  </@field>
                  <#-- SCIPIO: FIXME: This should not render any templates under /shop -->
                  <@render resource="component://content/widget/SurveyScreens.xml#SurveyResponseDetail" ctxVars={"surveyResponse":surveyResponse}/>
                  <#-- SCIPIO: TODO: REVIEW: The screens linked here are (besides being in /content) extremely confusing
                    and do not actually allow editing the survey response for the other - it confusingly creates new responses
                  <p>
                    <a href="<@serverUrl>/content/control/EditSurveyResponse?surveyResponseId=${surveyResponseId}${raw(externalKeyParam)}</@serverUrl>"
                      class="${styles.link_nav!} ${styles.action_view!}">${uiLabelMap.CommonEdit}</a>
                  </p>
                  -->
                </@fields>
              </@section>
            </@modal>
          </#if>
          <#local surveyDesc = survey.get("description", locale)!>
          <#if surveyDesc?has_content><#if interactive> - </#if><span class="survres-desc">${surveyDesc}</span></#if>
        </#if>
        <#if (maxInline != 0) && ("Y" == survey.showOnInvoice!)>
          <#import "component://content/webapp/content/common/contentlib.ftl" as contentlib>
          <@contentlib.renderSurveyResponse surveyResponse=surveyResponse tmplLoc="component://content/template/survey/qalistresult.ftl"
            srqaArgs=({"listClass":listClass, "max":maxInline} + srqaArgs)/>
        </#if>
      </div>
    </#list>
  </div>
  </#if>
</#macro>

<#function getOrderItemSurvResList orderItem>
  <#local orderItemSeqId = raw((orderItem.orderItemSeqId)!)>
  <#if !orderItemSeqId?has_content>
    <#return []>
  </#if>
  <#if orderItemSurvResMap??>
    <#return orderItemSurvResMap[orderItemSeqId]![]>
  <#else>
    <#return delegator.from("SurveyResponse").where("orderId", raw(orderItem.orderId!), "orderItemSeqId", orderItemSeqId).orderBy("orderItemSeqId").queryList()><#-- This less accurately reproduces cart order: .orderBy("-responseDate") -->
  </#if>
</#function>
