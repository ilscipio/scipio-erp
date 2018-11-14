<#--
This file is subject to the terms and conditions defined in the
files 'LICENSE' and 'NOTICE', which are part of this source
code package.
-->

<#-- SCIPIO: DEPRECATED TEMPLATE -->


<#-- SCIPIO: DEPRECATED old (preserve for links) Superseded by checkoutstepsfull.ftl
<@script>
function submitForm(form) {
   form.submit();
}
</@script>
<@menu type="button">
    <#assign submitFormOnClick><#if callSubmitForm??>javascript:submitForm(document['${escapeVal(parameters.formNameValue!, 'js')}']);</#if></#assign>
    <@menuitem type="link" href=makeOfbizUrl("setCustomer") onClick=submitFormOnClick text="Personal Info" />
    <@menuitem type="link" href=makeOfbizUrl("setShipping") class="+${styles.action_nav!} ${styles.action_update!}" onClick=submitFormOnClick disabled=(!(enableShippingAddress??)) text="Shipping Address" />
    <@menuitem type="link" href=makeOfbizUrl("setShipOptions")class="+${styles.action_nav!} ${styles.action_update!}" onClick=submitFormOnClick disabled=(!(enableShipmentMethod??)) text="Shipping Options" />
    <@menuitem type="link" href=makeOfbizUrl("setPaymentOption")class="+${styles.action_nav!} ${styles.action_update!}" onClick=submitFormOnClick disabled=(!(enablePaymentOptions??)) text="Payment Options" />
    <@menuitem type="link" href=makeOfbizUrl("setPaymentInformation?paymentMethodTypeId=${requestParameters.paymentMethodTypeId!}") class="+${styles.action_nav!} ${styles.action_update!}" onClick=submitFormOnClick disabled=(!(enablePaymentInformation??)) text="Payment Information" />
    <@menuitem type="link" href=makeOfbizUrl("reviewOrder") class="+${styles.action_nav!}" onClick=submitFormOnClick disabled=(!(enableReviewOrder??)) text="Review Order" />
</@menu>-->



