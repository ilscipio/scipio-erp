<#--
This file is subject to the terms and conditions defined in the
files 'LICENSE' and 'NOTICE', which are part of this source
code package.
-->
<#include "component://shop/webapp/shop/order/ordercommon.ftl">

<#-- SCIPIO: DEPRECATED TEMPLATE -->

<@script>
function submitForm(form) {
   form.submit();
}
</@script>

<@menu type="button">
    <#-- SCIPIO: TODO: localize -->
    <#assign submitFormOnClick><#if callSubmitForm??>javascript:submitForm(document['${escapeVal(parameters.formNameValue!, 'js')}']);</#if></#assign>
    <@menuitem type="link" href=makeOfbizUrl("quickAnonSetCustomer") class="+${styles.action_run_session!} ${styles.action_update!}" onClick=submitFormOnClick text="Personal Info" />
    <@menuitem type="link" href=makeOfbizUrl("quickAnonOrderReview") class="+${styles.action_run_session!} ${styles.action_update!}" onClick=submitFormOnClick text="Review Order" disabled=(!enableShipmentMethod??) />
</@menu>
