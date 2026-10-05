<#--
Scipio Commerce
Copyright (C) Ilscipio GmbH

This file is part of Scipio Commerce. Scipio Commerce is free software: you
can redistribute it and modify it under the terms of the GNU Affero General
Public License, version 3, as published by the Free Software Foundation.
Scipio Commerce is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License
for more details. You should have received a copy of the license with this
work (file LICENSE). If not, see <https://www.gnu.org/licenses/agpl-3.0.html>.
A commercial license is available from Ilscipio GmbH.

SPDX-License-Identifier: AGPL-3.0-only
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
    <@menuitem type="link" href=makePageUrl("setCustomer") onClick=submitFormOnClick text="Personal Info" />
    <@menuitem type="link" href=makePageUrl("setShipping") class="+${styles.action_nav!} ${styles.action_update!}" onClick=submitFormOnClick disabled=(!(enableShippingAddress??)) text="Shipping Address" />
    <@menuitem type="link" href=makePageUrl("setShipOptions")class="+${styles.action_nav!} ${styles.action_update!}" onClick=submitFormOnClick disabled=(!(enableShipmentMethod??)) text="Shipping Options" />
    <@menuitem type="link" href=makePageUrl("setPaymentOption")class="+${styles.action_nav!} ${styles.action_update!}" onClick=submitFormOnClick disabled=(!(enablePaymentOptions??)) text="Payment Options" />
    <@menuitem type="link" href=makePageUrl("setPaymentInformation?paymentMethodTypeId=${requestParameters.paymentMethodTypeId!}") class="+${styles.action_nav!} ${styles.action_update!}" onClick=submitFormOnClick disabled=(!(enablePaymentInformation??)) text="Payment Information" />
    <@menuitem type="link" href=makePageUrl("reviewOrder") class="+${styles.action_nav!}" onClick=submitFormOnClick disabled=(!(enableReviewOrder??)) text="Review Order" />
</@menu>-->



