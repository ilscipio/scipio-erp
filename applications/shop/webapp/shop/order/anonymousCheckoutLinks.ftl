<#--
Licensed to the Apache Software Foundation (ASF) under one
or more contributor license agreements.  See the NOTICE file
distributed with this work for additional information
regarding copyright ownership.  The ASF licenses this file
to you under the Apache License, Version 2.0 (the
"License"); you may not use this file except in compliance
with the License.  You may obtain a copy of the License at

http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing,
software distributed under the License is distributed on an
"AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
KIND, either express or implied.  See the License for the
specific language governing permissions and limitations
under the License.
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



