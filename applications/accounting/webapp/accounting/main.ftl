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

<@section>
    <@grid>
        <li>
            <@pul title="${uiLabelMap.AccountingInvoicesMenu}">
                <@pli><a href="<@ofbizUrl>findInvoices?noConditionFind=Y&amp;lookupFlag=Y</@ofbizUrl>">${uiLabelMap.AccountingShowAllInvoices}</a></@pli>
<#list invoiceTypes as invoiceType>
                    <@pli><a href="<@ofbizUrl>findInvoices?lookupFlag=Y&amp;invoiceTypeId=${invoiceType.invoiceTypeId}</@ofbizUrl>">${uiLabelMap.AccountingShowInvoices} ${invoiceType.get("description",locale)!invoiceType.invoiceTypeId}</a></@pli>
</#list>
            </@pul>
        </li>
        <li>
            <@pul title="${uiLabelMap.AccountingInvoicesMenu}: ${uiLabelMap.CommonStatus}">
<#list invoiceStatus as status>
                    <@pli><a href="<@ofbizUrl>findInvoices?lookupFlag=Y&amp;statusId=${status.statusId}</@ofbizUrl>">${uiLabelMap.AccountingShowInvoices} ${status.get("description",locale)!status.statusId}</a></@pli>
</#list>
            </@pul>
        </li>
         <li>
            <@pul title="${uiLabelMap.AccountingPaymentsMenu}">  
                <@pli><a href="<@ofbizUrl>findPayments?noConditionFind=Y&amp;lookupFlag=Y</@ofbizUrl>">${uiLabelMap.AccountingShowAllPayments}</a></@pli>
<#list paymentTypes as paymentType>
                    <@pli><a href="<@ofbizUrl>findPayments?lookupFlag=Y&amp;paymentTypeId=${paymentType.paymentTypeId}</@ofbizUrl>">${uiLabelMap.AccountingShowPayments} ${paymentType.get("description",locale)!paymentType.paymentTypeId}</a></@pli>
</#list>
            </@pul>
        </li>
        <li>
            <@pul title="${uiLabelMap.AccountingPaymentsMenu}: ${uiLabelMap.CommonPaymentMethodType}">  
<#list paymentMethodTypes as paymentMethodType>
                    <@pli><a href="<@ofbizUrl>findPayments?lookupFlag=Y&amp;paymentMethodTypeId=${paymentMethodType.paymentMethodTypeId}</@ofbizUrl>">${uiLabelMap.AccountingShowPayments} ${paymentMethodType.get("description",locale)!paymentMethodType.paymentMethodTypeId}</a></@pli>
</#list>
            </@pul>
        </li>
        <li>
            <@pul title="${uiLabelMap.AccountingPaymentsMenu}: ${uiLabelMap.CommonStatus}">  
<#list paymentStatus as status>
                    <@pli><a href="<@ofbizUrl>findPayments?lookupFlag=Y&amp;statusId=${status.statusId}</@ofbizUrl>">${uiLabelMap.AccountingShowPayments} ${status.get("description",locale)!status.statusId}</a></@pli>
</#list>
            </@pul>
        </li>
        <li>
            <@pul title="${uiLabelMap.AccountingAgreements}">
            
                        <@pli><a href="<@ofbizUrl>FindAgreement</@ofbizUrl>">${uiLabelMap.AccountingAgreementAvailable}</a></@pli>
            
              
             </@pul> 
        </li>
         <li>
            <@pul title="${uiLabelMap.AccountingBillingMenu}">
                <@pli><a href="<@ofbizUrl>FindBillingAccount</@ofbizUrl>">${uiLabelMap.CommonShow} ${uiLabelMap.AccountingCustomer} ${uiLabelMap.AccountingBillingAccount}</a></@pli>
            </@pul>
        </li>
        <li>     
            <@pul title="${uiLabelMap.AccountingFixedAssets}">
                <@pli><a href="<@ofbizUrl>ListFixedAssets</@ofbizUrl>">${uiLabelMap.AccountingShowAllFixedAssets}</a></@pli>
            </@pul>
        </li>
    </@grid>
</@section>
