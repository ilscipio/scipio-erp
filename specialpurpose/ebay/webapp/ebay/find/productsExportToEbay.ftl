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
<@script>
     function changeEbayCategory(categ) {
         document.forms["ProductsExportToEbay"].action = "<@ofbizUrl>ProductsExportToEbay?categoryCode="+categ+"</@ofbizUrl>";
         document.forms["ProductsExportToEbay"].submit();
     }

    function changeWebSite(Id) {
        var formId = Id ;
        formId.action="<@ofbizUrl>ProductsExportToEbay</@ofbizUrl>";
        formId.submit();
    }

     function activateSubmitButton() {
         categ = document.forms["ProductsExportToEbay"].ebayCategory.value;
         if (document.forms["ProductsExportToEbay"].submitButton) {
             if (categ != null && (categ.substring(0, 1) == 'Y' || categ == '')) {
                 document.forms["ProductsExportToEbay"].submitButton.disabled = false;
             } else {
                 document.forms["ProductsExportToEbay"].submitButton.disabled = true;
                 document.forms["ProductsExportToEbay"].submitButton.value = "Please select a category";
             }
         }
    }
</@script>
<div>
    <form id="ProductsExportToEbay" method="post" action="<@ofbizUrl>PostProductsToEbay</@ofbizUrl>" name="ProductsExportToEbay">
        <input type="hidden" name="productStoreId" value="${productStoreId!}" />
        <input type="hidden" name="selectResult" value="${selectResult!}"/>
             <@field type="select" label=uiLabelMap.FormFieldTitle_ebayCategory name="ebayCategory" onChange="javascript:changeEbayCategory(this.value)">
                <option value=""> </option>
                <#if categories??>
                  <#list categories as category>
                    <option value="${category.CategoryCode}" <#if categoryCode?? && categoryCode == category.CategoryCode>selected="selected"</#if>>${category.CategoryName}</option>
                  </#list>
                </#if>
             </@field>
            <#if hideExportOptions?has_content && hideExportOptions == "N">
            <@field type="select" label=uiLabelMap.CommonCountry name="country">
                <#if countries??>
                    <#list countries as country>
                        <option value="${country.geoCode}" <#if countryCode?? && countryCode == country.geoCode>selected="selected"</#if>>${country.get("geoName",locale)}</option>
                    </#list>
                </#if>
            </@field>
            <@field type="input" label=uiLabelMap.FormFieldTitle_location name="location" size="50" maxlength="50" value=parameters.location!"" />
            <@field type="select" label=uiLabelMap.FormFieldTitle_listingDuration name="listingDuration">
                <option value="Days_1">1 ${uiLabelMap.CommonDay}</option>
                <option value="Days_3">3 ${uiLabelMap.CommonDays}</option>
                <option value="Days_7">7 ${uiLabelMap.CommonDays}</option>
            </@field>
            <@field type="input" label=uiLabelMap.FormFieldTitle_startPrice name="startPrice" size="12" maxlength="12" value=parameters.startPrice!"" />
            <#assign fieldValue><#if parameters.quantity??>${parameters.quantity!}<#else>1</#if></#assign>
            <@field type="input" label=uiLabelMap.CommonQuantity name="quantity" size="12" maxlength="12" value=fieldValue />
            <@field type="select" label=uiLabelMap.CommonWebsite name="webSiteId" onChange="javascript:changeWebSite(document.getElementById('ProductsExportToEbay'));">
                <#list webSiteList as webSite>
                  <#assign displayDesc = webSite.siteName?default("${uiLabelMap.ProductNoDescription}")>
                  <#if (18 < displayDesc?length)>
                    <#assign displayDesc = displayDesc[0..15] + "...">
                  </#if>
                  <option value="${webSite.webSiteId}" <#if selectedWebSiteId! == webSite.webSiteId> selected="selected"</#if>>${displayDesc} [${webSite.webSiteId}]</option>
                </#list>
            </@field>
            <@field type="input" label=uiLabelMap.FormFieldTitle_webSiteUrl name="webSiteUrl" size="100" value=webSiteUrl!""/>
            <@field type="generic" label=uiLabelMap.FormFieldTitle_paymentMethodsAccepted>
                <@fields type="default-manual" ignoreParentField=true fieldArgs={"inline":true}>
                    <@table type="fields"> <#-- orig: class="basic-table" --> <#-- orig: cellspacing="0" -->
                        <@tr>
                            <@td align="right" width="23%">${uiLabelMap.FormFieldTitle_paymentPayPal}</@td>
                            <@td width="2%"><@field type="checkbox" name="paymentPayPal" checked=(parameters.paymentPayPal??) /></@td>
                            <@td align="right" width="23%">${uiLabelMap.FormFieldTitle_paymentVisaMC}</@td>
                            <@td width="2%"><@field type="checkbox" name="paymentVisaMC" checked=(parameters.paymentVisaMC??) /></@td>
                            <@td align="right" width="23%">${uiLabelMap.FormFieldTitle_paymentAmEx}</@td>
                            <@td width="2%"><@field type="checkbox" name="paymentAmEx" checked=(parameters.paymentAmEx??) /></@td>
                            <@td align="right" width="23%">${uiLabelMap.FormFieldTitle_paymentDiscover}</@td>
                            <@td width="2%"><@field type="checkbox" name="paymentDiscover" checked=(parameters.paymentDiscover??) /></@td>
                        </@tr>
                        <@tr>
                            <@td align="right" width="23%">${uiLabelMap.FormFieldTitle_paymentMOCC}</@td>
                            <@td width="2%"><@field type="checkbox" name="paymentMOCC" checked=(parameters.paymentMOCC??) /></@td>
                            <@td align="right" width="23%">${uiLabelMap.FormFieldTitle_paymentPersonalCheck}</@td>
                            <@td width="2%"><@field type="checkbox" name="paymentPersonalCheck" checked=(parameters.paymentPersonalCheck??) /></@td>
                            <@td align="right" width="23%">${uiLabelMap.FormFieldTitle_paymentCCAccepted}</@td>
                            <@td width="2%"><@field type="checkbox" name="paymentCCAccepted" checked=(parameters.paymentCCAccepted??) /></@td>
                            <@td align="right" width="23%">${uiLabelMap.FormFieldTitle_paymentCashInPerson}</@td>
                            <@td width="2%"><@field type="checkbox" name="paymentCashInPerson" checked=(parameters.paymentCashInPerson??) /></@td>
                        </@tr>
                        <@tr>
                            <@td align="right" width="23%">${uiLabelMap.FormFieldTitle_paymentCashOnPickup}</@td>
                            <@td width="2%"><@field type="checkbox" name="paymentCashOnPickup" checked=(parameters.paymentCashOnPickup??) /></@td>
                            <@td align="right" width="23%">${uiLabelMap.FormFieldTitle_paymentCOD}</@td>
                            <@td width="2%"><@field type="checkbox" name="paymentCOD" checked=(parameters.paymentCOD??) /></@td>
                            <@td align="right" width="23%">${uiLabelMap.FormFieldTitle_paymentCODPrePayDelivery}</@td>
                            <@td width="2%"><@field type="checkbox" name="paymentCODPrePayDelivery" checked=(parameters.paymentCODPrePayDelivery??) /></@td>
                            <@td align="right" width="23%">${uiLabelMap.FormFieldTitle_paymentMoneyXferAccepted}</@td>
                            <@td width="2%"><@field type="checkbox" name="paymentMoneyXferAccepted" checked=(parameters.paymentMoneyXferAccepted??) /></@td>
                        </@tr>
                    </@table>
                </@fields>
            </@field>
            <@field type="input" label=uiLabelMap.FormFieldTitle_payPalEmail name="payPalEmail" size="50" maxlength="50" value=parameters.payPalEmail!"" />
            <@field type="textarea" label=uiLabelMap.FormFieldTitle_customXml cols="60" rows="6" wrap="soft" name="customXml">${customXml!}</@field>
            <@field type="submit" text=uiLabelMap.EbayExportToEbay name="submitButton" class="+${styles.link_run_sys!} ${styles.action_send!} ${styles.action_external!}" />
            </#if>
    </form>
    <@script>
        activateSubmitButton();
    </@script>
</div>
