<!-- TODO: LEGAL STATEMENT -->

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

<#if security.hasEntityPermission("MANUAL", "_PAYMENT", session) || security.hasEntityPermission("ACCOUNTING", "_CREATE", session)>
  	${setRequestAttribute("validTx", "false")}
  	<form name="manualTxForm" method="post" action="<@ofbizUrl>manualETx</@ofbizUrl>">
		<#if requestParameters.paymentMethodId??>
		  <input type="hidden" name="paymentMethodId" value="${requestParameters.paymentMethodId}" />
		</#if>
		 
		<#if paymentMethodType?has_content>
			<@field type="display" label="${uiLabelMap.CommonPaymentMethodType}">${paymentMethodType.get("description",locale)}</@field>
			<input type="hidden" name="paymentMethodTypeId" value="${paymentMethodType.paymentMethodTypeId}" />
		<#else>
			<@field type="select" name="paymentMethodTypeId" label="${uiLabelMap.CommonPaymentMethodType}">
		    	<option value="CREDIT_CARD">${uiLabelMap.AccountingCreditCard}</option>
		    </@field>
		</#if>
		
		<#if currentStore?has_content>
			<@field type="display" label="${uiLabelMap.ProductProductStore}"><#if currentStore.storeName??>${currentStore.storeName}<#else>${currentStore.productStoreId}</#if></@field>
		    <input type="hidden" name="productStoreId" value="${currentStore.productStoreId}" />
		<#else>
			<@field type="select" name="productStoreId" label="${uiLabelMap.ProductProductStore}">
		    	<#list productStores as productStore>
		        	<option value="${productStore.productStoreId}"><#if productStore.storeName??>${productStore.storeName}<#else>${productStore.productStoreId}</#if></option>
		        </#list>
			</@field>
		</#if>
      
		<#if currentTx?has_content>
			<@field type="display" label="${uiLabelMap.AccountingTransactionType}">${currentTx.get("description",locale)}</@field>
			<input type="hidden" name="transactionType" value="${currentTx.enumId}" />
		<#else>
			<!-- TODO: Clarify how to handle JS events -->
            <#-- @field type="select" name="transactionType" onchange="javascript:document.manualTxForm.submit();" -->
            ${uiLabelMap.AccountingTransactionType}
           	<select name="transactionType" onchange="javascript:document.manualTxForm.submit();"> 
            	<#-- the select one option is so the list will fire on any seletion -->
              	<option value="Select one">${uiLabelMap.CommonSelectOne}</option>
              	<#list paymentSettings as setting>
                	<option value="${setting.enumId}">${setting.get("description",locale)}</option>
             	 </#list>
            </select>
            <#-- /@field -->
		</#if>

		<#-- payment method information -->
		<#if paymentMethodType?has_content && paymentMethodTypeId == "CREDIT_CARD">
			${screens.render("component://accounting/widget/PaymentScreens.xml#manualCCTx")}
	  	<#elseif paymentMethodType?has_content && paymentMethodTypeId == "GIFT_CARD">
			${screens.render("component://accounting/widget/PaymentScreens.xml#manualGCTx")}
	  	</#if>

     	<#if requestAttributes.validTx?default("false") == "true">
        	<hr/>
        	<#-- amount field -->
        	<@field type="input" label="${uiLabelMap.CommonAmount}" size="20" maxlength="30" name="amount" required=true tooltip="${uiLabelMap.CommonRequired}"/>
        	<#-- submit button -->
        	<@field type="submitarea">
            	<input type="submit" value="${uiLabelMap.CommonSubmit}" />
        	</@field>
      	<#elseif txType?has_content>
        	<@resultMsg>${uiLabelMap.AccountingTransactionTypeNotYetSupported}</@resultMsg>
      	</#if>
  	</form>
<#else>
	<@alert type="error">${uiLabelMap.AccountingPermissionError}</@alert>
</#if>
