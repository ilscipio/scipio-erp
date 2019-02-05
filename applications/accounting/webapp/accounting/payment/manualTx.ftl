<#--
This file is subject to the terms and conditions defined in the
files 'LICENSE' and 'NOTICE', which are part of this source
code package.
-->

<@section>
<#if security.hasEntityPermission("MANUAL", "_PAYMENT", request) || security.hasEntityPermission("ACCOUNTING", "_CREATE", request)>
    <#assign dummy = setRequestAttribute("validTx", "false")>
    <form name="manualTxForm" method="post" action="<@pageUrl>manualETx</@pageUrl>">
        <#if requestParameters.paymentMethodId??>
        <input type="hidden" name="paymentMethodId" value="${requestParameters.paymentMethodId}" />
        </#if>
        
        <#if paymentMethodType?has_content>
            <@field type="display" label=uiLabelMap.CommonPaymentMethodType>${paymentMethodType.get("description",locale)}</@field>
            <input type="hidden" name="paymentMethodTypeId" value="${paymentMethodType.paymentMethodTypeId}" />
        <#else>
            <@field type="select" name="paymentMethodTypeId" label=uiLabelMap.CommonPaymentMethodType>
                <option value="CREDIT_CARD">${uiLabelMap.AccountingCreditCard}</option>
            </@field>
        </#if>
        
        <#if currentStore?has_content>
            <@field type="display" label=uiLabelMap.ProductProductStore><#if currentStore.storeName??>${currentStore.storeName}<#else>${currentStore.productStoreId}</#if></@field>
            <input type="hidden" name="productStoreId" value="${currentStore.productStoreId}" />
        <#else>
            <@field type="select" name="productStoreId" label=uiLabelMap.ProductProductStore>
                <#list productStores as productStore>
                    <option value="${productStore.productStoreId}"><#if productStore.storeName??>${productStore.storeName}<#else>${productStore.productStoreId}</#if></option>
                </#list>
            </@field>
        </#if>
      
        <#if currentTx?has_content>
            <@field type="display" label=uiLabelMap.AccountingTransactionType>${currentTx.get("description",locale)}</@field>
            <input type="hidden" name="transactionType" value="${currentTx.enumId}" />
        <#else>
            <!-- TODO: Clarify how to handle JS events -->            
            <#--@field type="select" name="transactionType" onChange="javascript:document.manualTxForm.submit();" title="${uiLabelMap.AccountingTransactionType}"-->
            <@row>
                <@cell small=3 large=2>${uiLabelMap.AccountingTransactionType}</@cell>
                <@cell small=9 large=10>
                <select name="transactionType" onchange="javascript:document.manualTxForm.submit();"> 
                    <#-- the select one option is so the list will fire on any seletion -->
                    <option value="Select one">${uiLabelMap.CommonSelectOne}</option>
                    <#list paymentSettings as setting>
                        <option value="${setting.enumId}">${setting.get("description",locale)}</option>
                    </#list>
                </select>
                </@cell>
            </@row>
            <#-- /@field -->
        </#if>

        <#-- payment method information -->
        <#if paymentMethodType?has_content && paymentMethodTypeId == "CREDIT_CARD">
            <@render resource="component://accounting/widget/payments/PaymentScreens.xml#manualCCTx" />
        <#elseif paymentMethodType?has_content && paymentMethodTypeId == "GIFT_CARD">
            <@render resource="component://accounting/widget/payments/PaymentScreens.xml#manualGCTx" />
        </#if>

        <#if requestAttributes.validTx?default("false") == "true">
            <hr/>
            <#-- amount field -->
            <@field type="input" label=uiLabelMap.CommonAmount size="20" maxlength="30" name="amount" required=true/>
            <#-- submit button -->
            <@field type="submit" text=uiLabelMap.CommonSubmit class="+${styles.link_run_sys!} ${styles.action_add!}"/>
        <#elseif txType?has_content>
            <@commonMsg type="warning">${uiLabelMap.AccountingTransactionTypeNotYetSupported}</@commonMsg>
        </#if>
    </form>
<#else>
    <@commonMsg type="error">${uiLabelMap.AccountingPermissionError}</@commonMsg>
</#if>
</@section>
