<#-- SCIPIO -->
<#if finAccountTrans?has_content> <#-- FIXME: Ugly workaround, because variables set by entity-condition do not validate correctly on screen condition -->
<@section title=uiLabelMap.AccountingFinAccountTransaction>
    <@table type="fields" >
        <#if finAccountTrans.finAccountTransId?has_content>
            <@tr>
              <@td scope="row" class="${styles.grid_large!}3">${uiLabelMap.FormFieldTitle_finAccountTransId}</@td>
              <@td colspan="3">
                ${finAccountTrans.finAccountTransId!}
              </@td>
            </@tr>
        </#if>

        <#if finAccountTrans.statusId?has_content>
            <#assign currentStatus = finAccountTrans.getRelatedOne("StatusItem", false)/>
            <@tr>
              <@td scope="row" class="${styles.grid_large!}3">${uiLabelMap.CommonStatus}</@td>
              <@td colspan="3">
                ${currentStatus.get('description',locale)}
              </@td>
            </@tr>
        </#if>

        <#if finAccountTrans.finAccountTransTypeId?has_content>
            <#assign currType = finAccountTrans.getRelatedOne("FinAccountTransType", false)/>
            <@tr>
              <@td scope="row" class="${styles.grid_large!}3">${uiLabelMap.CommonType}</@td>
              <@td colspan="3">
                ${currType.get('description',locale)}
              </@td>
            </@tr>
        </#if>

        <#if finAccountTrans.glReconciliationId?has_content>
            <@tr>
              <@td scope="row" class="${styles.grid_large!}3">${uiLabelMap.FormFieldTitle_glReconciliationId}</@td>
              <@td colspan="3">
                <a href="<@pageUrl>ViewGlReconciliationWithTransaction?glReconciliationId=${finAccountTrans.glReconciliationId!}&finAccountId=${finAccountTrans.finAccountId!}</@pageUrl>">${finAccountTrans.glReconciliationId!}</a>
              </@td>
            </@tr>
        </#if>

        <#if finAccountTrans.transactionDate?has_content>
            <@tr>
                <@td scope="row" class="${styles.grid_large!}3">${uiLabelMap.FormFieldTitle_transactionDate}</@td>
                <@td colspan="3">
                  <@formattedDateTime date=finAccountTrans.transactionDate />             
                </@td>
            </@tr>
        </#if>

        <#if finAccountTrans.amount?has_content>
            <@tr>
                <@td scope="row" class="${styles.grid_large!}3">${uiLabelMap.CommonAmount}</@td>
                <@td colspan="3">
                    <@ofbizCurrency isoCode=payment.currencyUomId amount=(finAccountTrans.amount!)/>    
                </@td>
            </@tr>
        </#if>
    </@table>
</@section>
</#if>