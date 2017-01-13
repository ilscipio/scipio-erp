<@section>
    <form name="SaleOrdersByChannel" action="<@ofbizUrl>AnalyticsSales</@ofbizUrl>" method="post">
        <@field type="select" label=uiLabelMap.ProductProductStore name="productStoreId">
            <@field type="option" value="" 
              selected=(!productStoreId?has_content)>${uiLabelMap.CommonAny}</@field>
            <#list productStores as store>
                <@field type="option" value=store.productStoreId 
                  selected=(store.productStoreId == (productStoreId!""))>${store.storeName!(store.productStoreId!)}</@field>
            </#list>
        </@field>
        
        <@field name="fromDate" type="datetime" value=(parameters.fromDate!) label=uiLabelMap.CommonFrom />
        <@field name="thruDate" type="datetime" value=(parameters.thruDate!) label=uiLabelMap.CommonThru tooltip=uiLabelMap.CommonLeaveEmptyForNowDate/>
        <@field type="select" name="intervalScope" label=uiLabelMap.CommonTimeInterval required=true><#-- uiLabelMap.CommonIntervalScope -->
            <#assign intervals = Static["org.ofbiz.base.util.UtilDateTime"].TIME_INTERVALS />
            <#assign currInterval = chartIntervalScope!parameters.intervalScope!"">
            <#-- This contradicted the groovy which required an interval scope
            <option value=""></option>-->
            <#list intervals as interval>
                <option value="${interval}"<#if currInterval == interval> selected="selected"</#if>>${interval?capitalize}</option>
            </#list>
        </@field>
        <@field type="select" name="salesChannel" label=uiLabelMap.OrderSalesChannel>
            <option value="">${uiLabelMap.CommonAny}</option>
            <#list salesChannelList as salesChannel>
                <option value="${salesChannel.enumId}"<#if parameters.salesChannel?has_content && parameters.salesChannel == salesChannel.enumId> selected="selected"</#if>>${salesChannel.description}</option>
            </#list>
        </@field>
        
        <@field type="select" label=uiLabelMap.CommonCurrency name="currencyUomId" tooltip="${rawLabel('SfaOnlyConvertibleCurrenciesListed')} - ${rawLabel('SfaCurrencyConvertedUsingLastKnownRates')}">
            <option value=""<#if !parameters.currencyUomId?has_content> selected="selected"</#if>>${uiLabelMap.CommonDefault}</option>
            <#list currencies as currency>
                <option value="${currency.uomId}"<#if (parameters.currencyUomId!'') == currency.uomId> selected="selected"</#if>>${currency.uomId}</option>
            </#list>
        </@field>
       
        <@field name="run" type="submit" value="" text=uiLabelMap.CommonSubmit />
    </form>
</@section>
