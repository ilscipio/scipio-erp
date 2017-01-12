<@section>
    <form name="SaleOrdersByChannel" action="<@ofbizUrl>AnalyticsSales</@ofbizUrl>" method="POST">
        <@field name="fromDate" type="datetime" value=(parameters.fromDate!) label=uiLabelMap.CommonFrom />
        <@field name="thruDate" type="datetime" value=(parameters.thruDate!) label=uiLabelMap.CommonThru tooltip=uiLabelMap.CommonLeaveEmptyForNowDate/>
        <@field type="select" name="intervalScope" label=uiLabelMap.CommonIntervalScope>
            <#assign intervals = Static["org.ofbiz.base.util.UtilDateTime"].TIME_INTERVALS />
            <#assign currInterval = chartIntervalScope!parameters.intervalScope!"">
            <#-- This contradicted the groovy which required an interval scope
            <option value=""></option>-->
            <#list intervals as interval>
                <option value="${interval}"<#if currInterval == interval> selected="selected"</#if>>${interval?capitalize}</option>
            </#list>
        </@field>
        <@field type="select" name="salesChannel" label=uiLabelMap.OrderSalesChannel>
            <option value=""></option>
            <#list salesChannelList as salesChannel>
                <option value="${salesChannel.enumId}"<#if parameters.salesChannel?has_content && parameters.salesChannel == salesChannel.enumId> selected="selected"</#if>>${salesChannel.description}</option>
            </#list>
        </@field>
       
        <@field name="run" type="submit" value="" text=uiLabelMap.CommonSubmit />
    </form>
</@section>
