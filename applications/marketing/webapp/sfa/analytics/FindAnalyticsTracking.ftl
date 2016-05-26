<@section>
    <form name="TrackingCodeReportOptions" action="<@ofbizUrl>AnalyticsTracking</@ofbizUrl>" method="POST">
        <@field name="fromDate" type="datetime" value="" label=uiLabelMap.CommonFrom />
        <@field name="thruDate" type="datetime" value="" label=uiLabelMap.CommonThru />
        <@field type="select" name="intervalScope" label="${uiLabelMap.CommonIntervalScope}">
            <#assign intervals = Static["org.ofbiz.base.util.UtilDateTime"].TIME_INTERVALS />
            <option value=""></option>
            <#list intervals as interval>
                <option value="${interval}" <#if parameters.intervalScope?has_content && parameters.intervalScope == interval>selected="selected"</#if>>${interval?capitalize}</option>
            </#list>
       </@field>
        <@field name="trackingCodeId" type="select" label=uiLabelMap.MarketingTrackingCode>
            <#list trackingCodeList as trackingCode>
                <option value="${trackingCode.trackingCodeId}" <#if parameters.trackingCodeId?has_content && parameters.trackingCodeId == trackingCode.trackingCodeId>selected="selected"</#if>>${trackingCode.description}</option>
            </#list>
        </@field>
        <@field name="run" type="submit" value="" text=uiLabelMap.CommonSubmit />
    </form>
</@section>
