<@section>
    <form name="TrackingCodeReportOptions" action="<@ofbizUrl>AnalyticsTracking</@ofbizUrl>" method="POST">
        <@field name="fromDate" type="datetime" value="" label=uiLabelMap.CommonFrom />
        <@field name="thruDate" type="datetime" value="" label=uiLabelMap.CommonThru />
        <@field name="trackingCodeId" type="select" label=uiLabelMap.MarketingTrackingCode>
            <#list trackingCodeList as trackingCode>
                <option value="${trackingCode.trackingCodeId}">${trackingCode.description}</option>
            </#list>
        </@field>
        <@field name="run" type="submit" value="" text=uiLabelMap.CommonSubmit />
        
        


</@section>
