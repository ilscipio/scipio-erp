<#-- 
SCIPIO: common calendar defs
-->

<#macro calendarDateSwitcher period>
    <#local idNum = getRequestVar("calendarDateSwitcherIdNum")!0>
    <#local idNum = idNum + 1 />
    <#local dummy = setRequestVar("calendarDateSwitcherIdNum", idNum)>
    <#local formId = "calendar-date-switcher-" + idNum>
    <#local onChangeName = "calendarDateChange" + idNum>
    <@script>
        function ${onChangeName}(ev) {
            var targetDate = jQuery('#dayDateSwitcher').val();
            <#-- SCIPIO: this is the hard part: calculate the start time -->
            <#--alert('value: ' + targetDate);-->
            <#-- we can only accept yyyy-MM-dd right now -->
            var match = targetDate.match(/^\s*(\d\d\d\d)-(\d\d)-(\d\d)\s*$/);
            if (match) {
                var year = parseInt(match[1]);
                var month = parseInt(match[2]);
                var day = parseInt(match[3]);
                <#-- FIXME: we can't check the full date validity here. not big concern. -->
                if (month <= 12 && day <= 31) {
                    jQuery('#${formId}').submit();
                }
            }
        }
    </@script>
    <div style="width:8em;"><#-- parent should decide this: class="${styles.float_right!}" -->
      <form mode="get" action="<@ofbizUrl>${parameters._LAST_VIEW_NAME_}</@ofbizUrl>" id="${formId}">
        <input type="hidden" name="period" value="${period}" />
        <#-- Instead of startTime, we'll pass a startDate
        <input type="hidden" name="startTime" value="" />-->
        <input type="hidden" name="partyId" value="${parameters.partyId!}" />
        <input type="hidden" name="fixedAssetId" value="${parameters.fixedAssetId!}" />
        <input type="hidden" name="workEffortTypeId" value="${parameters.workEffortTypeId!}" />
        <input type="hidden" name="calendarType" value="${parameters.calendarType!}" />
        <input type="hidden" name="facilityId" value="${parameters.facilityId!}" />
        <input type="hidden" name="hideEvents" value="${parameters.hideEvents!}" />
        <@field type="datetime" dateType="date" name="startDate" shortDateInput=true readonly=true 
            widgetOnly=true id="dayDateSwitcher" datePostfixColumns=2 onChange="${onChangeName}();"
            value=start?string("yyyy-MM-dd")/>
      </form>
    </div>
</#macro>
