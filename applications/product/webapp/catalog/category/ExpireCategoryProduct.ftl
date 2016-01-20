<#-- TODO: License -->

<@section title="${uiLabelMap.ProductExpireAllProductMembers}">
    <form method="post" action="<@ofbizUrl>expireAllCategoryProductMembers</@ofbizUrl>" name="expireAllCategoryProductMembersForm">
        <input type="hidden" name="productCategoryId" value="${productCategoryId!}" />
        <input type="hidden" name="activeOnly" value="${activeOnly.toString()}" />
        <@field type="generic" label="${uiLabelMap.CommonDate}" required=true>
            <@htmlTemplate.renderDateTimeField name="thruDate" event="" action="" className=""  title="Format: yyyy-MM-dd HH:mm:ss.SSS" value="" size="25" maxlength="30" id="thruDate2" dateType="date" shortDateInput=false timeDropdownParamName="" defaultDateTimeString="" localizedIconTitle="" timeDropdown="" timeHourName="" classString="" hour1="" hour2="" timeMinutesName="" minutes="" isTwelveHour="" ampmName="" amSelected="" pmSelected="" compositeType="" formName=""/>
        </@field>
        <@field type="submitarea">
            <input type="submit" value="${uiLabelMap.CommonExpireAll}" class="${styles.link_run_sys!} ${styles.action_terminate!}" />
        </@field>
    </form>
</@section>
