<#-- TODO: License -->

<@section title="${uiLabelMap.ProductRemoveExpiredProductMembers}">
    <form method="post" action="<@ofbizUrl>removeExpiredCategoryProductMembers</@ofbizUrl>" name="removeExpiredCategoryProductMembersForm">
        <input type="hidden" name="productCategoryId" value="${productCategoryId!}" />
        <input type="hidden" name="activeOnly" value="${activeOnly.toString()}" />
        <@field type="generic" label="${uiLabelMap.Date}" required=true>
            <@htmlTemplate.renderDateTimeField name="validDate" event="" action="" className=""  title="Format: yyyy-MM-dd HH:mm:ss.SSS" value="" size="25" maxlength="30" id="validDate2" dateType="date" shortDateInput=false timeDropdownParamName="" defaultDateTimeString="" localizedIconTitle="" timeDropdown="" timeHourName="" classString="" hour1="" hour2="" timeMinutesName="" minutes="" isTwelveHour="" ampmName="" amSelected="" pmSelected="" compositeType="" formName=""/>
        </@field>
        <@field type="submitarea">
            <input type="submit" value="${uiLabelMap.CommonRemoveExpired}" class="${styles.link_run_sys!} ${styles.action_remove!}" />
        </@field>
    </form>
</@section>
