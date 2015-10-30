<#-- TODO: License -->

<@section title="${uiLabelMap.ProductAddProductCategoryMember}">
    <form method="post" action="<@ofbizUrl>addCategoryProductMember</@ofbizUrl>" name="addProductCategoryMemberForm">
        <input type="hidden" name="productCategoryId" value="${productCategoryId!}" />
        <input type="hidden" name="activeOnly" value="${activeOnly.toString()}" />
        <@row>
            <@cell columns=6>
                <@field type="generic" label="${uiLabelMap.CommonProduct}" required=true>
                    <@htmlTemplate.lookupField formName="addProductCategoryMemberForm" name="productId" id="productId" fieldFormName="LookupProduct"/>
                </@field>
            </@cell>
            <@cell columns=6>
                <@field type="generic" label="${uiLabelMap.CommonFrom}" required=true>
                    <@htmlTemplate.renderDateTimeField name="fromDate" event="" action="" className=""  title="Format: yyyy-MM-dd HH:mm:ss.SSS" value="" size="25" maxlength="30" id="fromDate1" dateType="date" shortDateInput=false timeDropdownParamName="" defaultDateTimeString="" localizedIconTitle="" timeDropdown="" timeHourName="" classString="" hour1="" hour2="" timeMinutesName="" minutes="" isTwelveHour="" ampmName="" amSelected="" pmSelected="" compositeType="" formName=""/>
                </@field>
            </@cell>
        </@row>
        <@row>
            <@cell columns=12>
        <@field type="generic" label="${uiLabelMap.CommonComments}">
            <textarea name="comments" rows="2" cols="40"></textarea>
        </@field>
            </@cell>
        </@row>
        <@field type="submitarea">
            <input type="submit" value="${uiLabelMap.CommonAdd}" />
        </@field>
    </form>
</@section>
