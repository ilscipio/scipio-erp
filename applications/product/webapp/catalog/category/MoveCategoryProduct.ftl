<#-- TODO: License -->

<@section title="${uiLabelMap.ProductCopyProductCategoryMembersToAnotherCategory}">
    <form method="post" action="<@ofbizUrl>copyCategoryProductMembers</@ofbizUrl>" name="copyCategoryProductMembersForm">
        <input type="hidden" name="productCategoryId" value="${productCategoryId!}" />
        <input type="hidden" name="activeOnly" value="${activeOnly.toString()}" />
        <@row>
            <@cell columns=4>
        <@field type="lookup" label="${uiLabelMap.ProductTargetProductCategory}" formName="copyCategoryProductMembersForm" name="productCategoryIdTo" id="productCategoryIdTo" fieldFormName="LookupProductCategory"/>
            </@cell>
            <@cell columns=4>
        <@field type="datetime" label="${uiLabelMap.ProductOptionalFilterWithDate}" name="validDate" value="" size="25" maxlength="30" id="validDate1"/>
            </@cell>
            <@cell columns=4>
        <@field type="select" label="${uiLabelMap.ProductIncludeSubCategories}" name="recurse">
                <option value="N">${uiLabelMap.CommonN}</option>
                <option value="Y">${uiLabelMap.CommonY}</option>
        </@field>
            </@cell>
        </@row>
        <@field type="submit" text="${uiLabelMap.CommonCopy}" class="${styles.link_run_sys!} ${styles.action_copy!}" />
    </form>
</@section>
