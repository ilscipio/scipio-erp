<#-- TODO: License -->

<@modal id="copyCategoryProductMembers">
    <@section title=uiLabelMap.ProductCopyProductCategoryMembersToAnotherCategory>
        <form method="post" action="<@ofbizUrl>copyCategoryProductMembers</@ofbizUrl>" name="copyCategoryProductMembersForm">
            <input type="hidden" name="productCategoryId" value="${productCategoryId!}" />
            <input type="hidden" name="activeOnly" value="${activeOnly.toString()}" />
            <@row>
                <@cell columns=12>
                    <@field type="lookup" label=uiLabelMap.ProductTargetProductCategory formName="copyCategoryProductMembersForm" name="productCategoryIdTo" id="productCategoryIdTo" fieldFormName="LookupProductCategory"/>
                </@cell>
            </@row>
            <@row>
                <@cell columns=12>
                    <@field type="datetime" label=uiLabelMap.ProductOptionalFilterWithDate name="validDate" value="" size="25" maxlength="30" id="validDate1"/>
                </@cell>
            </@row>
            <@row>
                <@cell columns=12>                    
                    <@field type="generic" label=uiLabelMap.ProductIncludeSubCategories>
                        <@field type="radio" name="recurse" value="Y" checked=((recurse)!"" == "Y") label=uiLabelMap.CommonYes />
                        <@field type="radio" name="recurse" value="N" checked=((recurse)!"" == "N") label=uiLabelMap.CommonNo />
                    </@field>
                </@cell>
            </@row>
            <@field type="submit" text=uiLabelMap.CommonCopy class="+${styles.link_run_sys!} ${styles.action_copy!}" />
        </form>
    </@section>
</@modal>
<@script>
    $(document).ready(function() {
        $('#modal_copyCategoryProductMembers').foundation('reveal','open');
    });
</@script>
