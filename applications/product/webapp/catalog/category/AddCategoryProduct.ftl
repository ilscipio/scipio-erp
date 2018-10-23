<#-- TODO: License -->

<@section>
    <form method="post" action="<@ofbizUrl>addCategoryProductMember</@ofbizUrl>" name="addProductCategoryMemberForm">
        <input type="hidden" name="productCategoryId" value="${productCategoryId!}" />
        <input type="hidden" name="activeOnly" value="${activeOnly.toString()}" />
        <@row>
            <@cell columns=12>
                <@field type="lookup" label=uiLabelMap.CommonProduct required=true formName="addProductCategoryMemberForm" name="productId" id="productId" fieldFormName="LookupProduct"/>
            </@cell>
        </@row>
        <@row>
             <@cell columns=12>
                <@field type="datetime" label=uiLabelMap.CommonFrom required=true name="fromDate" value="" size="25" maxlength="30" id="fromDate1"/>
            </@cell>
        </@row>
        <@row>
            <@cell columns=12>
                <@field type="textarea" label=uiLabelMap.CommonComments name="comments" rows="2" cols="40"></@field>
            </@cell>
        </@row>
        <@field type="submit" text=uiLabelMap.CommonAdd class="+${styles.link_run_sys!} ${styles.action_add!}" />
    </form>
</@section>