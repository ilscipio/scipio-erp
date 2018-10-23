<#-- TODO: License -->
<@modal id="removeExpiredCategoryProductMembers">
    <@section title=uiLabelMap.ProductRemoveExpiredProductMembers>
        <form method="post" action="<@ofbizUrl>removeExpiredCategoryProductMembers</@ofbizUrl>" name="removeExpiredCategoryProductMembersForm">
            <input type="hidden" name="productCategoryId" value="${productCategoryId!}" />
            <input type="hidden" name="activeOnly" value="${activeOnly.toString()}" />
            <@field type="datetime" label=uiLabelMap.Date required=true name="validDate" value="" size="25" maxlength="30" id="validDate2"/>
            <@field type="submit" text=uiLabelMap.CommonRemoveExpired class="+${styles.link_run_sys!} ${styles.action_remove!}" />
        </form>
    </@section>
</@modal>
<@script>
    $(document).ready(function() {
        $('#modal_removeExpiredCategoryProductMembers').foundation('reveal','open');
    });
</@script>
