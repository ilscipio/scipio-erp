<#-- TODO: License -->

<@modal id="expireAllCategoryProductMembers">
    <@section title=uiLabelMap.ProductExpireAllProductMembers>
        <form method="post" action="<@pageUrl>expireAllCategoryProductMembers</@pageUrl>" name="expireAllCategoryProductMembersForm">
            <input type="hidden" name="productCategoryId" value="${productCategoryId!}" />
            <input type="hidden" name="activeOnly" value="${activeOnly.toString()}" />
            <@field type="datetime" label=uiLabelMap.CommonDate required=true name="thruDate" value="" size="25" maxlength="30" id="thruDate2"/>
            <@field type="submit" text=uiLabelMap.CommonExpireAll class="+${styles.link_run_sys!} ${styles.action_terminate!}" />
        </form>
    </@section>
</@modal>
<@script>
    $(document).ready(function() {
        try {
            $('#modal_expireAllCategoryProductMembers').foundation('reveal','open');
        } catch(err) {
            try {
                $('#modal_expireAllCategoryProductMembers').modal('show');
            } catch(err) {
                //t.dispatchEvent(event);
            }
        }
    });
</@script>
