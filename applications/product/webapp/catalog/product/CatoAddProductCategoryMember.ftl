<#-- TODO: License -->

<@section title=sectionTitle>
    <form method="post" name="AddProductCategoryMember" action="<@ofbizUrl>addProductToCategory</@ofbizUrl>">
        <input type="hidden" name="productId" value="${parameters.productId!}" />    
        <@row>
            <@cell columns=12>
                <@field type="lookup" id="productCategoryId" name="productCategoryId" label=uiLabelMap.CommonCategory required=true formName="AddProductCategoryMember" fieldFormName="LookupProductCategory" />
            </@cell>
        </@row>
        <@row>
            <@cell columns=12>
                <@field type="datetime" label=uiLabelMap.CommonFrom required=true name="fromDate" size="25" maxlength="30" id="fromDate"/>
            </@cell>
        </@row>
        <@row>
            <@cell columns=12>
                <@field type="datetime" label=uiLabelMap.CommonThru name="thruDate" size="25" maxlength="30" id="thruDate"/>
            </@cell>
        </@row>
        <@row>
            <@cell columns=12>
                <@field type="input" name="sequenceNum" label=uiLabelMap.CommonSequenceNum  size=20 maxlength=40 />
            </@cell>
        </@row>
        <@row>
            <@cell columns=12>
                <@field type="text" name="quantity" label=uiLabelMap.ProductQuantity size=20 />
            </@cell>
        </@row>
        <@row>
            <@cell columns=12>
                <@field type="textarea" name="comments" label=uiLabelMap.ProductComnents rows=5 columns=5 />
            </@cell>
        </@row>
        <@field type="submit" text=uiLabelMap.CommonAdd class="+${styles.link_run_sys!} ${styles.action_add!}" />
    </form>
</@section>