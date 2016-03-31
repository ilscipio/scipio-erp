<#-- TODO: License -->

<@section>
    <form method="post" action="<@ofbizUrl>addProductCategoryToCategory</@ofbizUrl>" name="AddCategory">        
        <input type="hidden" name="showProductCategoryId" value="${productCategoryId!}" />
        <@row>
            <@cell columns=12>
                <#if productCategoryAssociationMode == "child">
                    <input type="hidden" name="parentProductCategoryId" value="${productCategoryId!}" />
                    <@field type="lookup" id="productCategoryId" name="productCategoryId" label=uiLabelMap.ProductCategory required=true formName="AddCategory" fieldFormName="LookupProductCategory" value=((requestParameters.productCategoryId)!)/>
                <#else>
                    <input type="hidden" name="productCategoryId" value="${productCategoryId!}" />
                    <@field type="lookup" id="parentProductCategoryId" name="parentProductCategoryId" label=uiLabelMap.ProductCategory required=true formName="AddCategory" fieldFormName="LookupProductCategory" value=((requestParameters.parentProductCategoryId)!)/>
                </#if>
            </@cell>
        </@row>
        <@row>
            <@cell columns=12>
                <@field type="datetime" label=uiLabelMap.CommonFrom required=true name="fromDate" value=((requestParameters.fromDate)!) size="25" maxlength="30" id="fromDateAdd1_${productCategoryAssociationMode}"/>
            </@cell>
        </@row>
        <@row>
            <@cell columns=12>
                <@field type="datetime" label=uiLabelMap.CommonThru name="thruDate" value=((requestParameters.thruDate)!) size="25" maxlength="30" id="fromDateAdd2_${productCategoryAssociationMode}"/>
            </@cell>
        </@row>
        <@row>
            <@cell columns=12>
                <@field type="input" name="sequenceNum" label=uiLabelMap.CommonSequenceNum value=((requestParameters.sequenceNum)!) size=20 maxlength=40 />
            </@cell>
        </@row>
        <@field type="submit" text=uiLabelMap.CommonAdd class="+${styles.link_run_sys!} ${styles.action_add!}" />
    </form>
</@section>