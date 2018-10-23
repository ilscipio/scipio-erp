<#if prodCatalogId?has_content>
    <@section title=sectionTitle>    
        <form method="post" action="<@ofbizUrl>addProductCategoryToProdCatalog</@ofbizUrl>" name="addProductCategoryMemberForm">    
            <input type="hidden" name="prodCatalogId" value="${prodCatalogId}"/>
            <@row>
                <@cell columns=12>
                    <@field type="lookup" id="productCategoryId" name="productCategoryId" label=uiLabelMap.ProductCategory required=true formName="addProductCategoryMemberForm" fieldFormName="LookupProductCategory" value=((requestParameters.productCategoryId)!)/>
                </@cell>
            </@row>
            <@row>
                <@cell columns=12>
                    <@field type="datetime" label=uiLabelMap.CommonFrom required=true name="fromDate" value=((requestParameters.fromDate)!) size="25" maxlength="30" id="fromDate1"/>
                </@cell>
            </@row>
            <@row>
                <@cell columns=12>
                    <@field type="select" label=uiLabelMap.CommonType name="prodCatalogCategoryTypeId" size="1" required=true>
                        <#assign selectedKey = "">
                        <#list prodCatalogCategoryTypes as prodCatalogCategoryTypeData>
                            <#if requestParameters.prodCatalogCategoryTypeId?has_content>
                                <#assign selectedKey = requestParameters.prodCatalogCategoryTypeId>                               
                            </#if>                           
                            <option <#if selectedKey == (prodCatalogCategoryTypeData.prodCatalogCategoryTypeId!)> selected="selected"</#if> value="${prodCatalogCategoryTypeData.prodCatalogCategoryTypeId}">${prodCatalogCategoryTypeData.get("description",locale)}</option>
                        </#list>
                    </@field>
                </@cell>
            </@row>
            <@row>
                <@cell columns=12>
                    <@field type="datetime" label=uiLabelMap.CommonThru name="thruDate" value="" size="25" maxlength="30" id="fromDate2" value=((requestParameters.thruDate)!)/>                      
                </@cell>
            </@row>
            <@row>
                <@cell columns=12>
                    <@field type="input" name="sequenceNum" label=uiLabelMap.CommonSequenceNum value=((requestParameters.sequenceNum)!) size=20 maxlength=40 />
                </@cell>
            </@row>

            <@row>
                <@cell>
                    <@field type="submit" name="Add" text=uiLabelMap.CommonAdd class="+${styles.link_run_sys!} ${styles.action_update!}"/>
                </@cell>
            </@row>            
        </form>
    </@section>
</#if>