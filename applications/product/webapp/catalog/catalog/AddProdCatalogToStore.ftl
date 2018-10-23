<#if prodCatalogId?has_content>
    <@section title=sectionTitle>    
        <form method="post" action="<@ofbizUrl>createProdCatalogStore</@ofbizUrl>" name="AddProductStoreCatalog">    
            <input type="hidden" name="prodCatalogId" value="${prodCatalogId}"/>
            <@row>
                <@cell columns=12>
                    <@field type="select" label=uiLabelMap.CommonStore name="productStoreId" size="1" required=true>
                        <#assign selectedKey = "">
                        <#list productStoreList as productStore>
                            <#if requestParameters.roleTypeId?has_content>
                                <#assign selectedKey = requestParameters.productStoreeId>                               
                            </#if>                           
                            <option<#if selectedKey == (productStore.productStoreId!)> selected="selected"</#if> value="${productStore.productStoreId}">${productStore.storeName!(productStore.productStoreId!)}</option>
                        </#list>
                    </@field>
                </@cell>
            </@row>
             <@row>
                <@cell columns=12>
                    <@field type="datetime" label=uiLabelMap.CommonFrom required=true name="fromDate" value=((requestParameters.fromDate)!) size="25" maxlength="30" id="fromDate1"/>
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