<@section title=sectionTitle>
    <form id="PrepareAddCategoryContentAssoc" name="PrepareAddCategoryContentAssoc" method="post" action="<@ofbizUrl>EditCategoryContent</@ofbizUrl>">
        <@field type="hidden" name="productCategoryId" value="${productCategoryId!}" />
        <@field type="select" label=uiLabelMap.ProductProdCatContentTypeId name="prodCatContentTypeId" size="1" required=true>
            <option value="">--</option>
            <#assign selectedKey = "">
            <#list productCategoryContentTypeList as productCategoryContentType>
                <#if requestParameters.prodCatContentTypeId?has_content>
                    <#assign selectedKey = requestParameters.prodCatContentTypeId>                               
                </#if>                           
                <option <#if selectedKey == productCategoryContentType.prodCatContentTypeId!> selected="selected"</#if> value="${productCategoryContentType.prodCatContentTypeId}">${productCategoryContentType.get("description",locale)}</option>
            </#list>
        </@field>
        <@script>
            jQuery(document).ready(function() {
                $('select[name=prodCatContentTypeId]').on('change', function(e) {
                    console.log("submitting form " + e);
                    document.forms['PrepareAddCategoryContentAssoc'].submit();
                });
            });
        </@script>         
    </form>
    <#if prodCatContentTypeId?has_content>
        <#if contentFormName == "EditCategoryContentSimpleText">
            <form id="EditCategoryContentSimpleText" name="EditCategoryContentSimpleText" method="post" action="<@ofbizUrl>createSimpleTextContentForCategory</@ofbizUrl>">
                <@row>
                    <@cell columns=12>
                        <@field type="datetime" label=uiLabelMap.CommonFrom required=true name="fromDate" value=((requestParameters.fromDate)!) size="25" maxlength="30" id="fromDate1"/>
                    </@cell>
                </@row>
                <@row>
                    <@cell columns=12>
                        <@field type="datetime" label=uiLabelMap.CommonThru name="thruDate" value=((requestParameters.thruDate)!) size="25" maxlength="30" id="fromDate2"/>
                    </@cell>
                </@row>
                <@row>
                    <@cell columns=12>
                        <@field type="input" label=uiLabelMap.FormFieldTitle_useDaysLimit name="useDaysLimit" />
                    </@cell>
                </@row>
                <@row>
                    <@cell columns=12>
                        <@field type="input" label=uiLabelMap.CommonDescription name="description" />
                    </@cell>
                </@row>
                <@row>
                    <@cell columns=12>
                        <@field type="input" label=uiLabelMap.CommonLocaleString name="localeString" />
                    </@cell>
                </@row>
                <@row>
                    <@cell columns=12>
                        <@field type="textarea" label=uiLabelMap.CommonText name="text" cols=80 rows=10 />
                    </@cell>
                </@row>
                <@row>
                    <@cell columns=12>
                        <@field type="submit" name="Add" text=uiLabelMap.CommonAdd class="+${styles.link_run_sys!} ${styles.action_update!}"/>
                    </@cell>
                </@row>
            </form>
        <#elseif contentFormName == "EditCategoryContentSEO">
            <form id="EditCategoryContentSEO" name="EditCategoryContentSEO" method="post" action="<@ofbizUrl>createContentSEOForCategory</@ofbizUrl>">
                <@row>
                    <@cell columns=12>
                        <@field type="input" label=uiLabelMap.PageTitle name="title" />
                    </@cell>
                </@row>
                <@row>
                    <@cell columns=12>
                        <@field type="textarea" label=uiLabelMap.MetaKeywords name="metaKeyword" cols=60 rows=5 />
                    </@cell>
                </@row>
                <@row>
                    <@cell columns=12>
                        <@field type="textarea" label=uiLabelMap.MetaDescription name="metaDescription" cols=60 rows=5 />
                    </@cell>
                </@row>
                <@row>
                    <@cell columns=12>
                        <@field type="submit" name="Add" text=uiLabelMap.CommonAdd class="+${styles.link_run_sys!} ${styles.action_update!}"/>
                    </@cell>
                </@row>
            </form>
        <#elseif contentFormName == "EditCategoryContentRelatedUrl">
            <form id="EditCategoryContentRelatedUrl" name="EditCategoryContentRelatedUrl" method="post" action="<@ofbizUrl>createRelatedUrlContentForCategory</@ofbizUrl>">
                <@row>
                    <@cell columns=12>
                        <@field type="datetime" label=uiLabelMap.CommonFrom required=true name="fromDate" value=((requestParameters.fromDate)!) size="25" maxlength="30" id="fromDate1"/>
                    </@cell>
                </@row>
                <@row>
                    <@cell columns=12>
                        <@field type="datetime" label=uiLabelMap.CommonThru name="thruDate" value=((requestParameters.thruDate)!) size="25" maxlength="30" id="fromDate2"/>
                    </@cell>
                </@row>
                <@row>
                    <@cell columns=12>
                         <@field type="input" label=uiLabelMap.CommonTitle name="title" size="50" />
                    </@cell>
                </@row>
                <@row>
                    <@cell columns=12>
                        <@field type="textarea" label=uiLabelMap.CommonDescription name="title" cols=50 rows=2 />
                    </@cell>
                </@row>
                <@row>
                    <@cell columns=12>
                        <@field type="textarea" label=uiLabelMap.CommonUrl name="url" cols=50 rows=2 />
                    </@cell>
                </@row>
                <@row>
                    <@cell columns=12>
                        <@field type="lookup" label=uiLabelMap.CommonLocaleString name="localeString" formName="EditCategoryContentRelatedUrl" fieldFormName="LookupLocale" />
                    </@cell>
                </@row>
                <@row>
                    <@cell columns=12>
                        <@field type="submit" name="Add" text=uiLabelMap.CommonAdd class="+${styles.link_run_sys!} ${styles.action_update!}"/>
                    </@cell>
                </@row>
            </form>
        <#elseif contentFormName == "EditCategoryContentDownload">   
            <form id="EditCategoryContentDownload" name="EditCategoryContentDownload" method="post" action="<@ofbizUrl>createDownloadContentForCategory</@ofbizUrl>">
                <@row>
                    <@cell columns=12>
                         <@field type="input" label=uiLabelMap.ProductContentId name="contentId" />
                    </@cell>
                </@row>
                <@row>
                    <@cell columns=12>
                        <@field type="datetime" label=uiLabelMap.CommonFrom required=true name="fromDate" value=((requestParameters.fromDate)!) size="25" maxlength="30" id="fromDate1"/>
                    </@cell>
                </@row>
                <@row>
                    <@cell columns=12>
                        <@field type="datetime" label=uiLabelMap.CommonThru required=true name="thruDate" value=((requestParameters.thruDate)!) size="25" maxlength="30" id="fromDate2"/>
                    </@cell>
                </@row>
                <@row>
                    <@cell columns=12>
                        <@field type="datetime" label=uiLabelMap.CommonPurchaseFrom required=true name="purchaseFromDate" value=((requestParameters.purchaseFromDate)!) size="25" maxlength="30" id="fromDate3"/>
                    </@cell>
                </@row>
                <@row>
                    <@cell columns=12>
                        <@field type="datetime" label=uiLabelMap.CommonPurchaseThru name="purchaseThruDate" value=((requestParameters.purchaseThruDate)!) size="25" maxlength="30" id="fromDate4"/>
                    </@cell>
                </@row>
                <@row>
                    <@cell columns=12>
                         <@field type="input" label=uiLabelMap.ProductUseCountLimit name="useCountLimit" />
                    </@cell>
                </@row>
                <@row>
                    <@cell columns=12>
                        <@field type="input" label=uiLabelMap.ProductUseTime name="useDaysLimit" />
                    </@cell>
                </@row>
                <@row>
                    <@cell columns=12>
                        <@field type="input" label=uiLabelMap.ProductCategoryDescription name="description" />
                    </@cell>
                </@row>
                <@row>
                    <@cell columns=12>
                        <@field type="file" label=uiLabelMap.ProductFile name="imageData" />
                    </@cell>
                </@row>
                <@row>
                    <@cell columns=12>
                        <@field type="submit" name="Add" text=uiLabelMap.CommonAdd class="+${styles.link_run_sys!} ${styles.action_update!}"/>
                    </@cell>
                </@row>
            </form> 
        </#if>
    </#if>
</@section>