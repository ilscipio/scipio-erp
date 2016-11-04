<@section title=sectionTitle>
    
    <#if parameters.addExistingContent?has_content>
        <@modal id="addExistingContent" label="">
            <form id="AddCategoryContentAssoc" name="AddCategoryContentAssoc" method="post" action="<@ofbizUrl>addContentToCategory</@ofbizUrl>">
                <@field type="hidden" name="productCategoryId" value=(productCategoryId!) />               
                <@field type="hidden" name="searchType" value="STARTS_WITH" />
                <@row>
                    <@cell columns=12>                        
                        <@field type="lookup" label=uiLabelMap.ProductContentId name="contentId" formName="AddCategoryContentAssoc" fieldFormName="LookupContent" value=((contentId)!) />
                    </@cell>
                </@row>
                <@field type="select" label=uiLabelMap.ProductProdCatContentTypeId name="prodCatContentTypeId" size="1" required=true>
                    <option value="">--</option>
                    <#assign selectedKey = "">
                    <#list productCategoryContentTypeList as productCategoryContentType>
                        <#if prodCatContentTypeId?has_content>
                            <#assign selectedKey = prodCatContentTypeId>                               
                        </#if>                           
                        <option<#if selectedKey == (productCategoryContentType.prodCatContentTypeId!)> selected="selected"</#if> value="${productCategoryContentType.prodCatContentTypeId}">${productCategoryContentType.get("description",locale)}</option>
                    </#list>
                </@field>
                <@row>
                    <@cell columns=12>
                        <@field type="datetime" label=uiLabelMap.CommonFrom required=true name="fromDate" value=((fromDate)!) size="25" maxlength="30" id="fromDate1"/>
                    </@cell>
                </@row>
                <@row>
                    <@cell columns=12>
                        <@field type="datetime" label=uiLabelMap.CommonThru name="thruDate" value=((thruDate)!) size="25" maxlength="30" id="fromDate2"/>
                    </@cell>
                </@row>
                <@row>
                    <@cell columns=12>
                        <@field type="datetime" label=uiLabelMap.FormFieldTitle_purchaseFromDate name="purchaseFromDate" value=((purchaseFromDate)!) size="25" maxlength="30" id="fromDate3"/>
                    </@cell>
                </@row>
                <@row>
                    <@cell columns=12>
                        <@field type="datetime" label=uiLabelMap.FormFieldTitle_purchaseThruDate name="purchaseThruDate" value=((purchaseThruDate)!) size="25" maxlength="30" id="fromDate4"/>
                    </@cell>
                </@row>
                <@row>
                    <@cell columns=12>
                         <@field type="input" label=uiLabelMap.ProductUseCountLimit name="useCountLimit" value=((useCountLimit)!) />
                    </@cell>
                </@row>
                <@row>
                    <@cell columns=12>
                        <@field type="input" label=uiLabelMap.ProductUseTime name="useDaysLimit" value=((useDaysLimit)!) />
                    </@cell>
                </@row>
                <@row>
                    <@cell columns=12>
                        <@field type="submit" name="Add" text=uiLabelMap.CommonAdd class="+${styles.link_run_sys!} ${styles.action_add!}"/>
                    </@cell>
                </@row>
            </form>
        </@modal>
        <@script>
            $(document).ready(function() { 
                console.log("before revealing...");
                $('#addExistingContent_modal').foundation('reveal','open');
            });
        </@script>
    </#if>

    <#if !prodCatContentTypeId?has_content>
        <form id="PrepareAddCategoryContentAssoc" name="PrepareAddCategoryContentAssoc" method="post" action="<@ofbizUrl>EditCategoryContent</@ofbizUrl>">
            <@field type="hidden" name="productCategoryId" value=(productCategoryId!) />
            <@field type="select" label=uiLabelMap.ProductSelectProductCategoryContentTypeId name="prodCatContentTypeId" size="1" required=true>
                <option value="">--</option>
                <#assign selectedKey = "">
                <#list productCategoryContentTypeList as productCategoryContentType>
                    <#if prodCatContentTypeId?has_content>
                        <#assign selectedKey = prodCatContentTypeId>                               
                    </#if>                           
                    <option<#if selectedKey == (productCategoryContentType.prodCatContentTypeId!)> selected="selected"</#if> value="${productCategoryContentType.prodCatContentTypeId}">${productCategoryContentType.get("description",locale)}</option>
                </#list>
            </@field>
            <@field type="submit" value=uiLabelMap.CommonCreate />        
        </form>
    <#else>
        <#assign fromDate=((requestParameters.fromDate)!) />
        <#if productCategoryContent?has_content>
            <#assign fromDate=((productCategoryContent.fromDate)!) />
        </#if>
        <#assign thruDate=((requestParameters.fromDate)!) />
        <#if productCategoryContent?has_content>
            <#assign thruDate=((productCategoryContent.thruDate)!) />
        </#if>       
        <#assign purchaseFromDate=((requestParameters.purchaseFromDate)!) />
        <#if productCategoryContent?has_content>
            <#assign purchaseFromDate=((productCategoryContent.purchaseFromDate)!) />
        </#if>
        <#assign purchaseThruDate=((requestParameters.purchaseThruDate)!) />
        <#if productCategoryContent?has_content>
            <#assign purchaseThruDate=((productCategoryContent.purchaseThruDate)!) />
        </#if>
        <#assign useCountLimit=((requestParameters.useCountLimit)!) />
        <#if productCategoryContent?has_content>
            <#assign useCountLimit=((productCategoryContent.useCountLimit)!) />
        </#if>
        <#assign useDaysLimit=((requestParameters.useDaysLimit)!) />
        <#if productCategoryContent?has_content>
            <#assign useDaysLimit=((productCategoryContent.useDaysLimit)!) />
        </#if>
        <#assign description=((requestParameters.description)!) />
        <#if content?has_content>
            <#assign description=((content.description)!) />
        </#if>

        <form id="${contentFormName}" name="${contentFormName}" method="post" <#if contentFormName == "EditCategoryContentDownload">enctype="multipart/form-data"</#if> action="<@ofbizUrl>${contentFormAction}</@ofbizUrl>">
            <@field type="hidden" name="productCategoryId" value=(productCategoryId!) />
            <@field type="hidden" name="prodCatContentTypeId" value=(prodCatContentTypeId!) />
            <#if content?has_content>
                <@field type="hidden" name="dataResourceId" value=(content.dataResourceId!) />
            </#if>          
            <@row>
                <@cell columns=12>
                    <@field type="display" label=uiLabelMap.ProductProdCatContentTypeId name="prodCatContentTypeIdDisplay" value=prodCatContentTypeId />
                </@cell>
            </@row>
            <@row>
                <@cell columns=12>
                    <#if productCategoryContent?has_content>
                        <@field type="display" label=uiLabelMap.ProductContentId name="contentIdDisplay" value=productCategoryContent.contentId />
                        <@field type="hidden" name="contentId" value=productCategoryContent.contentId />
                    <#else>
                        <@field type="input" label=uiLabelMap.ProductContentId name="contentId" value=((requestParameters.contentId)!) />
                    </#if>
                </@cell>
            </@row>
            <#-- EditCategoryContentSimpleText Form -->
            <#if contentFormName == "EditCategoryContentSimpleText">
                <@row>
                    <@cell columns=12>
                        <@field type="datetime" label=uiLabelMap.CommonFrom required=true name="fromDate" value=((fromDate)!) size="25" maxlength="30" id="fromDate1"/>
                    </@cell>
                </@row>
                <@row>
                    <@cell columns=12>
                        <@field type="datetime" label=uiLabelMap.CommonThru name="thruDate" value=((thruDate)!) size="25" maxlength="30" id="fromDate2"/>
                    </@cell>
                </@row>
                <@row>
                    <@cell columns=12>
                        <@field type="input" label=uiLabelMap.FormFieldTitle_useDaysLimit name="useDaysLimit" value=((useDaysLimit)!) />
                    </@cell>
                </@row>
                <@row>
                    <@cell columns=12>
                        <@field type="input" label=uiLabelMap.CommonDescription name="description" value=((description)!) />
                    </@cell>
                </@row>
                <@row>
                    <@cell columns=12>
                        <#assign localeString=((requestParameters.localeString)!) />
                        <#if content?has_content>
                            <#assign localeString=((content.localeString)!) />
                        </#if>
                        <@field type="input" label=uiLabelMap.ProductLocaleString name="localeString" value=((localeString)!) />
                    </@cell>
                </@row>
                <@row>
                    <@cell columns=12>
                        <#assign textData=((requestParameters.textData)!) />
                        <#if textDataMap?has_content>
                            <#assign textData=((textDataMap.textData)!) />
                            <@field type="hidden" name="textDataResourceId" value=textDataMap.dataResourceId />
                        </#if>
                        <@field type="textarea" label=uiLabelMap.CommonText name="text" cols=80 rows=10 value=((textData)!)/>
                    </@cell>
                </@row>
            <#-- EditCategoryContentSEO Form -->
            <#elseif contentFormName == "EditCategoryContentSEO">
                <@row>
                    <@cell columns=12>
                        <#assign textData=((requestParameters.textData)!) />
                        <#if textDataMap?has_content>
                            <#assign text=((textDataMap.textData)!) />
                            <@field type="hidden" name="dataResourceId" value=textDataMap.dataResourceId />
                        </#if>
                        <#if prodCatContentTypeId == "PAGE_TITLE">
                            <@field type="input" label=uiLabelMap.PageTitle name="title" value=((text)!) />
                        <#elseif prodCatContentTypeId == "META_KEYWORD">
                            <@field type="textarea" label=uiLabelMap.MetaKeywords name="metaKeyword" cols=60 rows=5 value=((text)!) />
                        <#elseif prodCatContentTypeId == "META_DESCRIPTION">
                            <@field type="textarea" label=uiLabelMap.MetaDescription name="metaDescription" cols=60 rows=5 value=((text)!) />
                        </#if>
                    </@cell>
                </@row>
            <#-- EditCategoryContentRelatedUrl Form -->
            <#elseif contentFormName == "EditCategoryContentRelatedUrl">                
                <@row>
                    <@cell columns=12>
                        <@field type="datetime" label=uiLabelMap.CommonFrom required=true name="fromDate" value=((fromDate)!) size="25" maxlength="30" id="fromDate1"/>
                    </@cell>
                </@row>
                <@row>
                    <@cell columns=12>
                        <@field type="datetime" label=uiLabelMap.CommonThru name="thruDate" value=((thruDate)!) size="25" maxlength="30" id="fromDate2"/>
                    </@cell>
                </@row>
                <@row>
                    <@cell columns=12>
                        <#assign title=((requestParameters.title)!) />
                        <#if contentDataResourceView?has_content>
                            <#assign title=((contentDataResourceView.drDataResourceName)!) />                            
                        </#if> 
                        <@field type="input" required=true label=uiLabelMap.CommonTitle name="title" value=((title)!) size="50" />
                    </@cell>
                </@row>
                <@row>
                    <@cell columns=12>
                        <#assign description=((requestParameters.description)!) />
                        <#if contentDataResourceView?has_content>
                            <#assign description=((contentDataResourceView.description)!) />
                        </#if>
                        <@field type="textarea" required=true label=uiLabelMap.CommonDescription name="description" value=((description)!) cols=50 rows=2 />
                    </@cell>
                </@row>
                <@row>
                    <@cell columns=12>
                        <#assign url=((requestParameters.url)!) />
                        <#if contentDataResourceView?has_content>
                            <#assign url=((contentDataResourceView.drObjectInfo)!) />
                        </#if>
                        <@field type="textarea" required=true label=uiLabelMap.CommonUrl name="url" value=((url)!) cols=50 rows=2 />
                    </@cell>
                </@row>
                <@row>
                    <#assign localeString=((requestParameters.localeString)!) />
                    <#if contentDataResourceView?has_content>
                        <#assign localeString=((contentDataResourceView.localeString)!) />
                    </#if>
                    <@cell columns=12>
                        <@field type="lookup" label=uiLabelMap.ProductLocaleString name="localeString" formName="EditCategoryContentRelatedUrl" fieldFormName="LookupLocale" value=((localeString)!) />
                    </@cell>
                </@row>
            <#-- EditCategoryContentDownload Form -->
            <#elseif contentFormName == "EditCategoryContentDownload">
                <@row>
                    <@cell columns=12>
                        <@field type="datetime" label=uiLabelMap.CommonFrom required=true name="fromDate" value=((fromDate)!) size="25" maxlength="30" id="fromDate1"/>
                    </@cell>
                </@row>
                <@row>
                    <@cell columns=12>
                        <@field type="datetime" label=uiLabelMap.CommonThru name="thruDate" value=((thruDate)!) size="25" maxlength="30" id="fromDate2"/>
                    </@cell>
                </@row>
                <@row>
                    <@cell columns=12>
                        <@field type="datetime" label=uiLabelMap.FormFieldTitle_purchaseFromDate name="purchaseFromDate" value=((purchaseFromDate)!) size="25" maxlength="30" id="fromDate3"/>
                    </@cell>
                </@row>
                <@row>
                    <@cell columns=12>
                        <@field type="datetime" label=uiLabelMap.FormFieldTitle_purchaseThruDate name="purchaseThruDate" value=((purchaseThruDate)!) size="25" maxlength="30" id="fromDate4"/>
                    </@cell>
                </@row>
                <@row>
                    <@cell columns=12>
                         <@field type="input" label=uiLabelMap.ProductUseCountLimit name="useCountLimit" value=((useCountLimit)!) />
                    </@cell>
                </@row>
                <@row>
                    <@cell columns=12>
                        <@field type="input" label=uiLabelMap.ProductUseTime name="useDaysLimit" value=((useDaysLimit)!) />
                    </@cell>
                </@row>
                <@row>
                    <@cell columns=12>
                        <@field type="input" label=uiLabelMap.ProductCategoryDescription name="description" value=((description)!) />
                    </@cell>
                </@row>
                <@row>
                    <@cell columns=12>
                        
                        
                        <@field type="file" required=true label=uiLabelMap.ProductFile name="imageData" id="imageData" placeholder=((contentDataResourceView.drDataResourceName)!)/>                            
                        
                        <#if contentDataResourceView?has_content>
                            <@field type="hidden" name="dataResourceTypeId" value=(contentDataResourceView.drDataResourceTypeId!) />                        
                            <@field type="hidden" name="fileDataResourceId" value=(contentDataResourceView.dataResourceId!) />
                            <a href="<@ofbizInterWebappUrl>/content/control/ViewBinaryDataResource?dataResourceId=${contentDataResourceView.dataResourceId!}</@ofbizInterWebappUrl>">
                                <@field type="display" value=(contentDataResourceView.drDataResourceName!) />
                            </a>
                        </#if>
                    </@cell>
                </@row>
            </#if>
            <@row>
                <@cell columns=12>
                    <#if productCategoryContent?has_content>
                        <@field type="submit" name="Update" text=uiLabelMap.CommonUpdate class="+${styles.link_run_sys!} ${styles.action_update!}"/>
                    <#else>
                        <@field type="submit" name="Add" text=uiLabelMap.CommonAdd class="+${styles.link_run_sys!} ${styles.action_add!}"/>
                    </#if>
                </@cell>
            </@row>
        </form> 
    </#if>
</@section>