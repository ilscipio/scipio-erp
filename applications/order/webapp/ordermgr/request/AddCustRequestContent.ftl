<@form name="AddCustRequestContent" method="POST" action="createCustRequestContent" enctype="multipart/form-data">
        <@field type="hidden" name="custRequestId" value=parameters.custRequestId!/>
        <#if parameters.contentName?has_content>
            <#assign contentName = parameters.contentName>    
        <#elseif media?has_content>
            <#assign contentName = media.contentName>
        <#else>
            <#assign contentName = "">
        </#if>
        <#if media?has_content>
            <@field type="display" name="contentId" label=uiLabelMap.CommonId><a href="<@ofbizUrl>editMedia?contentId=${media.contentId}</@ofbizUrl>">${media.contentId}</a></@field>
        </#if>
        <@field type="text" name="contentName" label=uiLabelMap.CommonName value=contentName required=true />       
        
        <@field type="lookup" name="contentId" label="Existing Content Id" formName="AddCustRequestContent" fieldFormName="LookupTreeContent" />            
        <@field type="lookup" name="contentIdFrom" label=uiLabelMap.ContentCompDocParentContentId formName="AddCustRequestContent" fieldFormName="LookupDetailContentTree" />            
        <@field type="select" label=uiLabelMap.CmsMediaTypes name="dataResourceTypeId" required=true>
            <option value="IMAGE_OBJECT"<#if (parameters.dataResourceTypeId!) == "IMAGE_OBJECT"> selected</#if>>${uiLabelMap.CommonImage}</option>
            <option value="VIDEO_OBJECT"<#if (parameters.dataResourceTypeId!) == "VIDEO_OBJECT"> selected</#if>>${uiLabelMap.ContentResourceVideo}</option>
            <option value="AUDIO_OBJECT"<#if (parameters.dataResourceTypeId!) == "AUDIO_OBJECT"> selected</#if>>${uiLabelMap.ContentResourceAudio}</option>
            <option value="DOCUMENT_OBJECT"<#if (parameters.dataResourceTypeId!) == "DOCUMENT_OBJECT"> selected</#if>>${uiLabelMap.CommonDocument}</option>                               
        </@field>
        <@field type="select" name="statusId" label="${uiLabelMap.CommonStatus}">
        	<#list statusItems as statusItem>
        		<option value="${statusItem.statusId}">${statusItem.description}</option>
        	</#list>
        </@field>
        <@field type="file" name="uploadedFile" label=uiLabelMap.CommonUpload required=true />        
        
        <@field type="submit" name="createAction" class="${styles.link_run_sys} ${styles.action_add}" />
</@form>