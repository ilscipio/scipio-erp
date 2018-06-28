<#assign sectionId="createCRContent">
<#if contentId?has_content>
    <#assign sectionId="viewCRContent">
</#if>

	<@section id=sectionId title="${uiLabelMap.PageTitleAddContent}">	
		<@form name="AddCustRequestContent" id="AddCustRequestContent" method="POST" action="createCustRequestContent" enctype="multipart/form-data">
		        <@field type="hidden" name="custRequestId" value=parameters.custRequestId!/>
		        <#if parameters.contentName?has_content>
		            <#assign contentName = parameters.contentName>    
		        <#elseif media?has_content>
		            <#assign contentName = media.contentName>
		        <#else>
		            <#assign contentName = "">
		        </#if>		       
		        <@field type="text" name="contentName" label=uiLabelMap.CommonName value=contentName required=true />      
		        
		        <#-- 
		        <@field type="lookup" name="contentId" label="Existing Content Id" formName="AddCustRequestContent" fieldFormName="LookupTreeContent" />            
		        <@field type="lookup" name="contentIdFrom" label=uiLabelMap.ContentCompDocParentContentId formName="AddCustRequestContent" fieldFormName="LookupDetailContentTree" />    
		        -->     
		                   
		        <@field type="select" label=uiLabelMap.CommonMediaTypes name="dataResourceTypeId" required=true>
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
		        <@field type="file" name="uploadedFile" label=uiLabelMap.CommonMedia required=true />	
				 
			    <@field type="submitarea">
			        <input type="submit" value="${uiLabelMap.CommonUpload}" class="${styles.link_run_sys!} ${styles.action_create!}" />
			    </@field>
		</@form>
	</@section>
	
	
		<@row>
	        <@cell columns=6 last=true>    
				<#assign progressOptions = {
			        "formSel" : "#AddCustRequestContent",
			        "progBarId" : "media_progress_bar",
			        "progTextBoxId" : "mediaProgressMsgBox",
			        
			        "msgContainerParentSel" : "#contentWrapper",
			        "msgContainerInsertMode" : "prepend",
			        
			        "iframeParentSel" : "#iframePlaceHolder",
			        "expectedResultContainerSel" : "#viewCRContent",
			        
			        "successResultContainerSel" : "#main-${styles.alert_wrap!} .alert-box.info",
			        "successResultAddWrapper" : true,
			        
			        "errorResultContainerSel" : "#main-${styles.alert_wrap!} .alert-box.alert",
			        "errorResultAddWrapper" : true,                        
			         
			        "resultContentReplace" : true,
			        "contentContainerSel" : "#createCRContent",
			        "resultContentContainerSel" : "#viewCRContent",
			        
			        "submitHook" : "validate"
			    }>
			    <@field type="submitarea" progressOptions=progressOptions label=uiLabelMap.CommonAdd />
	    	</@cell>
	    </@row>
    

<div id="iframePlaceHolder"></div>