<@form name="AddCustRequestContent" method="POST" action="createCustRequestContent" enctype="multipart/form-data"> 
        <#-- <actions>
            <set field="custRequestId" from-field="parameters.custRequestId"/>
            <entity-one entity-name="StatusItem" value-field="currentStatus" auto-field-map="false">
                <field-map field-name="statusId" from-field="content.statusId"/>
            </entity-one>
            <entity-one entity-name="DataResource" value-field="dataResource" auto-field-map="false">
                <field-map field-name="dataResourceId" from-field="content.dataResourceId"/>
            </entity-one>
        </actions> -->
        <@field type="hidden" name="custRequestId" value=parameters.custRequestId!/>
        <@field type="lookup" name="contentId" label="Existing Content Id" formName="AddCustRequestContent" fieldFormName="LookupTreeContent" />            
            
        <@field type="select" name="contentTypeId" label=uiLabelMap.ContenTypeId>
        	<#list contentTypes as contentType>
        		<option value="${contentType.contentTypeId}">${contentType.description}</option>
        	</#list>
        </@field>
        
        <@field type="select" name="contentTypeId" label=uiLabelMap.ContenTypeId>
        	<#list contentTypes as contentType>
        		<option value="${contentType.contentTypeId}">${contentType.description}</option>
        	</#list>
        </@field>
        
        <@field type="select" name="statusId" label="${uiLabelMap.CommonStatus}">
        	<#list statusItems as statusItem>
        		<option value="${statusItem.statusId}">${statusItem.description}</option>
        	</#list>
        </@field>
        <@field type="file" name="dataResourceName" label=uiLabelMap.CommonUpload required=true />
        
        <@field type="lookup" name="contentIdFrom" label=uiLabelMap.ContentCompDocParentContentId formName="AddCustRequestContent" fieldFormName="LookupDetailContentTree" />
        <@field type="submit" name="createAction" class="${styles.link_run_sys} ${styles.action_add}" />
</@form>