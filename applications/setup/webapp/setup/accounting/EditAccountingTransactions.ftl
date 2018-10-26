<#-- SCIPIO: SETUP fiscal periods implementation -->

<#include "component://setup/webapp/setup/common/common.ftl">

<#macro eatMarkupOut dir args={}>
  <#if dir?is_directive>
    <@dir args=args/><#t/>
  <#else>
    ${dir}<#t/>
  </#if>
</#macro>

<#-- SCIPIO 10/23/2018: Commenting out DATEV and Elster accounting import
<#macro eatImportDatevConfirmFields args={}>
   <@field type="hidden" name="orgPartyId" value=(params.orgPartyId)!/>
   <@field type="hidden" name="topGlAccountId" value=(params.topGlAccountId)!/>
   <@field type="select" name="dataCategoryId" label=uiLabelMap.SetupAccountingDatevDataCategory>
       <#list datevDataCategories as datevDataCategory>
               <option value="${datevDataCategory.dataCategoryId}">${datevDataCategory.dataCategoryName}</option>
       </#list>    
   </@field>
   <hr/>
   <@field type="file" name="uploadedFile" label=uiLabelMap.SetupAccountingDatevImportCSV />
</#macro>
<#macro eatImportElsterConfirmFields args={}>
   <@field type="hidden" name="orgPartyId" value=(params.orgPartyId)!/>
   <@field type="hidden" name="topGlAccountId" value=(params.topGlAccountId)!/>
   <@field type="file" name="uploadedFile" label=uiLabelMap.SetupAccountingElsterImportCSV />
</#macro>

<#assign eatObjectTypes = {
    "datev": {
        "import": {
            "type": "form",
            "mode": "show",
            "id": "eat-datev",
            "confirmMsg": 'SetupAccountingImportDatevCSVProceed',
            "confirmExtraMsg": rawLabelNoSubst(''),
            "confirmFields": eatImportDatevConfirmFields,
            "formAction": makeOfbizUrl('setupImportDatevDataCategory'),
            "defaultParams": wrapRawScript("function() { return; }")
        }        
    },
    "elster": {
        "import": {
            "type": "form",
            "mode": "show",
            "id": "eat-elster",
            "confirmMsg": rawLabelNoSubst(''),
            "confirmExtraMsg": rawLabelNoSubst(''),
            "confirmFields": eatImportElsterConfirmFields,
            "formAction": makeOfbizUrl('setupImportElsterDataCategory'),
            "defaultParams": wrapRawScript("function() { return; }")
        }      
    }
}>
-->
<#assign eatObjectTypes = {
	"acctgtype" : {
		"add" : {
			"type": "form",
            "mode": "show",
            "id": "eat-acctgtype",
            "formAction": makeOfbizUrl('setupCreateAcctgTransType'),
            "defaultParams": wrapRawScript("function() { return; }")
		}
	},
	"acctgentrytype" : {
		"add" : {
			"type": "form",
            "mode": "show",
            "id": "eat-acctgentrytype",
            "formAction": makeOfbizUrl('setupCreateAcctgTransactionEntryType'),
            "defaultParams": wrapRawScript("function() { return; }")
		}
	}
}>

<#assign eatObjectTypes = toSimpleMap(eatObjectTypes!{})>
<#assign eatDialogIdPrefix = eatDialogIdPrefix!"eat-dialog-">
<#assign eatDialogIdModalPrefix = eatDialogIdModalPrefix!("modal_" + eatDialogIdPrefix)>
<#assign eatStatsIdPrefix = eatStatsIdPrefix!"eat-stats-">

<@script>
    var actionProps = <@objectAsScript object=(ectActionProps!{}) lang='js'/>;

    var openModal = function(modalElem) {
        try {
            modalElem.foundation('reveal', 'open');
        } catch(err) {
            try {
                modalElem.modal('show'); 
            }
            catch(err) {
                //t.dispatchEvent(event); // FIXME?
            }
        }
    };
    var closeModal = function(modalElem) {
        try {
            modalElem.foundation('reveal', 'close');
        } catch(err) {
            try {
                modalElem.modal('hide'); 
            }
            catch(err) {
                //t.dispatchEvent(event); // FIXME?
            }
        }
    };
    
    var showPopupMsg = function(popupMsgModalId, msg, extraMsg) {
        var modalElem = jQuery('#' + popupMsgModalId);
        if (modalElem.length) {
            jQuery('.eat-dialogmsg', modalElem).html(msg);
            jQuery('.eat-dialogextramsg', modalElem).html(extraMsg || '');
            openModal(modalElem);
        } else {
            return alert(msg);
        }        
    };    
    var showConfirmMsg = function(confirmMsgModalId, msg, extraMsg, modalElem, continueCallback) {
        if ((!modalElem || !modalElem.length) && confirmMsgModalId) {
            modalElem = jQuery('#'+ confirmMsgModalId);
        }
        if (modalElem && modalElem.length) {
            jQuery('.eat-dialogmsg', modalElem).html(msg);
            jQuery('.eat-dialogextramsg', modalElem).html(extraMsg || '');
            jQuery('.eat-dialogbtn', modalElem).click(function(e) {
                e.preventDefault();
                e.stopImmediatePropagation();
                closeModal(modalElem);
                var selectedName = extractClassNameSuffix(jQuery(this), 'eat-dialogbtn-');
                continueCallback(selectedName);      
                return;          
            });            
            openModal(modalElem);
        } else {
            var result = confirm(msg);
            if (result) {
                continueCallback();
            }
        }
        return;
    };
    
    var extractClassNameSuffix = function(elem, prefix) {
        var classes = elem.attr('class').split(/\s+/);
        var result = null;
        var startsWith = function(str, prefix) {
            return (str.lastIndexOf(prefix, 0) === 0);
        };
        jQuery.each(classes, function(i, e) {
            if (startsWith(e, prefix)) {
                result = e.substring(prefix.length);
                return false;
            }
        });
        return result;
    };
    
    var runMultipartAjax = function(data, typeAction) {
        jQuery.ajax({
            url: '<@ofbizUrl>setupImportDatevDataCategory</@ofbizUrl>',
            data: data,                            
            async: true,
            type: "POST",
            contentType: false,
            processData: false,
            enctype: 'multipart/form-data',
            success: function(content) {
                displayStats(content, typeAction);            
            },
            error: function() {
                console.log("error");
            }
        });        
    }
    
     var displayStats = function(content, typeAction) {
         var statsContainer = jQuery('#${eatStatsIdPrefix}' + typeAction[1] + '-' + typeAction[2]);
         jQuery(statsContainer).html("");
         jQuery(statsContainer).show();
         jQuery(statsContainer).html(content);
     } 
    

    jQuery(document).ready(function() {
        jQuery('li.eat-menu-action a').click(function(e) {
            var confirmMsg = "";
            var confirmExtraMsg = "";
            var typeAction = this.id.split('-');
            if (typeAction && typeAction.length == 3) {
            	console.log("type ====> " + typeAction[1] + "  action ====> " + typeAction[2]);
                var modalElem = jQuery('#${eatDialogIdModalPrefix}' + typeAction[1] + '-' + typeAction[2]);
                if (typeAction[2] == "add" || typeAction[2] == "edit") {
                	
                } else if (typeAction[2] == "import") {
	                showConfirmMsg(null, confirmMsg, confirmExtraMsg, modalElem, function(action) {
	                    if (action == 'upload') {
	                        // check if the modal had any params, dump them into params
	                        var containsFile = false;
	                        var data = new FormData(jQuery('form.eat-dialogopts-form')[0]);
	                        
	                        jQuery('form.eat-dialogopts-form :input', modalElem).each(function(i, input) {
	                            input = jQuery(input);
	                            if (!containsFile && input.attr('type') == "file") {
	                                containsFile = true;
	                            }
	                        });
	                        if (containsFile) {
	                            jQuery('form.eat-dialogopts-form', modalElem).attr('enctype', 'multipart/form-data');
	                        }
	                        
	                        jQuery('form.eat-dialogopts-form')[0].reset();
	                        
	                        runMultipartAjax(data, typeAction);
	                    }
	                });
                }
            }
        });
    });
</@script>

<#assign defaultParams = {
}>

<#assign paramMaps = getWizardFormFieldValueMaps({
    "record":true, <#-- NOTE: must fallback with boolean true -->
    "defaults":defaultParams,
    "strictRecord":true <#-- TODO: REMOVE (debugging) -->
})>
<#assign params = paramMaps.values>
<#assign fixedParams = paramMaps.fixedValues>

<#-- forms -->
<#macro setupAcctgTransTypeForm id formActionType target params>
    <@form id=id name=id action=makeOfbizUrl(target) method="post" validate=setupFormValidate>
        <@defaultWizardFormFields exclude=[]/>
        <@field type="hidden" name="isAddAcctgTransType" value=(formActionType == "add")?string("Y", "N")/>
        <@field type="hidden" name="isCreateAcctgTransType" value=(formActionType == "new")?string("Y", "N")/>
        <@field type="hidden" name="isUpdateAcctgTransType" value=(formActionType == "edit")?string("Y", "N")/>

        <#assign fieldsRequired = true>
		<#if formActionType != "edit">
	      	<@field type="display" label=uiLabelMap.FormFieldTitle_parentAcctgTransTypeId><#rt/>
	            <span class="acctg-managefield acctg-managefield-for-parentAcctgTransTypeDesc">
	            	<@setupExtAppLink uri="/accounting/control/EditGlobalGlAccount?glAccountId=${rawString(params.parentGlAccountId!)}" text=params.parentGlAccountDesc!"_NA_"/>
	           	</span><#t/>
	        </@field><#lt/>
	        <@field type="hidden" name="parentTypeId" value=(params.parentTypeId!) class="+acctg-inputfield"/>
        <#else>
	        <@field type="select" name="parentTypeId" label=uiLabelMap.CommonParent class="+acctg-inputfield">
		      <option value="" disabled="disabled"></option>
		      <#list acctgTransTypes as acctgTransType>
		        <#assign selected = (rawString(params.parentTypeId!) == (acctgTransType.parentType!))>
		        <option value="${glResourceType.glResourceTypeId!}"<#if selected> selected="selected"</#if>>${glResourceType.description!}</option>
		      </#list>
		    </@field>
	    </#if>
	    <#if formActionType == "edit">
	        <@field type="display" label=uiLabelMap.FormFieldTitle_glAccountId><#rt/>
	            <span class="acctg-managefield acctg-managefield-for-glAccountId"><@setupExtAppLink uri="/accounting/control/EditGlobalGlAccount?glAccountId=${rawString(params.glAccountId!)}" text=params.glAccountId!/></span><#t/>
	        </@field><#lt/>
	        <@field type="hidden" name="glAccountId" value=(params.glAccountId!) class="+acctg-inputfield"/>
	    <#else>
	        <#-- TODO: REVIEW: required=true -->
	        <@field type="input" name="glAccountId" label=uiLabelMap.CommonId value=(params.glAccountId!) class="+acctg-inputfield"/>
	    </#if>
    </@form>
</#macro>

<#-- modals -->
<#macro eatDefActionConfirmMsgBtn args={}>
    <div class="modal-footer ${styles.text_right!}">
        <#-- NOTE: the value "continue" is extracted from the class and passed to the callback -->
        <a class="eat-dialogbtn eat-dialogbtn-continue ${styles.button!} btn-ok">${uiLabelMap.CommonContinue}</a>
    </div>
</#macro>

<#macro eatDefActionInnerContent props>
    <p class="eat-dialogmsg"></p>
      <#if props.confirmFields??>
        <@form class="+eat-dialogopts-form" action="${props.formAction!}" method="POST">
              <@fields type="default-compact">
                <@eatMarkupOut dir=props.confirmFields />
            </@fields>
        </@form>
      </#if>
    <p class="eat-dialogextramsg"></p>
</#macro>

<#macro eatDefActionMsgModals args={}>
    <#list args.objectTypes?keys as objectType>
        <#local actionMaps = toSimpleMap(args.objectTypes[objectType])>        
        <#list actionMaps?keys as action>
            <#local props = toSimpleMap(actionMaps[action])>            
            <#if props.confirmMsg?has_content>            
                <@modal id="${args.idPrefix}${rawString(objectType)}-${rawString(action)}" class="+eat-dialogmodal">
                    <@heading>${action} ${objectType}</@heading>
                    <@eatDefActionInnerContent props=props/>
                    <div class="modal-footer ${styles.text_right!}">                       
                       <a class="eat-dialogbtn eat-dialogbtn-cancel ${styles.button!} btn-cancel" href="javascript:void(0);">${uiLabelMap.CommonCancel}</a>
                       <#if action == "import">
                           <a class="eat-dialogbtn eat-dialogbtn-upload ${styles.button!} btn-ok" >${uiLabelMap.CommonUpload}</a>
                       </#if>
                    </div>
                </@modal>
            </#if>
        </#list>        
    </#list>
</#macro>
<@eatMarkupOut dir=eatDefActionMsgModals args={    
    "objectTypes": eatObjectTypes!{},
    "idPrefix": eatDialogIdPrefix,
    "idModalPrefix": eatDialogIdModalPrefix
}/>

<#-- Stats -->
<#macro eatStats args={}>
    <#list args.objectTypes?keys as objectType>
        <#assign actionMaps = toSimpleMap(args.objectTypes[objectType])>        
        <#list actionMaps?keys as action>
            <#assign props = toSimpleMap(actionMaps[action])>    
            <div style="display:none;" id="${args.idPrefix}${rawString(objectType)}-${rawString(action)}" class="+eat-stats"></div>
        </#list>
    </#list>
</#macro>

<#-- 
<@alert type"warning">
	${rawString("Be advise that this is currently working in progress")}
</@alert>
--> 

<@section id="mainSection">
    <#-- TODO: REVIEW: may make a difference later -->
    <@defaultWizardFormFields exclude=["topGlAccountId"]/>
    <#--<@field type="hidden" name="setupContinue" value="N"/> not needed yet-->
    <@row>
        <@cell medium=5 large=5>
            <@section title=uiLabelMap.SetupAccountingTransactionTypes>
                <@table>
                    <@thead>
                        <@tr>
                            <@td>${uiLabelMap.FormFieldTitle_acctgTransTypeId}</@td>
                            <@td>${uiLabelMap.CommonDescription}</@td>                    
                        </@tr>
                    </@thead>
                    <#list acctgTransTypes as type>
                        <@tr>
                            <@td>${type.acctgTransTypeId}</@td>
                            <@td>${type.description}</@td>                                
                        </@tr>
                    </#list>
                </@table>
            </@section>
        </@cell>
		<@cell medium=4 large=4>
            <@section title=uiLabelMap.SetupAccountingTransactionEntryTypes>
                <@table>
                    <@thead>
                        <@tr>
                            <@td>${uiLabelMap.FormFieldTitle_acctgTransEntryTypeId}</@td>
                            <@td>${uiLabelMap.CommonDescription}</@td>                    
                        </@tr>
                    </@thead>
                    <#list acctgTransEntryTypes as entryType>
                        <@tr>
                            <@td>${entryType.acctgTransEntryTypeId}</@td>
                            <@td>${entryType.description}</@td>                                
                        </@tr>
                    </#list>
                </@table>
            </@section>
        </@cell>
        <@cell medium=3 large=3>    
          <#-- ACTIONS MENU -->
          <@section title=uiLabelMap.CommonActions>
            <#-- MENU -->
            <ul class="side-nav">
                <@menuitem contentId="eat-acctgtype-add" class="+eat-menu-action" type="link" href="javascript:void(0);" text=uiLabelMap.SetupAddAccountingTransactionType />
                <@menuitem contentId="eat-acctgentrytype-add" class="+eat-menu-action" type="link" href="javascript:void(0);" text=uiLabelMap.SetupAddAccountingTransactionEntryType />
                <#-- SCIPIO 10/23/2018: 
                <hr/>
                <@menuitem contentId="eat-datev-import" class="+eat-menu-action" type="link" href="javascript:void(0);" text=uiLabelMap.SetupAccountingImportDatev />                       
                <@menuitem contentId="eat-elster-import" class="+eat-menu-action" type="link" href="javascript:void(0);" text=uiLabelMap.SetupAccountingImportElster />
				-->
           </ul>
          </@section>
        </@cell>
    </@row>
</@section>

<@eatMarkupOut dir=eatStats args={    
    "objectTypes": eatObjectTypes!{},
    "idPrefix": eatStatsIdPrefix
}/>
<@section title=uiLabelMap.PageTitleAddAcctgTransType containerId="acctg-newacctgtranstype" containerClass="+acctg-newacctgtranstypeid acctg-recordaction acctg-newrecord" 
    containerStyle=((targetRecordAction == "newacctgtranstype-new")?string("","display:none;"))>
  <#if targetRecordAction == "newacctgtranstypeid-new">
    <#assign paramMaps = initialParamMaps>
  <#else>
    <#assign paramMaps = getWizardFormFieldValueMaps({
      "record":true,
      "defaults":defaultParams
    })>
  </#if>
  <@setupAcctgTransTypeForm id="NewAcctgTransType" formActionType="new" target="setupCreateAcctgTransType" params=paramMaps.values  />
</@section>