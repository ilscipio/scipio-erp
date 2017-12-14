<#-- SCIPIO: SETUP fiscal periods implementation -->

<#include "component://setup/webapp/setup/common/common.ftl">

<#macro eatMarkupOut dir args={}>
  <#if dir?is_directive>
    <@dir args=args/><#t/>
  <#else>
    ${dir}<#t/>
  </#if>
</#macro>

<#macro eatImportDatevConfirmFields args={}>
   <@field type="hidden" name="organizationPartyId" value=(params.orgPartyId)!/>
   <@field type="input" name="delimiter" size="3" maxLength="1" label=uiLabelMap.SetupAccountingDatevCSVDelimiter />
   <@field type="input" name="quote" size="3" maxLength="1" label=uiLabelMap.SetupAccountingDatevCSVQuote />
   <@field type="checkbox" name="hasMetaHeader" label=uiLabelMap.SetupAccountingDatevCSVDelimiter />
   <@field type="checkbox" name="hasHeader" label=uiLabelMap.SetupAccountingDatevCSVDelimiter />
   <@field type="file" name="uploadedFile" label=uiLabelMap.SetupAccountingDatevImportCSV />
</#macro>
<#macro eatImportElsterConfirmFields args={}>
   <@field type="hidden" name="organizationPartyId" value=(params.orgPartyId)!/>
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
            "formAction": makeOfbizUrl('setupImportDatevTransactionEntries'),
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
            "formAction": makeOfbizUrl('setupImportElsterTransactionEntries'),
            "defaultParams": wrapRawScript("function() { return; }")
        }      
    }
}>

<#assign eatObjectTypes = toSimpleMap(eatObjectTypes!{})>
<#assign eatDialogIdPrefix = eatDialogIdPrefix!"eat-dialog-">
<#assign eatDialogIdModalPrefix = eatDialogIdModalPrefix!("modal_" + eatDialogIdPrefix)>

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
            jQuery('.eat-dialogbtn', modalElem).click(function() {
                closeModal(modalElem);
                var selectedName = extractClassNameSuffix(jQuery(this), 'eat-dialogbtn-');
                continueCallback(selectedName);
            });
            openModal(modalElem);
        } else {
            var result = confirm(msg);
            if (result) {
                continueCallback();
            }
        }
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

	jQuery(document).ready(function() {
		jQuery('.eat-menu-action').click(function(){
			var confirmMsg = "";
			var confirmExtraMsg = "";
			var typeAction = this.id.split('-');
			if (typeAction && typeAction.length == 3) {			
	            var modalElem = jQuery('#${eatDialogIdModalPrefix}' + typeAction[1] + '-' + typeAction[2]); 
	            showConfirmMsg(null, confirmMsg, confirmExtraMsg, modalElem, function(subActionType) {
	            	/*params.subActionType = subActionType;
	                if (preParamNamesMap && preParamNamesMap.subActionType) {
	                    if (typeof preParamNamesMap.subActionType === 'function') {
	                        preParamNamesMap.subActionType(subActionType, params, ai); 
	                    } else {
	                        params[preParamNamesMap.subActionType] = subActionType;
	                    }
	                }*/
	            
	            	// check if the modal had any params, dump them into params
	            	var containsFile = false;
	                jQuery('form.eat-dialogopts-form :input', modalElem).each(function(i, input) {	                	
	                    input = jQuery(input);
	                    console.log("input type ===> " + input.attr('type'));
	                    /*var name = input.prop('name');
	                    if (name) params[name] = input.val();*/
	                    if (!containsFile && input.attr('type') == "file") {
	                    	containsFile = true;
	                    }
	                });
	                if (containsFile) {
	                	console.log("containsFile");
	                	jQuery('form.eat-dialogopts-form', modalElem).attr('enctype', 'multipart/form-data');
	                }
	                jQuery('form.eat-dialogopts-form', modalElem).submit();
	            });
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
	                   <a class="eat-dialogbtn eat-dialogbtn-cancel ${styles.button!} btn-ok">${uiLabelMap.CommonCancel}</a>
	                   <#if action == "import">
	                   	<a class="eat-dialogbtn eat-dialogbtn-upload ${styles.button!} btn-ok">${uiLabelMap.CommonUpload}</a>
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


<@section title=uiLabelMap.SetupAccountingTransactions>		
	
    <#-- TODO: REVIEW: may make a difference later -->
    <@defaultWizardFormFields exclude=["topGlAccountId"]/>
    <#--<@field type="hidden" name="setupContinue" value="N"/> not needed yet-->
	<@row>
	    <@cell medium=9 large=9>
	    	<#-- 
	    	<@form method="get" action=makeOfbizUrl("setupAccounting") id="setupAccounting-accounting-entry-form">    	
	        </@form>
	        -->
	    </@cell>
	    <@cell medium=3 large=3>    
	      <#-- ACTIONS MENU -->
	      <@section title=uiLabelMap.CommonActions>	        
	        <#-- MENU -->
	        <ul class="side-nav">		       	
	        	<li>
	       			<@menuitem id="eat-datev-import" class="+eat-menu-action" type="link" href="javascript:void(0);" text=uiLabelMap.SetupAccountingImportDatev />
	       			<hr/>
	       			<@menuitem id="eat-elster-import" class="+eat-menu-action" type="link" href="javascript:void(0);" text=uiLabelMap.SetupAccountingImportElster />
	       		</li>
	       	</ul>
	      </@section>
		</@cell>
	</@row>
	
	<#-- 
	<div style="display:none;">
	  <@setupImportAccountingEntriesForm id="import-datev-form" target="setupImportDatevTransactionEntries" />
	  <@setupImportAccountingEntriesForm id="import-elster-form" target="setupImportElsterTransactionEntries" />
	</div>
	-->
</@section>