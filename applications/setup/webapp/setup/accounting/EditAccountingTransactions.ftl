<#-- SCIPIO: SETUP fiscal periods implementation -->

<#include "component://setup/webapp/setup/common/common.ftl">

<#-- ACTION PROPERTIES, links tree actions to setup forms -->
<#macro setupImportAccountingEntriesForm id target>
  <@form id=id action=makeOfbizUrl(target) method="post">	      
      
  </@form>
</#macro>

<#macro eatImportDatevConfirmFields args={}>
   <@field type="hidden" name="organizationPartyId" value="params.orgPartyId!"/>
   <@field type="file" name="uploadedFile" label=uiLabelMap.SetupAccountingDatevImportCSV />
</#macro>
<#macro eatImportElsterConfirmFields args={}>
   <@field type="hidden" name="organizationPartyId" value="params.orgPartyId!"/>
   <@field type="file" name="uploadedFile" label=uiLabelMap.SetupAccountingElsterImportCSV />
</#macro>

<#assign eatActionProps = {
    "datev": {
        "importdatev": {
            "type": "form",
            "mode": "show",
            "id": "eat-importdatev",
            "confirmMsg": rawLabelNoSubst(''),
            "confirmExtraMsg": rawLabelNoSubst(''),
            "confirmFields": eatImportDatevConfirmFields,
            "defaultParams": wrapRawScript("function() { return; }")
        }        
    },
    "elster": {
    	"importelster": {
            "type": "form",
            "mode": "show",
            "id": "eat-importdatev",
            "confirmMsg": rawLabelNoSubst(''),
            "confirmExtraMsg": rawLabelNoSubst(''),
            "confirmFields": eatImportElsterConfirmFields,
            "defaultParams": wrapRawScript("function() { return; }")
        }      
    }
}>

<#assign eatActionProps = toSimpleMap(eatActionProps!{})>


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

	jQuery(document).ready(function() {
		jQuery('#eat-datev-link').click(function(){
			var confirmExtraMsg = "";
            var modalElem = jQuery('#${eatDialogIdPrefix} + actionProps.objectType + '-' + actionProps.actionType);
            showConfirmMsg('', confirmMsg, confirmExtraMsg, modalElem, function(subActionType) {
            });
		});
	});
</@script>

<#assign eatDialogIdPrefix = eatDialogIdPrefix!"eat-dialog-">
<#assign eatDialogIdModalPrefix = eatDialogIdModalPrefix!("modal_" + eatDialogIdPrefix)>

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
        <@form class="+eat-dialogopts-form">
            <#-- TODO: REVIEW: unclear if the value should be "all" or "active" at current time or if should
                be checked by default -->
          	<@fields type="default-compact">
            	${props.confirmFields!}
            </@fields>
        </@form>
      </#if>
    <p class="eat-dialogextramsg"></p>
</#macro>

<#macro eatDefActionMsgModals args={}>
	<#list args.actionProps?keys as objectType>
        <#local actionMap = toSimpleMap(args.actionProps[objectType])>
        
        <#local props = actionMap["importdatev"]!{}>
        <#if props.confirmMsg?has_content>
            <@modal id="${args.idPrefix}${rawString(objectType)}-" class="+eat-dialogmodal">
                <@heading>${uiLabelMap.CommonWarning}</@heading>
                <@eatDefActionInnerContent props=props/>
                <div class="modal-footer ${styles.text_right!}">
                   <#-- NOTE: the value "remove"/"expire" is extracted from the class and passed to the callback -->
                   <a class="eat-dialogbtn eat-dialogbtn-remove ${styles.button!} btn-ok">${uiLabelMap.CommonRemove}</a>
                   <a class="eat-dialogbtn eat-dialogbtn-expire ${styles.button!} btn-ok">${uiLabelMap.CommonExpire}</a>
                </div>
            </@modal>
        </#if>
        
        <#local props = actionMap["importelster"]!{}>
        <#if props.confirmMsg?has_content>
            <@modal id="${args.idPrefix}${rawString(objectType)}-" class="+eat-dialogmodal">
                <@heading>${uiLabelMap.CommonWarning}</@heading>
                <@eatDefActionInnerContent props=props/>
                <@eatDefActionConfirmMsgBtn/>
            </@modal>
        </#if>
    </#list>
</#macro>

<@eatDefActionMsgModals args={	
	"actionProps": eatActionProps!{},
    "idPrefix": eatDialogIdPrefix,
    "idModalPrefix": eatDialogIdModalPrefix
}/>

<#-- OUTPUT MODALS (not in display:none;) -->
<#-- 
<@importMarkupOut dir=(eatDefActionMsgModals) args={
    "actionProps": importActionProps!{},
    "idPrefix": importDialogIdPrefix!,
    "idModalPrefix": importDialogIdModalPrefix!
}/>
-->

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
	       			<@menuitem id="import-datev-link" type="link" href="javascript:void(0);" text=uiLabelMap.CommonImportDatev />
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