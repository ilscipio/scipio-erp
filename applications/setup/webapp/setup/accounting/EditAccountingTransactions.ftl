<#-- SCIPIO: SETUP Gl Accounting transactions implementation -->

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

<#assign eatObjectProps = {
    "datev": {
        "import": {
            "type": "form",
            "mode": "show",
            "id": "eat-datev",
            "confirmMsg": 'SetupAccountingImportDatevCSVProceed',
            "confirmExtraMsg": rawLabelNoSubst(''),
            "confirmFields": eatImportDatevConfirmFields,
            "formAction": makePageUrl('setupImportDatevDataCategory'),
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
            "formAction": makePageUrl('setupImportElsterDataCategory'),
            "defaultParams": wrapRawScript("function() { return; }")
        }      
    }
}>
-->
<#assign eatAllHideShowFormIds = [
    "eat-newacctgtranstype", "eat-newacctgtransentrytype"
]>

<#assign eatObjectProps = {
    "acctgtype" : {
        "add" : {
            "type": "form",
            "mode": "show",
            "id": "eat-newacctgtranstype",
            "formAction": makePageUrl('setupCreateAcctgTransType'),
            "defaultParams": wrapRawScript("function() { return; }")
        }
    },
    "acctgentrytype" : {
        "add" : {
            "type": "form",
            "mode": "show",
            "id": "eat-newacctgtransentrytype",
            "formAction": makePageUrl('setupCreateAcctgTransEntryType'),
            "defaultParams": wrapRawScript("function() { return; }")
        }
    }
}>

<#assign eatObjectProps = toSimpleMap(eatObjectProps!{})>
<#assign eatDialogIdPrefix = eatDialogIdPrefix!"eat-dialog-">
<#assign eatDialogIdModalPrefix = eatDialogIdModalPrefix!("modal_" + eatDialogIdPrefix)>
<#assign eatStatsIdPrefix = eatStatsIdPrefix!"eat-stats-">

<@script>
    var actionAcctgTransProps = <@objectAsScript object=(eatObjectProps!{}) lang='js'/>;
    var acctgTransHideShowFormIds = <@objectAsScript object=(eatAllHideShowFormIds!{}) lang='js'/>;

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
            url: '<@pageUrl>setupImportDatevDataCategory</@pageUrl>',
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
                var modalElem = jQuery('#${eatDialogIdModalPrefix}' + typeAction[1] + '-' + typeAction[2]);
                if (typeAction[2] == "add" || typeAction[2] == "edit") {
                    props = actionAcctgTransProps[typeAction[1]][typeAction[2]];
                    if (props.type == 'form' && props.mode == "show") {
                    	if (acctgTransHideShowFormIds) {
                            jQuery.each(acctgTransHideShowFormIds, function(i, e) {
                                jQuery('#'+e).fadeOut();
                            });
                        }
                        $('#' + props.id).fadeIn();
                        setupControlMenu.setSubmitFormId(props.id + '-form');
                    }
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
    <@form id=id name=id action=makePageUrl(target) method="post" validate=setupFormValidate>
        <@defaultWizardFormFields exclude=[]/>
        <@field type="hidden" name="isAddAcctgTransType" value=(formActionType == "add")?string("Y", "N")/>
        <@field type="hidden" name="isCreateAcctgTransType" value=(formActionType == "new")?string("Y", "N")/>
        <@field type="hidden" name="isUpdateAcctgTransType" value=(formActionType == "edit")?string("Y", "N")/>

        <#assign fieldsRequired = true>
        <#if formActionType == "edit">
              <@field type="display" label=uiLabelMap.CommonParent><#rt/>
                <span class="acctg-managefield acctg-managefield-for-parentAcctgTransTypeDesc">
                    ${params.parentTypeId!}
                   </span><#t/>
            </@field><#lt/>
            <@field type="hidden" name="parentTypeId" value=(params.parentTypeId!) class="+acctg-inputfield"/>
        <#else>
            <@field type="select" name="parentTypeId" label=uiLabelMap.CommonParent class="+acctg-inputfield">
              <option value=""></option>
              <#list acctgParentTransTypes as acctgParentTransType>
                <#assign selected = (rawString(params.parentTypeId!) == (acctgParentTransType.parentTypeId!))>
                <option value="${acctgParentTransType.acctgTransTypeId!}"<#if selected> selected="selected"</#if>>${acctgParentTransType.description!}</option>
              </#list>
            </@field>
        </#if>
        <#if formActionType == "edit">
            <@field type="display" label=uiLabelMap.FormFieldTitle_acctgTransTypeId><#rt/>
                <span class="acctg-managefield acctg-managefield-for-acctgTransType"></span><#t/>
            </@field><#lt/>
            <@field type="hidden" name="acctgTransTypeId" value=(params.acctgTransTypeId!) class="+acctg-inputfield"/>
        <#else>
            <#-- TODO: REVIEW: required=true -->
            <@field type="input" name="acctgTransTypeId" label=uiLabelMap.FormFieldTitle_acctgTransTypeId value=(params.acctgTransTypeId!) class="+acctg-inputfield"/>
        </#if>
        <@field type="input" name="description" label=uiLabelMap.CommonDescription value=(params.description!) class="+acctg-inputfield"/>
    </@form>
</#macro>
<#macro setupAcctgTransEntryTypeForm id formActionType target params>
    <@form id=id name=id action=makePageUrl(target) method="post" validate=setupFormValidate>
        <@defaultWizardFormFields exclude=[]/>
        <@field type="hidden" name="isAddAcctgEntryTransType" value=(formActionType == "add")?string("Y", "N")/>
        <@field type="hidden" name="isCreateAcctgEntryTransType" value=(formActionType == "new")?string("Y", "N")/>
        <@field type="hidden" name="isUpdateAcctgEntryTransType" value=(formActionType == "edit")?string("Y", "N")/>

        <#assign fieldsRequired = true>
        <#if formActionType == "edit">
            <@field type="display" label=uiLabelMap.CommonParent><#rt/>
                <span class="acctg-managefield acctg-managefield-for-parentAcctgTransEntryTypeDesc">
                    ${params.parentTypeId!}
                   </span><#t/>
            </@field><#lt/>
            <@field type="hidden" name="parentTypeId" value=(params.parentTypeId!) class="+acctg-inputfield"/>
        <#else>
            <@field type="select" name="parentTypeId" label=uiLabelMap.CommonParent class="+acctg-inputfield">
              <option value=""></option>
              <#list acctgParentEntryTransTypes as acctgParentTransEntryType>
                <#assign selected = (rawString(params.parentTypeId!) == (acctgParentTransEntryType.parentTypeId!))>
                <option value="${acctgParentTransEntryType.acctgTransEntryTypeId!}"<#if selected> selected="selected"</#if>>${acctgParentTransEntryType.description!}</option>
              </#list>
            </@field>
        </#if>
        <#if formActionType == "edit">
            <@field type="display" label=uiLabelMap.FormFieldTitle_acctgTransEntryTypeId><#rt/>
                <span class="acctg-managefield acctg-managefield-for-acctgTransEntryTypeId">
                	${params.acctgTransEntryTypeId!}
               	</span><#t/>
            </@field><#lt/>
            <@field type="hidden" name="acctgTransEntryTypeId" value=(params.acctgTransEntryTypeId!) class="+acctg-inputfield"/>
        <#else>
            <#-- TODO: REVIEW: required=true -->
            <@field type="input" name="acctgTransEntryTypeId" label=uiLabelMap.FormFieldTitle_acctgTransEntryTypeId value=(params.acctgTransEntryTypeId!) class="+acctg-inputfield"/>
        </#if>
        <@field type="input" name="description" label=uiLabelMap.CommonDescription value=(params.description!) class="+acctg-inputfield"/>
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
    "objectTypes": eatObjectProps!{},
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

<@section id="mainAcctgTransactionsSection">
    <#-- TODO: REVIEW: may make a difference later -->
    <@defaultWizardFormFields exclude=["topGlAccountId"]/>
    <#--<@field type="hidden" name="setupContinue" value="N"/> not needed yet-->
    <@row>
        <@cell medium=5 large=5>
            <@section title=uiLabelMap.SetupAccountingTransactionTypes>
                <@table type="data-list">
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
                <@table type="data-list">
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
    "objectTypes": eatObjectProps!{},
    "idPrefix": eatStatsIdPrefix
}/>
<@section title=uiLabelMap.PageTitleAddAcctgTransType containerId="eat-newacctgtranstype" containerClass="+eat-newacctgtranstypeid acctg-recordaction acctg-newrecord" 
    containerStyle=((targetRecordAction == "newacctgtranstype-new")?string("","display:none;"))>
  <#if targetRecordAction == "newacctgtranstypeid-new">
    <#assign paramMaps = initialParamMaps>
  <#else>
    <#assign paramMaps = getWizardFormFieldValueMaps({
      "record":true,
      "defaults":defaultParams
    })>
  </#if>
  <@setupAcctgTransTypeForm id="eat-newacctgtranstype-form" formActionType="new" target="setupCreateAcctgTransType" params=paramMaps.values  />
</@section>

<@section title=uiLabelMap.PageTitleAddAcctgTransEntryType containerId="eat-newacctgtransentrytype" containerClass="+eat-newacctgtransentrytypeid acctg-recordaction acctg-newrecord" 
    containerStyle=((targetRecordAction == "newacctgtransentrytype-new")?string("","display:none;"))>
  <#if targetRecordAction == "newacctgtransentrytypeid-new">
    <#assign paramMaps = initialParamMaps>
  <#else>
    <#assign paramMaps = getWizardFormFieldValueMaps({
      "record":true,
      "defaults":defaultParams
    })>
  </#if>
  <@setupAcctgTransEntryTypeForm id="eat-newacctgtransentrytype-form" formActionType="new" target="setupCreateAcctgTransEntryType" params=paramMaps.values  />
</@section>