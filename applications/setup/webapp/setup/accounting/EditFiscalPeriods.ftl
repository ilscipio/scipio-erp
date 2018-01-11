<#-- SCIPIO: SETUP fiscal periods implementation -->

<#include "component://setup/webapp/setup/common/common.ftl">
<#include "component://accounting/webapp/accounting/ledger/tree/treecommon.ftl">

<#assign efpObjectTypes = { 
	"timePeriod": {   
	    "add": {
	        "type": "form",
	        "mode": "show",
	        "id": "efp-add-time-period",        
	        "formAction": makeOfbizUrl('setupCreateTimePeriod'),
	        "defaultParams": wrapRawScript("function() { return; }")
	    },
	    "edit": {
	    	"type": "form",
	        "mode": "show",
	        "id": "efp-edit-time-period",        
	        "formAction": makeOfbizUrl('setupUpdateTimePeriod'),
	        "defaultParams": wrapRawScript("function() { return; }")
	    }
    }
}>

<#assign efpObjectTypes = toSimpleMap(efpObjectTypes!{})>
<#assign efpIdPrefix = efpIdPrefix!"efp-timePeriod-">

<@script>
	var actionProps = <@objectAsScript object=(efpActionProps!{}) lang='js'/>;
    
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
		jQuery('li.efp-menu-action a').click(function(e) {			
			var typeAction = this.id.split('-');
			if (typeAction && typeAction.length == 3) {
	            <#-- var modalElem = jQuery('#${efpDialogIdModalPrefix}' + typeAction[1] + '-' + typeAction[2]); -->	             
	            console.log("typeaction-1 =====> " + typeAction[1] + "    typeaction-2 =====> " + typeAction[2]);
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

<#-- RENDERS SETUP FORMS -->
<#macro efpPostTreeArea extraArgs...>
    <@render type="screen" resource=setupTimePeriodForms.location name=setupTimePeriodForms.name/>
</#macro>

<#-- RENDERS DISPLAY OPTIONS -->
<#macro efpExtrasArea extraArgs...>
  <@section><#-- title=uiLabelMap.CommonDisplayOptions -->
    <@form action=makeOfbizUrl("setupAccounting") method="get">
      <@defaultWizardFormFields/>
    </@form>
  </@section>
</#macro>

<#-- CORE INCLUDE -->
<#include "component://accounting/webapp/accounting/period/tree/EditCustomTimePeriodCore.ftl">		
