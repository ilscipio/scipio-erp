<#--
Licensed to the Apache Software Foundation (ASF) under one
or more contributor license agreements.  See the NOTICE file
distributed with this work for additional information
regarding copyright ownership.  The ASF licenses this file
to you under the Apache License, Version 2.0 (the
"License"); you may not use this file except in compliance
with the License.  You may obtain a copy of the License at

http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing,
software distributed under the License is distributed on an
"AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
KIND, either express or implied.  See the License for the
specific language governing permissions and limitations
under the License.
-->

<#include "component://setup/webapp/setup/common/common.ftl">


<#assign defaultParams = {
	<#--"productStoreId": productStoreId! // not avail/crash -->
}>

<#assign paramMaps = getWizardFormFieldValueMaps({
    "record":userParty!true, <#-- NOTE: must fallback with boolean true -->
    "defaults":defaultParams,
    "strictRecord":true <#-- TODO: REMOVE (debugging) -->
})>
<#assign params = paramMaps.values>
<#assign fixedParams = paramMaps.fixedValues>

<@alert type="warning">WARNING: WORK-IN-PROGRESS</@alert>

<@form method="post" action=makeOfbizUrl(target) id="NewAccounting" name="NewAccounting">
    <@defaultWizardFormFields/>
  
  	<@commonMsg type="info-important">${uiLabelMap.CommonFieldsMarkedAreRequired}</@commonMsg>

	<@row>
	  <@cell columns=6>
    	  <fieldset>
    	    <legend>${uiLabelMap.SetupAccountingTaxAuthority}</legend>
        		<@field name="taxAuthPartyId" label=uiLabelMap.CommonParty required=true />
        		<@field name="taxAuthGeoId" label=uiLabelMap.CommonGeo />
            
		        <#-- <field name="taxAuthGeoId" title="${uiLabelMap.CommonGeo}" use-when="taxAuthority==null&amp;&amp;taxAuthGeoId==null" required-field="true" position="2">
		            <lookup target-form-name="LookupGeo"/>
		        </field>
		        <field  name="taxAuthGeoId" title="${uiLabelMap.CommonGeo}" use-when="taxAuthority==null&amp;&amp;taxAuthGeoId!=null" tooltip="${uiLabelMap.CommonCannotBeFound}:[${taxAuthGeoId}]">
		            <lookup target-form-name="LookupGeo"/>
		        </field> -->
				<#-- 
		        <field name="requireTaxIdForExemption" widget-style="+smallSelect">
		            <drop-down no-current-selected-key="Y">
		                <option key="Y" description="${uiLabelMap.CommonY}"/>
		                <option key="N" description="${uiLabelMap.CommonN}"/>
		            </drop-down>
		        </field>
		        <field name="includeTaxInPrice" widget-style="+smallSelect" position="2">
		            <drop-down no-current-selected-key="N">
		                <option key="Y" description="${uiLabelMap.CommonY}"/>
		                <option key="N" description="${uiLabelMap.CommonN}"/>
		            </drop-down>
		        </field>
		        <field name="taxIdFormatPattern" tooltip="${uiLabelMap.AccountingValidationPattern}"><text/></field>
		         -->
    	    
    	  </fieldset>
	  </@cell>
	
	  <@cell columns=6>
    	  <fieldset>
    	    <legend>${uiLabelMap.SetupAccountingTimePeriod}</legend>
    	    
    	  </fieldset>
	  </@cell>
	</@row>
	
	<@row>
	  <@cell columns=6>
    	  <fieldset>
    	    <legend>${uiLabelMap.SetupAccountingPaymentGateway}</legend>
    	    
    	  </fieldset>
	  </@cell>
	
	  <@cell columns=6>
    	  <fieldset>    
    	    <legend>${uiLabelMap.SetupAccountingGLAccountDefaults}</legend>
    	        
    	  </fieldset>
	  </@cell>
	</@row>
	
    <@field type="submit" title=uiLabelMap.CommonUpdate class="+${styles.link_run_sys} ${styles.action_update}"/>
</@form>

