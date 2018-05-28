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
}>

<#assign paramMaps = getWizardFormFieldValueMaps({
    "record":true, <#-- NOTE: must fallback with boolean true -->
    "defaults":defaultParams,
    "strictRecord":true <#-- TODO: REMOVE (debugging) -->
})>
<#assign params = paramMaps.values>
<#assign fixedParams = paramMaps.fixedValues>

<@tabs id="accountingTabs">
	<@tab id="preferencesTab" title="Preferences"><@render type="screen" resource="component://setup/widget/SetupScreens.xml" name="EditAcctgPreferences"/></@tab>
	<@tab id="glAccountsTab" title="Configure GL Accounts">
		<@render type="screen" resource="component://setup/widget/SetupScreens.xml" name="EditGLAccountTree" />
	</@tab>
	<@tab id="fiscalPeriodsTab" title="Configure Fiscal Periods"><@render type="screen" resource="component://setup/widget/SetupScreens.xml" name="EditFiscalPeriods"/></@tab>
	<@tab id="accountingTransactionsTab" title="Configure Accounting Transactions"><@render type="screen" resource="component://setup/widget/SetupScreens.xml" name="EditAccountingTransactions"/></@tab>
	<@tab id="journalTab" title="Configure Journal"><@render type="screen" resource="component://setup/widget/SetupScreens.xml" name="EditJournals"/></@tab>
</@tabs>     

