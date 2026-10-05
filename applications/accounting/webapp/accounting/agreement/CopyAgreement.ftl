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
<#--
Changes to this file: Copyright (C) Ilscipio GmbH. The changes are licensed
under the GNU Affero General Public License, version 3, or a commercial
license from Ilscipio GmbH (file LICENSE). The original code stays under
the Apache License, version 2.0, as stated above.
-->
<#if agreement??>
<@section title=uiLabelMap.PageTitleCopyAgreement>
    <form action="<@pageUrl>copyAgreement</@pageUrl>" method="post">
        <input type="hidden" name="agreementId" value="${agreementId}"/>    
        <@field type="checkbox" label=uiLabelMap.AccountingAgreementTerms name="copyAgreementTerms" value="Y" checked=true />
        <@field type="checkbox" label=uiLabelMap.ProductProducts name="copyAgreementProducts" value="Y" checked=true />
        <@field type="checkbox" label=uiLabelMap.Party name="copyAgreementParties" value="Y" checked=true />
        <@field type="checkbox" label=uiLabelMap.ProductFacilities name="copyAgreementFacilities" value="Y" checked=true />
        
        <@field type="submit" text=uiLabelMap.CommonCopy class="+${styles.link_run_sys!} ${styles.action_copy!}" />
    </form>
</@section>
</#if>