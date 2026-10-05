<#--
Scipio Commerce
Copyright (C) Ilscipio GmbH

This file is part of Scipio Commerce. Scipio Commerce is free software: you
can redistribute it and modify it under the terms of the GNU Affero General
Public License, version 3, as published by the Free Software Foundation.
Scipio Commerce is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License
for more details. You should have received a copy of the license with this
work (file LICENSE). If not, see <https://www.gnu.org/licenses/agpl-3.0.html>.
A commercial license is available from Ilscipio GmbH.

SPDX-License-Identifier: AGPL-3.0-only
-->
<#-- TODO: License -->

<@modal id="expireAllCategoryProductMembers">
    <@section title=uiLabelMap.ProductExpireAllProductMembers>
        <form method="post" action="<@pageUrl>expireAllCategoryProductMembers</@pageUrl>" name="expireAllCategoryProductMembersForm">
            <input type="hidden" name="productCategoryId" value="${productCategoryId!}" />
            <input type="hidden" name="activeOnly" value="${activeOnly.toString()}" />
            <@field type="datetime" label=uiLabelMap.CommonDate required=true name="thruDate" value="" size="25" maxlength="30" id="thruDate2"/>
            <@field type="submit" text=uiLabelMap.CommonExpireAll class="+${styles.link_run_sys!} ${styles.action_terminate!}" />
        </form>
    </@section>
</@modal>
<@script>
    $(document).ready(function() {
        try {
            let modalElem = $('#modal_expireAllCategoryProductMembers');
            ${modalControl('modalElem','open')}
        } catch(err) {
        }
    });
</@script>
