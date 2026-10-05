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
<#include "component://shop/webapp/shop/order/ordercommon.ftl">

<#-- SCIPIO: DEPRECATED TEMPLATE -->

<@script>
function submitForm(form) {
   form.submit();
}
</@script>

<@menu type="button">
    <#-- SCIPIO: TODO: localize -->
    <#assign submitFormOnClick><#if callSubmitForm??>javascript:submitForm(document['${escapeVal(parameters.formNameValue!, 'js')}']);</#if></#assign>
    <@menuitem type="link" href=makePageUrl("quickAnonSetCustomer") class="+${styles.action_run_session!} ${styles.action_update!}" onClick=submitFormOnClick text="Personal Info" />
    <@menuitem type="link" href=makePageUrl("quickAnonOrderReview") class="+${styles.action_run_session!} ${styles.action_update!}" onClick=submitFormOnClick text="Review Order" disabled=(!enableShipmentMethod??) />
</@menu>
