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
<#-- SCIPIO -->

<#include "component://webtools/webapp/webtools/common/common.ftl">

<#macro eiUnsafeEntityField values>
    <#-- SCIPIO: NOTE: the submitted disallowDangerousEntitiesWarn is the opposite boolean of the label text, 
        for service interface reasons -->
    <#-- TODO: better checked logic, right now inconsistent with rest of forms: checked=((values.disallowUnsafeEntityWarn!) == "false")  -->
    <@field type="checkbox" name="disallowUnsafeEntityWarn" 
        value="false" altValue="true"
        checked=false
        label=getLabel('WebtoolsAllowUnsafeEntitiesNamed', '', 
            {"unsafeEntityNames": Static["org.ofbiz.entity.util.EntityUtil"].getUnsafeEntitiesForUpdate(delegator)?join(", ")}) 
        tooltip=uiLabelMap.WebtoolsAllowUnsafeEntitiesDesc/>
</#macro>