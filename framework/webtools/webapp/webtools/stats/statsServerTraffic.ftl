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
<#assign library=chartLibrary!"foundation"/>
<#assign currData=rewrapMap(chartData, "raw-simple")/>
<#assign fieldIdNum=fieldIdNum!0/>

<@section title=title!"">
  <#if currData?has_content>
    <@chart type=chartType library=library>
      <#list mapKeys(currData) as key>
        <#assign date = key?date/>
          <@chartdata value=((currData[key].count)!0) title=key/>
      </#list>
    </@chart>
  <#else>
    <@commonMsg type="result-norecord" />
  </#if>
</@section>