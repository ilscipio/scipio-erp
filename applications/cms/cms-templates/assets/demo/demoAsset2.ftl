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
<#-- Demo asset template, dedicated to test cases -->


<@section title="Demo asset 2">
    <p>Test content: ${testContent!"missing"}</p>
    <p>demo_bool_1: ${(demo_bool_1!"missing")?string}</p>
    <p>demo_bool_2 (not empty parameters.demo_bool_2): ${(demo_bool_2!"missing")?string}</p>
    <p>demo_integer_1 (is number type? ${(demo_integer_1!false)?is_number?string}): ${demo_integer_1!"missing"}</p>
    <p>demo_double_1 (is number type? ${(demo_double_1!false)?is_number?string}): ${demo_double_1!"missing"}</p>
</@section>



