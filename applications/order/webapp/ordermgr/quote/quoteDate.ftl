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
<@section title=uiLabelMap.CommonDate>
        <@table type="fields"> <#-- orig: class="basic-table" --> <#-- orig: cellspacing="0" -->
            <@tr>
                <@td align="right" valign="top" width="20%">
                    &nbsp;${uiLabelMap.OrderOrderQuoteIssueDate}
                </@td>
                <@td valign="top" width="80%">
                    ${(quote.issueDate.toString())!}
                </@td>
            </@tr>
            <@tr type="util"><@td colspan="3"><hr /></@td></@tr>
            <@tr>
                <@td align="right" valign="top" width="20%">
                    &nbsp;${uiLabelMap.CommonValidFromDate}
                </@td>
                <@td valign="top" width="80%">
                    ${(quote.validFromDate.toString())!}
                </@td>
            </@tr>
            <@tr type="util"><@td colspan="3"><hr /></@td></@tr>
            <@tr>
                <@td align="right" valign="top" width="20%">
                    &nbsp;${uiLabelMap.CommonValidThruDate}
                </@td>
                <@td valign="top" width="80%">
                    ${(quote.validThruDate.toString())!}
                </@td>
            </@tr>
        </@table>
</@section>
