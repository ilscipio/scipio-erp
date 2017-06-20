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
<#-- SCIPIO Email template  - Header-->

<#-- Header -->
    <table align="center" class="wrapper header float-center">
        <tr>
            <td class="wrapper-inner">
                <table align="center" class="container">
                    <tr>
                        <td>
                            <table class="row collapse">
                                <tbody>
                                    <tr>
                                        <th class="small-6 large-6 columns first"  style="padding-right:16px;background:#f3f3f3;">
                                            <table>
                                                <tbody>
                                                    <tr>
                                                        <th>
                                                        <#-- LOGO -->
                                                        <#if baseUrl?has_content>
                                                            <@img width="200" height="50" src="${rawString(baseUrl)}/images/scipio/scipio-logo.png"/>
                                                        <#else>
                                                            <@img width="200" height="50" src="data:image/png;base64,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"/>
                                                        </#if>
                                                        </th>
                                                    </tr>
                                                </tbody>
                                            </table>
                                        </th>
                                        <th class="small-6 large-6 columns last"  style="padding-right:16px;background:#f3f3f3;">
                                            <table>
                                                <tbody>
                                                    <tr>
                                                        <th>
                                                            <p class="text-right">
                                                                <#-- Content  -->
                                                                
                                                            </p>
                                                        </th>
                                                    </tr>
                                                </tbody>
                                            </table>
                                        </th>
                                    </tr>
                                </tbody>
                            </table>
                        </td>
                    </tr>
                </table>
            </td>
        </tr>
    </table>
    
    <#-- Body -->
    <table align="center" class="container float-center">
      <tbody>
        <tr>
          <td>
            <table class="spacer">
              <tbody>
                <tr>
                  <td height="16px" style="font-size:16px;line-height:16px;">&#xA0;</td>
                </tr>
              </tbody>
            </table>
            <table class="row">
              <tbody>
                <tr>
                  <th class="small-12 large-12 columns first last" style="padding-left: 16px;padding-right:16px;">
                    <table>
                      <tr>
                        <th>