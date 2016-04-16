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
<@section>
    <#assign hero_content_wrap_style>
        position:absolute;
        top:200px;
        display:block;
        width:100%;
    </#assign>
    <#assign hero_content_style>
        max-width: 85em;
        margin-left: auto;
        margin-right: auto;
    </#assign>
    <#assign hero_title_style>
        display:inline-block;
        font-size: 34px;
        padding: 5px 10px;
        letter-spacing: 1px;
        background-color: #d30422;
        border-color: #de2d0f;
        color: white;
    </#assign>
    <@img src="https://unsplash.it/1400/480?random=1" width="100%" height="480px" type="bgcover">
        <#-- Some advertising statement - custom format to serve as an eyecatcher -->
        <div style="${hero_content_wrap_style!}">
            <div style="${hero_content_style}">
                <div style="${hero_title_style!}">Limited time only: 10% off!</div>
            </div>
        </div>
    </@img>
</@section>
