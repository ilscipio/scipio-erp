<#--
This file is subject to the terms and conditions defined in the
files 'LICENSE' and 'NOTICE', which are part of this source
code package.
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
        margin:10px;
        border-color: #de2d0f;
        color: white;
        float:left;
        clear: left;
    </#assign>
    <#assign hero_text_style>
        display:inline-block;
        font-size: 24px;
        padding: 5px 6px;
        margin:10px;
        letter-spacing: 1px;
        background-color: #e7e7e7;
        border-color: #c7c7c7;
        color: #4f4f4f;
        float:left;
        clear: left;
    </#assign>
    <@img src="https://source.unsplash.com/random/1400x480" width="100%" height="480px" type="bgcover">
        <#-- Some advertising statement - custom format to serve as an eyecatcher -->
        <div style="${hero_content_wrap_style!}">
            <div style="${hero_content_style}">
                <div style="${hero_title_style!}">SALE!</div>
                <div style="${hero_text_style!}">10% off your entire purchase</div>
            </div>
        </div>
    </@img>
</@section>
