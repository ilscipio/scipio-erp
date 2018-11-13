<#--
This file is subject to the terms and conditions defined in
 file 'LICENSE', which is part of this source code package.
-->

<hr />
<#if parameters.contentId??>
    <#assign id=parameters.contentId/>
    <@editRenderSubContent contentId="TEMPLATE_MASTER" mapKey="" editTemplate="true" subContentId=(id!)>
        <@renderSubContent throwExceptionOnError="false"/>
    </@editRenderSubContent>
</#if>
