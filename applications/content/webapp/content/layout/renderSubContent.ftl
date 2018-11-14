<#--
This file is subject to the terms and conditions defined in the
files 'LICENSE' and 'NOTICE', which are part of this source
code package.
-->

<hr />
<#if parameters.contentId??>
    <#assign id=parameters.contentId/>
    <@editRenderSubContent contentId="TEMPLATE_MASTER" mapKey="" editTemplate="true" subContentId=(id!)>
        <@renderSubContent throwExceptionOnError="false"/>
    </@editRenderSubContent>
</#if>
