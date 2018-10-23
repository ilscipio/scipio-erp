<#-- Demo asset template, dedicated to test cases -->

<#assign recurseDescStr = "">
<#assign recurseDefined = (recurseDepth?has_content && recurseDepth?is_number)>
<#if recurseDefined && (recurseDepth >= 1)>
    <#assign recurseDepth = recurseDepth - 1>
    <#assign recurseDescStr = " (recursive; levels left: " + recurseDepth?string + ")">
<#elseif recurseDefined && (recurseDepth == 0)>
    <#-- (do nothing) -->
<#else>
    <#assign recurseDepth = -1>
</#if>

<@section title=("Demo asset" + recurseDescStr + (rawString(invokeMethodDescStr!"")))>
  <#if content?has_content>
    <#-- FIXME: extremely inefficient processing technique...
        but fixing requires refactoring/revisiting way attributes work... -->
    <#assign interpretedContent = rawString(content)?interpret>
    <@interpretedContent/>
  </#if>
  
  <#if (recurseDepth > 0)>
    <#-- NOTE: odd recurse leves test lookup by ID, even numbers test by name -->
    <#if (recurseDepth % 2) == 0>
      <@asset name="DemoAsset" def="global" ctxVars={"recurseDepth": recurseDepth} ovrdCtxVars={"invokeMethodDescStr":" (invoked by name: DemoAsset)"}/><#-- , "content":content!"" -->
    <#else>
      <@asset id="DEMOASSET" def="global" ctxVars={"recurseDepth": recurseDepth} ovrdCtxVars={"invokeMethodDescStr":" (invoked by id: DEMOASSET)"}/><#-- , "content":content!"" -->
    </#if>
  <#elseif (recurseDepth == 0)>
    <p><em>NOTE: due to recursion, the content should be automatically repeated at every level, even though
        we are not explicitly re-passing it as a context var. If it does not show up at every level,
        it means something is wrong with the context passing (general renderer issue).</em></p>
  </#if>
    
</@section>



