<#-- Demo Macro Library asset 
  Should be included using:
    <@asset name="DemoLibAsset" def="global" mode="include" />
  or
    <@asset name="DemoLibAsset" def="global" mode="import" as="myNamespace"/>
-->

<#macro demoLibAssetMacro1>
  <p>Hello from demo library asset macro 1</p>
</#macro>

<#function runDemoLibAssetFunction2>
  <#return "Hello from demo library asset function 2">
</#function>
