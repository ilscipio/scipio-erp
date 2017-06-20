<#-- Code common to all the shop template files. -->
<#if !(fndShopCommonDefined!false)>

    <#-- userHasAccount is usually included by shop decorator, but want it here for fallback 
        and support for odd configurations - try to not depend too strictly on ShopActions in theme -->
    <#if !userHasAccount??>
        <#assign dummy = setContextField("permChecksSetGlobal", true)>
        <#assign dummy = Static["org.ofbiz.base.util.GroovyUtil"].runScriptAtLocation("component://shop/webapp/shop/WEB-INF/actions/common/CommonUserChecks.groovy", context)!>
        <#global userHasAccount = globalContext.userHasAccount>
    </#if>
    
    <#global fndShopCommonDefined = true>
</#if>