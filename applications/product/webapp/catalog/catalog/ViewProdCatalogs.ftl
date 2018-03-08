<@section>
    <#list productStores as productStore>
        <#if productStore_index %2 == 0><@row open=true close=false/></#if>
        <@cell columns=6>
            <@render resource="component://product/widget/catalog/CommonScreens.xml#ScipioViewCatalogTree"  ctxVars={"productStoreId":productStore.productStoreId!} asString=true />
        </@cell>
        <#if productStore_index %2 == 1><@row open=false close=true/></#if>
    </#list>
</@section>