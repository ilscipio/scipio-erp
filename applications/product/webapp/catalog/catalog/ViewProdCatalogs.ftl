<@section>
    <@grid columns=2>
    <#list productStores as productStore>
        <li><@render resource="component://product/widget/catalog/CommonScreens.xml#ScipioViewCatalogTree"  ctxVars={"productStoreId":productStore.productStoreId!} asString=true /></li>
    </#list>
    </@grid>
</@section>