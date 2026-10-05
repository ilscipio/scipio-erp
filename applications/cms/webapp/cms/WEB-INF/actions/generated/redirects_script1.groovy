import org.ofbiz.entity.util.*;
                    import org.ofbiz.entity.condition.*;
                    import com.ilscipio.scipio.cms.webapp.CmsWebappUtil;

                    List<GenericValue> websites = delegator.from("WebSite").where(EntityCondition.makeCondition("productStoreId", EntityJoinOperator.NOT_EQUAL, null)).orderBy("siteName").queryList()
                    import org.ofbiz.entity.GenericValue;

                    ctx = globalContext;
                    ctx.websites = websites;