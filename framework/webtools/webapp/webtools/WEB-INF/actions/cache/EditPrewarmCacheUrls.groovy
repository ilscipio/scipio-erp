import com.ilscipio.scipio.cms.webapp.CmsWebappUtil
import org.ofbiz.entity.GenericValue

List<GenericValue> websites = CmsWebappUtil.getWebSiteList(delegator);
context.websites = websites;