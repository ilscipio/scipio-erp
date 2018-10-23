/**
 * SCIPIO: data import page script (original).
 */
import java.util.*;
import java.io.*;
import org.ofbiz.base.util.*;
import org.ofbiz.entity.model.*;
import org.ofbiz.entity.util.*;
import org.ofbiz.entity.transaction.*;
import org.ofbiz.entity.condition.*;

final module = "CmsDataImport.groovy";

cmsErrorHandler = context.cmsErrorHandler;

messages = context.messages;
context.remove("messages"); // remove this to prevent any read by template
if (messages) {
    cmsErrorHandler.addContextEventMsgs(context, messages);
}
