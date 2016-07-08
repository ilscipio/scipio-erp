package com.ilscipio.scipio.cms.content;

import java.io.IOException;
import java.text.SimpleDateFormat;
import java.util.Arrays;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import javolution.util.FastList;
import javolution.util.FastMap;
import org.apache.commons.lang.StringUtils;
import org.ofbiz.base.lang.JSON;
import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.base.util.UtilProperties;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericEntityException;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.entity.condition.EntityCondition;
import org.ofbiz.entity.condition.EntityOperator;
import org.ofbiz.service.LocalDispatcher;

import com.ilscipio.scipio.cms.CmsException;
import com.ilscipio.scipio.cms.data.CmsDataObject;
import com.ilscipio.scipio.cms.data.CmsDataObjectCache;
import com.ilscipio.scipio.cms.template.CmsPageTemplate;

public class CmsPage extends CmsDataObject {
	private static final long serialVersionUID = -6442528536238200118L;
	private CmsPageTemplate template;
    private static CmsDataObjectCache<CmsPage> nameCache;
    private static CmsDataObjectCache<CmsPage> pathCache;
    private CmsPageContent contentModel;
    private Map<String, Map<String, ?>> products;

    public static UserRole DEFAULT_USER_ROLE = UserRole.CMS_VISITOR;
    public static final String module = CmsPage.class.getName();



    /**
     * This constructor creates a new CmsPage instance from a prefilled
     * GenericValue. It should be ensured that the GenericValue is valid, no
     * further validation takes place.
     * 
     * @param entity
     *            GenericValue of an CmsPage entity
     */
    public CmsPage(GenericValue entity) {

        super(entity);

    }

    /**
     * Creates a new CmsPage from a field map.
     * 
     * @param fields
     *            Map containing <em>field name</em> -> <em>value</em> mappings
    */
    public CmsPage(Map<String, ?> fields) {

        super(fields);
        try {
            entity.getDelegator().createOrStore(entity);
        } 
        catch (GenericEntityException e) {
            throw new CmsException("Could not save new page.", e, module);
        }
        
    } 

    /**
     * Creates a new CmsPage object using the data from database stored under
     * the given primary key.
     * 
     * @param primaryKey
     */
    public CmsPage(String primaryKey) {

        super(primaryKey);

    }

    
    /**
     * Helper class to identify if a page authorization is dealing with a user authorization
     * or a group authorization.
     *
     */
    private enum PageAuthPartyType {
        // Note: the name "party" in this type does not signify a "Party" entity type
        USER ("userId"),
        GROUP ("groupId");
        
        private final String fieldName;
        private PageAuthPartyType(String fieldName) {
            this.fieldName = fieldName;
        }
        
        public EntityCondition makeNullCond() { // This null
            return EntityCondition.makeCondition(fieldName, EntityOperator.EQUALS, null);
        }
        public EntityCondition makeAllOthersNullCond() { // All null except for this
            List<EntityCondition> conds = FastList.newInstance();
            for(PageAuthPartyType type : PageAuthPartyType.values()) {
                if (type != this) {
                    conds.add(type.makeNullCond());
                }
            }
            return EntityCondition.makeCondition(conds, EntityOperator.AND);            
        }
        
        public EntityCondition makeNonNullCond() { // This non-null
            return EntityCondition.makeCondition(fieldName, EntityOperator.NOT_EQUAL, null);
        }
        public EntityCondition makeExclusiveNonNullCond() { // All null except this, non-null
            return EntityCondition.makeCondition(
                    makeAllOthersNullCond(), 
                    EntityOperator.AND,
                    this.makeNonNullCond()
                );
        }
        
        public EntityCondition makeIdCond(String id) { // This equals ID
            return EntityCondition.makeCondition(fieldName, EntityOperator.EQUALS, id);
        }
        public EntityCondition makeExclusiveIdCond(String id) { // All null except this, equals ID
            return EntityCondition.makeCondition(
                    makeAllOthersNullCond(), 
                    EntityOperator.AND,
                    this.makeIdCond(id)
                );
        } 
    }

    
    /**
     * Adds the product to this page. It will be available in templates under
     * the given import name.
     * 
     * @param product
     *            The product to be added as GenericValue instance
     * @param importName
     *            The name this product should be available as
     */
    public void addProduct(GenericValue product, String importName) {

        addProduct(product.getString("productId"), importName);

    }

    /**
     * Adds the product to this page. It will be available in templates under
     * the given import name.
     * 
     * @param productId
     *            The id of the product to be added
     * @param importName
     *            The name this product should be available as
     */
    public void addProduct(String productId, String importName) {

        try {
            GenericValue productAssoc = entity.getDelegator().makeValue("CmsPageProductAssoc", "pageId", this.getId(),
                    "productId", productId, "importName", importName);
            productAssoc.setNextSeqId();
            entity.getDelegator().create(productAssoc);

            // reset products
            products = null;
        } catch (GenericEntityException e) {
            throw new CmsException(
                    String.format("Could not add product. Page: %s Product: %s", this.getName(), productId), e,    module);
        }

    }

    /**
     * Adds a new content version to this page. This version is not live at this
     * point but can be activated using {@link #setActiveVersion(String)}.
     * 
     * @param content
     *            Page content as map with fields, field values and asset
     *            content as embedded field -> value maps.
     * @throws IOException 
     */
    public CmsPageVersion addVersion(Map<String, ?> content) throws IOException {

        return addVersion(JSON.from(content).toString());

    }

    /**
     * Adds a new version directly from JSON.
     * 
     * @param jsonContent
     * @return
     */
    public CmsPageVersion addVersion(String jsonContent) {
        Debug.logInfo("addVersion : "+jsonContent, module);
        return new CmsPageVersion(UtilMisc.toMap("pageId", this.getId(), "content", jsonContent), this);

    }

    /**
     * Copies this page including all linked products and the lastest version.
     */
    @Override
    public CmsPage copy() {
        // copy the page itself
        CmsPage pageCopy = (CmsPage) super.copy();
        // copy products
        Map<String, Map<String, ?>> productEntries = getProducts();
        for (String name : productEntries.keySet()) {
            Map<String, ?> product = productEntries.get(name);
            pageCopy.addProduct((String) product.get("productId"), name);
        }
        
        // copy latest version
        CmsPageVersion lastVersion = getLastVersion();
        
        try {
            if (lastVersion != null) {
                CmsPageVersion newVer = pageCopy.addVersion(lastVersion.getContent());
                newVer.store();
            } else {
                CmsPageVersion newVer = pageCopy.addVersion(pageCopy.getContent());
                newVer.store();
            }
        }
        catch(GenericEntityException e) {
            throw new CmsException("Entity could not be stored.", e, module);
        } catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}

        return pageCopy;
    }

    /**
     * Returns the page version currently active for this page.
     * 
     * @return
     */
    public CmsPageVersion getActiveVersion() {

        String versionId = entity.getString("activeVersionId");
        CmsPageVersion version = null;
        if (versionId != null) {
            version = new CmsPageVersion(versionId);
        }

        return version;
    }
    
    /**
     * Returns the page version currently active for this page or a new version if none.
     * <p>
     * Mimics getLastVersion() behaviour.
     * 
     * @return
     */
    public CmsPageVersion getActiveOrNewVersion() {
        CmsPageVersion version = getActiveVersion();
        
        if (version == null) {
            version = new CmsPageVersion(FastMap.<String, Object> newInstance(), this);
        }
        
        return version;
    }    

    /**
     * Retrieves the users authorized to edit this page by user role.
     * 
     * @param role
     *            The role the users should have on the page
     * @return List of users
     */
    public List<GenericValue> getAuthorizedUsers(UserRole role) {
        return findUsersByPageRole(this, role);
    }
    
    /**
     * Retrieves the users authorized to edit this page by user role.
     * 
     * @param role
     *            The role the users should have on the page
     * @return List of users
     */
    public List<GenericValue> getAuthorizedGrouos(UserRole role) {
        return findGroupsByPageRole(this, role);
    }    

    /**
     * Returns the content as map.
     * 
     * @return page content
     */
    @SuppressWarnings("unchecked")
    public Map<String, ?> getContent() {
        Map<String, ?> content = null;
        try {
            content = (Map<String, ?>) JSON.from(entity.getRelatedOne("activeVersionId",true)).toObject(Map.class);
        } catch (GenericEntityException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
            return FastMap.<String, Object> newInstance();
        } catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
        return content != null ? content : FastMap.<String, Object> newInstance();
    }

    /**
     * Retrieves the content model of this page. This method returns the content
     * of the current live version.
     * 
     * @return Content model
     */
    public CmsPageContent getContentModel() {

        return getContentModel(null);

    }

    /**
     * Retrieves the content model of this page. If the context specifies this
     * call to be a preview call the last version or given version (as parameter
     * "version") is returned. Otherwise, the current live content is returned.
     * The content is preprocessed according to the given page call context.
     * 
     * @return Content model
     */
    @SuppressWarnings("unchecked")
    public CmsPageContent getContentModel(CmsPageContext context) {

        // Initialize contentModel in any case
        if (contentModel == null) {
            contentModel = new CmsPageContent(getActiveOrNewVersion().getContent(), this);
        }

        CmsPageContent cm = null;
        /*
         * Set content model for this render to last version or given version if
         * page view is in preview mode.
         */
        if (context != null && context.isPreview()) {
            CmsPageVersion previewVersion;
            if (context.getRequest().getParameter("version") != null) {
                previewVersion = this.getVersion(context.getRequest().getParameter("version"));
            } else {
                previewVersion = getActiveVersion();
            }

            if (previewVersion != null){
                cm = new CmsPageContent(previewVersion.getContent(), this);
            }else{
                //TODO: Add caching
                //cm = new CmsPageContent(previewVersion.getContent(), this);
            }
        }

        return cm != null ? cm : contentModel;

    }

    /**
     * Returns a descriptor of this page as map. The descriptor contains the
     * metadata of this page but not the content. The following values are
     * included:
     * 
     * <dl>
     * <dt>id</dt>
     * <dd>Unique page id</dd>
     * <dt>path</dt>
     * <dd>The path of this page</dd>
     * <dt>webSiteId</dt>
     * <dd>The id of the website the page belongs to</dd>
     * <dt>templatePageId</dt>
     * <dd>The unique id of the template associated with this page</dd>
     * <dt>products</dt>
     * <dd>List of products as maps containing the <em>importName</em>,
     * <em>productId</em>, and <em>name</em></dd>
     * <dt>versions</dt>
     * <dd>List of page versions as maps containing the <em>id</em>,
     * <em>date</em>, <em>comment</em>, and <em>active</em> status</dd>
     * </dl>
     * 
     * @return
     */
    public Map<String, Object> getDescriptor() {

        Map<String, Object> descriptor = new FastMap<String, Object>(getShortDescriptor());
        Map<String, Map<String, ?>> products = getProducts();
        List<Map<String, ?>> productList = FastList.<Map<String, ?>> newInstance();
        Map<String, ?> product = null;
        

        for (String productName : products.keySet()) {
            product = products.get(productName);
            productList.add(UtilMisc.toMap("importName", productName, "productId", product.get("productId"), "name",
                    product.get("internalName")));
        }

        descriptor.put("products", productList);
        
        List<Map<String, ?>> versionList = FastList.<Map<String, ?>> newInstance();
        SimpleDateFormat isoDate = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss.SSSZ");

        for (CmsPageVersion version : getVersions()) {
            String date = isoDate.format(version.getLastModified());
            versionList.add(UtilMisc.toMap("id", version.getId(), "date", date, "comment", version.getComment(), "createdBy",version.getCreatedByName(),"active",
                    new Boolean(version.getId().equals(entity.getString("activeVersionId")))));
        }
        String websiteId = getWebSiteId();
        if(websiteId.equals("cmsSite")){
            websiteId = "cms";
        }else{
            websiteId = websiteId.replace("Site", "");
        }
        
        return UtilMisc.toMap("id", getId(), "name", getName(), "path", getPath(), "webSiteId", websiteId, "pageTemplateId",
                getTemplate().getId(), "products", productList, "versions", versionList);

    }
    
    public Map<String, String> getShortDescriptor() {
        String websiteId = getWebSiteId();
        if(websiteId.equals("cmsSite")){
            websiteId = "cms";
        }else{
            websiteId = websiteId.replace("Site", "");
        }
        return UtilMisc.toMap("id", getId(), "name", getName(), "path", getPath(), "webSiteId", websiteId, "pageTemplateId",
                getTemplate().getId());
    }    
    

    /**
     * Returns the content of the page as fully formatted string.
     * 
     * @param context
     * @return
     */
    public String getFormattedContent(CmsPageContext context) {

        return getTemplate().process(getContentModel(context), context);

    }

    /**
     * Returns the most current page version.
     * 
     * @return Page version
     */
    public CmsPageVersion getLastVersion() {

        CmsPageVersion version = CmsPageVersion.findLast(this.getId());
        if (version == null) {
            version = new CmsPageVersion(FastMap.<String, Object> newInstance(), this);
        }
        return version;

    }

    /**
     * Returns the name of the page.
     * 
     * @return Page name
     */
    public String getName() {

        return entity.getString("pageName");

    }

    /**
     * Returns the page path.
     * 
     * @return
     */
    public String getPath() {

        return entity.getString("pagePath");

    }

    public Map<String, ?> getProduct(String name) {

        return getProducts().get(name);

    }

    public Map<String, Map<String, ?>> getProducts() {

        if (products == null) {
            products = new FastMap<String, Map<String, ?>>();

            try {
                List<GenericValue> assocEntities = entity.getRelated("CmsPageProductAssoc");
                GenericValue asse;
                String importName;
                for (GenericValue assoce : assocEntities) {
                    asse = assoce.getRelatedOne("Product");
                    importName = assoce.getString("importName") == null ? asse.getPkShortValueString() : assoce
                            .getString("importName");
                    products.put(importName, asse);
                }
            } catch (Exception e) {
                throw new CmsException("Products could not be retrieved for page: " + this.getName(), e, module);
            }
        }
        return products;

    }

    /**
     * Returns the template for this page.
     * 
     * @return
     */
    public CmsPageTemplate getTemplate() {

        if (template == null) {
            GenericValue templateEntity;
            try {
                templateEntity = entity.getRelatedOneCache(CmsPageTemplate.class.getSimpleName());
            } catch (GenericEntityException e) {
                throw new CmsException("Error when retrieving page template. Page: " + getName() + " Template ID: "
                        + entity.getString("pageTemplateId"), e, module);
            }
            template = new CmsPageTemplate(templateEntity);
        }

        return template;

    }

    /**
     * Returns the template descriptor as map. This descriptor is mainly used
     * serialized as JSON for the editor front end.
     * 
     * @return
     */
    public Map<String, Object> getTemplateDescriptor() {

        return getTemplate().getDescriptor();

    }

    /**
     * Gets user's authorization.
     * <p>
     * Note: Delegator and dispatcher needed here; can't have
     * these as instance variables because they're not serializable and instances of this class *could* find
     * their way into session attributes.
     * 
     * @param userId
     * @param delegator
     * @param dispatcher
     * @return
     */
    public UserRole getUserAuthorization(String userId, Delegator delegator, LocalDispatcher dispatcher) {

        return findPageRoleForUser(this, userId, delegator, dispatcher);

    }

    /**
     * Returns a specific page version for this page.
     * 
     * @param versionId
     * @return
     */
    public CmsPageVersion getVersion(String versionId) {

        return CmsPageVersion.find(this.getId(), versionId);

    }

    public List<CmsPageVersion> getVersions() {

        return CmsPageVersion.findAll(this.getId());

    }

    /**
     * Returns id of website to which the page belongs.
     * 
     * @return website id
     */
    public String getWebSiteId() {

        return entity.getString("webSiteId");

    }

    /**
     * Removes the page from the database.
     * 
     * @return True if delete was successful, otherwise false
     * @throws GenericEntityException
     */
    public boolean remove() {

        int rowsAffected = 0;
        try {
            // delete CmsPageAuthorization
            List<GenericValue> pageAuthorizations = entity.getRelated("CmsPageAuthorization");
            for (GenericValue pageAuthorization : pageAuthorizations) {
                pageAuthorization.remove();
            }
            // delete CmsPageProductAssoc
            List<GenericValue> pageProducts = entity.getRelated("CmsPageProductAssoc");
            for (GenericValue pageProduct : pageProducts) {
                pageProduct.remove();
            }
            // delete CmsPageVersion
            getDelegator().removeByAnd("CmsPageVersion", "pageId", this.getId());
            rowsAffected = getDelegator().removeValue(entity, true);
        } catch (GenericEntityException e) {
            throw new CmsException("Entity could not be removed.", e, module);
        }
        return rowsAffected > 0 ? true : false;

    }

    public void removeProduct(String productId) {

        removeProducts(productId);

    }

    public void removeProducts() {

        removeProducts(null);

    }

    private void removeProducts(String productId) {

        try {
            List<GenericValue> assocEntities = entity.getRelated("CmsPageProductAssoc");
            for (GenericValue asse : assocEntities) {
                if (productId == null || asse.getString("productId").equals(productId)) {
                    asse.remove();
                }
            }
        } catch (GenericEntityException e) {
            new CmsException("Could not remove product links from page.", e, module);
        }

    }

    public void removeUserAuthorization(String userId) {

        try {
            getDelegator().removeByAnd("CmsPageAuthorization", "pageId", this.getId(), "userId", userId);
        } catch (GenericEntityException e) {
            throw new CmsException(String.format("Could not remove user authorization. Page: %s UserId: %s", this.getName(),
                    userId), e, module);
        }

    }
    
    public void removeGroupAuthorization(String groupId) {

        try {
            getDelegator().removeByAnd("CmsPageAuthorization", "pageId", this.getId(), "groupId", groupId);
        } catch (GenericEntityException e) {
            throw new CmsException(String.format("Could not remove group authorization. Page: %s UserId: %s", this.getName(),
                    groupId), e, module);
        }

    }    

    /**
     * Sets the version with the given version id as live version.
     * 
     * @param versionId
     */
    public void setActiveVersion(String versionId) {

        CmsPageVersion version = new CmsPageVersion(versionId);
        entity.set("activeVersionId", version.getId());
        try {
            entity.getDelegator().createOrStore(entity);
        } catch (GenericEntityException e) {
            throw new CmsException("Could not activate versionId:" + versionId + " for pageId: " + this.getId(), e, module);
        }

    }

    /**
     * Sets the content of a page as map.
     * 
     * @param content
     * @throws IOException 
     */
    public void setContent(Map<String, ?> content) throws IOException {

        setContent(JSON.from(content).toString());

    }

    /**
     * Sets the content of a page as CmsPageContent object.
     * 
     * @param content
     */
    public void setContent(CmsPageContent content) {
        try{
        JSON json = JSON.from(content);
        Debug.logInfo(json.toString(), module);
        
        setContent(json.toString());
        }catch(Exception e){
            Debug.logError(e, module);
        }
    }

    /**
     * Sets the content of the page as json string.
     * 
     * @param jsonContent
     */
    public void setContent(String jsonContent) {
        addVersion(jsonContent);
    }

    /**
     * Sets the name of the page.
     * 
     * @param name
     */
    public void setName(String name) {

        entity.setString("pageName", name);

    }

    /**
     * Sets id of pageTemplate to which the page belongs.
     * 
     * @param pageTemplate
     *            id
     */
    public void setPageTemplateId(String pageTemplateId) {

        entity.setString("pageTemplateId", pageTemplateId);

    }

    /**
     * Sets the page path. The path is normalized and any trailing slashs are
     * removed.
     * 
     * @param path
     */
    public void setPath(String path) {

        entity.setString("pagePath", normalizePath(path));

    }

    /**
     * Sets the template of this page. The page has to be stored to persist this
     * change.
     * 
     * @param template
     */
    public void setTemplate(CmsPageTemplate template) {

        entity.set("templateId", template.getId());
        this.template = template;

    }

    public void setUserAuthorization(String userId, String roleTypeId) {

        setUserAuthorization(userId, Enum.valueOf(UserRole.class, roleTypeId));

    }

    public void setUserAuthorization(String userId, UserRole role) {

        removeUserAuthorization(userId);
        try {
            GenericValue auth = getDelegator().makeValue("CmsPageAuthorization", "pageId", this.getId(), "userId", userId,
                    "roleTypeId", role.toString());
            auth.setNextSeqId();
            auth.create();
        } catch (GenericEntityException e) {
            throw new CmsException(String.format("Could not set user authorization. Page: %s UserId: %s Role: %s",
                    this.getName(), userId, role.toString()), e, module);
        }

    }
    
    
    public void setGroupAuthorization(String groupId, String roleTypeId) {

        setGroupAuthorization(groupId, Enum.valueOf(UserRole.class, roleTypeId));

    }
    
    public void setGroupAuthorization(String groupId, UserRole role) {
        
        removeGroupAuthorization(groupId);
        try {
            GenericValue auth = getDelegator().makeValue("CmsPageAuthorization", "pageId", this.getId(), "groupId", groupId,
                    "roleTypeId", role.toString());
            auth.setNextSeqId();
            auth.create();
        } catch (GenericEntityException e) {
            throw new CmsException(String.format("Could not set group authorization. Page: %s UserId: %s Role: %s",
                    this.getName(), groupId, role.toString()), e, module);
        }
        
    }
    

    /**
     * Sets id of website to which the page belongs.
     * 
     * @param website
     *            id
     */
    public void setWebSiteId(String webSiteId) {
        entity.setString("webSiteId", webSiteId);
    }

    public Map<String, Object> toMap() {
        return this.getDescriptor();
    }

    /**
     * Returns the page with the given name which should be unique within one
     * webSiteId.
     * 
     * @param name
     * @param webSiteId
     * @return
     */
    public static CmsPage findByName(String name, String webSiteId) {

        if (nameCache == null) {
            nameCache = CmsDataObject.<CmsPage> getNewCache();
        }
        String key = webSiteId + "::" + name;
        CmsPage page = nameCache.get(key);

        if (page == null) {
            Debug.logVerbose("Retrieving page from database: " + name, module);
            page = CmsDataObject.<CmsPage> findFirst(UtilMisc.toMap("pageName", name, "webSiteId", webSiteId), CmsPage.class);

            nameCache.put(key, page);
        } else {
            Debug.logVerbose("Retrieving page from cache: " + name, module);
        }

        return page;

    }

    /**
     * Returns the page with the given path which should be unique within one
     * webSiteId.
     * 
     * @param name
     * @param webSiteId
     * @return
     */
    public static CmsPage findByPath(String path, String webSiteId) {

        path = normalizePath(path);
        if (pathCache == null) {
            int expiration = UtilProperties.getPropertyAsInteger("cms.properties", "cache.page.expiration",
                    CmsDataObjectCache.getDefaultExpiration());
            pathCache = CmsDataObject.<CmsPage> getNewCache(expiration);
        }
        String key = webSiteId + "::" + path;
        Debug.logInfo("Finding page "+key, module);
        CmsPage page = pathCache.get(key);
        if (page == null) {
            Debug.logInfo("Retrieving page from database: " + path +" webSiteId :"+webSiteId, module);
            page = CmsDataObject.<CmsPage> findFirst(UtilMisc.toMap("pagePath", path, "webSiteId", webSiteId), CmsPage.class);
            pathCache.put(key, page);
        } else {
            Debug.logInfo("Retrieving page from cache: " + path, module);
        }

        return page;

    }

    /**
     * Returns all pages of a given website.
     * 
     * @param webSiteId
     *            Id of the website
     * @return List of pages
     */
    public static List<CmsPage> findAll() {

        return CmsDataObject.<CmsPage> findAll(CmsPage.class,UtilMisc.toList("pageName ASC"));

    }
    
    /**
     * Returns all pages of a given website.
     * 
     * @param webSiteId
     *            Id of the website
     * @return List of pages
     */
    public static List<CmsPage> findByWebSiteId(String webSiteId) {

        return CmsDataObject.<CmsPage> findAll(UtilMisc.toMap("webSiteId", webSiteId), CmsPage.class,UtilMisc.toList("pageName ASC"));

    }

    /**
     * Returns the most senior role that the user is assigned for the given page.
     * <p>
     * A user can be part of many groups and also be named individually in the authorization
     * for a given page.
     * 
     * @param page
     * @param userId
     * @param delegator
     * @param dispatcher
     * @return
     */
    public static UserRole findPageRoleForUser(CmsPage page, String userId, 
            Delegator delegator, LocalDispatcher dispatcher) {

        UserRole role;
        List<GenericValue> auths = null;
        try {
            // New in security groups: 
            // We find all the auths for which the user is explicitly specified or
            // the auths for which one of his security groups is specified.
            
            // Condition to find specific users
            EntityCondition userCond = makeUserCond(userId, delegator, dispatcher);
                
            // Condition to find user security groups
            EntityCondition userSecGroupsCond = makeUserSecGroupsCond(userId, delegator, dispatcher);
            
            // Combined user & group condition
            EntityCondition userOrGroupsCond;
            if (userSecGroupsCond != null) {
                userOrGroupsCond = EntityCondition.makeCondition(userSecGroupsCond, EntityOperator.OR, userCond);
            }
            else {
                userOrGroupsCond = userCond;
            }
            
            // Page condition
            EntityCondition whereCond = EntityCondition.makeCondition(
                    EntityCondition.makeCondition("pageId", EntityOperator.EQUALS, page.getId()),
                    EntityOperator.AND,
                    userOrGroupsCond);
            
            auths = delegator.findList("CmsPageAuthorization", whereCond, null, null, null, false);
        } catch (GenericEntityException e) {
            throw new CmsException("Could not retrieve page role for user. Page: " + page.getName() + " User: " + userId, e,
                    module);
        }
        if (UtilValidate.isNotEmpty(auths)) {
            // Here, the user might have multiple permissions per page due to overlapping groups and individual rights.
            // Return the role that gives him the most privilege.
            role = UserRole.findMostSeniorRole(auths, "roleTypeId");
        } else {
            role = DEFAULT_USER_ROLE;
        }
        return role;

    }
    
    private static EntityCondition makeUserCond(String userId, Delegator delegator, LocalDispatcher dispatcher) {
        return PageAuthPartyType.USER.makeExclusiveIdCond(userId);
    }
    
    private static EntityCondition makeUserSecGroupsCond(String userId, Delegator delegator, LocalDispatcher dispatcher) {
        // Get all the user's groups
        Iterator<GenericValue> userSecGroups = 
            dispatcher.getSecurity().findUserLoginSecurityGroupByUserLoginId(userId);
        
        List<EntityCondition> userSecGroupsConds = FastList.newInstance();
        while(userSecGroups.hasNext()) {
            GenericValue userSecGroup = userSecGroups.next();
            String groupId = userSecGroup.getString("groupId");
            userSecGroupsConds.add(
                    PageAuthPartyType.GROUP.makeIdCond(groupId)
            );
        }
        
        EntityCondition userSecGroupsCond = null;
        if (!userSecGroupsConds.isEmpty()) {
            // Make an OR condition to select all rows containing any one of the user's groups
            userSecGroupsCond = EntityCondition.makeCondition(userSecGroupsConds, EntityOperator.OR);
            
            // When making this condition, make sure it is exclusive; all other ID identifiers (userId, etc.) must be null for integrity/consistency
            userSecGroupsCond = EntityCondition.makeCondition(userSecGroupsCond, EntityOperator.AND,
                    PageAuthPartyType.GROUP.makeAllOthersNullCond()
            );
        }

        return userSecGroupsCond;
    }
    

    /**
     * Finds users currently authorized for the given page with the given role - only those 
     * named individually in the page authorization. Does not attempt to get all users from all allowed groups.
     * 
     * @param page
     * @param role
     * @return
     */
    public static List<GenericValue> findUsersByPageRole(CmsPage page, UserRole role) {

        List<GenericValue> users = null;
        try {
            List<EntityCondition> conds = FastList.newInstance();
            conds.add(EntityCondition.makeCondition("pageId", page.getId()));
            conds.add(EntityCondition.makeCondition("roleTypeId", role.toString()));
            conds.add(PageAuthPartyType.USER.makeExclusiveNonNullCond());
            EntityCondition whereCond = EntityCondition.makeCondition(conds, EntityOperator.AND);
            users = getDelegator().findList("CmsPageAuthorization", whereCond, null, null, null, false);
        } catch (GenericEntityException e) {
            throw new CmsException("Could not retrieve users with page role. Page: " + page.getName() + " Role:"
                    + role.toString(), e, module);
        }
        return users;

    }
    
    public static enum UserRole {
        CMS_ADMIN(4), 
        CMS_EDITOR(3), 
        CMS_SUPERVISOR(2), 
        CMS_VISITOR(1);
        
        private final int seniorityRank; // Higher -> more senior
        UserRole(int seniorityRank) {
            this.seniorityRank = seniorityRank;
        }
        
        public boolean isMoreSeniorThan(UserRole other) {
            if (other == null) {
                return true;
            }
            else {
                return this.seniorityRank > other.seniorityRank;
            }
        }
        
        public static UserRole findMostSeniorRole(Iterable<UserRole> roles) {
            UserRole result = null;
            if (roles != null) {
                for(UserRole role : roles) {
                    if (role != null) {
                        if (role.isMoreSeniorThan(result)) {
                            result = role;
                        }
                    }
                }
            }
            return result;
        }
        
        public static <T extends Map<String, Object>> UserRole findMostSeniorRole(Iterable<T> auths, String roleFieldName) {
            List<UserRole> roles = FastList.newInstance();
            if (auths != null) {
                for (T auth : auths) {
                    UserRole roleToCheck = Enum.valueOf(UserRole.class, (String) auth.get(roleFieldName));
                    roles.add(roleToCheck);
                }
            }
            return UserRole.findMostSeniorRole(roles);
        }
    }
    
    /**
     * Finds security groups currently authorized for the given page with the given role.
     * 
     * @param page
     * @param role
     * @return
     */
    public static List<GenericValue> findGroupsByPageRole(CmsPage page, UserRole role) {

        List<GenericValue> groups = null;
        try {
            List<EntityCondition> conds = FastList.newInstance();
            conds.add(EntityCondition.makeCondition("pageId", page.getId()));
            conds.add(EntityCondition.makeCondition("roleTypeId", role.toString()));
            conds.add(PageAuthPartyType.GROUP.makeExclusiveNonNullCond());
            EntityCondition whereCond = EntityCondition.makeCondition(conds, EntityOperator.AND);
            groups = getDelegator().findList("CmsPageAuthorization", whereCond, null, null, null, false);
        } catch (GenericEntityException e) {
            throw new CmsException("Could not retrieve users with page role. Page: " + page.getName() + " Role:"
                    + role.toString(), e, module);
        }
        return groups;

    }
    
    public static String normalizePath(String path) {

        return StringUtils.lowerCase(StringUtils.stripEnd(path, "/ "));

    }

    public boolean isActive() {
        
        return entity.get("activeVersionId") != null ? true : false;
        
    }
}
