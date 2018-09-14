package com.ilscipio.scipio.cms.template;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import java.util.Locale;
import java.util.Map;

import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericEntityException;
import org.ofbiz.entity.GenericValue;

import com.ilscipio.scipio.cms.CmsException;

/**
 * Complex template, supporting attributes, but NOT supporting versioning.
 */
public abstract class CmsComplexTemplate extends CmsTemplate {

    private static final long serialVersionUID = -3008583211100355776L;

    //private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    protected List<CmsAttributeTemplate> attributeTemplates = null;

    protected CmsComplexTemplate(GenericValue entity) {
        super(entity);
    }

    protected CmsComplexTemplate(Delegator delegator, Map<String, ?> fields) {
        super(delegator, fields);
    }

    protected CmsComplexTemplate(CmsComplexTemplate other, Map<String, Object> copyArgs) {
        super(other, copyArgs);
        this.attributeTemplates = copyAttributeTemplates(this, other.getAttributeTemplates(), copyArgs);
    }

    public static List<CmsAttributeTemplate> copyAttributeTemplates(CmsComplexTemplate template, List<CmsAttributeTemplate> otherAttr, Map<String, Object> copyArgs) {
        List<CmsAttributeTemplate> attrList = new ArrayList<>(otherAttr.size());
        for(CmsAttributeTemplate attr : otherAttr) {
            CmsAttributeTemplate attrCopy = attr.copy(copyArgs);
            attrCopy.clearTemplate(); // these will be updated on store
            attrList.add(attrCopy);
        }
        return attrList;
    }

    @Override
    public void update(Map<String, ?> fields, boolean setIfEmpty) {
        super.update(fields, setIfEmpty);
    }

    /**
     * 2016: Loads ALL this object's content into the current instance.
     * <p>
     * WARN: IMPORTANT: AFTER THIS CALL,
     * NO FURTHER CALLS ARE ALLOWED TO MODIFY THE INSTANCE IN MEMORY.
     * Essential for thread safety!!!
     */
    @Override
    public void preload(PreloadWorker preloadWorker) {
        super.preload(preloadWorker);
        this.attributeTemplates = preloadWorker.preloadDeep(this.getAttributeTemplates());
    }

    @Override
    public String getName() {
        return entity.getString("templateName");
    }

    public void setName(String name) {
        preventIfImmutable();
        entity.setString("templateName", name);
    }

    public String getDescription() {
        return getDescription(entity, null);
    }

    public String getDescription(Locale locale) {
        return getDescription(entity, locale);
    }

    /**
     * Get description with optional locale.
     * NOTE: the value stored in the entity if present takes precedence over
     * the translations, because we have to do something closer to the content wrappers for
     * this.
     */
    public static String getDescription(GenericValue entity, Locale locale) {
        return getStoredOrLocalizedField(entity, "description", locale);
    }

    public void setDescription(String description) {
        preventIfImmutable();
        entity.setString("description", description);
    }

    public String getTemplateName() {
        return entity.getString("templateName");
    }

    public void setActive(Boolean active) {
        preventIfImmutable();
        entity.set("active", active);
    }

    public Boolean getActive() {
        return entity.getBoolean("active");
    }

    public void setCreatedBy(String createdBy) {
        preventIfImmutable();
        entity.setString("createdBy", createdBy);
    }

    public String getCreatedBy() {
        return entity.getString("createdBy");
    }

    /**
     * NOTE: Class can override this and return null to signify no attribute templates support.
     * <p>
     * 2017-02-20: this returns the attributes sorted first by expandPosition and then by inputPosition.
     */
    public List<CmsAttributeTemplate> getAttributeTemplates() {
        List<CmsAttributeTemplate> attributeTemplates = this.attributeTemplates;
        if (attributeTemplates == null) {
            attributeTemplates = new ArrayList<>();
            try {
                // NOTE: NULL values should sort to top, which matches default of value 0
                List<GenericValue> ate = entity.getRelated(CmsAttributeTemplate.class.getSimpleName(), null,
                        UtilMisc.toList("expandPosition ASC NULLS FIRST", "inputPosition ASC NULLS FIRST"), false);
                for (GenericValue at : ate) {
                    attributeTemplates.add(CmsAttributeTemplate.getWorker().makeFromValue(at));
                }
            } catch (GenericEntityException e) {
                throw new CmsException("Attribute templates could not be retrieved. Template: " + getName(), e);
            }
            this.attributeTemplates = attributeTemplates;
        }
        return attributeTemplates;
    }

    /**
     * Returns the attribute templates sorted first by expandPosition (global order relative to CmsScriptTemplates)
     * and then by inputPosition (relative to each other only).
     */
    public List<CmsAttributeTemplate> getExpansionSortedAttributeTemplates() {
        return getAttributeTemplates();
    }

    /**
     * Returns attributes sorted by inputPosition only (NOT by expandPosition).
     * <p>
     * Probably shouldn't be used for anything other than UI.
     */
    public List<CmsAttributeTemplate> getLocallySortedAttributeTemplates() {
        List<CmsAttributeTemplate> srcAttributeTemplates = this.getAttributeTemplates();
        if (srcAttributeTemplates == null) {
            return null;
        }

        List<CmsAttributeTemplate> attributeTemplates = new ArrayList<>(srcAttributeTemplates);
        Collections.<CmsAttributeTemplate> sort(attributeTemplates, new Comparator<CmsAttributeTemplate>() {
            @Override
            public int compare(CmsAttributeTemplate first, CmsAttributeTemplate second) {
                return first.getInputPosition().compareTo(second.getInputPosition());
            }
        });

        return attributeTemplates;
    }

    public void addAttributeTemplate(CmsAttributeTemplate template) {
        preventIfImmutable();

        try {
            List<CmsAttributeTemplate> templates = getAttributeTemplates();
            if (templates != null) {
                template.setTemplate(this);
                templates.add(template);
            }
        } catch (Exception e) {
            throw new CmsException("Could not add attribute template to page template. Template Name: " + getName(), e);
        }
    }

    @Override
    public void store() throws CmsException {
        super.store();

        if (this.attributeTemplates != null) {
            for(CmsAttributeTemplate attributeTemplate : this.attributeTemplates) {
                if (!attributeTemplate.hasTemplate()) {
                    attributeTemplate.setTemplate(this);
                    attributeTemplate.store();
                }
            }
        }
    }

    /**
     * Explicitly Stores only the template record. For special circumstances.
     */
    @Override
    protected void storeSelfOnly() throws CmsException {
        super.storeSelfOnly();
    }

    @Override
    public int remove() throws CmsException {
        int rowsAffected = 0;
        // Delete all attributes (IF APPLICABLE - subclasses may not support)
        List<CmsAttributeTemplate> attributeTemplates = this.getAttributeTemplates();
        if (attributeTemplates != null) {
            rowsAffected += removeAll(attributeTemplates);
        }
        return rowsAffected + super.remove();
    }

    @Override
    public Map<String, Object> getDescriptor(Locale locale) {
        preventIfImmutable(); // WARN: currently dangerous if called from rendering!

        Map<String, Object> descriptor = super.getDescriptor(locale);
        descriptor.put("name", getName());
        descriptor.put("description", getDescription(locale));

        List<CmsAttributeTemplate> attributeTemplates = getLocallySortedAttributeTemplates();

        List<Map<String, ?>> atts = new ArrayList<>();
        for (CmsAttributeTemplate attributeTemplate : attributeTemplates) {
            atts.add(attributeTemplate.getDescriptor(locale));
        }
        descriptor.put("attributes", atts);

        return descriptor;
    }

}
