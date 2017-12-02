package com.ilscipio.scipio.cms.template;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.EnumSet;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;

import org.apache.commons.lang.StringUtils;
import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericValue;

import com.ilscipio.scipio.cms.CmsException;
import com.ilscipio.scipio.cms.CmsInputException;
import com.ilscipio.scipio.cms.content.CmsPage.UserRole;
import com.ilscipio.scipio.cms.data.CmsDataException;
import com.ilscipio.scipio.cms.data.CmsDataObject;
import com.ilscipio.scipio.cms.template.AttributeExpander.ExpandLang;

public class CmsAttributeTemplate extends CmsDataObject {

    private static final long serialVersionUID = 8475667988447131968L;
    
    public static final String module = CmsAttributeTemplate.class.getName();

    /**
     * The default expand position (relative to scripts).
     * DEV NOTE: if ever changed, make sure to update the sorting code in {@link CmsComplexTemplate#getExpansionSortedAttributeTemplates}.
     */
    public static final Long DEFAULT_EXPAND_POSITION = 0L;
    /**
     * The default input position (relative to other attributes only).
     * DEV NOTE: if ever changed, make sure to update the sorting code in {@link CmsComplexTemplate#getExpansionSortedAttributeTemplates}.
     */
    public static final Long DEFAULT_INPUT_POSITION = 0L;
    
    /**
     * Attribute type.
     * NOTE: Type-related implementations can be found in {@link AttributeExpander}.
     */
    public enum Type {
        // TODO?: re-evaluate the old string types, SHORT_TEXT and LONG_TEXT, which are very generic.
        // for now am creating a expandLang=FTL value
        // to handle Freemarker code interpretation, but it could also have been done
        // using a dedicated type here... both could be supported simultaneously anyway, in case compatibility needed.
        
        SHORT_TEXT, 
        LONG_TEXT, 
        //DYNAMIC_LIST, // 2017-02-08: not supported
        BOOLEAN,
        INTEGER,
        // TODO?: NOT CURRENTLY SELECTABLE - NOT IMPLEMENTED - OMITTED FROM DISPLAYVALUES
        COMPLEX_LIST, // NOTE: may be different from DYNAMIC_LIST (old)
        COMPLEX_MAP;
        
        private static final List<Type> displayValues = Collections.unmodifiableList(new ArrayList<Type>(Arrays.asList(new Type[] {
                SHORT_TEXT, LONG_TEXT, BOOLEAN, INTEGER
        })));
        
        public static final Set<Type> STRING_TYPES = Collections.unmodifiableSet(EnumSet.of(SHORT_TEXT, LONG_TEXT));
        public static final List<Type> STRING_TYPES_LIST = Collections.unmodifiableList(new ArrayList<>(STRING_TYPES));

        public static Set<Type> getStringTypes() {
            return STRING_TYPES;
        }
        
        public static List<Type> getStringTypesList() {
            return STRING_TYPES_LIST;
        }
        
        public boolean isStringType() {
            return STRING_TYPES.contains(this);
        }
        
        public static boolean isStringType(Type type) {
            return type == null || type.isStringType();
        }
        
        public static Type fromString(String str, Type defaultType) throws IllegalArgumentException {
            if (UtilValidate.isEmpty(str)) return defaultType;
            return Enum.valueOf(Type.class, str);
        }
        
        public static Type fromStringSafe(String str, Type defaultType) {
            try {
                return fromString(str, defaultType);
            } catch(Exception e) {
                Debug.logWarning("Cms: Invalid attribute type value encountered: '" + str 
                        + "'; using default: '" + defaultType + "'", module);
            }
            return defaultType;
        }
        
        public static List<Type> getDisplayValues() {
            return displayValues;
        }
    }
    
    protected CmsAttributeTemplate(GenericValue entity) {
        super(entity);
    }

    public CmsAttributeTemplate(Delegator delegator, Map<String, ?> fields) {
        super(delegator, fields);
    }
    
    protected CmsAttributeTemplate(CmsAttributeTemplate other, Map<String, Object> copyArgs) {
        super(other, copyArgs);
        // we don't have to the IDs here, this is not stored until later so caller can update them
        //if (Boolean.TRUE.equals(copyArgs.get("clearIds")) {
        //    this.
        //}
    }

    @Override    
    public void update(Map<String, ?> fields, boolean setIfEmpty) {
        super.update(fields, setIfEmpty);
    }

    @Override
    public CmsAttributeTemplate copy(Map<String, Object> copyArgs) throws CmsException {
        return new CmsAttributeTemplate(this, copyArgs);
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
    }
    
    @Override
    protected void verifyNewFields(Delegator delegator, Map<String, Object> fields, boolean isNew) throws CmsException {
        // NOTE: 2017-04-11: we can now safely modify the fields in-place from this call.
        CmsDataObject.trimFields(fields, "targetType");
        
        String groupingField;
        if (UtilValidate.isNotEmpty((String) fields.get("pageTemplateId"))) {
            groupingField = "pageTemplateId";
        } else if (UtilValidate.isNotEmpty((String) fields.get("assetTemplateId"))) {
            groupingField = "assetTemplateId";
        } else {
            if (isNew) {
                throw new CmsInputException("Missing pageTemplateId or assetTemplateId"); // TODO: localize
            } else {
                if (UtilValidate.isNotEmpty(this.getEntity().getString("pageTemplateId"))) {
                    groupingField = "pageTemplateId";
                } else if (UtilValidate.isNotEmpty(this.getEntity().getString("assetTemplateId"))) {
                    groupingField = "assetTemplateId";
                } else {
                    throw new CmsDataException("Missing pageTemplateId or assetTemplateId in existing entity"); // TODO: localize
                }
            }
        }
        verifyUniqueName(delegator, fields, isNew, "attributeName", false, groupingField, false, true);
        String targetType = (String) fields.get("targetType");
        if (targetType != null) {
            try {
                AttributeExpander.checkValidJavaType(targetType);
            } catch (ClassNotFoundException e) {
                throw new CmsInputException("Invalid targetType: " + targetType 
                        + ". Should be a short or fully-qualified Java type such as: "
                        + StringUtils.join(AttributeExpander.getJavaTypeExampleList(), ", ") + ", ...", e); // TODO: localize
            }
        }
    }

    public void setHelp(String help) {
        entity.setString("inputHelp", help);
    }

    public String getHelp() {
        return entity.getString("inputHelp");
    }

    public void setName(String name) {
        entity.setString("attributeName", name);
    }

    public String getName() {
        return entity.getString("attributeName");
    }

    public void setDisplayName(String displayName) {
        entity.setString("displayName", displayName);
    }

    public String getDisplayName() {
        return entity.getString("displayName");
    }

    public void setDefaultValue(String defaultValue) {
        entity.setString("defaultValue", defaultValue);
    }

    public String getDefaultValue() {
        return entity.getString("defaultValue");
    }
    
    public String getCleanedDefaultValue() {
        String defVal = getDefaultValue();
        return StringUtils.isNotBlank(defVal) ? defVal : null;
    }

    public void setType(String type) {
        entity.setString("inputType", type);
    }

    public void setType(Type type) {
        entity.setString("inputType", type.toString());
    }

    public Type getType() {
        return Type.fromStringSafe(getTypeRaw(), Type.SHORT_TEXT);
    }
    
    public String getTypeRaw() {
        return entity.getString("inputType");
    }
    
    public static ExpandLang getDefaultExpandLang() {
        return AttributeExpander.ExpandLang.getDefaultExpandLang();
    }
    
    public void setExpandLang(ExpandLang expandLang) {
        entity.setString("expandLang", expandLang.toString());
    }
    
    /**
     * NOTE: returns system default when null.
     */
    public ExpandLang getExpandLang() {
        String expandLangStr = getExpandLangRaw();
        ExpandLang defaultLang = getDefaultExpandLang();
        try {
            return ExpandLang.fromString(expandLangStr, defaultLang);
        } catch(IllegalArgumentException e) {
            Debug.logWarning("Cms: Error in CmsAttributeTemplate '" + getId() + "': invalid expandLang value encountered: '" + 
                    expandLangStr + "'; using default instead: '" + defaultLang + "'", module);
            return defaultLang;
        }
    }
    
    /**
     * NOTE: returns null as null
     */
    public String getExpandLangRaw() {
        return entity.getString("expandLang");
    }
    
    public AttributeExpander getExpander() {
        return AttributeExpander.getExpander(getExpandLang());
    }
    
    public void setExpandPosition(Long expandPosition) {
        entity.set("expandPosition", expandPosition);
    }
    
    /**
     * The position of attribute evaluation relative to Scripts' inputPosition.
     */
    public Long getExpandPosition() {
        return entity.getLong("expandPosition") != null ? entity.getLong("expandPosition") : DEFAULT_EXPAND_POSITION;
    }
    
    /**
     * Returns true if the script is intended to expand (and possibly evaluate) before
     * given script.
     * <p>
     * The attributes expands before the script if its expandPosition is lower or equal than the script's 
     * inputPosition (NOTE: goes through its Assoc entity).
     * In other words, for the same position, attribute always runs first.
     */
    public boolean expandsBefore(CmsScriptTemplate scriptTemplate) {
        return getExpandPosition() <= scriptTemplate.getInputPosition();
    }
    

    public void setInputPosition(Long inputPosition) {
        entity.set("inputPosition", inputPosition);
    }

    /**
     * The position of attributes relative to each other (only).
     */
    public Long getInputPosition() {
        return entity.getLong("inputPosition") != null ? entity.getLong("inputPosition") : DEFAULT_INPUT_POSITION;
    }
    
    public void setMaxLength(Long maxLength) {
        entity.set("maxLength", maxLength);
    }

    public Long getMaxLength() {
        return entity.getLong("maxLength") != null ? entity.getLong("maxLength") : null; // no, bad: 0L
    }
    
    public void setRegularExpression(String regexp) {
        entity.set("regularExpression", regexp);
    }

    public String getRegularExpression() {
        return entity.getString("regularExpression") != null ? entity.getString("regularExpression") : "";
    }
    
    public String getTargetType() {
        return entity.getString("targetType");
    }

    public void setRequired(boolean required) {
        entity.set("required", required);
    }

    public boolean isRequired() {
        return entity.getBoolean("required") != null ? entity.getBoolean("required") : false;
    }
    
    public void setPermission(String roleTypeId) {
        setPermission(Enum.valueOf(UserRole.class, roleTypeId));
    }
    
    public void setPermission(UserRole role) {
        entity.set("permission", role.toString());
    }

    public UserRole getPermission() {
        return entity.getString("permission") != null ? Enum.valueOf(UserRole.class, entity.getString("permission")) : UserRole.CMS_EDITOR;
    }
    
    public void setTemplate(CmsComplexTemplate t) {
        if (t instanceof CmsPageTemplate) {
            entity.setString("pageTemplateId", t.getId());
            entity.setString("assetTemplateId", null);
        } else if (t instanceof CmsAssetTemplate) {
            entity.setString("pageTemplateId", null);
            entity.setString("assetTemplateId", t.getId());
        } else {
            throw new IllegalArgumentException("Attribute templates can only be linked to page "
                    + "or asset templates.");
        }
    }
    
    /**
     * WARN: this is a temporary operation; if caller forgets to update the template,
     * result will be dangling attributes in the system.
     */
    public void clearTemplate() {
        entity.setString("pageTemplateId", null);
        entity.setString("assetTemplateId", null);
    }
    
    public boolean hasTemplate() {
        return entity.get("pageTemplateId") != null || entity.get("assetTemplateId") != null;
    }
    
    /**
     * TODO: REVIEW: What is this again?
     */
    void updateTemplateFields(Map<String, ?> fields) {
        entity.setAllFields(fields, false, null, false);
    }

    @Override
    public Map<String, Object> getDescriptor(Locale locale) {
        preventIfImmutable(); // WARN: currently dangerous if called from rendering!

        Map<String, Object> map = super.getDescriptor(locale);
        map.putAll(UtilMisc.toMap("name", getName(), 
                    "id", getId(),
                    "displayName", getDisplayName(),
                    "type", getType().toString(), 
                    "targetType", getTargetType(),
                    "defaultValue", getDefaultValue(), 
                    "help", getHelp(), 
                    "required", new Boolean(isRequired()).toString(),
                    "permission", getPermission().toString(),
                    "maxLength", getMaxLength(),
                    "regularExpression", getRegularExpression().toString(),
                    "expandLang", getExpandLangRaw(),
                    "expandPosition", getExpandPosition(),
                    "inputPosition", getInputPosition()
                ));
        return map;
    }

    @Override
    public AttributeTemplateWorker getWorkerInst() {
        return AttributeTemplateWorker.worker;
    }
    
    public static AttributeTemplateWorker getWorker() {
        return AttributeTemplateWorker.worker;
    }

    public static class AttributeTemplateWorker extends DataObjectWorker<CmsAttributeTemplate> {
        private static final AttributeTemplateWorker worker = new AttributeTemplateWorker();
        
        protected AttributeTemplateWorker() {
            super(CmsAttributeTemplate.class);
        }

        @Override
        public CmsAttributeTemplate makeFromValue(GenericValue value) throws CmsException {
            return new CmsAttributeTemplate(value);
        }

        @Override
        public CmsAttributeTemplate makeFromFields(Delegator delegator, Map<String, ?> fields) throws CmsException {
            return new CmsAttributeTemplate(delegator, fields);
        }
    }
}
