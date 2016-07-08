package com.ilscipio.scipio.cms.template;

import java.util.Map;

import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.entity.GenericEntityException;
import org.ofbiz.entity.GenericValue;
import com.ilscipio.scipio.cms.CmsException;
import com.ilscipio.scipio.cms.content.CmsPage.UserRole;
import com.ilscipio.scipio.cms.data.CmsDataObject;

public class CmsAttributeTemplate extends CmsDataObject {

    private static final long serialVersionUID = 15334054236167270L;
    public static final String module = CmsAttributeTemplate.class.getName();

    public static enum Type {
        SHORT_TEXT, LONG_TEXT, DYNAMIC_LIST, BOOLEAN
    };

    public CmsAttributeTemplate(Map<String, ?> fields) {
        super(fields);
    }

    public CmsAttributeTemplate(String primaryKey) {
        super(primaryKey);
    }

    public CmsAttributeTemplate(GenericValue entity) {
        super(entity);
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

    public void setType(String type) {
        entity.setString("inputType", type);
    }

    public void setType(Type type) {
        entity.setString("inputType", type.toString());
    }

    public Type getType() {
        return entity.getString("inputType") != null ? Enum
                .valueOf(CmsAttributeTemplate.Type.class,
                        entity.getString("inputType")) : Type.SHORT_TEXT;
    }

    public void setPosition(Long position) {
        entity.set("inputPosition", position);
    }

    public Long getPosition() {
        return entity.getLong("inputPosition") != null ? entity
                .getLong("inputPosition") : 0L;
    }
    
    public void setMaxLength(Long maxLength) {
        entity.set("maxLength", maxLength);
    }

    public Long getMaxLength() {
        return entity.getLong("maxLength") != null ? entity
                .getLong("maxLength") : 0L;
    }
    
    public void setRegexp(String regexp) {
        entity.set("regexp", regexp);
    }

    public String getRegexp() {
        return entity.getString("regexp") != null ? entity.getString("regexp") : "";
    }

    public void setRequired(boolean required) {
        entity.set("required", required);
    }

    public boolean isRequired() {
        return entity.getBoolean("required") != null ? entity
                .getBoolean("required") : false;
    }

    public void setMarkdown(boolean markdown) {
        entity.set("markdown", markdown);
    }

    public boolean getMarkdown() {
        return entity.getBoolean("markdown") != null ? entity
                .getBoolean("markdown") : true;
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
    
    public void setTemplate(CmsTemplate t) {
        if (t instanceof CmsPageTemplate) {
            entity.setString("pageTemplateId", t.getId());
        } else if (t instanceof CmsAssetTemplate) {
            entity.setString("assetTemplateId", t.getId());
        } else {
            throw new IllegalArgumentException(
                    "Attribute templates can only be linked to page or asset templates.");
        }
    }

    public void setTemplate(CmsAssetTemplate at) {
        entity.setString("assetTemplateId", at.getId());
    }

    public Map<String, ?> getDescriptor() {
        return UtilMisc
                .toMap("name", getName(), "displayName", getDisplayName(),
                        "type", getType().toString(), "defaultValue",
                        getDefaultValue(), "help", getHelp(), "position",
                        getPosition(), "required",
                        new Boolean(isRequired()).toString(),
                        "markdown", new Boolean(getMarkdown()).toString(),
                        "permission", getPermission().toString(),
                        "maxLength",getMaxLength().toString(),
                        "regexp",getRegexp().toString()
                        );
    }

}
