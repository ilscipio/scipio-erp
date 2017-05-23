package org.ofbiz.widget.model.ftl;

import java.util.Map;

import org.ofbiz.widget.model.ContainsExpr;
import org.ofbiz.widget.model.ModelWidget;
import org.ofbiz.widget.model.ModelWidgetVisitor;

/**
 * TODO: Special wrapper for FTL elements to pass off as widgets.
 * Currently useless, no support for FTL matching and support uncertain.
 */
@SuppressWarnings("serial")
public class ModelFtlWidget extends ModelWidget implements FtlWrapperWidget, ModelWidget.IdAttrWidget, ContainsExpr.FlexibleContainsExprAttrWidget {
    private final String dirName;
    private final String location;
    private final String id;
    private final ContainsExpr containsExpr;
    
    public ModelFtlWidget(String name, String dirName, String location, String id, String containsExpr) {
        super(name != null ? name : "");
        this.dirName = dirName;
        this.location = location;
        this.id = id;
        this.containsExpr = ContainsExpr.getInstanceOrDefault(containsExpr);
    }
    
    public ModelFtlWidget(String name, String dirName, String location, String id) {
        this(name, dirName, location, id, null);
    }

    @Override
    public void accept(ModelWidgetVisitor visitor) throws Exception {
    }

    @Override
    public String getContainerLocation() {
        return location;
    }

    @Override
    public String getWidgetType() {
        // WARN: we have to prefix this otherwise there's a risk we'll interfere with widget names
        return "ftl-" + dirName;
    }
    
    @Override
    public String getTagName() {
        return dirName;
    }

    @Override
    public String getId() {
        return id;
    }

    @Override
    public ContainsExpr getContainsExpr(Map<String, Object> context) {
        return containsExpr;
    }
}