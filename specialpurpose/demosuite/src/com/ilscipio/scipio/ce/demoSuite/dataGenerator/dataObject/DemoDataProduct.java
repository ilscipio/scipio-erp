package com.ilscipio.scipio.ce.demoSuite.dataGenerator.dataObject;

public class DemoDataProduct implements DemoDataObject {

    private String id;
    private String name;
    private String description;
    private String longDescription;
    private String price;
    private String type;
    private String category;
    private String categoryType;
    private String catalogCategoryType;

    public String getId() {
        return id;
    }

    public void setId(String id) {
        this.id = id;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public String getDescription() {
        return description;
    }

    public void setDescription(String description) {
        this.description = description;
    }

    public String getPrice() {
        return price;
    }

    public void setPrice(String price) {
        this.price = price;
    }

    public String getLongDescription() {
        return longDescription;
    }

    public void setLongDescription(String longDescription) {
        this.longDescription = longDescription;
    }

    public String getCategory() {
        return category;
    }

    public void setCategory(String category) {
        this.category = category;
    }

    public String getCategoryType() {
        return categoryType;
    }

    public void setCategoryType(String categoryType) {
        this.categoryType = categoryType;
    }

    public String getCatalogCategoryType() {
        return catalogCategoryType;
    }

    public void setCatalogCategoryType(String catalogCategoryType) {
        this.catalogCategoryType = catalogCategoryType;
    }

    public String getType() {
        return type;
    }

    public void setType(String type) {
        this.type = type;
    }

}
