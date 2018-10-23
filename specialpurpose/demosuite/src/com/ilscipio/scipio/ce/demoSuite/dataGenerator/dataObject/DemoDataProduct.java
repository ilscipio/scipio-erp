package com.ilscipio.scipio.ce.demoSuite.dataGenerator.dataObject;

import java.math.BigDecimal;
import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.List;

public class DemoDataProduct implements AbstractDataObject {

    private String id;
    private String name;
    private String description;
    private String longDescription;
    private List<DemoDataProductPrice> prices = new ArrayList<DemoDataProductPrice>();
    private String type;
    private String category;
    private String categoryType;
    private String catalogCategoryType;
    private Timestamp introductionDate;

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

    public Timestamp getIntroductionDate() {
        return introductionDate;
    }

    public void setIntroductionDate(Timestamp introductionDate) {
        this.introductionDate = introductionDate;
    }

    public List<DemoDataProductPrice> getPrices() {
        return prices;
    }

    public void setPrices(List<DemoDataProductPrice> prices) {
        this.prices = prices;
    }

    public void addPrice(DemoDataProductPrice price) {
        this.prices.add(price);
    }

    public class DemoDataProductPrice {
        private BigDecimal price;
        private String type;
        private String purpose;
        private String currency;

        public DemoDataProductPrice(BigDecimal price, String type, String purpose, String currency) {
            this.price = price;
            this.type = type;
            this.purpose = purpose;
            this.currency = currency;
        }

        public BigDecimal getPrice() {
            return price;
        }

        public void setPrice(BigDecimal price) {
            this.price = price;
        }

        public String getType() {
            return type;
        }

        public void setType(String type) {
            this.type = type;
        }

        public String getCurrency() {
            return currency;
        }

        public void setCurrency(String currency) {
            this.currency = currency;
        }

        public String getPurpose() {
            return purpose;
        }

        public void setPurpose(String purpose) {
            this.purpose = purpose;
        }
    }

}
