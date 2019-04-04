/*******************************************************************************
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements.  See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership.  The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License.  You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations
 * under the License.
 *******************************************************************************/

package org.ofbiz.product.config;

import java.io.Serializable;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.GeneralException;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.DelegatorFactory;
import org.ofbiz.entity.GenericEntityException;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.entity.util.EntityQuery;
import org.ofbiz.service.LocalDispatcher;
import org.ofbiz.service.ServiceContainer;
import org.ofbiz.service.ServiceUtil;

/**
 * Product Config Wrapper: gets product config to display
 */
@SuppressWarnings("serial")
public class ProductConfigWrapper implements Serializable {

    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    protected transient LocalDispatcher dispatcher;
    protected String dispatcherName;
    protected String productStoreId;
    protected String catalogId;
    protected String webSiteId;
    protected String currencyUomId;
    protected transient Delegator delegator;
    protected String delegatorName = null;
    protected GenericValue product = null; // the aggregated product
    protected GenericValue autoUserLogin = null;
    protected BigDecimal listPrice = BigDecimal.ZERO;
    protected BigDecimal basePrice = BigDecimal.ZERO;
    protected BigDecimal defaultPrice = BigDecimal.ZERO;
    protected String configId = null; // Id of persisted ProductConfigWrapper
    protected List<ConfigItem> questions = null; // ProductConfigs

    /**
     * SCIPIO: If the product had an original list price, this will be non-null.
     * This will be null if the product does not have an explicit list price.
     * Required because the listPrice member above is always initialized to ZERO,
     * so it can't be used to check this.
     * Added 2017-08-22.
     */
    protected BigDecimal originalListPrice = null;

    /** Creates a new instance of ProductConfigWrapper */
    public ProductConfigWrapper() {
    }

    public ProductConfigWrapper(Delegator delegator, LocalDispatcher dispatcher, String productId, String productStoreId, String catalogId, String webSiteId, String currencyUomId, Locale locale, GenericValue autoUserLogin) throws Exception {
        init(delegator, dispatcher, productId, productStoreId, catalogId, webSiteId, currencyUomId, locale, autoUserLogin);
    }

    /**
     * Copy constructor (exactCopy==false).
     */
    public ProductConfigWrapper(ProductConfigWrapper pcw) {
        this(pcw, false);
    }
    
    /**
     * Copy constructor.
     * SCIPIO: Added exactCopy
     */
    public ProductConfigWrapper(ProductConfigWrapper pcw, boolean exactCopy) {
        if (exactCopy) {
            // SCIPIO: full copy
            dispatcher = pcw.dispatcher;
            dispatcherName = pcw.dispatcherName;
            delegator = pcw.delegator;
            delegatorName = pcw.delegatorName;
            product = pcw.product;
            configId = pcw.configId;
            List<ConfigItem> questions = null;
            if (pcw.questions != null) {
                questions = new ArrayList<>();
                for (ConfigItem ci: pcw.questions) {
                    questions.add(new ConfigItem(ci, exactCopy));
                }
            }
            this.questions = questions;
        } else {
            // legacy
            product = GenericValue.create(pcw.product);

            delegator = pcw.getDelegator();
            delegatorName = delegator.getDelegatorName();
            dispatcher = pcw.getDispatcher();
            dispatcherName = dispatcher.getName();
            // SCIPIO: Use local var
            //questions = new ArrayList<>();
            List<ConfigItem> questions = null;
            if (pcw.questions != null) {
                questions = new ArrayList<>();
                for (ConfigItem ci: pcw.questions) {
                    questions.add(new ConfigItem(ci));
                }
            }
            this.questions = questions;
        }
        
        listPrice = pcw.listPrice;
        basePrice = pcw.basePrice;
        defaultPrice = pcw.defaultPrice;
        
        productStoreId = pcw.productStoreId;
        catalogId = pcw.catalogId;
        webSiteId = pcw.webSiteId;
        currencyUomId = pcw.currencyUomId;
        autoUserLogin = pcw.autoUserLogin;
        
        originalListPrice = pcw.originalListPrice; // SCIPIO
    }

    private void init(Delegator delegator, LocalDispatcher dispatcher, String productId, String productStoreId, String catalogId, String webSiteId, String currencyUomId, Locale locale, GenericValue autoUserLogin) throws Exception {
        product = EntityQuery.use(delegator).from("Product").where("productId", productId).queryOne();
        if (product == null || !"AGGREGATED".equals(product.getString("productTypeId")) && !"AGGREGATED_SERVICE".equals(product.getString("productTypeId"))) {
            throw new ProductConfigWrapperException("Product " + productId + " is not an AGGREGATED product.");
        }
        this.dispatcher = dispatcher;
        this.dispatcherName = dispatcher.getName();
        this.productStoreId = productStoreId;
        this.catalogId = catalogId;
        this.webSiteId = webSiteId;
        this.currencyUomId = currencyUomId;
        this.delegator = delegator;
        this.delegatorName = delegator.getDelegatorName();
        this.autoUserLogin = autoUserLogin;

        // get the list Price, the base Price
        Map<String, Object> priceContext = UtilMisc.toMap("product", product, "prodCatalogId", catalogId, "webSiteId", webSiteId, "productStoreId", productStoreId,
                                      "currencyUomId", currencyUomId, "autoUserLogin", autoUserLogin);
        Map<String, Object> priceMap = dispatcher.runSync("calculateProductPrice", priceContext);
        if (ServiceUtil.isError(priceMap)) {
            String errorMessage = ServiceUtil.getErrorMessage(priceMap);
            throw new GeneralException(errorMessage);
        }
        BigDecimal originalListPrice = (BigDecimal) priceMap.get("listPrice");
        BigDecimal price = (BigDecimal) priceMap.get("price");
        this.originalListPrice = originalListPrice; // SCIPIO: new
        if (originalListPrice != null) {
            listPrice = originalListPrice;
        }
        if (price != null) {
            basePrice = price;
        }
        questions = new ArrayList<>();
        if ("AGGREGATED".equals(product.getString("productTypeId")) || "AGGREGATED_SERVICE".equals(product.getString("productTypeId"))) {
            List<GenericValue> questionsValues = EntityQuery.use(delegator).from("ProductConfig").where("productId", productId).orderBy("sequenceNum").filterByDate().queryList();
            Set<String> itemIds = new HashSet<>();
            for (GenericValue questionsValue: questionsValues) {
                ConfigItem oneQuestion = new ConfigItem(questionsValue);
                oneQuestion.setContent(locale, "text/html"); // TODO: mime-type shouldn't be hardcoded
                if (itemIds.contains(oneQuestion.getConfigItem().getString("configItemId"))) {
                    oneQuestion.setFirst(false);
                } else {
                    itemIds.add(oneQuestion.getConfigItem().getString("configItemId"));
                }
                questions.add(oneQuestion);
                List<GenericValue> configOptions = EntityQuery.use(delegator).from("ProductConfigOption").where("configItemId", oneQuestion.getConfigItemAssoc().getString("configItemId")).orderBy("sequenceNum").queryList();
                for (GenericValue configOption: configOptions) {
                    ConfigOption option = new ConfigOption(delegator, dispatcher, configOption, oneQuestion, catalogId, webSiteId, currencyUomId, autoUserLogin);
                    oneQuestion.addOption(option);
                }
            }
            this.setDefaultPrice();
        }
    }

    /**
     * SCIPIO: Tests to ensure the wrapper is an exact copy of the other; used to verify the exact copy code.
     * NOTE: This is NOT the same as a logical Object equals override! This is mainly for testing.
     */
    public void ensureExactEquals(ProductConfigWrapper other) {
        try {
            ensureExactEquals(this.dispatcher, other.dispatcher);
            ensureExactEquals(this.dispatcherName, other.dispatcherName);
            ensureExactEquals(this.productStoreId, other.productStoreId);
            ensureExactEquals(this.catalogId, other.catalogId);
            ensureExactEquals(this.webSiteId, other.webSiteId);
            ensureExactEquals(this.currencyUomId, other.currencyUomId);
            ensureExactEquals(this.delegator, other.delegator);
            ensureExactEquals(this.delegatorName, other.delegatorName);
            ensureExactEquals(this.product, other.product);
            ensureExactEquals(this.autoUserLogin, other.autoUserLogin);
            ensureExactEquals(this.listPrice, other.listPrice);
            ensureExactEquals(this.basePrice, other.basePrice);
            ensureExactEquals(this.defaultPrice, other.defaultPrice);
            ensureExactEquals(this.configId, other.configId);
            ensureExactEquals(this.questions, other.questions);
        } catch(IllegalStateException e) {
            throw new IllegalStateException("ProductConfigWrapper field not equal: " + e.getMessage(), e);
        }
    }

    static void ensureExactEquals(Object first, Object second) {
        if (first == null) {
            if (second != null) {
                throw new IllegalStateException("values not equal: " + first + ", " + second);
            } else {
                return;
            }
        }
        if (!first.getClass().equals(second.getClass())) {
            throw new IllegalStateException("values not equal: " + first + " (" + first.getClass() + "), " 
                    + second + " (" + second.getClass() + ")");
        }
        if (first instanceof ConfigItem) {
            ((ConfigItem) first).ensureExactEquals((ConfigItem) second);
        } else if (first instanceof ConfigOption) {
            ((ConfigOption) first).ensureExactEquals((ConfigOption) second);
        } else if (first instanceof GenericValue) {
            if (!first.equals(second)) {
                throw new IllegalStateException("GenericValues not equal: " + first + ", " + second);
            }
        } else if (first instanceof Map) {
            Map<?, ?> firstMap = (Map<?, ?>) first;
            Map<?, ?> secondMap = (Map<?, ?>) second;
            if (firstMap.size() != secondMap.size()) {
                throw new IllegalStateException("Maps not equal: " + first + ", " + second);
            }
            for(Map.Entry<?, ?> entry : firstMap.entrySet()) {
                ensureExactEquals(entry.getValue(), secondMap.get(entry.getKey()));
            }
            if (!first.equals(second)) { // WARN: This may break on new fields
                throw new IllegalStateException("Maps not equal: " + first + ", " + second);
            }
        } else if (first instanceof List) {
            List<?> firstList = (List<?>) first;
            List<?> secondList = (List<?>) second;
            if (firstList.size() != secondList.size()) {
                throw new IllegalStateException("Lists not equal: " + first + ", " + second);
            }
            for(int i=0; i<firstList.size(); i++) {
                ensureExactEquals(firstList.get(i), secondList.get(i));
            }
        } else if (first.getClass().isArray()) {
            if (!Arrays.equals((Object[]) first, (Object[]) second)) {
                throw new IllegalStateException("Arrays not equal: " + first + ", " + second);
            }
        } else {
            if (!first.equals(second)) {
                throw new IllegalStateException("Values not equal: " + first + ", " + second);
            }
        }
    }

    public void loadConfig(Delegator delegator, String configId) throws Exception {
        //configure ProductConfigWrapper according to ProductConfigConfig entity
        if (UtilValidate.isNotEmpty(configId)) {
            this.configId = configId;
            List<GenericValue> productConfigConfig = EntityQuery.use(delegator).from("ProductConfigConfig").where("configId", configId).queryList();
            if (UtilValidate.isNotEmpty(productConfigConfig)) {
                for (GenericValue pcc: productConfigConfig) {
                    String configItemId = pcc.getString("configItemId");
                    String configOptionId = pcc.getString("configOptionId");
                    Long sequenceNum = pcc.getLong("sequenceNum");
                    String comments = pcc.getString("description");
                    this.setSelected(configItemId, sequenceNum, configOptionId, comments);
                }
            }
        }
    }

    public void setSelected(String configItemId, Long sequenceNum, String configOptionId, String comments) throws Exception {
        for (int i = 0; i < questions.size(); i++) {
            ConfigItem ci = questions.get(i);
            if (ci.configItemAssoc.getString("configItemId").equals(configItemId) && ci.configItemAssoc.getLong("sequenceNum").equals(sequenceNum)) {
                List<ConfigOption> avalOptions = ci.getOptions();
                for (int j = 0; j < avalOptions.size(); j++) {
                    ConfigOption oneOption = avalOptions.get(j);
                    if (oneOption.configOption.getString("configOptionId").equals(configOptionId)) {
                        setSelected(i, j, comments);
                        break;
                    }
                }
            }
        }
    }

    public void resetConfig() {
        for (ConfigItem ci: questions) {
            if (!ci.isStandard()) {
                List<ConfigOption> options = ci.getOptions();
                for (ConfigOption co: options) {
                    co.setSelected(false);
                    co.setComments(null);
                }
            }
        }
    }

    /**
     * SCIPIO: Resets all selections including standard items and comments,
     * virtual component options, or otherwise any selections that may be set
     * by {@link ProductConfigWorker#fillProductConfigWrapper} (no validation).
     * Should correspond to the initial state of a newly-constructed ProductConfigWrapper
     * using {@link #init}.
     */
    public void resetConfigFull() {
        for (ConfigItem ci: questions) {
            List<ConfigOption> options = ci.getOptions();
            for (ConfigOption co: options) {
                co.setSelected(false);
                co.setComments(null);
                co.componentOptions = null;
            }
        }
    }

    public void setDefaultConfig() {
        resetConfig();
        for (ConfigItem ci: questions) {
            if (ci.isMandatory()) {
                ConfigOption co = ci.getDefault();
                if (co != null) {
                    co.setSelected(true);
                } else if (ci.getOptions().size() > 0) {
                    co = ci.getOptions().get(0);
                    co.setSelected(true);
                }
            }
        }
    }

    public String getConfigId() {
        return configId;
    }

    public Delegator getDelegator() {
        if (delegator == null) {
            delegator = DelegatorFactory.getDelegator(delegatorName);
        }
        return delegator;
    }

    public LocalDispatcher getDispatcher() {
        if (dispatcher == null) {
            dispatcher = ServiceContainer.getLocalDispatcher(dispatcherName, this.getDelegator());
        }
        return dispatcher;
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        // // SCIPIO: 2018-10-09: TODO: REVIEW: Should this be a significant field?
        //result = prime * result + ((delegatorName == null) ? 0 : delegatorName.hashCode());
        result = prime * result + ((product == null) ? 0 : product.hashCode());
        result = prime * result + ((questions == null) ? 0 : questions.hashCode());
        return result;
    }

    @Override
    public boolean equals(Object obj) {
        if (!(obj instanceof ProductConfigWrapper)) {
            return false;
        }
        ProductConfigWrapper cw = (ProductConfigWrapper)obj;
        if (!product.getString("productId").equals(cw.getProduct().getString("productId"))) {
            return false;
        }
        List<ConfigItem> cwq = cw.getQuestions();
        if (questions.size() != cwq.size()) {
            return false;
        }
        for (int i = 0; i < questions.size(); i++) {
            ConfigItem ci = questions.get(i);
            if (!ci.equals(cwq.get(i))) {
                return false;
            }
        }
        return true;
    }

    @Override
    public String toString() {
        return questions.toString();
    }

    public List<ConfigItem> getQuestions() {
        return questions;
    }

    public GenericValue getProduct() {
        return product;
    }

    public void setSelected(int question, int option, String comments) throws Exception {
        ConfigItem ci = questions.get(question);
        List<ConfigOption> avalOptions = ci.getOptions();
        if (ci.isSingleChoice()) {
            for (int j = 0; j < avalOptions.size(); j++) {
                ConfigOption oneOption = avalOptions.get(j);
                oneOption.setSelected(false);
                oneOption.setComments(null);
            }
        }
        ConfigOption theOption = null;
        if (option >= 0 && option < avalOptions.size()) {
            theOption = avalOptions.get(option);
        }
        if (theOption != null) {
            theOption.setSelected(true);
            theOption.setComments(comments);
        }
    }

    public void setSelected(int question, int option, int component, String componentOption) throws Exception {
        //  set variant products
        ConfigOption theOption = getItemOtion(question, option);
        List<GenericValue> components = theOption.getComponents();
        GenericValue oneComponent = components.get(component);
        if (theOption.isVirtualComponent(oneComponent)) {
            if (theOption.componentOptions == null) {
                theOption.componentOptions = new HashMap<>();
            }
            theOption.componentOptions.put(oneComponent.getString("productId"), componentOption);

            //  recalculate option price
            theOption.recalculateOptionPrice(this);
        }
    }

    public List<ConfigOption> getSelectedOptions() {
        List<ConfigOption> selectedOptions = new ArrayList<>();
        for (ConfigItem ci: questions) {
            if (ci.isStandard()) {
                selectedOptions.addAll(ci.getOptions());
            } else {
                for (ConfigOption oneOption: ci.getOptions()) {
                    if (oneOption.isSelected()) {
                        selectedOptions.add(oneOption);
                    }
                }
            }
        }
        return selectedOptions;
    }

    public List<ConfigOption> getDefaultOptions() {
        List<ConfigOption> defaultOptions = new ArrayList<>();
        for (ConfigItem ci: questions) {
            ConfigOption co = ci.getDefault();
            if (co != null) {
                defaultOptions.add(co);
            }
        }
        return defaultOptions;
    }

    public BigDecimal getTotalListPrice() {
        BigDecimal totalListPrice = listPrice;
        List<ConfigOption> options = getSelectedOptions();
        for (ConfigOption oneOption: options) {
            totalListPrice = totalListPrice.add(oneOption.getListPrice());
        }
        return totalListPrice;
    }

    public BigDecimal getTotalPrice() {
        BigDecimal totalPrice = basePrice;
        List<ConfigOption> options = getSelectedOptions();
        for (ConfigOption oneOption: options) {
            totalPrice = totalPrice.add(oneOption.getPrice());
        }
        return totalPrice;
    }

    private void setDefaultPrice() {
        BigDecimal totalPrice = basePrice;
        List<ConfigOption> options = getDefaultOptions();
        for (ConfigOption oneOption: options) {
            totalPrice = totalPrice.add(oneOption.getPrice());
        }
        defaultPrice = totalPrice;
    }

    public BigDecimal getDefaultPrice() {
        return defaultPrice;
    }

    /**
     * SCIPIO: Returns the original list price for the product from initialization.
     * If the product had no list price, this will be null.
     * Added 2017-08-22.
     */
    public BigDecimal getOriginalListPrice() {
        return originalListPrice;
    }

    /**
     * SCIPIO: Returns true if the product had a list price on initialization.
     * NOTE: this is sometimes needed as extra check because {@link #getTotalListPrice} always
     * returns a number (zero) even when there was no original list price.
     * Added 2017-08-22.
     */
    public boolean hasOriginalListPrice() {
        return originalListPrice != null;
    }

    public boolean isCompleted() {
        boolean completed = true;
        for (ConfigItem ci: questions) {
            if (!ci.isStandard() && ci.isMandatory()) {
                List<ConfigOption> availOptions = ci.getOptions();
                for (ConfigOption oneOption: availOptions) {
                    if (oneOption.isSelected()) {
                        completed = true;
                        break;
                    }
                    completed = false;
                }
                if (!completed) {
                    break;
                }
            }
        }
        return completed;
    }

    public ConfigOption getItemOtion(int itemIndex, int optionIndex) {
        if (questions.size() > itemIndex) {
            ConfigItem ci = questions.get(itemIndex);
            List<ConfigOption> options = ci.getOptions();
            if (options.size() > optionIndex) {
                ConfigOption co = options.get(optionIndex);
                return co;
            }
        }

        return null;
    }

    public class ConfigItem implements java.io.Serializable {
        GenericValue configItem;
        GenericValue configItemAssoc;
        ProductConfigItemContentWrapper content;
        List<ConfigOption> options;
        boolean first;

        public ConfigItem(GenericValue questionAssoc) throws Exception {
            configItemAssoc = questionAssoc;
            configItem = configItemAssoc.getRelatedOne("ConfigItemProductConfigItem", false);
            options = new ArrayList<>();
            first = true;
        }

        /**
         * Copy constructor (exactCopy==false).
         */
        public ConfigItem(ConfigItem ci) {
            this(ci, false);
        }
        
        /**
         * Copy constructor.
         * SCIPIO: Added exactCopy flag.
         */
        public ConfigItem(ConfigItem ci, boolean exactCopy) {
            if (exactCopy) {
                // SCIPIO: full copy
                configItem = ci.configItem;
                configItemAssoc = ci.configItemAssoc;
                List<ConfigOption> options = new ArrayList<>();
                for (ConfigOption co: ci.options) {
                    options.add(new ConfigOption(co, exactCopy, this));
                }
                this.options = options;
            } else {
                // legacy
                configItem = GenericValue.create(ci.configItem);
                configItemAssoc = GenericValue.create(ci.configItemAssoc);
                List<ConfigOption> options = new ArrayList<>();
                for (ConfigOption co: ci.options) {
                    options.add(new ConfigOption(co, this));
                }
                this.options = options;
            }
            first = ci.first;
            content = ci.content; // SCIPIO: NOTE: The wrapper is immutable so no need to clone
        }

        /**
         * SCIPIO: Tests to ensure the wrapper is an exact copy of the other; used to verify the exact copy code.
         * NOTE: This is NOT the same as a logical Object equals override! This is mainly for testing.
         */
        void ensureExactEquals(ConfigItem other) {
            try {
                ProductConfigWrapper.ensureExactEquals(this.configItem, other.configItem);
                ProductConfigWrapper.ensureExactEquals(this.configItemAssoc, other.configItemAssoc);
                ProductConfigWrapper.ensureExactEquals(this.content, other.content);
                ProductConfigWrapper.ensureExactEquals(this.options, other.options);
                ProductConfigWrapper.ensureExactEquals(this.first, other.first);
            } catch(IllegalStateException e) {
                throw new IllegalStateException("ConfigItem field not equal: " + e.getMessage(), e);
            }
        }
        
        public void setContent(Locale locale, String mimeTypeId) {
            content = new ProductConfigItemContentWrapper(dispatcher, configItem, locale, mimeTypeId);
        }

        public ProductConfigItemContentWrapper getContent() {
            return content;
        }

        public GenericValue getConfigItem() {
            return configItem;
        }

        public GenericValue getConfigItemAssoc() {
            return configItemAssoc;
        }

        public boolean isStandard() {
            return "STANDARD".equals(configItemAssoc.getString("configTypeId"));
        }

        public boolean isSingleChoice() {
            return "SINGLE".equals(configItem.getString("configItemTypeId"));
        }

        public boolean isMandatory() {
            return configItemAssoc.getString("isMandatory") != null && "Y".equals(configItemAssoc.getString("isMandatory"));
        }

        public boolean isFirst() {
            return first;
        }

        public void setFirst(boolean newValue) {
            first = newValue;
        }

        public void addOption(ConfigOption option) {
            options.add(option);
        }

        public List<ConfigOption> getOptions() {
            return options;
        }

        public String getQuestion() {
            String question = "";
            if (UtilValidate.isNotEmpty(configItemAssoc.getString("description"))) {
                question = configItemAssoc.getString("description");
            } else {
                if (content != null) {
                    question = content.get("DESCRIPTION"); // SCIPIO: don't use "html" here
                    if (question == null) {
                        question = "";
                    }
                } else {
                    question = (configItem.getString("description") != null? configItem.getString("description"): "");
                }
            }
            return question;
        }

        public String getDescription() {
            String description = "";
            if (UtilValidate.isNotEmpty(configItemAssoc.getString("longDescription"))) {
                description = configItemAssoc.getString("longDescription");
            } else {
                if (content != null) {
                    description = content.get("LONG_DESCRIPTION"); // SCIPIO: don't use "html" here
                    if (description == null) {
                        description = "";
                    }
                } else {
                    description = (configItem.getString("longDescription") != null? configItem.getString("longDescription"): "");
                }
            }
            return description;
        }

        public boolean isSelected() {
            if (isStandard()) {
                return true;
            }
            for (ConfigOption oneOption: getOptions()) {
                if (oneOption.isSelected()) {
                    return true;
                }
            }
            return false;
        }

        public ConfigOption getSelected() {
            for (ConfigOption oneOption: getOptions()) {
                if (oneOption.isSelected()) {
                    return oneOption;
                }
            }
            return null;
        }

        public ConfigOption getDefault() {
            String defaultConfigOptionId = configItemAssoc.getString("defaultConfigOptionId");
            if (UtilValidate.isNotEmpty(defaultConfigOptionId)) {
                for (ConfigOption oneOption : getOptions()) {
                    String currentConfigOptionId = oneOption.getId();
                    if (defaultConfigOptionId.compareToIgnoreCase(currentConfigOptionId) == 0 ) {
                        return oneOption;
                    }
                }
            }
            return null;
        }

        @Override
        public boolean equals(Object obj) {
            if (obj == null || !(obj instanceof ConfigItem)) {
                return false;
            }
            ConfigItem ci = (ConfigItem)obj;
            if (!configItem.getString("configItemId").equals(ci.getConfigItem().getString("configItemId"))) {
                return false;
            }
            List<ConfigOption> opts = ci.getOptions();
            if (options.size() != opts.size()) {
                return false;
            }
            for (int i = 0; i < options.size(); i++) {
                ConfigOption co = options.get(i);
                if (!co.equals(opts.get(i))) {
                    return false;
                }
            }
            return true;
        }

        @Override
        public String toString() {
            return configItem.getString("configItemId");
        }
    }

    public class ConfigOption implements java.io.Serializable {
        BigDecimal optionListPrice = BigDecimal.ZERO;
        BigDecimal optionPrice = BigDecimal.ZERO;
        Date availabilityDate = null;
        List<GenericValue> componentList = null; // lists of ProductConfigProduct
        Map<String, String> componentOptions = null;
        GenericValue configOption = null;
        boolean selected = false;
        boolean available = true;
        ConfigItem parentConfigItem = null;
        String comments = null;  //  comments for production run entered during ordering

        public ConfigOption(Delegator delegator, LocalDispatcher dispatcher, GenericValue option, ConfigItem configItem, String catalogId, String webSiteId, String currencyUomId, GenericValue autoUserLogin) throws Exception {
            configOption = option;
            parentConfigItem = configItem;
            componentList = option.getRelated("ConfigOptionProductConfigProduct", null, null, false);
            for (GenericValue oneComponent: componentList) {
                BigDecimal listPrice = BigDecimal.ZERO;
                BigDecimal price = BigDecimal.ZERO;
                // Get the component's price
                Map<String, Object> fieldMap = UtilMisc.toMap("product", oneComponent.getRelatedOne("ProductProduct", false), "prodCatalogId", catalogId, "webSiteId", webSiteId, "currencyUomId", currencyUomId, "productPricePurposeId", "COMPONENT_PRICE", "autoUserLogin", autoUserLogin, "productStoreId",productStoreId);
                Map<String, Object> priceMap = dispatcher.runSync("calculateProductPrice", fieldMap);
                if (ServiceUtil.isError(priceMap)) {
                    String errorMessage = ServiceUtil.getErrorMessage(priceMap);
                    throw new GeneralException(errorMessage);
                }
                BigDecimal componentListPrice = (BigDecimal) priceMap.get("listPrice");
                BigDecimal componentPrice = (BigDecimal) priceMap.get("price");
                Boolean validPriceFound = (Boolean)priceMap.get("validPriceFound");
                BigDecimal mult = BigDecimal.ONE;
                if (oneComponent.getBigDecimal("quantity") != null) {
                    mult = oneComponent.getBigDecimal("quantity");
                }
                if (mult.compareTo(BigDecimal.ZERO) == 0) {
                    mult = BigDecimal.ONE;
                }
                if (validPriceFound) {
                    if (componentListPrice != null) {
                        listPrice = componentListPrice;
                    }
                    if (componentPrice != null) {
                        price = componentPrice;
                    }
                } else {
                    fieldMap.put("productPricePurposeId", "PURCHASE");
                    Map<String, Object> purchasePriceResultMap = dispatcher.runSync("calculateProductPrice", fieldMap);
                    if (ServiceUtil.isError(purchasePriceResultMap)) {
                        String errorMessage = ServiceUtil.getErrorMessage(purchasePriceResultMap);
                        throw new GeneralException(errorMessage);
                    }
                    BigDecimal purchaseListPrice = (BigDecimal) purchasePriceResultMap.get("listPrice");
                    BigDecimal purchasePrice = (BigDecimal) purchasePriceResultMap.get("price");
                    if (purchaseListPrice != null) {
                        listPrice = purchaseListPrice;
                    }
                    if (purchasePrice != null) {
                        price = purchasePrice;
                    }
                }
                optionListPrice = optionListPrice.add(listPrice.multiply(mult));
                optionPrice = optionPrice.add(price.multiply(mult));
                // TODO: get the component's availability date
            }
        }

        /**
         * Copy constructor (exactCopy==false, same parentConfigItem).
         * @deprecated SCIPIO: 2018-11-22: Use an overload with parentConfigItem instead.
         */
        @Deprecated
        public ConfigOption(ConfigOption co) {
            this(co, false, null);
        }

        /**
         * Copy constructor (exactCopy==false).
         */
        public ConfigOption(ConfigOption co, ConfigItem parentConfigItem) {
            this(co, false, parentConfigItem);
        }

        /**
         * Copy constructor.
         * SCIPIO: Added exactCopy.
         */
        public ConfigOption(ConfigOption co, boolean exactCopy, ConfigItem parentConfigItem) {
            if (exactCopy) {
                configOption = co.configOption;
                List<GenericValue> componentList = new ArrayList<>(); // SCIPIO: Use local var
                for (GenericValue component: co.componentList) {
                    componentList.add(component);
                }
                this.componentList = componentList;
                comments = co.getComments();
            } else {
                configOption = GenericValue.create(co.configOption);
                List<GenericValue> componentList = new ArrayList<>(); // SCIPIO: Use local var
                for (GenericValue component: co.componentList) {
                    componentList.add(GenericValue.create(component));
                }
                this.componentList = componentList;
                comments = co.getComments();
            }
            
            this.parentConfigItem = (parentConfigItem != null) ? parentConfigItem : co.parentConfigItem;
            // SCIPIO: 2018-10-09: This must be cloned
            //componentOptions = co.componentOptions;
            componentOptions = (co.componentOptions != null) ? new HashMap<>(co.componentOptions) : null;
            optionListPrice = co.optionListPrice;
            optionPrice = co.optionPrice;
            available = co.available;
            selected = co.selected;
        }

        /**
         * SCIPIO: Tests to ensure the wrapper is an exact copy of the other; used to verify the exact copy code.
         * NOTE: This is NOT the same as a logical Object equals override! This is mainly for testing.
         */
        void ensureExactEquals(ConfigOption other) {
            try {
                ProductConfigWrapper.ensureExactEquals(this.optionListPrice, other.optionListPrice);
                ProductConfigWrapper.ensureExactEquals(this.optionPrice, other.optionPrice);
                ProductConfigWrapper.ensureExactEquals(this.availabilityDate, other.availabilityDate);
                ProductConfigWrapper.ensureExactEquals(this.componentList, other.componentList);
                ProductConfigWrapper.ensureExactEquals(this.componentOptions, other.componentOptions);
                ProductConfigWrapper.ensureExactEquals(this.configOption, other.configOption);
                ProductConfigWrapper.ensureExactEquals(this.selected, other.selected);
                ProductConfigWrapper.ensureExactEquals(this.available, other.available);
                // Skip, otherwise endless loop
                //ProductConfigWrapper.ensureExactEquals(this.parentConfigItem, other.parentConfigItem);
                ProductConfigWrapper.ensureExactEquals(this.comments, other.comments);
            } catch(IllegalStateException e) {
                throw new IllegalStateException("ConfigOption field not equal: " + e.getMessage(), e);
            }
        }
        
        public void recalculateOptionPrice(ProductConfigWrapper pcw) throws Exception {
            optionListPrice = BigDecimal.ZERO;
            optionPrice = BigDecimal.ZERO;
            for (GenericValue oneComponent: componentList) {
                BigDecimal listPrice = BigDecimal.ZERO;
                BigDecimal price = BigDecimal.ZERO;
                GenericValue oneComponentProduct = oneComponent.getRelatedOne("ProductProduct", false);
                String variantProductId = (componentOptions != null) ? componentOptions.get(oneComponent.getString("productId")) : null; // SCIPIO: added null check

                if (UtilValidate.isNotEmpty(variantProductId)) {
                    oneComponentProduct = EntityQuery.use(delegator).from("Product").where("productId", variantProductId).queryOne();
                }

                // Get the component's price
                Map<String, Object> fieldMap = UtilMisc.toMap("product", oneComponentProduct, "prodCatalogId", pcw.catalogId, "webSiteId", pcw.webSiteId, "currencyUomId", pcw.currencyUomId, "productPricePurposeId", "COMPONENT_PRICE", "autoUserLogin", pcw.autoUserLogin, "productStoreId",productStoreId);
                Map<String, Object> priceMap = pcw.getDispatcher().runSync("calculateProductPrice", fieldMap);
                if (ServiceUtil.isError(priceMap)) { // SCIPIO: 2018-10-09: Fixed duplicate calculateProductPrice call
                    String errorMessage = ServiceUtil.getErrorMessage(priceMap);
                    throw new GeneralException(errorMessage);
                }
                BigDecimal componentListPrice = (BigDecimal) priceMap.get("listPrice");
                BigDecimal componentPrice = (BigDecimal) priceMap.get("price");
                Boolean validPriceFound = (Boolean)priceMap.get("validPriceFound");
                BigDecimal mult = BigDecimal.ONE;
                if (oneComponent.getBigDecimal("quantity") != null) {
                    mult = oneComponent.getBigDecimal("quantity");
                }
                if (mult.compareTo(BigDecimal.ZERO) == 0) {
                    mult = BigDecimal.ONE;
                }
                if (validPriceFound) {
                    if (componentListPrice != null) {
                        listPrice = componentListPrice;
                    }
                    if (componentPrice != null) {
                        price = componentPrice;
                    }
                } else {
                    fieldMap.put("productPricePurposeId", "PURCHASE");
                    Map<String, Object> purchasePriceResultMap = pcw.getDispatcher().runSync("calculateProductPrice", fieldMap);
                    if (ServiceUtil.isError(purchasePriceResultMap)) {
                        String errorMessage = ServiceUtil.getErrorMessage(purchasePriceResultMap);
                        throw new GeneralException(errorMessage);
                    }
                    BigDecimal purchaseListPrice = (BigDecimal) purchasePriceResultMap.get("listPrice");
                    BigDecimal purchasePrice = (BigDecimal) purchasePriceResultMap.get("price");
                    if (purchaseListPrice != null) {
                        listPrice = purchaseListPrice;
                    }
                    if (purchasePrice != null) {
                        price = purchasePrice;
                    }
                }
                optionListPrice = optionListPrice.add(listPrice.multiply(mult));
                optionPrice = optionPrice.add(price.multiply(mult));
            }
        }

        public String getOptionName() {
            return (configOption.getString("configOptionName") != null? configOption.getString("configOptionName"): "no option name");
        }

        public String getOptionName(Locale locale) {

            return (configOption.getString("configOptionName") != null? (String) configOption.get("configOptionName", locale): "no option name");
        }

        public String getDescription() {
            return (configOption.getString("description") != null? configOption.getString("description"): "no description");
        }

        public String getDescription(Locale locale) {
            return (configOption.getString("description") != null? (String) configOption.get("description", locale): "no description");
        }

        public String getId() {
            return configOption.getString("configOptionId");
        }

        public String getComments() {
            return comments;
        }

        public void setComments(String comments) {
            this.comments = comments;
        }

        public BigDecimal getListPrice() {
            return optionListPrice;
        }

        public BigDecimal getPrice() {
            return optionPrice;
        }

        public BigDecimal getOffsetListPrice() {
            ConfigOption defaultConfigOption = parentConfigItem.getDefault();
            if (parentConfigItem.isSingleChoice() && UtilValidate.isNotEmpty(defaultConfigOption)) {
                return optionListPrice.subtract(defaultConfigOption.getListPrice());
            }
            // can select multiple or no default; show full price
            return optionListPrice;
        }

        public BigDecimal getOffsetPrice() {
            ConfigOption defaultConfigOption = parentConfigItem.getDefault();
            if (parentConfigItem.isSingleChoice() && UtilValidate.isNotEmpty(defaultConfigOption)) {
                return optionPrice.subtract(defaultConfigOption.getPrice());
            } 
            // can select multiple or no default; show full price
            return optionPrice;
        }

        public boolean isDefault() {
            ConfigOption defaultConfigOption = parentConfigItem.getDefault();
            if (this.equals(defaultConfigOption)) {
                return true;
            }
            return false;
        }

        public boolean hasVirtualComponent () {
           List <GenericValue> components = getComponents();
           if (UtilValidate.isNotEmpty(components)) {
               for (GenericValue component : components) {
                   if (isVirtualComponent(component)) {
                       return true;
                   }
               }
           }

           return false;
       }

        public boolean isVirtualComponent (GenericValue component) {
            int index = getComponents().indexOf(component);
            if (index != -1) {
                try {
                    GenericValue product = component.getRelatedOne("ProductProduct", false);
                    return "Y".equals(product.getString("isVirtual"));
                } catch (GenericEntityException e) {
                    Debug.logWarning(e.getMessage(), module);
                }
            }
            return false;
        }

        public boolean isSelected() {
            return selected;
        }

        public void setSelected(boolean newValue) {
            selected = newValue;
        }

        public boolean isAvailable() {
            return available;
        }

        public void setAvailable(boolean newValue) {
            available = newValue;
        }

        public List<GenericValue> getComponents() {
            return componentList;
        }

        public Map<String, String> getComponentOptions() {
            return componentOptions;
        }


        @Override
        public int hashCode() {
            final int prime = 31;
            int result = 1;
            // SCIPIO: 2018-10-09: TODO: REVIEW: Should this be a significant field?
            //result = prime * result + getOuterType().hashCode();
            //result = prime * result + ((componentList == null) ? 0 : componentList.hashCode());
            // SCIPIO: 2018-10-09: fixed this to use UtilValidate.isNotEmpty, treat null and empty as same
            //result = prime * result + ((componentOptions == null) ? 0 : componentOptions.hashCode());
            result = prime * result + ((UtilValidate.isEmpty(componentOptions)) ? 0 : componentOptions.hashCode());
            return result;
        }


        @Override
        public boolean equals(Object obj) {
            if (obj == null || !(obj instanceof ConfigOption)) {
                return false;
            }
            ConfigOption co = (ConfigOption)obj;
            // SCIPIO: 2018-10-09: fixed missing null/empty checks, and treat null and empty as same
            //if (componentOptions != null && !componentOptions.equals(co.getComponentOptions())) {
            if (UtilValidate.isEmpty(componentOptions)) {
                if (UtilValidate.isNotEmpty(co.getComponentOptions())) {
                    return false;
                }
            } else if (!componentOptions.equals(co.getComponentOptions())) {
                return false;
            }

            return isSelected() == co.isSelected();
        }


        @Override
        public String toString() {
            return configOption.getString("configItemId") + "/" + configOption.getString("configOptionId") + (isSelected()? "*": "");
        }

        @SuppressWarnings("unused")
        private ProductConfigWrapper getOuterType() {
            return ProductConfigWrapper.this;
        }

    }

}
