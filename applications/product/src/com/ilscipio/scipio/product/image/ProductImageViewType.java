package com.ilscipio.scipio.product.image;

import java.io.Serializable;

public class ProductImageViewType implements Serializable {
    protected final String productContentTypeId;
    protected final String viewType;
    protected final String viewNumber;

    protected ProductImageViewType(String productContentTypeId, String viewType, String viewNumber) {
        this.productContentTypeId = productContentTypeId;
        this.viewType = viewType;
        this.viewNumber = viewNumber;
    }

    public static ProductImageViewType from(String viewType, String viewNumber) throws IllegalArgumentException {
        String productContentTypeId;
        viewType = viewType.toLowerCase();
        if (viewType.contains("main")) {
            productContentTypeId = "ORIGINAL_IMAGE_URL";
        } else if (viewType.contains("additional") && viewNumber != null && !"0".equals(viewNumber)) {
            productContentTypeId = "ADDITIONAL_IMAGE_" + viewNumber;
        } else {
            throw new IllegalArgumentException("Invalid viewType/viewNumber: [" + viewType + "/" + viewNumber + "]");
        }
        return new ProductImageViewType(productContentTypeId, viewType, viewNumber);
    }

    public static ProductImageViewType from(String productContentTypeId) throws IllegalArgumentException {
        String viewType;
        String viewNumber;
        if ("ORIGINAL_IMAGE_URL".equals(productContentTypeId) || "LARGE_IMAGE_URL".equals(productContentTypeId)
                || "MEDIUM_IMAGE_URL".equals(productContentTypeId) || "SMALL_IMAGE_URL".equals(productContentTypeId) || "DETAIL_IMAGE_URL".equals(productContentTypeId)) {
            viewType = "main";
            viewNumber = "0"; // TODO: REVIEW: should be "0" or null?
        } else if (productContentTypeId.startsWith("ADDITIONAL_IMAGE_")) {
            viewType = "additional";
            viewNumber = productContentTypeId.substring("ADDITIONAL_IMAGE_".length());
            if (viewNumber.isEmpty()) {
                throw new IllegalArgumentException("Unsupported productContentTypeId [" + productContentTypeId
                        + "]: invalid productContentTypeId name format (ADDITIONAL_IMAGE_n)");
            }
        } else if (productContentTypeId.startsWith("XTRA_IMG_")) { // do the corresponding ADDITIONAL_IMAGE
            viewType = "additional";
            String viewInfo = productContentTypeId.substring("XTRA_IMG_".length());
            String[] parts = viewInfo.split("_", 2);
            if (parts.length != 2 || parts[0].isEmpty() || parts[1].isEmpty()) {
                throw new IllegalArgumentException("Unsupported productContentTypeId [" + productContentTypeId
                        + "]: invalid productContentTypeId name format (XTRA_IMG_n_abc)");
            }
            viewNumber = parts[0];
        } else {
            throw new IllegalArgumentException("Unsupported productContentTypeId [" + productContentTypeId
                    + "]: invalid productContentTypeId name format (XTRA_IMG_n_abc)");
        }
        return new ProductImageViewType(productContentTypeId, viewType, viewNumber);
    }

    public String getProductContentTypeId() {
        return productContentTypeId;
    }

    public String getViewType() {
        return viewType;
    }

    public String getViewNumber() {
        return viewNumber;
    }

    public boolean isMain() {
        return getViewType().contains("main");
    }

    public boolean isAdditional() {
        return !isMain();
    }
}
