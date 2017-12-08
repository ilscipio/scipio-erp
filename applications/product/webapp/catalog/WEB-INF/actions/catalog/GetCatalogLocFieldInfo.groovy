/**
 * SCIPIO: Describes the simple text content ProductContent and ProductCategoryContent field types.
 * TODO: REVIEW: I'm not sure the best place to store this yet.
 */

catalogLocFieldInfo = [
    category: [
        fieldNames: ["categoryName", "description", "longDescription"],
        typeNames: ["CATEGORY_NAME", "DESCRIPTION", "LONG_DESCRIPTION"],
        typeNameListStr: '[CATEGORY_NAME, DESCRIPTION, LONG_DESCRIPTION]', // used by catalog tree
        typeInfo: ["LONG_DESCRIPTION":["isLong":true]],
        typeField: "prodCatContentTypeId"
    ],
    product: [
        fieldNames: ["productName", "description", "longDescription"],
        typeNames: ["PRODUCT_NAME", "DESCRIPTION", "LONG_DESCRIPTION"],
        typeNameListStr: '[PRODUCT_NAME, DESCRIPTION, LONG_DESCRIPTION]',
        typeInfo: ["LONG_DESCRIPTION":["isLong":true]],
        typeField: "productContentTypeId"
    ] 
];
context.catalogLocFieldInfo = catalogLocFieldInfo;