targetRecordAction = context.targetRecordAction;
                    submitFormIdMap = [
                        "catalog-new": "NewCatalog",
                        "catalog-edit": "EditCatalog",
                        "catalog-add": "AddCatalog",
                        "category-new": "NewCategory",
                        "category-edit": "EditCategory",
                        "category-add": "AddCategory",
                        "product-new": "NewProduct",
                        "product-edit": "EditProduct",
                        "product-add": "AddProduct",
                    ];
                    context.submitFormIdMap = submitFormIdMap;
                    context.titleProperty = (targetRecordAction == "catalog-new" ) ? "ProductNewCatalog" : "ProductEditCatalog"; // forms have their own section title, so not bothering with this
                    context.submitFormId = submitFormIdMap[targetRecordAction];