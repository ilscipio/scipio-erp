storeLocales = org.ofbiz.product.store.ProductStoreWorker.getStoreLocales(request);
                    if (storeLocales) {
                        context.availableLocales = storeLocales;
                    }