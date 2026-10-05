if (context.result?.listIt != null) {
                        context.invoices = result.listIt.getCompleteList();
                        result.listIt.close();
                    } else {
                        context.invoices = [];
                    }