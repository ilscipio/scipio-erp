SCIPIO: These are optional files for solr configsets, which cannot
cannot be included in our active stock configsets by default.

The files under default/ are automatically copied to each solr/configsets conf 
on build if missing; others you may copy and modify yourself. Note that you may
need to tweak the .gitignore file under your configset depending on your
project setup needs.

You may find further examples in the appropriate Solr sources under:

  solr/server/solr/configsets/sample_techproducts_configs/conf

Solr website:

  https://lucene.apache.org/solr/
