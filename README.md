# Bachelor_Thesis_2017_Sources

Content:

* GIS
* Notebook
  * DataAnalysis -> Analyse des données avec python
    * data -> DATASETS
    * 0_DataCleaning.ipynb -> Nettoyage de la première version du dataset et génération des versions suivantes
    * 1_Climatic and Environnement Data Exploration.ipynb -> Exploration des données climatiques et de sol
    * 3_SOM_Exploration -> Exécution et tests des Self Organizing Maps
    * 4_RandomForest.ipynb -> Vide, a été exécuté avec R pour des raisons de disponibilité de serveurs
    * 5_CorrelationMatrix.ipynb -> Matrices de corrélations diverses.
    * KohonenUtils.py -> script pour SOM, inutilisé mais intéressant.  

  * Preprocessing -> Scripts python d'extraction et de merging des divers données
    * Averages.py -> Permet l'extraction des données climatiques
    * AUTRES: étapes d'agglomération divers à partir de sources de données multiples



  * R
    * Projects/BachelorThesis/ -> contient le projet principal avec R
      * PCA -> scripts pour les PCAs
      * PLS -> scripts pour les PLSs
      * SOM -> scripts pour les SOMs
      * PartialPlots -> Résultat par variable de Random Forest
      * VariabilityAnalysis -> analyse des modèles créés par RandomForest avec uniquement les variables peut corrélées (haute variabilité). Contient aussi les partial plots.
      * VARIETY_ANALYSIS -> Contient pour chaque Variété de café des analyses descriptives de base
    * ViewSOM -> Quelques tests avec SOM
    * Draft_Tests -> Brouillons, entrainement
