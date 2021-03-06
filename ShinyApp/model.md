# Prediction Model

The model used for predicting the survival rate in this Shiny App calculates the probability of passenger survival based on their sex, class of travel, title, travelling with a family or not, age and which port they departed from. A **Random Forest** was used to create the model.  

The model was created offline and stored in an RDS file which is then read in during the loading of the App. This speeds up the loading of the App web page since the generation of the model can take some time.

This model has an accuracy of approximately 0.77 (which is not great but for the purposes of this Shiny App demonstration, it will suffice.)

For the full R code used to generate the model, please visit the github repository at https://github.com/dvd940/Developing.Data.Products/tree/master/model
