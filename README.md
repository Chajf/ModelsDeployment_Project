# Visual Model bUIlding

<p align="center">
    <a href="https://michciak.shinyapps.io/marek_kozinski/">Check deployed version!</a>
</p>

## Project description

The web application, built using R Shiny, allows users to upload and analyze their datasets in an interactive and user-friendly manner.
It is designed to facilitate both exploratory data analysis and the creation of machine learning models without requiring users to have extensive programming knowledge.
The app provides robust tools for data preprocessing, visualization, and model evaluation, making it a comprehensive solution for data analysis and predictive modeling.

### Project's problem area

Many individuals and organizations struggle with analyzing their data and building predictive models due to a lack of technical expertise in programming and statistics.
This application addresses these challenges by providing an intuitive interface that simplifies the data analysis process.
It helps users clean their data, explore its characteristics, and build predictive models with minimal effort.
The application is particularly useful for educators, researchers, and business analysts who need to derive insights from their data quickly and efficiently.

## Functionalities

#### Data Upload and Management
- Upload your own datasets in CSV format.

<p align="center">
  <img src="/images/placeholder.png">
</p>

- View and manage uploaded data.
- Impute missing values.
- Display data in a tabular format.

<p align="center">
  <img src="/images/placeholder.png">
</p>

#### Data Exploration and Visualization
- Calculate and display basic statistics for numeric and character variables.

<p align="center">
  <img src="/images/placeholder.png">
</p>

- Generate plots for single variables (histograms, bar charts).
- Create distribution plots for up to two variables (scatter plots, box plots).

<p align="center">
  <img src="/images/placeholder.png">
</p>


#### Machine Learning Model Creation
- Select target variable for classification or regression tasks.
- Choose model type (e.g., decision tree, random forest).
- Set train-test split proportion.
- Apply preprocessing steps (e.g., normalization, encoding categorical variables).
- Train the selected model and evaluate its performance.

<p align="center">
  <img src="/images/placeholder.png">
</p>

#### Model Evaluation and Predictions
- Display performance metrics for train and test sets.
- Provide summary reports of model performance.

<p align="center">
  <img src="/images/placeholder.png">
</p>

### Tasks

- [x] Project description

>Assigne: Michał and Patryk

- [x] Design of user interface layout:

    - Creating concept of graphical user interface

    - Identify key functionalities for user interface

<p align="center">
  <img src="/layout_concpet.png">
</p>

>Assigne: Michał and Patryk

- [x] Implementation of user interface concept - creating UI layout based on created concept

>Assigne: Michał and Patryk

- [x] GUI visual improvements - making application look astonishing

>Assigne: Michał

- [x] Providing data loading and variable type selection funcionality

>Assigne: Patryk

- [x] Providing data imputation funcionality

>Assigne: Michał

- [x] Implementation of Data Exploration Tools:

    - Write functions to calculate basic statistics.

    - Develop plotting features for single and dual variable distributions.

>Assigne: Michał

- [x] Machine Learning Module Development:
    
    - Integrate machine learning library for model training with shiny app.
    
    - Create options for selecting target variables and model types.
    
    - Implement train-test split functionality and preprocessing steps.

>Assigne: Patryk

- [x] Model Evaluation and Reporting:
    
    - Develop modules to calculate and display performance metrics.
    
    - Generate summary reports for model evaluation - metrics with predictions in tabular form.

>Assigne: Patryk

- [x] Testing and Debugging:
    
    - Perform extensive testing to identify and fix bugs.
    
    - Ensure that the application works smoothly in various scenarios.

>Assigne: Michał and Patryk

## Project Plan and Future

### Done:

- [x] Definition of the project's problem area
- [x] First layout conept
- [x] Project description
- [x] Tasks definition
- [x] Tasks assignment
- [x] First running build
- [x] Functionality testing
- [x] Deployment

### Plans:

- [ ] Containerization
- [ ] More model types
- [ ] More preprocessing steps
- [ ] Model downloading functionalities
- [ ] Generating reports from dataset
- [ ] Allowing other file types than CSV