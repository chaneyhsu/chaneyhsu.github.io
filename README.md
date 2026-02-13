# Portfolio

#### Technical Skills: Python, R, STATA, SQL

#### [Connect with me on LinkedIn](https://www.linkedin.com/in/chenweihsu/)

## Education
- M.S., Applied Data Analytics @ Boston University (_December 2025_)
- B.A., Economics, Digital Marketing & Analytics @ University of Connecticut (_May 2024_)

## Projects

### Data Warehousing & ETL Process for 311 Service Analytics
- Architected and implemented an end-to-end ETL pipeline in Python (Pandas) and SQL to ingest, cleanse, and transform large-scale municipal 311 service request datasets into structured warehouse tables with well-defined grains
- Designed a constellation schema in PostgreSQL with multiple fact tables (Service_Requests_Fact, Service_Performance_Fact) and conformed dimensions (Date_DIM, Location_DIM, Service_DIM, Department_DIM) to enable scalable cross-domain reporting
- Built an automated pipeline workflow to extract raw data, execute transformation scripts, validate outputs, and load dimensional models with minimal manual intervention
- Implemented Slowly Changing Dimensions (SCD Type 2 and Type 3) to preserve historical attribute changes while maintaining temporal integrity
- Developed data quality validation checks including null audits, duplicate detection, referential integrity enforcement, and SLA/on-time performance flag logic
- Structured the warehouse using a layered architecture (raw → staging → cleaned → dimensional) to ensure modularity, reproducibility, and maintainability
- Optimized query performance through indexing strategies, incremental loading, and normalization techniques
- [Constellation Schema](https://github.com/chaneyhsu/chaneyhsu.github.io/blob/aad04a190c5f8747ea54d860974014ec49a1a781/assets/files/311_Proj_Constellation_Schema.pdf)
- [SQL Code](https://github.com/chaneyhsu/chaneyhsu.github.io/blob/aad04a190c5f8747ea54d860974014ec49a1a781/assets/files/311_Project.sql)
- [ETL Process](https://github.com/chaneyhsu/chaneyhsu.github.io/blob/aad04a190c5f8747ea54d860974014ec49a1a781/assets/files/311_Project_ETL_Process.ipynb )

### Schema Design & Storage Model Analysis for Analytical Workloads
- Designed and implemented three alternative storage architectures—normalized schema (3NF), star schema, and semi-structured JSON model—across PostgreSQL and Snowflake to evaluate trade-offs in analytical workloads
- Conducted controlled benchmarking experiments to compare query execution time, storage footprint, join complexity, and aggregation efficiency across schema designs
- Analyzed how schema normalization levels impact join depth, query readability, and optimizer behavior under large-scale analytical queries
- Evaluated performance implications of columnar storage (Snowflake) versus row-based storage (PostgreSQL), including differences in compression, scan efficiency, and predicate pushdown
- Quantified trade-offs between schema flexibility and analytical performance when using semi-structured JSON models versus structured dimensional designs
- Documented schema-driven impacts on query planning, execution paths, and maintainability for enterprise-scale reporting systems
- [Snowflake Code](https://github.com/chaneyhsu/chaneyhsu.github.io/blob/381c8a72ffe66ce708467923bf0bba3d04b9624c/assets/files/Snowflake_311.sql)
- [SQL Code](https://github.com/chaneyhsu/chaneyhsu.github.io/blob/381c8a72ffe66ce708467923bf0bba3d04b9624c/assets/files/Snowflake_311_Project.sql)
- [BCNF Testing](https://github.com/chaneyhsu/chaneyhsu.github.io/blob/381c8a72ffe66ce708467923bf0bba3d04b9624c/assets/files/Snowflake_Prof_BCNF.pdf )
- [Star Schema and ERD](https://github.com/chaneyhsu/chaneyhsu.github.io/blob/381c8a72ffe66ce708467923bf0bba3d04b9624c/assets/files/Snowflake_Proj_Star_Schema_ERD.pdf )

### Statistical Methods for Meta-Analysis of Mindfulness-Based Stress Reduction Studies
- Co-authored a preprint developing and presenting systematic methods and code for empirically estimating pre-post correlations in repeated measures designs, addressing common gaps in meta-analytic studies
- [Extracting Pre-Post Correlations for Meta-Analyses of Repeated Measures Designs](https://osf.io/hzj4d/)

### Predictive Modeling for Quality of Life Classification Using ACS Data
- Developed and compared 36 classification models to predict independent living difficulty from U.S. Census data
- Applied LASSO, Random Forest, and Recursive Feature Elimination in RStudio for dimensionality reduction and feature selection
- Balanced dataset using oversampling and class weights to boost minority class detection
- Achieved 84.2% true positive rate in final model; assessed performance via ROC-AUC, F1, and MCC
- [Paper](https://github.com/chaneyhsu/chaneyhsu.github.io/blob/83e8c37fabfee157509b5d3470dd1057ae6b04fb/assets/files/Predictive%20Modeling%20for%20Quality%20of%20Life%20Classification%20Using%20ACS%20Data.pdf)
- [RStudio Code](https://github.com/chaneyhsu/chaneyhsu.github.io/blob/82450e198b1d68bfe69da57dea84aea9ee14e559/assets/files/Predictive%20Modeling%20for%20Quality%20of%20Life%20Classification%20Using%20ACS%20Data.R)

### Real Estate Cost Analysis
- Preprocessed and cleaned real estate data, building a regression model with R, engineering key features (price per sqft, property age, convenience), and applying data normalization techniques (Z-score scaling) to enhance model accuracy in predicting cost trends, leading to improved accuracy and more reliable predictions
- Evaluated model performance using R-squared and Mean Squared Error, optimizing hyperparameters to improve accuracy and provide insights that enhanced business decisions and cost optimization
- [Presentation](https://github.com/chaneyhsu/chaneyhsu.github.io/blob/82450e198b1d68bfe69da57dea84aea9ee14e559/assets/files/Real%20Estate%20Cost%20Analysis.pdf)
- [RMarkdown Code](https://github.com/chaneyhsu/chaneyhsu.github.io/blob/82450e198b1d68bfe69da57dea84aea9ee14e559/assets/files/Real%20Estate%20Cost%20Analysis.Rmd)

### Evaluation of Predictive Algorithms
- Explored and evaluated Artificial Neural Networks in a team of 3, focusing on Recurrent Neural Networks (RNNs), Long Short-Term Memory (LSTM) Networks, and Gated Recurrent Units (GRUs), developing and training models in Python to assess performance across various tasks
- Implemented RNNs, LSTMs, and GRUs, identifying that LSTMs excel in time-series analysis, while GRUs and RNNs outperform simpler tasks, improving model efficiency
- [Paper with Python Code](https://github.com/chaneyhsu/chaneyhsu.github.io/blob/82450e198b1d68bfe69da57dea84aea9ee14e559/assets/files/Evaluation%20of%20Predictive%20Algorithms.pdf)

### Statistical Methods For Air Permeability in Textiles Research
- Validated and recreated the analysis of textile data using multiple linear regression to predict air permeability based on weft/warp yarn densities and fabric mass per unit area; achieved R² = 0.84 in baseline model
- Evaluated model assumptions using ANOVA, t-tests, and variance inflation factor (VIF) to identify and address multicollinearity
- Enhanced original study by conducting tertiary regression models excluding individual predictors, and compared their statistical performance
- Validated significant predictors and model fit through hypothesis testing (α = 0.05) and visualized model diagnostics using R
- Concluded weft and warp densities were the most significant predictors; recommended reduced models for improved interpretability without sacrificing predictive power
- [Paper with RMarkdown Code](https://github.com/chaneyhsu/chaneyhsu.github.io/blob/82450e198b1d68bfe69da57dea84aea9ee14e559/assets/files/Statistical%20Methods%20For%20Air%20Permeability%20in%20Textiles%20Research.pdf)

### Time Series Analytics for Stock Market Forecasting
- Built a forecasting model for Microsoft (MSFT) stock using Python and time series methods including STL decomposition, ARIMA, and exponential smoothing to analyze trends and seasonality
- Forecasted MSFT stock trends using ARIMA and exponential smoothing, reducing prediction error by 20% vs. baseline
- Engineered features such as lag variables, moving averages, and rolling statistics to enhance predictive performance
- Evaluated model performance using backtesting and RMSE, achieving low forecast error across multiple test windows
- Compared forecasting approaches (ARIMA, naive seasonal baseline) and visualized results to highlight model improvements
- [Presentation](https://github.com/chaneyhsu/chaneyhsu.github.io/blob/67f860904e1c4cb75d4795cfe95a590aac11e9d1/assets/files/time-series-presentation.pdf)

### College Basketball Strategy App using KenPom Data
- Designed and deployed a multi-functional scouting app in Streamlit that analyzes NCAA Division I basketball teams using KenPom advanced stats
- Enabled team comparisons based on Adjusted Tempo, Adjusted Offensive Efficiency (ORtg), and Adjusted Defensive Efficiency (DRtg) to support lineup strategy decisions
- Built a “Game Risk & Volatility Estimator” that quantifies upset risk using differences in Net Rating, Strength of Schedule, and Luck, generating narrative summaries and confidence scores
- Integrated fuzzy string matching and auto-suggestions to handle user input errors and improve usability for coaches
- [Streamlit Web App](https://basketball-strategy-tool.streamlit.app/)
- [Presentation](https://github.com/chaneyhsu/chaneyhsu.github.io/blob/67f860904e1c4cb75d4795cfe95a590aac11e9d1/assets/files/basketball-analytics-presentation.pdf)

### College Basketball Lineup Strategy Analytics
- Developed an interactive lineup optimization dashboard in Streamlit to simulate team offensive efficiency based on projected rotation minutes
- Modeled weighted lineup impact using player-level advanced metrics (Net Rating, ORtg, Usage, eFG%) to estimate aggregate team performance
- Built scenario-based simulations allowing adjustment of tempo, shot profile, and usage distribution to evaluate strategic fit
- Implemented dynamic On/Off impact calculations to quantify marginal efficiency gains from lineup substitutions
- Designed player radar visualizations to profile role archetypes and support coaching decision-making
- Applied data validation, minute-weight normalization, and sensitivity testing to ensure projection stability
- [Streamlit Web App - Lineup Strategy Analytics](https://lineup-strategy-analytics.streamlit.app/)
