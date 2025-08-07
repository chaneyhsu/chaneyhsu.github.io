# Portfolio

#### Technical Skills: Python, R, STATA, SQL

#### [Connect with me on LinkedIn](https://www.linkedin.com/in/chenweihsu/)

## Education
- M.S., Applied Data Analytics @ Boston University (_December 2025_)
- B.A., Economics, Digital Marketing & Analytics @ University of Connecticut (_May 2024_)

## Projects
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
- [Presentation]()

### College Basketball Strategy App using KenPom Data
- Designed and deployed a multi-functional scouting app in Streamlit that analyzes NCAA Division I basketball teams using KenPom advanced stats
- Enabled team comparisons based on Adjusted Tempo, Adjusted Offensive Efficiency (ORtg), and Adjusted Defensive Efficiency (DRtg) to support lineup strategy decisions
- Built a “Game Risk & Volatility Estimator” that quantifies upset risk using differences in Net Rating, Strength of Schedule, and Luck, generating narrative summaries and confidence scores
- Integrated fuzzy string matching and auto-suggestions to handle user input errors and improve usability for coaches
- [Streamlit Web App](https://basketball-strategy-tool.streamlit.app/)
- [Presentation]()
