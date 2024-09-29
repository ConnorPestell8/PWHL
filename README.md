# PWHL
Welcome to the PWHL Advanced Analytics repository! This project focuses on integrating advanced analytics into the Professional Women’s Hockey League (PWHL), exploring key performance metrics that can enhance team strategies and evaluation.

The primary objectives of this repository are:
* **Exploratory Data Analysis**: Compare expected goals (xG) between the PWHL and the NHL, providing insights into the performance of teams in the inaugural PWHL season.
* **Win Prediction Model**: Developed and evaluated a win prediction model using gradient boosting and logistic regression, leveraging critical metrics such as expected goals, shooting percentage, and save percentage.

Through this analysis, we contributed to the growing body of knowledge in women’s sports analytics, assisting teams in crafting effective strategies and improving overall competition. As the PWHL continues to evolve, this repository serves as a foundation for further enhancements and insights with the data from future seasons.

## File Breakdown
### fixing_shots.R
This script is responsible for cleaning and processing shot data from PWHL games. It includes steps to gather play-by-play data, filter out shot attempts, and adjust shot coordinates to correct any inaccuracies in the dataset. Key components of this script include:
* **Data Gathering**: Collects detailed play-by-play data for each game.
* **Filtering**: Isolates shot attempts from the play-by-play data.
* **Coordinate Adjustment**: Modifies shot coordinates to ensure accurate representation on the rink, addressing any discrepancies noted in the data production phase
### PWHL_Shots_Analysis.R
This script processes shot data specifically from the PWHL for the 2024 season. It creates visualizations that represent shot locations on the rink. Key components of this script include:
* **Data Loading**: Loads cleaned shot data into the environment.
* **Visualization**: Generates shot location plots, allowing for a visual analysis of where shots are taken during games.
### NHL_Shots_Analysis.R
This script handles shot data from the NHL for the seasons 2014-2022. Similar to the PWHL analysis, it visualizes shot locations on the rink, helping to identify trends and patterns. Key components of this script include:
** **Data Loading**: Loads cleaned shot data into the environment.
* **Visualization**: Generates shot location plots, allowing for a visual analysis of where shots are taken during games.
### PWHL_Rolling_Avg_Analysis.R
This script transforms play-by-play data from each game into a set of rolling weighted statistics for each team. The weighted statistics on a game-by-game basis are saved to be used in GraphCreation.R to visualize the progression of the statistics for each team at every game in the season. Key components of this script include:
* **Data Loading**: Loads in deatiled play-by-play data for each game.
*
### GraphCreation.R
This script is creates the graphs showing the progression of the weighted statistics 
