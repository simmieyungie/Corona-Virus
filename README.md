# Corona-Virus
Create a shiny App on Sentiment Analysis of Corona Virus


## E-Rum 2020 Covid'19 contest Dashboard
This dashboard was built using Shiny R framework. It was designed to using twitter's API to collect tweets and perform real time sentiment Analysis on the collection. Find the Shiny app hosted [here[(https://simmie.shinyapps.io/Corona_Dashboard/?_ga=2.227603421.1973774054.1590876545-1563876717.1589398630).

## Components
1. There is a slider that controls how many tweets should be returned. The default amount is 500 and the highest is 18000. It is noteworthy that the higher the tweets, the longer it may take for  the app to load. 
2. On the **default tab**, you have Result of the **Sentiment Analysis**. This includes;
- NRC Sentiments which shows the overall reactions of the tweets.
- Top 10 words occuring in the retrieved tweets.
- A Word cloud 
- Top 10 most positive and most negative words used.
- Sentiment Polarity: This shows the extreme negativity, positivity or neutrality of each tweet. Analyzed using the Bing Lexicon.

3. On the next tab, you have the real-time covid'19 statistics. It provides a general and simple overview of the covid'19 pandemic.
- Total recoveries
- Total deaths
- Total active cases
- Total infections 

4. There is a tweet table in the second page. It presents each tweets in a searchable and interactive table. You can search the entire tweets or column wise to find what you're interested in. The symboll **>>** is a clickable link to where any tweet originates from on twitter itself.

5. The about app page presents a simple description of the page. 

#### The App View
