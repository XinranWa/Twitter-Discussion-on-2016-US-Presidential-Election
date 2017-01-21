# Twitter Discussion on 2016 US Presidential Election
Using the data of one month's tweets sorted out by candidates' names, we examined how each candidate was presented on Twitter and predicted the trend in discussion. Data was collected and processed using Twitter API, and was further cleaned in R to classify by candidate, location and discussion topic.

Data visualizations such as word cloud and geographic map were developed in RShiny and Gephi. We also used JavaScript libraries (d3, dc, crossfilter) to generated interactive crossfilter and bubbly overlay chart so that users could easily navigate by candidates, parties, topics discussed, time and tweets' sentiment.

Furthermore, we conducted text analysis and developed prediction models by applying statistical methods to Twitter metrics. The predictive model selected by Stepwise algorithm and LASSO implied a strong relationship between poll, tweets sentiment and volumes of tweets. However this relationship should not be considered as any causal relationship. 
