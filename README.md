# Russian Public Opinion Tracker
**UPDATE**: The tracker is no longer being maintained.  
## Summary

This project tracked weekly public opinion polls on Vladimir Putin's popularity starting with his third term in office (beginning May 2012). It combines published data from the [**Levada Center**](https://www.levada.ru/indikatory/), the [**Public Opinion Foundation**](https://fom.ru/Politika/10946) (FOM), and the [**Russian Public Opinion Research Center**](https://wciom.ru/ratings/dejatelnost-gosudarstvennykh-institutov/) (VtSIOM). VtSIOM is a state-owned corporation and FOM is closely linked with the Presidential Administration. Levada is generally considered to be the most independent pollster in Russia, although it operates in an environment with considerable challenges to obtaining accurate estimates of public opinion. As an interesting aside, Levada and FOM were both founded as breakaway companies from VtSIOM, which itself was founded as the All-Union Center for the Study of Public Opinion during Perestroika.

You can view the tracker [here](https://biznesslanch.github.io/Russian-Public-Opinion-Tracker/opinion_tracker.html). Weekly survey-specific estimates of approval and disapproval are displayed as blue and red points. The solid lines represent loess regression lines estimated over the data and the shaded areas represent 95% confidence intervals for those loess-estimates. 

Interestingly, approval estimates for all three surveys are similar to each other, but disapproval ratings in the Levada data are typically higher than the other two sources. The reason appears to be that percentage of respondents with 'No Answer' responses in the Levada data is much lower than 'Hard to Say' responses in the FOM data (the VtSIOM don't include this response). In the last data points for the two surveys (late September and early October 2020), for example, 'No Answer' was estimated to be 1% in the Levada data vs. 15% in the FOM data. Even with differences in response wording, the gap in disapproval ratings between the surveys is mostly a function of this gap . Given that Putin had been in office for over 20 years at this point, it's up to the reader about whether it's plausible that 15% of people had no opinion of him at that point.

## Code repository

These files include all of the data and code needed to produce the Russian Public Opinion Tracker that was published at Biznesslanch.
The tracker was updated with new data on a weekly basis, but several updates to the structure of the FOM and VtSIOM pages hosting the data made updating the web-scraping script challenging. I've left it here as an example project that could be extended or picked up later.


