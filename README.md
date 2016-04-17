# Sports_Hackathon
Score trend before and after regular timeout

__Project name__: Score trend before and after regular timeout
__Team name__: Ace Hackers
__Team members__: Wuge Zhou, Jing Mu, Yinxiang Gao, Yue Wang
__Data source__: 2011-2012 playbyplay data (http://basketballvalue.com/downloads.php)

Project description:
    Our team tries to analyze how effective the time-out strategy is in NBA games. At first, we classify time-outs into two groups, namely regular time-out and time-out at end-of-game. Usually speaking, regular time-out is used to stop opponent team momentum and time-out at end-of- game may be more complex. Our project focuses on the regular time-out. Our goal is to find the relationship between regular time-out and final scores. In the following, we will describe our projects in detail.
    Firstly, we clean data and extract the time-out time and real-time scores from the original ugly data.
    Secondly, we make statistical description on time-out data to explore data structure. By drawing the histogram of the time-out data, we try to find the pattern when coaches use time-out. Also, we split time-out data into home team and away team and draw histogram of them separately. In addition, we separate time-out data into high final score difference and low final score difference and draw time-out data of them separately. By studying these plots, we explore the time-out data structure intuitively.
    Finally, we define time-out efficiency as (d1-d2)-(d2-d3) for home team and as (d2-d3)-(d1-d2) for away team, where d1 is the score difference when 6 score changes happening before time-out, d2 is the score difference at time-out and d3 is the score difference when 6 score changes happening after time-out. Because (d1-d2) means the change of score difference before time-out and (d2-d3) means the change of score difference after time-out, so the greater the time-out efficiency, the better the time-out strategy. Next, we perform regression between efficiency and final scores. By studying the relationship, we could dig out the efficiency of time-out usage.


